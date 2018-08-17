{-# LANGUAGE OverloadedStrings #-}

module CSV2Ledger
  ( FormatSpec(..), defaultFormatSpec, dropUtf8BOM, loadCsvFiles
  , euro, postExpense, postIncome, postExpense', postIncome'
  , simplePosting, isIncomeTransaction, isExpenseTransaction
  , mkTag, (=~), postLiability, postLiability'
  )
  where

import Control.Monad as M
import Data.ByteString.Lazy as BS ( ByteString, pack, stripPrefix )
import Data.ByteString.Lazy.UTF8 as BS ( fromString )
import Data.Decimal
import Data.List ( nub, sortOn, sortBy )
import Data.Maybe
import Data.Text as T ( Text, null, unpack )
import Data.Vector as V ( Vector, toList )
import Hledger.Data
import Hledger.Utils.Regex
import System.IO as IO

data FormatSpec a = FormatSpec
  { filenameOrder :: FilePath -> FilePath -> Ordering
  , fileEncoding :: TextEncoding
  , skipLines :: Int
  , decodeCSV :: ByteString -> Either String (Vector a)
  , toTransaction :: a -> Transaction
  }

defaultFormatSpec :: FormatSpec a
defaultFormatSpec = FormatSpec
  { filenameOrder = compare
  , fileEncoding = utf8
  , skipLines = 0
  , decodeCSV = error "back-end must implement decodeCSV"
  , toTransaction = error "back-end must implement toTransaction"
  }

dropUtf8BOM :: ByteString -> ByteString
dropUtf8BOM x = fromMaybe x (stripPrefix bom x)
  where
    bom :: ByteString
    bom = BS.pack [0xEF,0xBB,0xBF]

----- Tools for implementing toTransaction

euro :: Decimal -> Amount
euro n = amount { acommodity = "EUR"
                , aquantity = roundTo 2 n
                , astyle = amountstyle { asprecision=2, ascommodityspaced=True }
                }

postExpense :: AccountName -> Transaction -> Posting
postExpense acc = postExpense' acc missingamt

postExpense' :: AccountName -> Amount -> Transaction -> Posting
postExpense' acc amt t
  | isExpenseTransaction t = simplePosting ("Ausgaben:" <> acc) amt
  | otherwise              = error $ "invalid expense:\n" ++ show t

postIncome :: AccountName -> Transaction -> Posting
postIncome acc = postIncome' acc missingamt

postIncome' :: AccountName -> Amount -> Transaction -> Posting
postIncome' acc amt t
  | isIncomeTransaction t = simplePosting ("Einkommen:" <> acc) amt
  | otherwise             = error $ "invalid income:\n" ++ show t

simplePosting :: AccountName -> Amount -> Posting
simplePosting acc amt = nullposting
  { pamount = if amt == missingamt then missingmixedamt else mixed [amt]
  , paccount = acc
  }

postLiability :: AccountName -> Posting
postLiability acc = postLiability' acc missingamt

postLiability' :: AccountName -> Amount -> Posting
postLiability' acc = simplePosting ("Schulden:" <> acc)

isIncomeTransaction :: Transaction -> Bool
isIncomeTransaction = not . isExpenseTransaction

isExpenseTransaction :: Transaction -> Bool
isExpenseTransaction t = all (fromMaybe err . isNegativeMixedAmount . pamount) (tpostings t)
  where err = error ("too many postings in transaction:\n" ++ show t)

mkTag :: Text -> Text -> [Text]
mkTag tag content = [tag <> ": " <> content | not (T.null content) ]

(=~) :: Text -> Regexp -> Bool
str =~ regexp = regexMatchesCI regexp (T.unpack str)

loadCsvFiles :: Eq a => FormatSpec a -> [FilePath] -> IO [Transaction]
loadCsvFiles format files = do
  ts' <- M.forM (sortBy (filenameOrder format) files) $ \fn ->
    withFile fn ReadMode $ \h -> do
      hSetEncoding h (fileEncoding format)
      _ <- M.replicateM (skipLines format) (hGetLine h)
      buf <- IO.hGetContents h
      case decodeCSV format (dropUtf8BOM (fromString buf)) of
        Left err -> fail err
        Right rs -> return (V.toList rs)
  return $ sortOn tdate (map (toTransaction format) (nub (concat ts')))