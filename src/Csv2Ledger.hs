{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}

module Csv2Ledger
  ( FormatSpec(..), defaultFormatSpec, dropUtf8BOM, loadCsvFiles
  , euro, postExpense, postIncome, postExpense', postIncome'
  , simplePosting, isIncomeTransaction, isExpenseTransaction
  , mkTag, (=~), postLiability, postLiability', collapsSpace
  , simpleMain, pCurrency, parseAmount
  )
  where

import "base-compat" Prelude.Compat

import Control.Monad as M
import Control.Monad.State.Strict
import Data.ByteString.Lazy as BS ( ByteString, pack, stripPrefix )
import Data.ByteString.Lazy.UTF8 as BS ( fromString )
import Data.Decimal
import Data.List ( nub, sortOn, sortBy )
import Data.String ( IsString(..) )
import Data.Maybe
import Data.Text ( Text )
import qualified Data.Text as T
import Data.Vector as V ( Vector, toList )
import Hledger.Data
import Hledger.Read
import Hledger.Utils.Regex
import System.Environment
import System.IO as IO
import qualified Text.Megaparsec as Parsec ( runParser, eof )

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
                , astyle = amountstyle { asprecision = Precision 2, ascommodityspaced = True }
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

-- |A convenient variant of 'regexMatch' that copes with 'Text' rather than
-- 'String' data.
(=~) :: Text -> Regexp -> Bool
str =~ regexp = regexMatch regexp (T.unpack str)

loadCsvFiles :: Eq a => FormatSpec a -> [FilePath] -> IO [Transaction]
loadCsvFiles format files = do
  ts' <- M.forM (sortBy (filenameOrder format) files) $ \fn ->
    withFile fn ReadMode $ \h -> do
      hSetEncoding h (fileEncoding format)
      _ <- M.replicateM (skipLines format) (hGetLine h)
      buf <- IO.hGetContents h
      case decodeCSV format (dropUtf8BOM (BS.fromString buf)) of
        Left err -> fail err
        Right rs -> return (V.toList rs)
  return $ sortOn tdate (map (toTransaction format) (nub (concat ts')))

simpleMain :: Eq a => FormatSpec a -> IO ()
simpleMain fmt = getArgs >>= loadCsvFiles fmt >>= mapM_ (putStr . showTransactionUnelided)

pCurrency :: String -> Text -> Text -> Amount
pCurrency ctx curr amt = parseAmount ctx (curr <> " " <> amt)

parseAmount :: String -> Text -> Amount
parseAmount ctx input =
  case Parsec.runParser (evalStateT (amountp <* Parsec.eof) nulljournal) ctx input of
     Left  _ -> error ("invalid amount  " <> show (T.unpack input) <> ", context " <> show ctx)
     Right r -> r { astyle = amountstyle { asprecision = Precision 2, ascommodityspaced = True } }

collapsSpace :: Text -> Text
collapsSpace = T.unwords . T.words . T.strip

instance IsString Regexp where
  fromString = toRegexCI'
