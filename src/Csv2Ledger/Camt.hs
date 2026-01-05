{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | Parse the CSV CAMT format for \"cash management\".

module Csv2Ledger.Camt where

import Csv2Ledger

import Data.ByteString.Lazy as BSL
import Data.Char
import Data.Csv
import Data.Text as T
import Data.Text.Encoding as T
import Data.Text.Lazy.Encoding as TL
import Data.Time.Calendar
import Data.Time.Format
import Data.Vector as V
import Hledger.Data

data CAMT = CAMT { auftragskonto :: !Text
                 , buchungstag :: !GermanDateWithoutCentury
                 , valutadatum :: !GermanDateWithoutCentury
                 , buchungstext :: !Text
                 , verwendungszweck :: !Text
                 , glaeubiger :: !Text
                 , mandatsreferenz :: !Text
                 , kundenreferenz :: !Text
                 , sammlerreferenz :: !Text
                 , lastschriftUrsprungsbetrag :: !Text
                 , auslagenersatzRuecklastschrift :: !Text
                 , beguenstigter :: !Text
                 , kontonummer :: !Text
                 , bic :: !Text
                 , betrag :: !Amount
                 , info :: Text
                 }
  deriving (Show)

instance FromNamedRecord CAMT where
  parseNamedRecord m = do auftragskonto <- m .: "Auftragskonto"
                          buchungstag <- m .: "Buchungstag"
                          valutadatum <- m .: "Valutadatum"
                          buchungstext <- m .: "Buchungstext"
                          verwendungszweck <- m .: "Verwendungszweck"
                          glaeubiger <- m .: "Glaeubiger ID"
                          mandatsreferenz <- m .: "Mandatsreferenz"
                          kundenreferenz <- m .: "Kundenreferenz (End-to-End)"
                          sammlerreferenz <- m .: "Sammlerreferenz"
                          lastschriftUrsprungsbetrag <- m .: "Lastschrift Ursprungsbetrag"
                          auslagenersatzRuecklastschrift <- m .: "Auslagenersatz Ruecklastschrift"
                          beguenstigter <- m .: "Beguenstigter/Zahlungspflichtiger"
                          kontonummer <- m .: "Kontonummer/IBAN"
                          bic <- m .: "BIC (SWIFT-Code)"
                          betrag' <- m .: "Betrag"
                          waehrung <- m .: "Waehrung"
                          let betrag = pCurrency (Prelude.show m) waehrung betrag'
                          info <- m .: "Info"
                          pure CAMT {..}

buf :: FilePath -> IO BSL.ByteString
buf = fmap (TL.encodeUtf8 . TL.decodeLatin1) . BSL.readFile

claus :: FilePath -> IO (Either String (Header, Vector CAMT))
claus = fmap (decodeByNameWith opts) . buf

opts :: DecodeOptions
opts = defaultDecodeOptions { decDelimiter = fromIntegral (ord ';') }

newtype GermanDateWithoutCentury = GermanDateWithoutCentury { getDay :: Day }
  deriving (Show, Eq, Ord)

instance FromField GermanDateWithoutCentury where
    parseField = fmap GermanDateWithoutCentury . parseTimeM False defaultTimeLocale "%d.%m.%y" . T.unpack . T.decodeUtf8
