{-|
Module      : Data.Dates
Description : Operations with dates
Copyright   :
License     :
Maintainer  :
Stability   : experimental

-- Parse arbitrary date formats.
-- Date formats are specified as strings:
--
--  * "DD.MM.YYY"
--
--  * "YYYY\/MM\/DD"
--
--  * "DD\/MM\/YYYY, HH:mm:SS"
--
--  * "YY.MM.DD[, HH:mm:SS]"
--
--  * and so on.
---}

{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

module Data.Dates.Formats
  (FormatElement (..), Format,
   FormatParser,
   parseFormat, pFormat, formatParser,
   parseDateFormat
  )
where

import Control.Applicative ((<$>))
import Text.Parsec

import Data.Dates.Types
import Data.Dates.Internal (number, adjYear)

-- | Date\/time format element
data FormatElement =
    YEAR   Bool Int
  | MONTH  Bool Int
  | DAY    Bool Int
  | HOUR   Bool Int
  | MINUTE Bool Int
  | SECOND Bool Int
  | Whitespace Bool
  | Fixed  Bool String
  deriving (Eq, Show)

-- | Convenience declaration of format parser type
type FormatParser a = Parsec String Bool a

-- | Date\/time format
type Format = [FormatElement]

nchars ∷ Char → FormatParser Int
nchars c = length <$> (many1 $ char c)

brackets :: FormatParser a → FormatParser a
brackets p = lBracket *> p <* rBracket
  where
    lBracket = char '[' *> setState False
    rBracket = char ']' <* setState True

-- | Parser yielding a 'Format'
pFormat :: FormatParser Format
pFormat = do
    elems ← many1 $ try (brackets format) <|> format
    return $ concat elems

  where
    format :: FormatParser Format
    format =
      many1 $ choice $ map try [ element YEAR   'Y'
                               , element MONTH  'M'
                               , element DAY    'D'
                               , element HOUR   'H'
                               , element MINUTE 'm'
                               , element SECOND 'S'
                               , whitespaces
                               , fixed]

    element constr c = do
      mandatory ← getState
      constr mandatory <$> nchars c

    whitespaces = do
      _ ← many1 $ oneOf " \r\n\t"
      mandatory ← getState
      return $ Whitespace mandatory

    fixed = do
      mandatory ← getState
      Fixed mandatory <$> (many1 $ noneOf "YMDHmS[] \t\r\n")

pYearNumber ∷ Stream s m Char => Int → ParsecT s st m DateTime
pYearNumber n = do
  y ← adjYear <$> number n 10000
  return mempty {year = y}

pMonthNumber ∷ Stream s m Char => Int → ParsecT s st m DateTime
pMonthNumber n = do
  m ← number n 12
  return $ mempty {month = m}

pDayNumber ∷ Stream s m Char => Int → ParsecT s st m DateTime
pDayNumber n = do
  d ← number n 31
  return $ mempty {day = d}

pHourNumber ∷ Stream s m Char => Int → ParsecT s st m DateTime
pHourNumber n = do
  h ← number n 23
  return $ mempty {hour = h}

pMinuteNumber ∷ Stream s m Char => Int → ParsecT s st m DateTime
pMinuteNumber n = do
  m ← number n 59
  return $ mempty {minute = m}

pSecondNumber ∷ Stream s m Char => Int → ParsecT s st m DateTime
pSecondNumber n = do
  s ← number n 59
  return $ mempty {second = s}

opt :: Stream s m Char => Monoid a => Bool → ParsecT s st m a → ParsecT s st m a
opt True  p = p
opt False p = option mempty p

-- | Parse a format string to yield a 'Format'
parseFormat :: String → Either ParseError Format
parseFormat formatStr = runParser pFormat True "(date format string)" formatStr

-- | Make Parser for specified date format.
formatParser ∷ Stream s m Char => Format → ParsecT s st m DateTime
formatParser format = mconcat <$> mapM parser format
  where
    parser (YEAR   m n) = opt m $ pYearNumber   n
    parser (MONTH  m n) = opt m $ pMonthNumber  n
    parser (DAY    m n) = opt m $ pDayNumber    n
    parser (HOUR   m n) = opt m $ pHourNumber   n
    parser (MINUTE m n) = opt m $ pMinuteNumber n
    parser (SECOND m n) = opt m $ pSecondNumber n
    parser (Whitespace m) = opt m ((many1 $ oneOf " \t\r\n") >> return mempty)
    parser (Fixed  m s) = opt m ( string s >> return mempty )

-- | Parse date\/time in specified format.
parseDateFormat :: String  -- ^ Format string, i.e. "DD.MM.YY"
                 → String  -- ^ String to parse
                 → Either ParseError DateTime
parseDateFormat formatStr str = do
  format ← parseFormat formatStr
  runParser (formatParser format) () "(date)" str
