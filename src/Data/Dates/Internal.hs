{-|
Module      : Data.Dates.Types
Description : Internal module for dates and times
Copyright   :
License     :
Maintainer  :
Stability   : experimental

-- | Internal module implement date and time parsing
-}

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE UnicodeSyntax      #-}

module Data.Dates.Internal where

import           Data.Char

import           Text.Parsec

-- | Parser version of Prelude.read
tryRead :: (Read a) => String -> ParsecT s st m a
tryRead str =
  case reads str of
    [(res, "")] -> return res
    _           -> fail $ "Cannot read: " ++ str

-- | Parse a string of decimal digits to yield a ParsecT wrapping the 'Int'
tryReadInt ∷ (Num a) ⇒ String → ParsecT s st m a
tryReadInt str =
  if all isDigit str
    then return $ fromIntegral $ foldl (\n d → 10*n+digitToInt d) 0 str
    else fail $ "Cannot read: " ++ str

-- | Apply parser N times
times ∷ (Stream s m Char)
     ⇒ Int
     → ParsecT s st m t
     → ParsecT s st m [t]
times 0 _ = return []
times n p = do
  ts ← times (n-1) p
  t ← optionMaybe p
  case t of
    Just t' → return (ts ++ [t'])
    Nothing → return ts

-- | Parse natural number of N digits
-- which is not greater than M
number ∷ Stream s m Char
       ⇒ Int   -- ^ Number of digits
       → Int   -- ^ Maximum value
       → ParsecT s st m Int
number n m = do
  t ← tryReadInt =<< (n `times` digit)
  if t > m
    then fail "number too large"
    else return t

-- | Parse a year number, with adjustment if 0..99
pYearNumber ∷ Stream s m Char => ParsecT s st m Int
pYearNumber = adjYear <$> number 4 10000

-- | two-digit-year cutoff per Excel; POSIX standard is 69
twoDigitYearCutoff :: Int
twoDigitYearCutoff = 30

-- | Adjust one- or two-digit dates
-- | 0..29 => 2000..2029, 30..99 => 1930..1999
adjYear :: Int → Int
adjYear y
   | y < cutOff = y + 2000
   |    y < 100 = y + 1900
   |  otherwise = y
  where cutOff = twoDigitYearCutoff

-- | Parse a month number
pMonthNumber ∷ Stream s m Char => ParsecT s st m Int
pMonthNumber = number 2 12

-- | Parse a day number
pDayNumber ∷ Stream s m Char => ParsecT s st m Int
pDayNumber = number 2 31
