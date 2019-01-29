{-|
Module      : Data.Dates
Description : Operations with dates
Copyright   :
License     :
Maintainer  :
Stability   : experimental

-- | Operations with dates
-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE UnicodeSyntax      #-}

module Data.Dates
  (DateTime (..),
   Time (..),
   WeekDay (..),
   parseDate,
   pDate,
   parseDay,
   pDay,
   pDateTime, pTime,
   pDateInterval,
   getCurrentDateTime,
   tryRead, tryReadInt,
   DateIntervalType (..),
   DateInterval (..),
   dayToDateTime, dateTimeToDay,
   weekdayToInterval,
   weekdayNumber,
   intToWeekday,
   dateWeekDay,
   lastMonday, nextMonday,
   modifyDate,
   datesDifference,
   addInterval, negateInterval, minusInterval,
   addTime
  ) where

-- import Prelude.Unicode
import           Data.Char                   (toUpper)
import           Data.List
import           Data.Time.Calendar          as C
import           Data.Time.Calendar.WeekDate
import           Data.Time.LocalTime
import           Text.Parsec
-- import Data.Generics
import           Data.Char                   (toLower)

import           Data.Data
import           Data.Dates.Internal
import           Data.Dates.Types


-- | Units for a date interval, as an enum
data DateIntervalType = Day | Week | Month | Year
  deriving (Eq,Show,Read,Data,Typeable)

-- | Date interval as a unit plus an 'Integer'
data DateInterval = Days Integer
                  | Weeks Integer
                  | Months Integer
                  | Years Integer
  deriving (Eq,Show,Data,Typeable)

toInterval :: DateIntervalType → Integer → DateInterval
toInterval Day   n = Days   n
toInterval Week  n = Weeks  n
toInterval Month n = Months n
toInterval Year  n = Years  n

-- | Days of the wekk as an enum
data WeekDay =
    Monday
  | Tuesday
  | Wednesday
  | Thursday
  | Friday
  | Saturday
  | Sunday
  deriving (Eq, Show, Read, Ord, Enum, Bounded, Data, Typeable)

-- | Weekday as interval from Monday, so that
-- weekdayToInterval Monday == 0 and
-- weekdayToInterval Sunday == 6.
weekdayToInterval ∷ WeekDay → DateInterval
weekdayToInterval wd = Days (fromIntegral $ fromEnum wd)

-- | Number of weekday, with Monday == 1 and Sunday == 7.
weekdayNumber ∷ WeekDay → Int
weekdayNumber wd = fromEnum wd + 1

-- | Reverse for weekdayNumber
intToWeekday ∷ Int → WeekDay
intToWeekday i = toEnum (i - 1)

-- | The Monday at the beginning of the current week
lastMonday ∷ DateTime → DateTime
lastMonday dt = dt `minusInterval` weekdayToInterval (dateWeekDay dt)

-- | The Monday at the beginning of the next week
nextMonday ∷ DateTime → DateTime
nextMonday dt = lastMonday dt `addInterval` Weeks 1

-- | Get current date and time.
getCurrentDateTime ∷  IO DateTime
getCurrentDateTime = do
  zt ← getZonedTime
  let lt = zonedTimeToLocalTime zt
      ld = localDay lt
      ltod = localTimeOfDay lt
      (y,m,d) = toGregorian ld
      h = todHour ltod
      mins = todMin ltod
      s = round $ todSec ltod
  return $ DateTime (fromIntegral y) m d h mins s

-- | Get weekday of given date.
dateWeekDay ∷ DateTime → WeekDay
dateWeekDay dt =
  let (_,_,wd) = toWeekDate (dateTimeToDay dt)
  in  intToWeekday wd

uppercase ∷ String → String
uppercase = map toUpper

isPrefixOfI ∷  String → String → Bool
p `isPrefixOfI` s = (uppercase p) `isPrefixOf` (uppercase s)

lookupS ∷ String → [(String,a)] → Maybe a
lookupS _ [] = Nothing
lookupS k ((k',v):other) | k `isPrefixOfI` k' = Just v
                         | otherwise          = lookupS k other

monthsN ∷ [(String,Int)]
monthsN = zip months [1..]

lookupMonth ∷ String → Maybe Int
lookupMonth n = lookupS n monthsN

date ∷  Int → Int → Int → DateTime
date y m d = DateTime y m d 0 0 0

-- | add a 'Time' offset to a 'DateTime' time
addTime ∷  DateTime → Time → DateTime
addTime dt t = dt {
                 hour = tHour t + hour dt,
                 minute = tMinute t + minute dt,
                 second = tSecond t + second dt }

euroNumDate ∷ Stream s m Char => ParsecT s st m DateTime
euroNumDate = do
  d ← pDayNumber
  _ <- char '.'
  m ← pMonthNumber
  _ <- char '.'
  y ← pYearNumber
  return $ date y m d

americanDate ∷ Stream s m Char => ParsecT s st m DateTime
americanDate = do
  y ← pYearNumber
  _ <- char '/'
  m ← pMonthNumber
  _ <- char '/'
  d ← pDayNumber
  return $ date y m d

iso8601Date ∷ Stream s m Char => ParsecT s st m DateTime
iso8601Date = do
  y ← pYearNumber
  _ <- char '-'
  m ← pMonthNumber
  _ <- char '-'
  d ← pDayNumber
  return $ date y m d

euroNumDate' ∷ Stream s m Char => Int → ParsecT s st m DateTime
euroNumDate' y = do
  d ← pDayNumber
  _ <- char '.'
  m ← pMonthNumber
  return $ date y m d

americanDate' ∷ Stream s m Char => Int → ParsecT s st m DateTime
americanDate' y = do
  m ← pMonthNumber
  _ <- char '/'
  d ← pDayNumber
  return $ date y m d

strDate ∷ Stream s m Char => ParsecT s st m DateTime
strDate = do
  d ← pDayNumber
  _ <- space
  ms ← many1 letter
  case lookupMonth ms of
    Nothing → fail $ "unknown month: "++ms
    Just m  → do
      _ <- space
      y ← pYearNumber
      notFollowedBy $ char ':'
      return $ date y m d

strDate' ∷ Stream s m Char => Int → ParsecT s st m DateTime
strDate' y = do
  d ← pDayNumber
  _ <- space
  ms ← many1 letter
  case lookupMonth ms of
    Nothing → fail $ "unknown month: "++ms
    Just m  → return $ date y m d

time24 ∷ Stream s m Char => ParsecT s st m Time
time24 = do
  h ← number 2 23
  _ <- char ':'
  m ← number 2 59
  x ← optionMaybe $ char ':'
  case x of
    Nothing → return $ Time h m 0
    Just _ → do
      s ← number 2 59
      notFollowedBy letter
      return $ Time h m s

ampm ∷ Stream s m Char => ParsecT s st m Int
ampm = do
  s ← many1 letter
  case map toUpper s of
    "AM" → return 0
    "PM" → return 12
    _    → fail "AM/PM expected"

time12 ∷ Stream s m Char => ParsecT s st m Time
time12 = do
  h ← number 2 12
  _ <- char ':'
  m ← number 2 59
  x ← optionMaybe $ char ':'
  s ← case x of
            Nothing → return 0
            Just _  → number 2 59
  optional space
  hd ← ampm
  return $ Time (h+hd) m s

-- | Parse a time string, either 12hr or 24hr
pTime ∷ Stream s m Char => ParsecT s st m Time
pTime = choice $ map try [time12, time24]

pAbsDate ∷ Stream s m Char => Int → ParsecT s st m DateTime
pAbsDate y =
  choice $ map try $ [
                       euroNumDate
                     , americanDate
                     , strDate
                     , iso8601Date
                     , strDate' y
                     , euroNumDate' y
                     , americanDate' y]

pAbsDateTime ∷ Stream s m Char => Int → ParsecT s st m DateTime
pAbsDateTime y = do
  dt <- pAbsDate y
  optional $ char ','
  s ← optionMaybe space
  case s of
    Nothing → return dt
    Just _ → do
      t ← pTime
      return $ dt `addTime` t

-- | Convert date from DateTime to Day
dateTimeToDay ∷  DateTime → Day
dateTimeToDay dt = fromGregorian (fromIntegral $ year dt) (month dt) (day dt)

-- | Convert date from Day to DateTime
dayToDateTime ∷  Day → DateTime
dayToDateTime dt =
  let (y,m,d) = toGregorian dt
  in  date (fromIntegral y) m d

-- | Modify DateTime with pure function on Day
modifyDate ∷  (t → Day → Day) → t → DateTime → DateTime
modifyDate fn x dt =
  let d = dayToDateTime $ fn x $ dateTimeToDay dt
  in  d {hour   = hour   dt,
         minute = minute dt,
         second = second dt}

-- | Add date interval to DateTime
addInterval ∷  DateTime → DateInterval → DateTime
addInterval dt (Days ds)   = modifyDate addDays ds dt
addInterval dt (Weeks ws)  = modifyDate addDays (ws*7) dt
addInterval dt (Months ms) = modifyDate addGregorianMonthsClip ms dt
addInterval dt (Years ys)  = modifyDate addGregorianYearsClip ys dt

-- | Negate DateInterval value: Days 3 → Days (-3).
negateInterval ∷ DateInterval → DateInterval
negateInterval (Days n)   = Days (negate n)
negateInterval (Weeks n)  = Weeks (negate n)
negateInterval (Months n) = Months (negate n)
negateInterval (Years n)  = Years (negate n)

-- | Subtract DateInterval from DateTime.
minusInterval ∷ DateTime → DateInterval → DateTime
minusInterval dt int = dt `addInterval` negateInterval int

-- | Number of days between two dates
datesDifference ∷ DateTime → DateTime → Integer
datesDifference d1 d2 =
  abs $ toModifiedJulianDay (dateTimeToDay d1) -
        toModifiedJulianDay (dateTimeToDay d2)

maybePlural ∷ Stream s m Char => String → ParsecT s st m String
maybePlural str = do
  r ← string str
  optional $ char 's'
  return (capitalize r)

pDateIntervalType ∷ Stream s m Char => ParsecT s st m DateIntervalType
pDateIntervalType = do
  s ← choice $ map maybePlural ["day", "week", "month", "year"]
  case toLower (head s) of
    'd' → return Day
    'w' → return Week
    'm' → return Month
    'y' → return Year
    _   → fail $ "Unknown date interval type: " ++ s

-- | Parse something of the form <n> <units>, e.g. 5 days
pDateInterval ∷ Stream s m Char => ParsecT s st m DateInterval
pDateInterval = do
  n ← many1 digit
  spaces
  tp ← pDateIntervalType
  ( toInterval tp ) `fmap` tryReadInt n

pRelDate ∷ Stream s m Char => DateTime → ParsecT s st m DateTime
pRelDate dt = do
  offs ← try futureDate
     <|> try passDate
     <|> try today
     <|> try tomorrow
     <|> yesterday
  return $ dt `addInterval` offs

lastDate ∷ Stream s m Char => DateTime → ParsecT s st m DateTime
lastDate now = do
    _ <- string "last"
    spaces
    try byweek <|> try bymonth <|> byyear
  where
    byweek = do
      wd ← try (string "week" >> return Monday) <|> pWeekDay
      let monday = lastMonday now
          monday' = if wd > dateWeekDay now
                      then monday `minusInterval` Weeks 1
                      else monday
      return $ monday' `addInterval` weekdayToInterval wd

    bymonth = do
      _ <- string "month"
      return $ now {day = 1}

    byyear = do
      _ <- string "year"
      return $ now {month = 1, day = 1}

nextDate ∷ Stream s m Char => DateTime → ParsecT s st m DateTime
nextDate now = do
    _ <- string "next"
    spaces
    try byweek <|> try bymonth <|> byyear
  where
    byweek = do
      wd ← try (string "week" >> return Monday) <|> pWeekDay
      let monday = nextMonday now
          monday' = if wd > dateWeekDay now
                      then monday `minusInterval` Weeks 1
                      else monday
      return $ monday' `addInterval` weekdayToInterval wd

    bymonth = do
      _ <- string "month"
      return (now `addInterval` Months 1) {day = 1}

    byyear = do
      _ <- string "year"
      return (now `addInterval` Years 1) {month = 1, day = 1}

pWeekDay ∷ Stream s m Char => ParsecT s st m WeekDay
pWeekDay =
    weekday >>= toDay
  where
    lc   = "mondaytueswnhrfi"
    uc   = uppercase lc
    word letters = many1 (oneOf letters)
    weekday = word $ lc ++ uc
    toDay w =
        case map toLower w of
          "monday"    → return Monday
          "tuesday"   → return Tuesday
          "wednesday" → return Wednesday
          "thursday"  → return Thursday
          "friday"    → return Friday
          "saturday"  → return Saturday
          "sunday"    → return Sunday
          _           → fail $ "Unknown weekday: " ++ w

futureDate ∷ Stream s m Char => ParsecT s st m DateInterval
futureDate = do
  _ <- string "in "
  n ← many1 digit
  _ <- char ' '
  tp ← pDateIntervalType
  ( toInterval tp ) `fmap` tryReadInt n

passDate ∷ Stream s m Char => ParsecT s st m DateInterval
passDate = do
  n ← many1 digit
  _ <- char ' '
  tp ← pDateIntervalType
  _ <- string " ago"
  ( ( toInterval tp ) . negate ) `fmap` tryReadInt n

today ∷ Stream s m Char => ParsecT s st m DateInterval
today = do
  _ <- string "today" <|> string "now"
  return $ Days 0

tomorrow ∷ Stream s m Char => ParsecT s st m DateInterval
tomorrow = do
  _ <- string "tomorrow"
  return $ Days 1

yesterday ∷ Stream s m Char => ParsecT s st m DateInterval
yesterday = do
  _ <- string "yesterday"
  return $ Days (-1)

pByWeek ∷ Stream s m Char => DateTime → ParsecT s st m DateTime
pByWeek dt =
  try (lastDate dt) <|> nextDate dt

-- | Parsec parser for DateTime.
pDateTime ∷ Stream s m Char => DateTime       -- ^ Current date / time, to use as base for relative dates
          → ParsecT s st m DateTime
pDateTime dt =
      (try $ pRelDate dt)
  <|> (try $ pByWeek dt)
  <|> (try $ pAbsDateTime $ year dt)

-- | Parsec parser for Date only.
pDate ∷ Stream s m Char => DateTime       -- ^ Current date / time, to use as base for relative dates
          → ParsecT s st m DateTime
pDate dt =
      (try $ pRelDate dt)
  <|> (try $ pByWeek dt)
  <|> (try $ pAbsDate $ year dt)

-- | Parse date/time
parseDate ∷ DateTime  -- ^ Current date / time, to use as base for relative dates
          → String    -- ^ String to parse
          → Either ParseError DateTime
parseDate dt s = runParser (pDate dt <?> msg) () "" s
    where msg = "absolute or relative date/time, e.g.\
              \ \"2019-03-12 15:30\" or \"yesterday\""


-- | Parsec parser for 'C.Day'
pDay :: Stream s m Char =>
    C.Day
 -- ^ Current day, to use as base for relative day
 -> ParsecT s t m C.Day
pDay dt = dateTimeToDay <$> pDate (dayToDateTime dt)

-- | Parse as 'C.Day'
parseDay ::
    C.Day
 -- ^ Current day, to use as base for relative days
 -> String
 -- ^ String to parse
 -> Either ParseError C.Day
parseDay dt = runParser (pDay dt <?> msg) () ""
    where msg = "absolute or relative day, e.g.\
               \ \"2019-03-12\" or \"yesterday\""
