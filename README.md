# Poker
Just to be clear -- this is an alpha project, completely unsupported, and sole contributor.  Hence it is open to completely history-trashing squash/merge/update/complete replacement.


![Dogs](https://github.com/scottcmarks/Poker/blob/feature/sqlite/dogs.jpg)

merging is driven by the db.  Each data record links to a note, and in addition to an action and a type, by means of two mapping tables.  Each record that maps to the type "header" starts a chunk, in which all records that map to "merged" are dropped while their amounts are summed.  At the end of the chunk that sum is used to adjust the total in the header before the whole chunk is written out.  This is accomplished by means of a foldr working from the end back to the head.  The chunks must therefore be strict, but fortunately they are short.

Most important ability:

poker AMOUNT [NOTE [DATE]]

AMOUNT is a positive or negative integer
DATE defaults to today as YYYY-MM-DD (digits, no hours/minutes/seconds).
NOTE defaults to the first note (index order) that is a Profit and a Header.

This is the same as the explicit command "cash"
It means to compute my previous cash bankroll, presumably what I could have taken to the most recent game, and record (DATE, AMOUNT-previous, NOTE),
maintaining referential integrity and complaining if that's infeasible.

poker notes
poker notes [-a|--add NEWNOTE]
          | [-d|--delete EXISTINGNOTE]
          | [-r|--rename EXISTINGNOTE NEWNOTE]
poker update AMOUNT DATE NOTE -- add an explicit record to the db
poker sql "Your SQL here"

poker export csv [-w|--whitespace|
             [tgs|toGoogleSheet] [SHEETNAME [KEYFILENAME]]
poker import csv, [fgs|fromGoogleSheet] ?? -- only new Db?
poker list
poker count
poker chunks
-- the last four take an undocumented -r/--raw option that skips merging

Basically I should be able to just go e.g.

poker 390

after finishing a game with $390 in cash, or

poker -90 "Pete's"   -- lost 90 bucks,

poker cash -- see what it thinks I should have in cash,

poker -500 ATM  -- depositing 500 in excess cash

poker -100 Jackie 2019-01-31  -- gave Jackie 100 for bridge

poker export  -- update the Google Sheet "Poker"

... etc.


FIXME: not picking up the git version in Options.Applicative.Simple.simpleVersion

Text.Regex.PCRE from regex-pcre for verifying date format YYYYMMDD or YYYY-MM-DD (converted to the latter)
Tolerant but verifying on date format.
Note verified as being in Notes, and if not, added thereto.
"notes" command?

Date checking

poker notes NOTE

Change internal representation of Date (to UTCTime) and Note (Key Record_Note?) and Action (header|merged|regular)
Tests with bespoke Test_Poker.db created using insert on Record_Note Record_Action etc.

Split "Profit" entries into Buy-In, Re-Buy, and Cash Out to allow finer risk analysis?  Different kind of merge (toward following instead of preceding)


(Read a) => Map a b -> Parser b -- read an a, look it up, Maybe b ... Parser!
