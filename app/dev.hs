:load app/Main.hs

import RIO
import Conduit
import ConduitExtras
import Control.Monad.Trans.Resource
import Database.Persist.Sql
import Database.Persist.Sqlite
import System.IO

import SqlExtras
import SqlRIO
import Poker
import Run

rc = runConduitRes

verbose = True
dbFilePath = "Poker.db"
options = AppOptions{ appOptionsVerbose    = verbose , appOptionsDbFilePath = dbFilePath}
pc <- mkDefaultProcessContext
app = App{ appLogFunc = mempty , appProcessContext = pc, appAppOptions     = options, appVerbose=verbose, appDbFilePath=fromString dbFilePath }
runApp = runRIO app
rs = runSqliteActions . rc :: HasDbFilePath env =>  ConduitT () Void  (ResourceT (SqlActionT (RIO env))) m -> RIO env m

ra = runApp . rs

recordPokerSource = recordSource :: CRSA () Record_Poker
records <- ra $ recordPokerSource .| sinkList

-- recordTriple :: Record_Poker -> (String, Int, Key Record_Note)
recordTriple (Record_Poker d a n)=(d, a, n)

showSink = sinkC (putStrLn . show)


aSource = recordSource :: CRSA () Record_Action
aActionSource = aSource .| mapC record_ActionAction
app = ra $ aActionSource .| showSink
apl <- ra $ aActionSource .| sinkList

tSource = recordSource :: CRSA () Record_Type
tTypeSource = tSource .| mapC record_TypeType
tpp = ra $ tTypeSource .| showSink
tpl <- ra $ tTypeSource .| sinkList

nSource = recordSource :: CRSA () Record_Note
nNoteSource = nSource .| mapC record_NoteNote
npp = ra $ nNoteSource .| showSink
npl <- ra $ nNoteSource .| sinkList


pairRNA (Record_Note_to_Action nid aid) = (nid, aid)
naSource = recordSource :: CRSA () Record_Note_to_Action
naPairsSource = naSource .| mapC pairRNA
napp = ra $ naPairsSource .| showSink
napl <- ra $ naPairsSource .| sinkList
naMap = fromList napl

pairRNT (Record_Note_to_Type nid tid) = (nid, tid)
ntSource = recordSource :: CRSA () Record_Note_to_Type
ntPairsSource = ntSource .| mapC pairRNT
ntpp = ra $ ntPairsSource .| showSink
ntpl = ra $ ntPairsSource .| sinkList
ntMap <- fromList <$> ntpl
record_PokerType_id = ((ntMap !) . record_PokerNote_id) :: Record_Poker -> Key Record_Type
:t record_PokerType_id


etSource = sqlSource :: CRSA () (Entity Record_Type)
etTypeSource = etSource .| mapC (\e -> (record_TypeType $ entityVal e, entityKey e))
etpp = ra $ etTypeSource .| showSink
etpl <- ra $ etTypeSource .| sinkList


type_idKey s = maybe (error ("No key for type " ++ s)) snd  (Data.List.find ((== s) .fst) etpl)
headerType_idKey = type_idKey "header"
mergedType_idKey = type_idKey "merged"
isTypeKey k r = k == record_PokerType_id r
isHeader = isTypeKey headerType_idKey 
isMerged = isTypeKey mergedType_idKey


all = ra $ recordPokerSource .| takeChunksC isHeader .| listChunksC
