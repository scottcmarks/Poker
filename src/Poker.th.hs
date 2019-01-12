-- /Users/scott/Poker/src/Poker.hs:(31,1)-(71,2): Splicing declarations
instance Database.Persist.Class.PersistField.PersistField Record_Action where
  Database.Persist.Class.PersistField.toPersistValue
    = \ ent_atPF
        -> (Database.Persist.Types.Base.PersistMap
              GHC.Base.$
                (GHC.List.zip ((GHC.Base.map Data.Text.pack) ["action"]))
                  (GHC.Base.map Database.Persist.Class.PersistField.toPersistValue
                     GHC.Base.$
                       Database.Persist.Class.PersistEntity.toPersistFields ent_atPF))
  Database.Persist.Class.PersistField.fromPersistValue
    = ((\ x_atPG
          -> let columns_atPH = Data.HashMap.Strict.fromList x_atPG
             in
               (Database.Persist.Class.PersistEntity.fromPersistValues
                  GHC.Base.$
                    (GHC.Base.map
                       (\ name_atPI
                          -> case
                                 (Data.HashMap.Base.lookup (Data.Text.pack name_atPI)) columns_atPH
                             of
                               GHC.Maybe.Just v_atPJ -> v_atPJ
                               GHC.Maybe.Nothing -> Database.Persist.Types.Base.PersistNull)
                       GHC.Base.$ ["action"])))
         Control.Monad.<=<
           Database.Persist.Class.PersistField.getPersistMap)
instance Database.Persist.Sql.Class.PersistFieldSql Record_Action where
  Database.Persist.Sql.Class.sqlType _
    = Database.Persist.Types.Base.SqlString
instance Database.Persist.Class.PersistField.PersistField Record_Note where
  Database.Persist.Class.PersistField.toPersistValue
    = \ ent_atPK
        -> (Database.Persist.Types.Base.PersistMap
              GHC.Base.$
                (GHC.List.zip ((GHC.Base.map Data.Text.pack) ["note"]))
                  (GHC.Base.map Database.Persist.Class.PersistField.toPersistValue
                     GHC.Base.$
                       Database.Persist.Class.PersistEntity.toPersistFields ent_atPK))
  Database.Persist.Class.PersistField.fromPersistValue
    = ((\ x_atPL
          -> let columns_atPM = Data.HashMap.Strict.fromList x_atPL
             in
               (Database.Persist.Class.PersistEntity.fromPersistValues
                  GHC.Base.$
                    (GHC.Base.map
                       (\ name_atPN
                          -> case
                                 (Data.HashMap.Base.lookup (Data.Text.pack name_atPN)) columns_atPM
                             of
                               GHC.Maybe.Just v_atPO -> v_atPO
                               GHC.Maybe.Nothing -> Database.Persist.Types.Base.PersistNull)
                       GHC.Base.$ ["note"])))
         Control.Monad.<=<
           Database.Persist.Class.PersistField.getPersistMap)
instance Database.Persist.Sql.Class.PersistFieldSql Record_Note where
  Database.Persist.Sql.Class.sqlType _
    = Database.Persist.Types.Base.SqlString
instance Database.Persist.Class.PersistField.PersistField Record_Type where
  Database.Persist.Class.PersistField.toPersistValue
    = \ ent_atPP
        -> (Database.Persist.Types.Base.PersistMap
              GHC.Base.$
                (GHC.List.zip ((GHC.Base.map Data.Text.pack) ["type"]))
                  (GHC.Base.map Database.Persist.Class.PersistField.toPersistValue
                     GHC.Base.$
                       Database.Persist.Class.PersistEntity.toPersistFields ent_atPP))
  Database.Persist.Class.PersistField.fromPersistValue
    = ((\ x_atPQ
          -> let columns_atPR = Data.HashMap.Strict.fromList x_atPQ
             in
               (Database.Persist.Class.PersistEntity.fromPersistValues
                  GHC.Base.$
                    (GHC.Base.map
                       (\ name_atPS
                          -> case
                                 (Data.HashMap.Base.lookup (Data.Text.pack name_atPS)) columns_atPR
                             of
                               GHC.Maybe.Just v_atPT -> v_atPT
                               GHC.Maybe.Nothing -> Database.Persist.Types.Base.PersistNull)
                       GHC.Base.$ ["type"])))
         Control.Monad.<=<
           Database.Persist.Class.PersistField.getPersistMap)
instance Database.Persist.Sql.Class.PersistFieldSql Record_Type where
  Database.Persist.Sql.Class.sqlType _
    = Database.Persist.Types.Base.SqlString
instance Database.Persist.Class.PersistField.PersistField Record_Note_to_Action where
  Database.Persist.Class.PersistField.toPersistValue
    = \ ent_atPU
        -> (Database.Persist.Types.Base.PersistMap
              GHC.Base.$
                (GHC.List.zip
                   ((GHC.Base.map Data.Text.pack) ["note_id", "action_id"]))
                  (GHC.Base.map Database.Persist.Class.PersistField.toPersistValue
                     GHC.Base.$
                       Database.Persist.Class.PersistEntity.toPersistFields ent_atPU))
  Database.Persist.Class.PersistField.fromPersistValue
    = ((\ x_atPV
          -> let columns_atPW = Data.HashMap.Strict.fromList x_atPV
             in
               (Database.Persist.Class.PersistEntity.fromPersistValues
                  GHC.Base.$
                    (GHC.Base.map
                       (\ name_atPX
                          -> case
                                 (Data.HashMap.Base.lookup (Data.Text.pack name_atPX)) columns_atPW
                             of
                               GHC.Maybe.Just v_atPY -> v_atPY
                               GHC.Maybe.Nothing -> Database.Persist.Types.Base.PersistNull)
                       GHC.Base.$ ["note_id", "action_id"])))
         Control.Monad.<=<
           Database.Persist.Class.PersistField.getPersistMap)
instance Database.Persist.Sql.Class.PersistFieldSql Record_Note_to_Action where
  Database.Persist.Sql.Class.sqlType _
    = Database.Persist.Types.Base.SqlString
instance Database.Persist.Class.PersistField.PersistField Record_Note_to_Type where
  Database.Persist.Class.PersistField.toPersistValue
    = \ ent_atPZ
        -> (Database.Persist.Types.Base.PersistMap
              GHC.Base.$
                (GHC.List.zip
                   ((GHC.Base.map Data.Text.pack) ["note_id", "type_id"]))
                  (GHC.Base.map Database.Persist.Class.PersistField.toPersistValue
                     GHC.Base.$
                       Database.Persist.Class.PersistEntity.toPersistFields ent_atPZ))
  Database.Persist.Class.PersistField.fromPersistValue
    = ((\ x_atQ0
          -> let columns_atQ1 = Data.HashMap.Strict.fromList x_atQ0
             in
               (Database.Persist.Class.PersistEntity.fromPersistValues
                  GHC.Base.$
                    (GHC.Base.map
                       (\ name_atQ2
                          -> case
                                 (Data.HashMap.Base.lookup (Data.Text.pack name_atQ2)) columns_atQ1
                             of
                               GHC.Maybe.Just v_atQ3 -> v_atQ3
                               GHC.Maybe.Nothing -> Database.Persist.Types.Base.PersistNull)
                       GHC.Base.$ ["note_id", "type_id"])))
         Control.Monad.<=<
           Database.Persist.Class.PersistField.getPersistMap)
instance Database.Persist.Sql.Class.PersistFieldSql Record_Note_to_Type where
  Database.Persist.Sql.Class.sqlType _
    = Database.Persist.Types.Base.SqlString
instance Database.Persist.Class.PersistField.PersistField Record_Poker where
  Database.Persist.Class.PersistField.toPersistValue
    = \ ent_atQ4
        -> (Database.Persist.Types.Base.PersistMap
              GHC.Base.$
                (GHC.List.zip
                   ((GHC.Base.map Data.Text.pack) ["date", "amount", "note_id"]))
                  (GHC.Base.map Database.Persist.Class.PersistField.toPersistValue
                     GHC.Base.$
                       Database.Persist.Class.PersistEntity.toPersistFields ent_atQ4))
  Database.Persist.Class.PersistField.fromPersistValue
    = ((\ x_atQ5
          -> let columns_atQ6 = Data.HashMap.Strict.fromList x_atQ5
             in
               (Database.Persist.Class.PersistEntity.fromPersistValues
                  GHC.Base.$
                    (GHC.Base.map
                       (\ name_atQ7
                          -> case
                                 (Data.HashMap.Base.lookup (Data.Text.pack name_atQ7)) columns_atQ6
                             of
                               GHC.Maybe.Just v_atQ8 -> v_atQ8
                               GHC.Maybe.Nothing -> Database.Persist.Types.Base.PersistNull)
                       GHC.Base.$ ["date", "amount", "note_id"])))
         Control.Monad.<=<
           Database.Persist.Class.PersistField.getPersistMap)
instance Database.Persist.Sql.Class.PersistFieldSql Record_Poker where
  Database.Persist.Sql.Class.sqlType _
    = Database.Persist.Types.Base.SqlString
data Record_Action
  = Record_Action {record_ActionAction :: !String}
  deriving (Show, Typeable)
type Record_ActionId =
    Database.Persist.Class.PersistEntity.Key Record_Action
instance Database.Persist.Class.PersistEntity.PersistEntity Record_Action where
  type Database.Persist.Class.PersistEntity.PersistEntityBackend Record_Action = Database.Persist.Sql.Types.Internal.SqlBackend
  data Database.Persist.Class.PersistEntity.Unique Record_Action
  newtype Database.Persist.Class.PersistEntity.Key Record_Action
    = Record_ActionKey {unRecord_ActionKey :: (Database.Persist.Class.PersistStore.BackendKey Database.Persist.Sql.Types.Internal.SqlBackend)}
    deriving (GHC.Show.Show,
              GHC.Read.Read,
              GHC.Classes.Eq,
              GHC.Classes.Ord,
              Web.PathPieces.PathPiece,
              Web.Internal.HttpApiData.ToHttpApiData,
              Web.Internal.HttpApiData.FromHttpApiData,
              Database.Persist.Class.PersistField.PersistField,
              Database.Persist.Sql.Class.PersistFieldSql,
              Data.Aeson.Types.ToJSON.ToJSON,
              Data.Aeson.Types.FromJSON.FromJSON)
  data Database.Persist.Class.PersistEntity.EntityField Record_Action typ
    = typ
      Data.Type.Equality.~
      Database.Persist.Class.PersistEntity.Key Record_Action =>
      Record_ActionId |
      typ Data.Type.Equality.~ String => Record_ActionAction
  Database.Persist.Class.PersistEntity.keyToValues
    = ((: [])
         GHC.Base..
           (Database.Persist.Class.PersistField.toPersistValue
              GHC.Base.. unRecord_ActionKey))
  Database.Persist.Class.PersistEntity.keyFromValues
    = (GHC.Base.fmap Record_ActionKey
         GHC.Base..
           (Database.Persist.Class.PersistField.fromPersistValue
              GHC.Base.. Database.Persist.TH.headNote))
  Database.Persist.Class.PersistEntity.entityDef _
    = (((((((((Database.Persist.Types.Base.EntityDef
                 (Database.Persist.Types.Base.HaskellName
                    (Database.Persist.TH.packPTH "Record_Action")))
                (Database.Persist.Types.Base.DBName
                   (Database.Persist.TH.packPTH "actions")))
               (((((((Database.Persist.Types.Base.FieldDef
                        (Database.Persist.Types.Base.HaskellName
                           (Database.Persist.TH.packPTH "Id")))
                       (Database.Persist.Types.Base.DBName
                          (Database.Persist.TH.packPTH "id")))
                      ((Database.Persist.Types.Base.FTTypeCon GHC.Maybe.Nothing)
                         (Database.Persist.TH.packPTH "Record_ActionId")))
                     Database.Persist.Types.Base.SqlInt64)
                    [])
                   GHC.Types.True)
                  ((Database.Persist.Types.Base.ForeignRef
                      (Database.Persist.Types.Base.HaskellName
                         (Database.Persist.TH.packPTH "Record_Action")))
                     ((Database.Persist.Types.Base.FTTypeCon
                         (GHC.Maybe.Just (Database.Persist.TH.packPTH "Data.Int")))
                        (Database.Persist.TH.packPTH "Int64")))))
              [Database.Persist.TH.packPTH "sql=actions"])
             [((((((Database.Persist.Types.Base.FieldDef
                      (Database.Persist.Types.Base.HaskellName
                         (Database.Persist.TH.packPTH "action")))
                     (Database.Persist.Types.Base.DBName
                        (Database.Persist.TH.packPTH "action")))
                    ((Database.Persist.Types.Base.FTTypeCon GHC.Maybe.Nothing)
                       (Database.Persist.TH.packPTH "String")))
                   Database.Persist.Types.Base.SqlString)
                  [])
                 GHC.Types.True)
                Database.Persist.Types.Base.NoReference])
            [])
           [])
          [Database.Persist.TH.packPTH "Show",
           Database.Persist.TH.packPTH "Typeable"])
         (Data.Map.Internal.fromList []))
        GHC.Types.False
  Database.Persist.Class.PersistEntity.toPersistFields
    (Record_Action x_atQ9)
    = [Database.Persist.Class.PersistField.SomePersistField x_atQ9]
  Database.Persist.Class.PersistEntity.fromPersistValues [x1_atQb]
    = Record_Action
        Data.Functor.<$>
          (Database.Persist.TH.mapLeft
             (Database.Persist.TH.fieldError
                (Database.Persist.TH.packPTH "action"))
             GHC.Base.. Database.Persist.Class.PersistField.fromPersistValue)
            x1_atQb
  Database.Persist.Class.PersistEntity.fromPersistValues x_atQa
    = (Data.Either.Left
         GHC.Base.$
           (GHC.Base.mappend
              (Database.Persist.TH.packPTH
                 "Record_Action: fromPersistValues failed on: "))
             (Data.Text.pack GHC.Base.$ GHC.Show.show x_atQa))
  Database.Persist.Class.PersistEntity.persistUniqueToFieldNames _
    = GHC.Err.error "Degenerate case, should never happen"
  Database.Persist.Class.PersistEntity.persistUniqueToValues _
    = GHC.Err.error "Degenerate case, should never happen"
  Database.Persist.Class.PersistEntity.persistUniqueKeys
    (Record_Action _action_atQc)
    = []
  Database.Persist.Class.PersistEntity.persistFieldDef
    Record_ActionId
    = ((((((Database.Persist.Types.Base.FieldDef
              (Database.Persist.Types.Base.HaskellName
                 (Database.Persist.TH.packPTH "Id")))
             (Database.Persist.Types.Base.DBName
                (Database.Persist.TH.packPTH "id")))
            ((Database.Persist.Types.Base.FTTypeCon GHC.Maybe.Nothing)
               (Database.Persist.TH.packPTH "Record_ActionId")))
           Database.Persist.Types.Base.SqlInt64)
          [])
         GHC.Types.True)
        ((Database.Persist.Types.Base.ForeignRef
            (Database.Persist.Types.Base.HaskellName
               (Database.Persist.TH.packPTH "Record_Action")))
           ((Database.Persist.Types.Base.FTTypeCon
               (GHC.Maybe.Just (Database.Persist.TH.packPTH "Data.Int")))
              (Database.Persist.TH.packPTH "Int64")))
  Database.Persist.Class.PersistEntity.persistFieldDef
    Record_ActionAction
    = ((((((Database.Persist.Types.Base.FieldDef
              (Database.Persist.Types.Base.HaskellName
                 (Database.Persist.TH.packPTH "action")))
             (Database.Persist.Types.Base.DBName
                (Database.Persist.TH.packPTH "action")))
            ((Database.Persist.Types.Base.FTTypeCon GHC.Maybe.Nothing)
               (Database.Persist.TH.packPTH "String")))
           Database.Persist.Types.Base.SqlString)
          [])
         GHC.Types.True)
        Database.Persist.Types.Base.NoReference
  Database.Persist.Class.PersistEntity.persistIdField
    = Record_ActionId
  Database.Persist.Class.PersistEntity.fieldLens Record_ActionId
    = (Database.Persist.TH.lensPTH
         Database.Persist.Class.PersistEntity.entityKey)
        (\ (Database.Persist.Class.PersistEntity.Entity _ value_atQd)
           key_atQe
           -> (Database.Persist.Class.PersistEntity.Entity key_atQe)
                value_atQd)
  Database.Persist.Class.PersistEntity.fieldLens Record_ActionAction
    = (Database.Persist.TH.lensPTH
         (record_ActionAction
            GHC.Base.. Database.Persist.Class.PersistEntity.entityVal))
        (\ (Database.Persist.Class.PersistEntity.Entity key_atQf
                                                        value_atQg)
           x_atQh
           -> (Database.Persist.Class.PersistEntity.Entity key_atQf)
                value_atQg {record_ActionAction = x_atQh})
instance Database.Persist.Class.PersistStore.ToBackendKey Database.Persist.Sql.Types.Internal.SqlBackend Record_Action where
  Database.Persist.Class.PersistStore.toBackendKey
    = unRecord_ActionKey
  Database.Persist.Class.PersistStore.fromBackendKey
    = Record_ActionKey
data Record_Note
  = Record_Note {record_NoteNote :: !String}
  deriving (Show, Typeable)
type Record_NoteId =
    Database.Persist.Class.PersistEntity.Key Record_Note
instance Database.Persist.Class.PersistEntity.PersistEntity Record_Note where
  type Database.Persist.Class.PersistEntity.PersistEntityBackend Record_Note = Database.Persist.Sql.Types.Internal.SqlBackend
  data Database.Persist.Class.PersistEntity.Unique Record_Note
  newtype Database.Persist.Class.PersistEntity.Key Record_Note
    = Record_NoteKey {unRecord_NoteKey :: (Database.Persist.Class.PersistStore.BackendKey Database.Persist.Sql.Types.Internal.SqlBackend)}
    deriving (GHC.Show.Show,
              GHC.Read.Read,
              GHC.Classes.Eq,
              GHC.Classes.Ord,
              Web.PathPieces.PathPiece,
              Web.Internal.HttpApiData.ToHttpApiData,
              Web.Internal.HttpApiData.FromHttpApiData,
              Database.Persist.Class.PersistField.PersistField,
              Database.Persist.Sql.Class.PersistFieldSql,
              Data.Aeson.Types.ToJSON.ToJSON,
              Data.Aeson.Types.FromJSON.FromJSON)
  data Database.Persist.Class.PersistEntity.EntityField Record_Note typ
    = typ
      Data.Type.Equality.~
      Database.Persist.Class.PersistEntity.Key Record_Note =>
      Record_NoteId |
      typ Data.Type.Equality.~ String => Record_NoteNote
  Database.Persist.Class.PersistEntity.keyToValues
    = ((: [])
         GHC.Base..
           (Database.Persist.Class.PersistField.toPersistValue
              GHC.Base.. unRecord_NoteKey))
  Database.Persist.Class.PersistEntity.keyFromValues
    = (GHC.Base.fmap Record_NoteKey
         GHC.Base..
           (Database.Persist.Class.PersistField.fromPersistValue
              GHC.Base.. Database.Persist.TH.headNote))
  Database.Persist.Class.PersistEntity.entityDef _
    = (((((((((Database.Persist.Types.Base.EntityDef
                 (Database.Persist.Types.Base.HaskellName
                    (Database.Persist.TH.packPTH "Record_Note")))
                (Database.Persist.Types.Base.DBName
                   (Database.Persist.TH.packPTH "notes")))
               (((((((Database.Persist.Types.Base.FieldDef
                        (Database.Persist.Types.Base.HaskellName
                           (Database.Persist.TH.packPTH "Id")))
                       (Database.Persist.Types.Base.DBName
                          (Database.Persist.TH.packPTH "id")))
                      ((Database.Persist.Types.Base.FTTypeCon GHC.Maybe.Nothing)
                         (Database.Persist.TH.packPTH "Record_NoteId")))
                     Database.Persist.Types.Base.SqlInt64)
                    [])
                   GHC.Types.True)
                  ((Database.Persist.Types.Base.ForeignRef
                      (Database.Persist.Types.Base.HaskellName
                         (Database.Persist.TH.packPTH "Record_Note")))
                     ((Database.Persist.Types.Base.FTTypeCon
                         (GHC.Maybe.Just (Database.Persist.TH.packPTH "Data.Int")))
                        (Database.Persist.TH.packPTH "Int64")))))
              [Database.Persist.TH.packPTH "sql=notes"])
             [((((((Database.Persist.Types.Base.FieldDef
                      (Database.Persist.Types.Base.HaskellName
                         (Database.Persist.TH.packPTH "note")))
                     (Database.Persist.Types.Base.DBName
                        (Database.Persist.TH.packPTH "note")))
                    ((Database.Persist.Types.Base.FTTypeCon GHC.Maybe.Nothing)
                       (Database.Persist.TH.packPTH "String")))
                   Database.Persist.Types.Base.SqlString)
                  [])
                 GHC.Types.True)
                Database.Persist.Types.Base.NoReference])
            [])
           [])
          [Database.Persist.TH.packPTH "Show",
           Database.Persist.TH.packPTH "Typeable"])
         (Data.Map.Internal.fromList []))
        GHC.Types.False
  Database.Persist.Class.PersistEntity.toPersistFields
    (Record_Note x_atQi)
    = [Database.Persist.Class.PersistField.SomePersistField x_atQi]
  Database.Persist.Class.PersistEntity.fromPersistValues [x1_atQk]
    = Record_Note
        Data.Functor.<$>
          (Database.Persist.TH.mapLeft
             (Database.Persist.TH.fieldError
                (Database.Persist.TH.packPTH "note"))
             GHC.Base.. Database.Persist.Class.PersistField.fromPersistValue)
            x1_atQk
  Database.Persist.Class.PersistEntity.fromPersistValues x_atQj
    = (Data.Either.Left
         GHC.Base.$
           (GHC.Base.mappend
              (Database.Persist.TH.packPTH
                 "Record_Note: fromPersistValues failed on: "))
             (Data.Text.pack GHC.Base.$ GHC.Show.show x_atQj))
  Database.Persist.Class.PersistEntity.persistUniqueToFieldNames _
    = GHC.Err.error "Degenerate case, should never happen"
  Database.Persist.Class.PersistEntity.persistUniqueToValues _
    = GHC.Err.error "Degenerate case, should never happen"
  Database.Persist.Class.PersistEntity.persistUniqueKeys
    (Record_Note _note_atQl)
    = []
  Database.Persist.Class.PersistEntity.persistFieldDef Record_NoteId
    = ((((((Database.Persist.Types.Base.FieldDef
              (Database.Persist.Types.Base.HaskellName
                 (Database.Persist.TH.packPTH "Id")))
             (Database.Persist.Types.Base.DBName
                (Database.Persist.TH.packPTH "id")))
            ((Database.Persist.Types.Base.FTTypeCon GHC.Maybe.Nothing)
               (Database.Persist.TH.packPTH "Record_NoteId")))
           Database.Persist.Types.Base.SqlInt64)
          [])
         GHC.Types.True)
        ((Database.Persist.Types.Base.ForeignRef
            (Database.Persist.Types.Base.HaskellName
               (Database.Persist.TH.packPTH "Record_Note")))
           ((Database.Persist.Types.Base.FTTypeCon
               (GHC.Maybe.Just (Database.Persist.TH.packPTH "Data.Int")))
              (Database.Persist.TH.packPTH "Int64")))
  Database.Persist.Class.PersistEntity.persistFieldDef
    Record_NoteNote
    = ((((((Database.Persist.Types.Base.FieldDef
              (Database.Persist.Types.Base.HaskellName
                 (Database.Persist.TH.packPTH "note")))
             (Database.Persist.Types.Base.DBName
                (Database.Persist.TH.packPTH "note")))
            ((Database.Persist.Types.Base.FTTypeCon GHC.Maybe.Nothing)
               (Database.Persist.TH.packPTH "String")))
           Database.Persist.Types.Base.SqlString)
          [])
         GHC.Types.True)
        Database.Persist.Types.Base.NoReference
  Database.Persist.Class.PersistEntity.persistIdField = Record_NoteId
  Database.Persist.Class.PersistEntity.fieldLens Record_NoteId
    = (Database.Persist.TH.lensPTH
         Database.Persist.Class.PersistEntity.entityKey)
        (\ (Database.Persist.Class.PersistEntity.Entity _ value_atQm)
           key_atQn
           -> (Database.Persist.Class.PersistEntity.Entity key_atQn)
                value_atQm)
  Database.Persist.Class.PersistEntity.fieldLens Record_NoteNote
    = (Database.Persist.TH.lensPTH
         (record_NoteNote
            GHC.Base.. Database.Persist.Class.PersistEntity.entityVal))
        (\ (Database.Persist.Class.PersistEntity.Entity key_atQo
                                                        value_atQp)
           x_atQq
           -> (Database.Persist.Class.PersistEntity.Entity key_atQo)
                value_atQp {record_NoteNote = x_atQq})
instance Database.Persist.Class.PersistStore.ToBackendKey Database.Persist.Sql.Types.Internal.SqlBackend Record_Note where
  Database.Persist.Class.PersistStore.toBackendKey = unRecord_NoteKey
  Database.Persist.Class.PersistStore.fromBackendKey = Record_NoteKey
data Record_Type
  = Record_Type {record_TypeType :: !String}
  deriving (Show, Typeable)
type Record_TypeId =
    Database.Persist.Class.PersistEntity.Key Record_Type
instance Database.Persist.Class.PersistEntity.PersistEntity Record_Type where
  type Database.Persist.Class.PersistEntity.PersistEntityBackend Record_Type = Database.Persist.Sql.Types.Internal.SqlBackend
  data Database.Persist.Class.PersistEntity.Unique Record_Type
  newtype Database.Persist.Class.PersistEntity.Key Record_Type
    = Record_TypeKey {unRecord_TypeKey :: (Database.Persist.Class.PersistStore.BackendKey Database.Persist.Sql.Types.Internal.SqlBackend)}
    deriving (GHC.Show.Show,
              GHC.Read.Read,
              GHC.Classes.Eq,
              GHC.Classes.Ord,
              Web.PathPieces.PathPiece,
              Web.Internal.HttpApiData.ToHttpApiData,
              Web.Internal.HttpApiData.FromHttpApiData,
              Database.Persist.Class.PersistField.PersistField,
              Database.Persist.Sql.Class.PersistFieldSql,
              Data.Aeson.Types.ToJSON.ToJSON,
              Data.Aeson.Types.FromJSON.FromJSON)
  data Database.Persist.Class.PersistEntity.EntityField Record_Type typ
    = typ
      Data.Type.Equality.~
      Database.Persist.Class.PersistEntity.Key Record_Type =>
      Record_TypeId |
      typ Data.Type.Equality.~ String => Record_TypeType
  Database.Persist.Class.PersistEntity.keyToValues
    = ((: [])
         GHC.Base..
           (Database.Persist.Class.PersistField.toPersistValue
              GHC.Base.. unRecord_TypeKey))
  Database.Persist.Class.PersistEntity.keyFromValues
    = (GHC.Base.fmap Record_TypeKey
         GHC.Base..
           (Database.Persist.Class.PersistField.fromPersistValue
              GHC.Base.. Database.Persist.TH.headNote))
  Database.Persist.Class.PersistEntity.entityDef _
    = (((((((((Database.Persist.Types.Base.EntityDef
                 (Database.Persist.Types.Base.HaskellName
                    (Database.Persist.TH.packPTH "Record_Type")))
                (Database.Persist.Types.Base.DBName
                   (Database.Persist.TH.packPTH "types")))
               (((((((Database.Persist.Types.Base.FieldDef
                        (Database.Persist.Types.Base.HaskellName
                           (Database.Persist.TH.packPTH "Id")))
                       (Database.Persist.Types.Base.DBName
                          (Database.Persist.TH.packPTH "id")))
                      ((Database.Persist.Types.Base.FTTypeCon GHC.Maybe.Nothing)
                         (Database.Persist.TH.packPTH "Record_TypeId")))
                     Database.Persist.Types.Base.SqlInt64)
                    [])
                   GHC.Types.True)
                  ((Database.Persist.Types.Base.ForeignRef
                      (Database.Persist.Types.Base.HaskellName
                         (Database.Persist.TH.packPTH "Record_Type")))
                     ((Database.Persist.Types.Base.FTTypeCon
                         (GHC.Maybe.Just (Database.Persist.TH.packPTH "Data.Int")))
                        (Database.Persist.TH.packPTH "Int64")))))
              [Database.Persist.TH.packPTH "sql=types"])
             [((((((Database.Persist.Types.Base.FieldDef
                      (Database.Persist.Types.Base.HaskellName
                         (Database.Persist.TH.packPTH "type")))
                     (Database.Persist.Types.Base.DBName
                        (Database.Persist.TH.packPTH "type")))
                    ((Database.Persist.Types.Base.FTTypeCon GHC.Maybe.Nothing)
                       (Database.Persist.TH.packPTH "String")))
                   Database.Persist.Types.Base.SqlString)
                  [])
                 GHC.Types.True)
                Database.Persist.Types.Base.NoReference])
            [])
           [])
          [Database.Persist.TH.packPTH "Show",
           Database.Persist.TH.packPTH "Typeable"])
         (Data.Map.Internal.fromList []))
        GHC.Types.False
  Database.Persist.Class.PersistEntity.toPersistFields
    (Record_Type x_atQr)
    = [Database.Persist.Class.PersistField.SomePersistField x_atQr]
  Database.Persist.Class.PersistEntity.fromPersistValues [x1_atQt]
    = Record_Type
        Data.Functor.<$>
          (Database.Persist.TH.mapLeft
             (Database.Persist.TH.fieldError
                (Database.Persist.TH.packPTH "type"))
             GHC.Base.. Database.Persist.Class.PersistField.fromPersistValue)
            x1_atQt
  Database.Persist.Class.PersistEntity.fromPersistValues x_atQs
    = (Data.Either.Left
         GHC.Base.$
           (GHC.Base.mappend
              (Database.Persist.TH.packPTH
                 "Record_Type: fromPersistValues failed on: "))
             (Data.Text.pack GHC.Base.$ GHC.Show.show x_atQs))
  Database.Persist.Class.PersistEntity.persistUniqueToFieldNames _
    = GHC.Err.error "Degenerate case, should never happen"
  Database.Persist.Class.PersistEntity.persistUniqueToValues _
    = GHC.Err.error "Degenerate case, should never happen"
  Database.Persist.Class.PersistEntity.persistUniqueKeys
    (Record_Type _type_atQu)
    = []
  Database.Persist.Class.PersistEntity.persistFieldDef Record_TypeId
    = ((((((Database.Persist.Types.Base.FieldDef
              (Database.Persist.Types.Base.HaskellName
                 (Database.Persist.TH.packPTH "Id")))
             (Database.Persist.Types.Base.DBName
                (Database.Persist.TH.packPTH "id")))
            ((Database.Persist.Types.Base.FTTypeCon GHC.Maybe.Nothing)
               (Database.Persist.TH.packPTH "Record_TypeId")))
           Database.Persist.Types.Base.SqlInt64)
          [])
         GHC.Types.True)
        ((Database.Persist.Types.Base.ForeignRef
            (Database.Persist.Types.Base.HaskellName
               (Database.Persist.TH.packPTH "Record_Type")))
           ((Database.Persist.Types.Base.FTTypeCon
               (GHC.Maybe.Just (Database.Persist.TH.packPTH "Data.Int")))
              (Database.Persist.TH.packPTH "Int64")))
  Database.Persist.Class.PersistEntity.persistFieldDef
    Record_TypeType
    = ((((((Database.Persist.Types.Base.FieldDef
              (Database.Persist.Types.Base.HaskellName
                 (Database.Persist.TH.packPTH "type")))
             (Database.Persist.Types.Base.DBName
                (Database.Persist.TH.packPTH "type")))
            ((Database.Persist.Types.Base.FTTypeCon GHC.Maybe.Nothing)
               (Database.Persist.TH.packPTH "String")))
           Database.Persist.Types.Base.SqlString)
          [])
         GHC.Types.True)
        Database.Persist.Types.Base.NoReference
  Database.Persist.Class.PersistEntity.persistIdField = Record_TypeId
  Database.Persist.Class.PersistEntity.fieldLens Record_TypeId
    = (Database.Persist.TH.lensPTH
         Database.Persist.Class.PersistEntity.entityKey)
        (\ (Database.Persist.Class.PersistEntity.Entity _ value_atQv)
           key_atQw
           -> (Database.Persist.Class.PersistEntity.Entity key_atQw)
                value_atQv)
  Database.Persist.Class.PersistEntity.fieldLens Record_TypeType
    = (Database.Persist.TH.lensPTH
         (record_TypeType
            GHC.Base.. Database.Persist.Class.PersistEntity.entityVal))
        (\ (Database.Persist.Class.PersistEntity.Entity key_atQx
                                                        value_atQy)
           x_atQz
           -> (Database.Persist.Class.PersistEntity.Entity key_atQx)
                value_atQy {record_TypeType = x_atQz})
instance Database.Persist.Class.PersistStore.ToBackendKey Database.Persist.Sql.Types.Internal.SqlBackend Record_Type where
  Database.Persist.Class.PersistStore.toBackendKey = unRecord_TypeKey
  Database.Persist.Class.PersistStore.fromBackendKey = Record_TypeKey
data Record_Note_to_Action
  = Record_Note_to_Action {record_Note_to_ActionNote_id :: !(Database.Persist.Class.PersistEntity.Key Record_Note),
                           record_Note_to_ActionAction_id :: !(Database.Persist.Class.PersistEntity.Key Record_Action)}
  deriving (Show, Typeable)
type Record_Note_to_ActionId =
    Database.Persist.Class.PersistEntity.Key Record_Note_to_Action
instance Database.Persist.Class.PersistEntity.PersistEntity Record_Note_to_Action where
  type Database.Persist.Class.PersistEntity.PersistEntityBackend Record_Note_to_Action = Database.Persist.Sql.Types.Internal.SqlBackend
  data Database.Persist.Class.PersistEntity.Unique Record_Note_to_Action
  newtype Database.Persist.Class.PersistEntity.Key Record_Note_to_Action
    = Record_Note_to_ActionKey {unRecord_Note_to_ActionKey :: (Database.Persist.Class.PersistStore.BackendKey Database.Persist.Sql.Types.Internal.SqlBackend)}
    deriving (GHC.Show.Show,
              GHC.Read.Read,
              GHC.Classes.Eq,
              GHC.Classes.Ord,
              Web.PathPieces.PathPiece,
              Web.Internal.HttpApiData.ToHttpApiData,
              Web.Internal.HttpApiData.FromHttpApiData,
              Database.Persist.Class.PersistField.PersistField,
              Database.Persist.Sql.Class.PersistFieldSql,
              Data.Aeson.Types.ToJSON.ToJSON,
              Data.Aeson.Types.FromJSON.FromJSON)
  data Database.Persist.Class.PersistEntity.EntityField Record_Note_to_Action typ
    = typ
      Data.Type.Equality.~
      Database.Persist.Class.PersistEntity.Key Record_Note_to_Action =>
      Record_Note_to_ActionId |
      typ
      Data.Type.Equality.~
      Database.Persist.Class.PersistEntity.Key Record_Note =>
      Record_Note_to_ActionNote_id |
      typ
      Data.Type.Equality.~
      Database.Persist.Class.PersistEntity.Key Record_Action =>
      Record_Note_to_ActionAction_id
  Database.Persist.Class.PersistEntity.keyToValues
    = ((: [])
         GHC.Base..
           (Database.Persist.Class.PersistField.toPersistValue
              GHC.Base.. unRecord_Note_to_ActionKey))
  Database.Persist.Class.PersistEntity.keyFromValues
    = (GHC.Base.fmap Record_Note_to_ActionKey
         GHC.Base..
           (Database.Persist.Class.PersistField.fromPersistValue
              GHC.Base.. Database.Persist.TH.headNote))
  Database.Persist.Class.PersistEntity.entityDef _
    = (((((((((Database.Persist.Types.Base.EntityDef
                 (Database.Persist.Types.Base.HaskellName
                    (Database.Persist.TH.packPTH "Record_Note_to_Action")))
                (Database.Persist.Types.Base.DBName
                   (Database.Persist.TH.packPTH "note_to_action")))
               (((((((Database.Persist.Types.Base.FieldDef
                        (Database.Persist.Types.Base.HaskellName
                           (Database.Persist.TH.packPTH "Id")))
                       (Database.Persist.Types.Base.DBName
                          (Database.Persist.TH.packPTH "id")))
                      ((Database.Persist.Types.Base.FTTypeCon GHC.Maybe.Nothing)
                         (Database.Persist.TH.packPTH "Record_Note_to_ActionId")))
                     Database.Persist.Types.Base.SqlInt64)
                    [])
                   GHC.Types.True)
                  ((Database.Persist.Types.Base.ForeignRef
                      (Database.Persist.Types.Base.HaskellName
                         (Database.Persist.TH.packPTH "Record_Note_to_Action")))
                     ((Database.Persist.Types.Base.FTTypeCon
                         (GHC.Maybe.Just (Database.Persist.TH.packPTH "Data.Int")))
                        (Database.Persist.TH.packPTH "Int64")))))
              [Database.Persist.TH.packPTH "sql=note_to_action"])
             [((((((Database.Persist.Types.Base.FieldDef
                      (Database.Persist.Types.Base.HaskellName
                         (Database.Persist.TH.packPTH "note_id")))
                     (Database.Persist.Types.Base.DBName
                        (Database.Persist.TH.packPTH "note_id")))
                    ((Database.Persist.Types.Base.FTTypeCon GHC.Maybe.Nothing)
                       (Database.Persist.TH.packPTH "Record_NoteId")))
                   (Database.Persist.Sql.Class.sqlType
                      (Data.Proxy.Proxy :: Data.Proxy.Proxy GHC.Int.Int64)))
                  [])
                 GHC.Types.True)
                ((Database.Persist.Types.Base.ForeignRef
                    (Database.Persist.Types.Base.HaskellName
                       (Database.Persist.TH.packPTH "Record_Note")))
                   ((Database.Persist.Types.Base.FTTypeCon
                       (GHC.Maybe.Just (Database.Persist.TH.packPTH "Data.Int")))
                      (Database.Persist.TH.packPTH "Int64"))),
              ((((((Database.Persist.Types.Base.FieldDef
                      (Database.Persist.Types.Base.HaskellName
                         (Database.Persist.TH.packPTH "action_id")))
                     (Database.Persist.Types.Base.DBName
                        (Database.Persist.TH.packPTH "action_id")))
                    ((Database.Persist.Types.Base.FTTypeCon GHC.Maybe.Nothing)
                       (Database.Persist.TH.packPTH "Record_ActionId")))
                   (Database.Persist.Sql.Class.sqlType
                      (Data.Proxy.Proxy :: Data.Proxy.Proxy GHC.Int.Int64)))
                  [])
                 GHC.Types.True)
                ((Database.Persist.Types.Base.ForeignRef
                    (Database.Persist.Types.Base.HaskellName
                       (Database.Persist.TH.packPTH "Record_Action")))
                   ((Database.Persist.Types.Base.FTTypeCon
                       (GHC.Maybe.Just (Database.Persist.TH.packPTH "Data.Int")))
                      (Database.Persist.TH.packPTH "Int64")))])
            [])
           [])
          [Database.Persist.TH.packPTH "Show",
           Database.Persist.TH.packPTH "Typeable"])
         (Data.Map.Internal.fromList []))
        GHC.Types.False
  Database.Persist.Class.PersistEntity.toPersistFields
    (Record_Note_to_Action x_atQA x_atQB)
    = [Database.Persist.Class.PersistField.SomePersistField x_atQA,
       Database.Persist.Class.PersistField.SomePersistField x_atQB]
  Database.Persist.Class.PersistEntity.fromPersistValues
    [x1_atQD, x2_atQE]
    = Record_Note_to_Action
        Data.Functor.<$>
          (Database.Persist.TH.mapLeft
             (Database.Persist.TH.fieldError
                (Database.Persist.TH.packPTH "note_id"))
             GHC.Base.. Database.Persist.Class.PersistField.fromPersistValue)
            x1_atQD
        GHC.Base.<*>
          (Database.Persist.TH.mapLeft
             (Database.Persist.TH.fieldError
                (Database.Persist.TH.packPTH "action_id"))
             GHC.Base.. Database.Persist.Class.PersistField.fromPersistValue)
            x2_atQE
  Database.Persist.Class.PersistEntity.fromPersistValues x_atQC
    = (Data.Either.Left
         GHC.Base.$
           (GHC.Base.mappend
              (Database.Persist.TH.packPTH
                 "Record_Note_to_Action: fromPersistValues failed on: "))
             (Data.Text.pack GHC.Base.$ GHC.Show.show x_atQC))
  Database.Persist.Class.PersistEntity.persistUniqueToFieldNames _
    = GHC.Err.error "Degenerate case, should never happen"
  Database.Persist.Class.PersistEntity.persistUniqueToValues _
    = GHC.Err.error "Degenerate case, should never happen"
  Database.Persist.Class.PersistEntity.persistUniqueKeys
    (Record_Note_to_Action _note_id_atQF _action_id_atQG)
    = []
  Database.Persist.Class.PersistEntity.persistFieldDef
    Record_Note_to_ActionId
    = ((((((Database.Persist.Types.Base.FieldDef
              (Database.Persist.Types.Base.HaskellName
                 (Database.Persist.TH.packPTH "Id")))
             (Database.Persist.Types.Base.DBName
                (Database.Persist.TH.packPTH "id")))
            ((Database.Persist.Types.Base.FTTypeCon GHC.Maybe.Nothing)
               (Database.Persist.TH.packPTH "Record_Note_to_ActionId")))
           Database.Persist.Types.Base.SqlInt64)
          [])
         GHC.Types.True)
        ((Database.Persist.Types.Base.ForeignRef
            (Database.Persist.Types.Base.HaskellName
               (Database.Persist.TH.packPTH "Record_Note_to_Action")))
           ((Database.Persist.Types.Base.FTTypeCon
               (GHC.Maybe.Just (Database.Persist.TH.packPTH "Data.Int")))
              (Database.Persist.TH.packPTH "Int64")))
  Database.Persist.Class.PersistEntity.persistFieldDef
    Record_Note_to_ActionNote_id
    = ((((((Database.Persist.Types.Base.FieldDef
              (Database.Persist.Types.Base.HaskellName
                 (Database.Persist.TH.packPTH "note_id")))
             (Database.Persist.Types.Base.DBName
                (Database.Persist.TH.packPTH "note_id")))
            ((Database.Persist.Types.Base.FTTypeCon GHC.Maybe.Nothing)
               (Database.Persist.TH.packPTH "Record_NoteId")))
           Database.Persist.Types.Base.SqlInt64)
          [])
         GHC.Types.True)
        ((Database.Persist.Types.Base.ForeignRef
            (Database.Persist.Types.Base.HaskellName
               (Database.Persist.TH.packPTH "Record_Note")))
           ((Database.Persist.Types.Base.FTTypeCon
               (GHC.Maybe.Just (Database.Persist.TH.packPTH "Data.Int")))
              (Database.Persist.TH.packPTH "Int64")))
  Database.Persist.Class.PersistEntity.persistFieldDef
    Record_Note_to_ActionAction_id
    = ((((((Database.Persist.Types.Base.FieldDef
              (Database.Persist.Types.Base.HaskellName
                 (Database.Persist.TH.packPTH "action_id")))
             (Database.Persist.Types.Base.DBName
                (Database.Persist.TH.packPTH "action_id")))
            ((Database.Persist.Types.Base.FTTypeCon GHC.Maybe.Nothing)
               (Database.Persist.TH.packPTH "Record_ActionId")))
           Database.Persist.Types.Base.SqlInt64)
          [])
         GHC.Types.True)
        ((Database.Persist.Types.Base.ForeignRef
            (Database.Persist.Types.Base.HaskellName
               (Database.Persist.TH.packPTH "Record_Action")))
           ((Database.Persist.Types.Base.FTTypeCon
               (GHC.Maybe.Just (Database.Persist.TH.packPTH "Data.Int")))
              (Database.Persist.TH.packPTH "Int64")))
  Database.Persist.Class.PersistEntity.persistIdField
    = Record_Note_to_ActionId
  Database.Persist.Class.PersistEntity.fieldLens
    Record_Note_to_ActionId
    = (Database.Persist.TH.lensPTH
         Database.Persist.Class.PersistEntity.entityKey)
        (\ (Database.Persist.Class.PersistEntity.Entity _ value_atQH)
           key_atQI
           -> (Database.Persist.Class.PersistEntity.Entity key_atQI)
                value_atQH)
  Database.Persist.Class.PersistEntity.fieldLens
    Record_Note_to_ActionNote_id
    = (Database.Persist.TH.lensPTH
         (record_Note_to_ActionNote_id
            GHC.Base.. Database.Persist.Class.PersistEntity.entityVal))
        (\ (Database.Persist.Class.PersistEntity.Entity key_atQJ
                                                        value_atQK)
           x_atQL
           -> (Database.Persist.Class.PersistEntity.Entity key_atQJ)
                value_atQK {record_Note_to_ActionNote_id = x_atQL})
  Database.Persist.Class.PersistEntity.fieldLens
    Record_Note_to_ActionAction_id
    = (Database.Persist.TH.lensPTH
         (record_Note_to_ActionAction_id
            GHC.Base.. Database.Persist.Class.PersistEntity.entityVal))
        (\ (Database.Persist.Class.PersistEntity.Entity key_atQJ
                                                        value_atQK)
           x_atQL
           -> (Database.Persist.Class.PersistEntity.Entity key_atQJ)
                value_atQK {record_Note_to_ActionAction_id = x_atQL})
instance Database.Persist.Class.PersistStore.ToBackendKey Database.Persist.Sql.Types.Internal.SqlBackend Record_Note_to_Action where
  Database.Persist.Class.PersistStore.toBackendKey
    = unRecord_Note_to_ActionKey
  Database.Persist.Class.PersistStore.fromBackendKey
    = Record_Note_to_ActionKey
data Record_Note_to_Type
  = Record_Note_to_Type {record_Note_to_TypeNote_id :: !(Database.Persist.Class.PersistEntity.Key Record_Note),
                         record_Note_to_TypeType_id :: !(Database.Persist.Class.PersistEntity.Key Record_Type)}
  deriving (Show, Typeable)
type Record_Note_to_TypeId =
    Database.Persist.Class.PersistEntity.Key Record_Note_to_Type
instance Database.Persist.Class.PersistEntity.PersistEntity Record_Note_to_Type where
  type Database.Persist.Class.PersistEntity.PersistEntityBackend Record_Note_to_Type = Database.Persist.Sql.Types.Internal.SqlBackend
  data Database.Persist.Class.PersistEntity.Unique Record_Note_to_Type
  newtype Database.Persist.Class.PersistEntity.Key Record_Note_to_Type
    = Record_Note_to_TypeKey {unRecord_Note_to_TypeKey :: (Database.Persist.Class.PersistStore.BackendKey Database.Persist.Sql.Types.Internal.SqlBackend)}
    deriving (GHC.Show.Show,
              GHC.Read.Read,
              GHC.Classes.Eq,
              GHC.Classes.Ord,
              Web.PathPieces.PathPiece,
              Web.Internal.HttpApiData.ToHttpApiData,
              Web.Internal.HttpApiData.FromHttpApiData,
              Database.Persist.Class.PersistField.PersistField,
              Database.Persist.Sql.Class.PersistFieldSql,
              Data.Aeson.Types.ToJSON.ToJSON,
              Data.Aeson.Types.FromJSON.FromJSON)
  data Database.Persist.Class.PersistEntity.EntityField Record_Note_to_Type typ
    = typ
      Data.Type.Equality.~
      Database.Persist.Class.PersistEntity.Key Record_Note_to_Type =>
      Record_Note_to_TypeId |
      typ
      Data.Type.Equality.~
      Database.Persist.Class.PersistEntity.Key Record_Note =>
      Record_Note_to_TypeNote_id |
      typ
      Data.Type.Equality.~
      Database.Persist.Class.PersistEntity.Key Record_Type =>
      Record_Note_to_TypeType_id
  Database.Persist.Class.PersistEntity.keyToValues
    = ((: [])
         GHC.Base..
           (Database.Persist.Class.PersistField.toPersistValue
              GHC.Base.. unRecord_Note_to_TypeKey))
  Database.Persist.Class.PersistEntity.keyFromValues
    = (GHC.Base.fmap Record_Note_to_TypeKey
         GHC.Base..
           (Database.Persist.Class.PersistField.fromPersistValue
              GHC.Base.. Database.Persist.TH.headNote))
  Database.Persist.Class.PersistEntity.entityDef _
    = (((((((((Database.Persist.Types.Base.EntityDef
                 (Database.Persist.Types.Base.HaskellName
                    (Database.Persist.TH.packPTH "Record_Note_to_Type")))
                (Database.Persist.Types.Base.DBName
                   (Database.Persist.TH.packPTH "note_to_type")))
               (((((((Database.Persist.Types.Base.FieldDef
                        (Database.Persist.Types.Base.HaskellName
                           (Database.Persist.TH.packPTH "Id")))
                       (Database.Persist.Types.Base.DBName
                          (Database.Persist.TH.packPTH "id")))
                      ((Database.Persist.Types.Base.FTTypeCon GHC.Maybe.Nothing)
                         (Database.Persist.TH.packPTH "Record_Note_to_TypeId")))
                     Database.Persist.Types.Base.SqlInt64)
                    [])
                   GHC.Types.True)
                  ((Database.Persist.Types.Base.ForeignRef
                      (Database.Persist.Types.Base.HaskellName
                         (Database.Persist.TH.packPTH "Record_Note_to_Type")))
                     ((Database.Persist.Types.Base.FTTypeCon
                         (GHC.Maybe.Just (Database.Persist.TH.packPTH "Data.Int")))
                        (Database.Persist.TH.packPTH "Int64")))))
              [Database.Persist.TH.packPTH "sql=note_to_type"])
             [((((((Database.Persist.Types.Base.FieldDef
                      (Database.Persist.Types.Base.HaskellName
                         (Database.Persist.TH.packPTH "note_id")))
                     (Database.Persist.Types.Base.DBName
                        (Database.Persist.TH.packPTH "note_id")))
                    ((Database.Persist.Types.Base.FTTypeCon GHC.Maybe.Nothing)
                       (Database.Persist.TH.packPTH "Record_NoteId")))
                   (Database.Persist.Sql.Class.sqlType
                      (Data.Proxy.Proxy :: Data.Proxy.Proxy GHC.Int.Int64)))
                  [])
                 GHC.Types.True)
                ((Database.Persist.Types.Base.ForeignRef
                    (Database.Persist.Types.Base.HaskellName
                       (Database.Persist.TH.packPTH "Record_Note")))
                   ((Database.Persist.Types.Base.FTTypeCon
                       (GHC.Maybe.Just (Database.Persist.TH.packPTH "Data.Int")))
                      (Database.Persist.TH.packPTH "Int64"))),
              ((((((Database.Persist.Types.Base.FieldDef
                      (Database.Persist.Types.Base.HaskellName
                         (Database.Persist.TH.packPTH "type_id")))
                     (Database.Persist.Types.Base.DBName
                        (Database.Persist.TH.packPTH "type_id")))
                    ((Database.Persist.Types.Base.FTTypeCon GHC.Maybe.Nothing)
                       (Database.Persist.TH.packPTH "Record_TypeId")))
                   (Database.Persist.Sql.Class.sqlType
                      (Data.Proxy.Proxy :: Data.Proxy.Proxy GHC.Int.Int64)))
                  [])
                 GHC.Types.True)
                ((Database.Persist.Types.Base.ForeignRef
                    (Database.Persist.Types.Base.HaskellName
                       (Database.Persist.TH.packPTH "Record_Type")))
                   ((Database.Persist.Types.Base.FTTypeCon
                       (GHC.Maybe.Just (Database.Persist.TH.packPTH "Data.Int")))
                      (Database.Persist.TH.packPTH "Int64")))])
            [])
           [])
          [Database.Persist.TH.packPTH "Show",
           Database.Persist.TH.packPTH "Typeable"])
         (Data.Map.Internal.fromList []))
        GHC.Types.False
  Database.Persist.Class.PersistEntity.toPersistFields
    (Record_Note_to_Type x_atQM x_atQN)
    = [Database.Persist.Class.PersistField.SomePersistField x_atQM,
       Database.Persist.Class.PersistField.SomePersistField x_atQN]
  Database.Persist.Class.PersistEntity.fromPersistValues
    [x1_atQP, x2_atQQ]
    = Record_Note_to_Type
        Data.Functor.<$>
          (Database.Persist.TH.mapLeft
             (Database.Persist.TH.fieldError
                (Database.Persist.TH.packPTH "note_id"))
             GHC.Base.. Database.Persist.Class.PersistField.fromPersistValue)
            x1_atQP
        GHC.Base.<*>
          (Database.Persist.TH.mapLeft
             (Database.Persist.TH.fieldError
                (Database.Persist.TH.packPTH "type_id"))
             GHC.Base.. Database.Persist.Class.PersistField.fromPersistValue)
            x2_atQQ
  Database.Persist.Class.PersistEntity.fromPersistValues x_atQO
    = (Data.Either.Left
         GHC.Base.$
           (GHC.Base.mappend
              (Database.Persist.TH.packPTH
                 "Record_Note_to_Type: fromPersistValues failed on: "))
             (Data.Text.pack GHC.Base.$ GHC.Show.show x_atQO))
  Database.Persist.Class.PersistEntity.persistUniqueToFieldNames _
    = GHC.Err.error "Degenerate case, should never happen"
  Database.Persist.Class.PersistEntity.persistUniqueToValues _
    = GHC.Err.error "Degenerate case, should never happen"
  Database.Persist.Class.PersistEntity.persistUniqueKeys
    (Record_Note_to_Type _note_id_atQR _type_id_atQS)
    = []
  Database.Persist.Class.PersistEntity.persistFieldDef
    Record_Note_to_TypeId
    = ((((((Database.Persist.Types.Base.FieldDef
              (Database.Persist.Types.Base.HaskellName
                 (Database.Persist.TH.packPTH "Id")))
             (Database.Persist.Types.Base.DBName
                (Database.Persist.TH.packPTH "id")))
            ((Database.Persist.Types.Base.FTTypeCon GHC.Maybe.Nothing)
               (Database.Persist.TH.packPTH "Record_Note_to_TypeId")))
           Database.Persist.Types.Base.SqlInt64)
          [])
         GHC.Types.True)
        ((Database.Persist.Types.Base.ForeignRef
            (Database.Persist.Types.Base.HaskellName
               (Database.Persist.TH.packPTH "Record_Note_to_Type")))
           ((Database.Persist.Types.Base.FTTypeCon
               (GHC.Maybe.Just (Database.Persist.TH.packPTH "Data.Int")))
              (Database.Persist.TH.packPTH "Int64")))
  Database.Persist.Class.PersistEntity.persistFieldDef
    Record_Note_to_TypeNote_id
    = ((((((Database.Persist.Types.Base.FieldDef
              (Database.Persist.Types.Base.HaskellName
                 (Database.Persist.TH.packPTH "note_id")))
             (Database.Persist.Types.Base.DBName
                (Database.Persist.TH.packPTH "note_id")))
            ((Database.Persist.Types.Base.FTTypeCon GHC.Maybe.Nothing)
               (Database.Persist.TH.packPTH "Record_NoteId")))
           Database.Persist.Types.Base.SqlInt64)
          [])
         GHC.Types.True)
        ((Database.Persist.Types.Base.ForeignRef
            (Database.Persist.Types.Base.HaskellName
               (Database.Persist.TH.packPTH "Record_Note")))
           ((Database.Persist.Types.Base.FTTypeCon
               (GHC.Maybe.Just (Database.Persist.TH.packPTH "Data.Int")))
              (Database.Persist.TH.packPTH "Int64")))
  Database.Persist.Class.PersistEntity.persistFieldDef
    Record_Note_to_TypeType_id
    = ((((((Database.Persist.Types.Base.FieldDef
              (Database.Persist.Types.Base.HaskellName
                 (Database.Persist.TH.packPTH "type_id")))
             (Database.Persist.Types.Base.DBName
                (Database.Persist.TH.packPTH "type_id")))
            ((Database.Persist.Types.Base.FTTypeCon GHC.Maybe.Nothing)
               (Database.Persist.TH.packPTH "Record_TypeId")))
           Database.Persist.Types.Base.SqlInt64)
          [])
         GHC.Types.True)
        ((Database.Persist.Types.Base.ForeignRef
            (Database.Persist.Types.Base.HaskellName
               (Database.Persist.TH.packPTH "Record_Type")))
           ((Database.Persist.Types.Base.FTTypeCon
               (GHC.Maybe.Just (Database.Persist.TH.packPTH "Data.Int")))
              (Database.Persist.TH.packPTH "Int64")))
  Database.Persist.Class.PersistEntity.persistIdField
    = Record_Note_to_TypeId
  Database.Persist.Class.PersistEntity.fieldLens
    Record_Note_to_TypeId
    = (Database.Persist.TH.lensPTH
         Database.Persist.Class.PersistEntity.entityKey)
        (\ (Database.Persist.Class.PersistEntity.Entity _ value_atQT)
           key_atQU
           -> (Database.Persist.Class.PersistEntity.Entity key_atQU)
                value_atQT)
  Database.Persist.Class.PersistEntity.fieldLens
    Record_Note_to_TypeNote_id
    = (Database.Persist.TH.lensPTH
         (record_Note_to_TypeNote_id
            GHC.Base.. Database.Persist.Class.PersistEntity.entityVal))
        (\ (Database.Persist.Class.PersistEntity.Entity key_atQV
                                                        value_atQW)
           x_atQX
           -> (Database.Persist.Class.PersistEntity.Entity key_atQV)
                value_atQW {record_Note_to_TypeNote_id = x_atQX})
  Database.Persist.Class.PersistEntity.fieldLens
    Record_Note_to_TypeType_id
    = (Database.Persist.TH.lensPTH
         (record_Note_to_TypeType_id
            GHC.Base.. Database.Persist.Class.PersistEntity.entityVal))
        (\ (Database.Persist.Class.PersistEntity.Entity key_atQV
                                                        value_atQW)
           x_atQX
           -> (Database.Persist.Class.PersistEntity.Entity key_atQV)
                value_atQW {record_Note_to_TypeType_id = x_atQX})
instance Database.Persist.Class.PersistStore.ToBackendKey Database.Persist.Sql.Types.Internal.SqlBackend Record_Note_to_Type where
  Database.Persist.Class.PersistStore.toBackendKey
    = unRecord_Note_to_TypeKey
  Database.Persist.Class.PersistStore.fromBackendKey
    = Record_Note_to_TypeKey
data Record_Poker
  = Record_Poker {record_PokerDate :: !String,
                  record_PokerAmount :: !Int,
                  record_PokerNote_id :: !(Database.Persist.Class.PersistEntity.Key Record_Note)}
  deriving (Show, Typeable)
type Record_PokerId =
    Database.Persist.Class.PersistEntity.Key Record_Poker
instance Database.Persist.Class.PersistEntity.PersistEntity Record_Poker where
  type Database.Persist.Class.PersistEntity.PersistEntityBackend Record_Poker = Database.Persist.Sql.Types.Internal.SqlBackend
  data Database.Persist.Class.PersistEntity.Unique Record_Poker
  newtype Database.Persist.Class.PersistEntity.Key Record_Poker
    = Record_PokerKey {unRecord_PokerKey :: (Database.Persist.Class.PersistStore.BackendKey Database.Persist.Sql.Types.Internal.SqlBackend)}
    deriving (GHC.Show.Show,
              GHC.Read.Read,
              GHC.Classes.Eq,
              GHC.Classes.Ord,
              Web.PathPieces.PathPiece,
              Web.Internal.HttpApiData.ToHttpApiData,
              Web.Internal.HttpApiData.FromHttpApiData,
              Database.Persist.Class.PersistField.PersistField,
              Database.Persist.Sql.Class.PersistFieldSql,
              Data.Aeson.Types.ToJSON.ToJSON,
              Data.Aeson.Types.FromJSON.FromJSON)
  data Database.Persist.Class.PersistEntity.EntityField Record_Poker typ
    = typ
      Data.Type.Equality.~
      Database.Persist.Class.PersistEntity.Key Record_Poker =>
      Record_PokerId |
      typ Data.Type.Equality.~ String => Record_PokerDate |
      typ Data.Type.Equality.~ Int => Record_PokerAmount |
      typ
      Data.Type.Equality.~
      Database.Persist.Class.PersistEntity.Key Record_Note =>
      Record_PokerNote_id
  Database.Persist.Class.PersistEntity.keyToValues
    = ((: [])
         GHC.Base..
           (Database.Persist.Class.PersistField.toPersistValue
              GHC.Base.. unRecord_PokerKey))
  Database.Persist.Class.PersistEntity.keyFromValues
    = (GHC.Base.fmap Record_PokerKey
         GHC.Base..
           (Database.Persist.Class.PersistField.fromPersistValue
              GHC.Base.. Database.Persist.TH.headNote))
  Database.Persist.Class.PersistEntity.entityDef _
    = (((((((((Database.Persist.Types.Base.EntityDef
                 (Database.Persist.Types.Base.HaskellName
                    (Database.Persist.TH.packPTH "Record_Poker")))
                (Database.Persist.Types.Base.DBName
                   (Database.Persist.TH.packPTH "data")))
               (((((((Database.Persist.Types.Base.FieldDef
                        (Database.Persist.Types.Base.HaskellName
                           (Database.Persist.TH.packPTH "Id")))
                       (Database.Persist.Types.Base.DBName
                          (Database.Persist.TH.packPTH "id")))
                      ((Database.Persist.Types.Base.FTTypeCon GHC.Maybe.Nothing)
                         (Database.Persist.TH.packPTH "Record_PokerId")))
                     Database.Persist.Types.Base.SqlInt64)
                    [])
                   GHC.Types.True)
                  ((Database.Persist.Types.Base.ForeignRef
                      (Database.Persist.Types.Base.HaskellName
                         (Database.Persist.TH.packPTH "Record_Poker")))
                     ((Database.Persist.Types.Base.FTTypeCon
                         (GHC.Maybe.Just (Database.Persist.TH.packPTH "Data.Int")))
                        (Database.Persist.TH.packPTH "Int64")))))
              [Database.Persist.TH.packPTH "sql=data"])
             [((((((Database.Persist.Types.Base.FieldDef
                      (Database.Persist.Types.Base.HaskellName
                         (Database.Persist.TH.packPTH "date")))
                     (Database.Persist.Types.Base.DBName
                        (Database.Persist.TH.packPTH "date")))
                    ((Database.Persist.Types.Base.FTTypeCon GHC.Maybe.Nothing)
                       (Database.Persist.TH.packPTH "String")))
                   Database.Persist.Types.Base.SqlString)
                  [])
                 GHC.Types.True)
                Database.Persist.Types.Base.NoReference,
              ((((((Database.Persist.Types.Base.FieldDef
                      (Database.Persist.Types.Base.HaskellName
                         (Database.Persist.TH.packPTH "amount")))
                     (Database.Persist.Types.Base.DBName
                        (Database.Persist.TH.packPTH "amount")))
                    ((Database.Persist.Types.Base.FTTypeCon GHC.Maybe.Nothing)
                       (Database.Persist.TH.packPTH "Int")))
                   Database.Persist.Types.Base.SqlInt64)
                  [])
                 GHC.Types.True)
                Database.Persist.Types.Base.NoReference,
              ((((((Database.Persist.Types.Base.FieldDef
                      (Database.Persist.Types.Base.HaskellName
                         (Database.Persist.TH.packPTH "note_id")))
                     (Database.Persist.Types.Base.DBName
                        (Database.Persist.TH.packPTH "note_id")))
                    ((Database.Persist.Types.Base.FTTypeCon GHC.Maybe.Nothing)
                       (Database.Persist.TH.packPTH "Record_NoteId")))
                   (Database.Persist.Sql.Class.sqlType
                      (Data.Proxy.Proxy :: Data.Proxy.Proxy GHC.Int.Int64)))
                  [])
                 GHC.Types.True)
                ((Database.Persist.Types.Base.ForeignRef
                    (Database.Persist.Types.Base.HaskellName
                       (Database.Persist.TH.packPTH "Record_Note")))
                   ((Database.Persist.Types.Base.FTTypeCon
                       (GHC.Maybe.Just (Database.Persist.TH.packPTH "Data.Int")))
                      (Database.Persist.TH.packPTH "Int64")))])
            [])
           [])
          [Database.Persist.TH.packPTH "Show",
           Database.Persist.TH.packPTH "Typeable"])
         (Data.Map.Internal.fromList []))
        GHC.Types.False
  Database.Persist.Class.PersistEntity.toPersistFields
    (Record_Poker x_atQY x_atQZ x_atR0)
    = [Database.Persist.Class.PersistField.SomePersistField x_atQY,
       Database.Persist.Class.PersistField.SomePersistField x_atQZ,
       Database.Persist.Class.PersistField.SomePersistField x_atR0]
  Database.Persist.Class.PersistEntity.fromPersistValues
    [x1_atR2, x2_atR3, x3_atR4]
    = Record_Poker
        Data.Functor.<$>
          (Database.Persist.TH.mapLeft
             (Database.Persist.TH.fieldError
                (Database.Persist.TH.packPTH "date"))
             GHC.Base.. Database.Persist.Class.PersistField.fromPersistValue)
            x1_atR2
        GHC.Base.<*>
          (Database.Persist.TH.mapLeft
             (Database.Persist.TH.fieldError
                (Database.Persist.TH.packPTH "amount"))
             GHC.Base.. Database.Persist.Class.PersistField.fromPersistValue)
            x2_atR3
        GHC.Base.<*>
          (Database.Persist.TH.mapLeft
             (Database.Persist.TH.fieldError
                (Database.Persist.TH.packPTH "note_id"))
             GHC.Base.. Database.Persist.Class.PersistField.fromPersistValue)
            x3_atR4
  Database.Persist.Class.PersistEntity.fromPersistValues x_atR1
    = (Data.Either.Left
         GHC.Base.$
           (GHC.Base.mappend
              (Database.Persist.TH.packPTH
                 "Record_Poker: fromPersistValues failed on: "))
             (Data.Text.pack GHC.Base.$ GHC.Show.show x_atR1))
  Database.Persist.Class.PersistEntity.persistUniqueToFieldNames _
    = GHC.Err.error "Degenerate case, should never happen"
  Database.Persist.Class.PersistEntity.persistUniqueToValues _
    = GHC.Err.error "Degenerate case, should never happen"
  Database.Persist.Class.PersistEntity.persistUniqueKeys
    (Record_Poker _date_atR5 _amount_atR6 _note_id_atR7)
    = []
  Database.Persist.Class.PersistEntity.persistFieldDef Record_PokerId
    = ((((((Database.Persist.Types.Base.FieldDef
              (Database.Persist.Types.Base.HaskellName
                 (Database.Persist.TH.packPTH "Id")))
             (Database.Persist.Types.Base.DBName
                (Database.Persist.TH.packPTH "id")))
            ((Database.Persist.Types.Base.FTTypeCon GHC.Maybe.Nothing)
               (Database.Persist.TH.packPTH "Record_PokerId")))
           Database.Persist.Types.Base.SqlInt64)
          [])
         GHC.Types.True)
        ((Database.Persist.Types.Base.ForeignRef
            (Database.Persist.Types.Base.HaskellName
               (Database.Persist.TH.packPTH "Record_Poker")))
           ((Database.Persist.Types.Base.FTTypeCon
               (GHC.Maybe.Just (Database.Persist.TH.packPTH "Data.Int")))
              (Database.Persist.TH.packPTH "Int64")))
  Database.Persist.Class.PersistEntity.persistFieldDef
    Record_PokerDate
    = ((((((Database.Persist.Types.Base.FieldDef
              (Database.Persist.Types.Base.HaskellName
                 (Database.Persist.TH.packPTH "date")))
             (Database.Persist.Types.Base.DBName
                (Database.Persist.TH.packPTH "date")))
            ((Database.Persist.Types.Base.FTTypeCon GHC.Maybe.Nothing)
               (Database.Persist.TH.packPTH "String")))
           Database.Persist.Types.Base.SqlString)
          [])
         GHC.Types.True)
        Database.Persist.Types.Base.NoReference
  Database.Persist.Class.PersistEntity.persistFieldDef
    Record_PokerAmount
    = ((((((Database.Persist.Types.Base.FieldDef
              (Database.Persist.Types.Base.HaskellName
                 (Database.Persist.TH.packPTH "amount")))
             (Database.Persist.Types.Base.DBName
                (Database.Persist.TH.packPTH "amount")))
            ((Database.Persist.Types.Base.FTTypeCon GHC.Maybe.Nothing)
               (Database.Persist.TH.packPTH "Int")))
           Database.Persist.Types.Base.SqlInt64)
          [])
         GHC.Types.True)
        Database.Persist.Types.Base.NoReference
  Database.Persist.Class.PersistEntity.persistFieldDef
    Record_PokerNote_id
    = ((((((Database.Persist.Types.Base.FieldDef
              (Database.Persist.Types.Base.HaskellName
                 (Database.Persist.TH.packPTH "note_id")))
             (Database.Persist.Types.Base.DBName
                (Database.Persist.TH.packPTH "note_id")))
            ((Database.Persist.Types.Base.FTTypeCon GHC.Maybe.Nothing)
               (Database.Persist.TH.packPTH "Record_NoteId")))
           Database.Persist.Types.Base.SqlInt64)
          [])
         GHC.Types.True)
        ((Database.Persist.Types.Base.ForeignRef
            (Database.Persist.Types.Base.HaskellName
               (Database.Persist.TH.packPTH "Record_Note")))
           ((Database.Persist.Types.Base.FTTypeCon
               (GHC.Maybe.Just (Database.Persist.TH.packPTH "Data.Int")))
              (Database.Persist.TH.packPTH "Int64")))
  Database.Persist.Class.PersistEntity.persistIdField
    = Record_PokerId
  Database.Persist.Class.PersistEntity.fieldLens Record_PokerId
    = (Database.Persist.TH.lensPTH
         Database.Persist.Class.PersistEntity.entityKey)
        (\ (Database.Persist.Class.PersistEntity.Entity _ value_atR8)
           key_atR9
           -> (Database.Persist.Class.PersistEntity.Entity key_atR9)
                value_atR8)
  Database.Persist.Class.PersistEntity.fieldLens Record_PokerDate
    = (Database.Persist.TH.lensPTH
         (record_PokerDate
            GHC.Base.. Database.Persist.Class.PersistEntity.entityVal))
        (\ (Database.Persist.Class.PersistEntity.Entity key_atRa
                                                        value_atRb)
           x_atRc
           -> (Database.Persist.Class.PersistEntity.Entity key_atRa)
                value_atRb {record_PokerDate = x_atRc})
  Database.Persist.Class.PersistEntity.fieldLens Record_PokerAmount
    = (Database.Persist.TH.lensPTH
         (record_PokerAmount
            GHC.Base.. Database.Persist.Class.PersistEntity.entityVal))
        (\ (Database.Persist.Class.PersistEntity.Entity key_atRa
                                                        value_atRb)
           x_atRc
           -> (Database.Persist.Class.PersistEntity.Entity key_atRa)
                value_atRb {record_PokerAmount = x_atRc})
  Database.Persist.Class.PersistEntity.fieldLens Record_PokerNote_id
    = (Database.Persist.TH.lensPTH
         (record_PokerNote_id
            GHC.Base.. Database.Persist.Class.PersistEntity.entityVal))
        (\ (Database.Persist.Class.PersistEntity.Entity key_atRa
                                                        value_atRb)
           x_atRc
           -> (Database.Persist.Class.PersistEntity.Entity key_atRa)
                value_atRb {record_PokerNote_id = x_atRc})
instance Database.Persist.Class.PersistStore.ToBackendKey Database.Persist.Sql.Types.Internal.SqlBackend Record_Poker where
  Database.Persist.Class.PersistStore.toBackendKey
    = unRecord_PokerKey
  Database.Persist.Class.PersistStore.fromBackendKey
    = Record_PokerKey
migrateAll :: Database.Persist.Sql.Types.Migration
migrateAll
  = do let defs_atRd
             = [(((((((((Database.Persist.Types.Base.EntityDef
                           (Database.Persist.Types.Base.HaskellName
                              (Database.Persist.TH.packPTH "Record_Action")))
                          (Database.Persist.Types.Base.DBName
                             (Database.Persist.TH.packPTH "actions")))
                         (((((((Database.Persist.Types.Base.FieldDef
                                  (Database.Persist.Types.Base.HaskellName
                                     (Database.Persist.TH.packPTH "Id")))
                                 (Database.Persist.Types.Base.DBName
                                    (Database.Persist.TH.packPTH "id")))
                                ((Database.Persist.Types.Base.FTTypeCon GHC.Maybe.Nothing)
                                   (Database.Persist.TH.packPTH "Record_ActionId")))
                               Database.Persist.Types.Base.SqlInt64)
                              [])
                             GHC.Types.True)
                            ((Database.Persist.Types.Base.ForeignRef
                                (Database.Persist.Types.Base.HaskellName
                                   (Database.Persist.TH.packPTH "Record_Action")))
                               ((Database.Persist.Types.Base.FTTypeCon
                                   (GHC.Maybe.Just (Database.Persist.TH.packPTH "Data.Int")))
                                  (Database.Persist.TH.packPTH "Int64")))))
                        [Database.Persist.TH.packPTH "sql=actions"])
                       [((((((Database.Persist.Types.Base.FieldDef
                                (Database.Persist.Types.Base.HaskellName
                                   (Database.Persist.TH.packPTH "action")))
                               (Database.Persist.Types.Base.DBName
                                  (Database.Persist.TH.packPTH "action")))
                              ((Database.Persist.Types.Base.FTTypeCon GHC.Maybe.Nothing)
                                 (Database.Persist.TH.packPTH "String")))
                             Database.Persist.Types.Base.SqlString)
                            [])
                           GHC.Types.True)
                          Database.Persist.Types.Base.NoReference])
                      [])
                     [])
                    [Database.Persist.TH.packPTH "Show",
                     Database.Persist.TH.packPTH "Typeable"])
                   (Data.Map.Internal.fromList []))
                  GHC.Types.False,
                (((((((((Database.Persist.Types.Base.EntityDef
                           (Database.Persist.Types.Base.HaskellName
                              (Database.Persist.TH.packPTH "Record_Note")))
                          (Database.Persist.Types.Base.DBName
                             (Database.Persist.TH.packPTH "notes")))
                         (((((((Database.Persist.Types.Base.FieldDef
                                  (Database.Persist.Types.Base.HaskellName
                                     (Database.Persist.TH.packPTH "Id")))
                                 (Database.Persist.Types.Base.DBName
                                    (Database.Persist.TH.packPTH "id")))
                                ((Database.Persist.Types.Base.FTTypeCon GHC.Maybe.Nothing)
                                   (Database.Persist.TH.packPTH "Record_NoteId")))
                               Database.Persist.Types.Base.SqlInt64)
                              [])
                             GHC.Types.True)
                            ((Database.Persist.Types.Base.ForeignRef
                                (Database.Persist.Types.Base.HaskellName
                                   (Database.Persist.TH.packPTH "Record_Note")))
                               ((Database.Persist.Types.Base.FTTypeCon
                                   (GHC.Maybe.Just (Database.Persist.TH.packPTH "Data.Int")))
                                  (Database.Persist.TH.packPTH "Int64")))))
                        [Database.Persist.TH.packPTH "sql=notes"])
                       [((((((Database.Persist.Types.Base.FieldDef
                                (Database.Persist.Types.Base.HaskellName
                                   (Database.Persist.TH.packPTH "note")))
                               (Database.Persist.Types.Base.DBName
                                  (Database.Persist.TH.packPTH "note")))
                              ((Database.Persist.Types.Base.FTTypeCon GHC.Maybe.Nothing)
                                 (Database.Persist.TH.packPTH "String")))
                             Database.Persist.Types.Base.SqlString)
                            [])
                           GHC.Types.True)
                          Database.Persist.Types.Base.NoReference])
                      [])
                     [])
                    [Database.Persist.TH.packPTH "Show",
                     Database.Persist.TH.packPTH "Typeable"])
                   (Data.Map.Internal.fromList []))
                  GHC.Types.False,
                (((((((((Database.Persist.Types.Base.EntityDef
                           (Database.Persist.Types.Base.HaskellName
                              (Database.Persist.TH.packPTH "Record_Type")))
                          (Database.Persist.Types.Base.DBName
                             (Database.Persist.TH.packPTH "types")))
                         (((((((Database.Persist.Types.Base.FieldDef
                                  (Database.Persist.Types.Base.HaskellName
                                     (Database.Persist.TH.packPTH "Id")))
                                 (Database.Persist.Types.Base.DBName
                                    (Database.Persist.TH.packPTH "id")))
                                ((Database.Persist.Types.Base.FTTypeCon GHC.Maybe.Nothing)
                                   (Database.Persist.TH.packPTH "Record_TypeId")))
                               Database.Persist.Types.Base.SqlInt64)
                              [])
                             GHC.Types.True)
                            ((Database.Persist.Types.Base.ForeignRef
                                (Database.Persist.Types.Base.HaskellName
                                   (Database.Persist.TH.packPTH "Record_Type")))
                               ((Database.Persist.Types.Base.FTTypeCon
                                   (GHC.Maybe.Just (Database.Persist.TH.packPTH "Data.Int")))
                                  (Database.Persist.TH.packPTH "Int64")))))
                        [Database.Persist.TH.packPTH "sql=types"])
                       [((((((Database.Persist.Types.Base.FieldDef
                                (Database.Persist.Types.Base.HaskellName
                                   (Database.Persist.TH.packPTH "type")))
                               (Database.Persist.Types.Base.DBName
                                  (Database.Persist.TH.packPTH "type")))
                              ((Database.Persist.Types.Base.FTTypeCon GHC.Maybe.Nothing)
                                 (Database.Persist.TH.packPTH "String")))
                             Database.Persist.Types.Base.SqlString)
                            [])
                           GHC.Types.True)
                          Database.Persist.Types.Base.NoReference])
                      [])
                     [])
                    [Database.Persist.TH.packPTH "Show",
                     Database.Persist.TH.packPTH "Typeable"])
                   (Data.Map.Internal.fromList []))
                  GHC.Types.False,
                (((((((((Database.Persist.Types.Base.EntityDef
                           (Database.Persist.Types.Base.HaskellName
                              (Database.Persist.TH.packPTH "Record_Note_to_Action")))
                          (Database.Persist.Types.Base.DBName
                             (Database.Persist.TH.packPTH "note_to_action")))
                         (((((((Database.Persist.Types.Base.FieldDef
                                  (Database.Persist.Types.Base.HaskellName
                                     (Database.Persist.TH.packPTH "Id")))
                                 (Database.Persist.Types.Base.DBName
                                    (Database.Persist.TH.packPTH "id")))
                                ((Database.Persist.Types.Base.FTTypeCon GHC.Maybe.Nothing)
                                   (Database.Persist.TH.packPTH "Record_Note_to_ActionId")))
                               Database.Persist.Types.Base.SqlInt64)
                              [])
                             GHC.Types.True)
                            ((Database.Persist.Types.Base.ForeignRef
                                (Database.Persist.Types.Base.HaskellName
                                   (Database.Persist.TH.packPTH "Record_Note_to_Action")))
                               ((Database.Persist.Types.Base.FTTypeCon
                                   (GHC.Maybe.Just (Database.Persist.TH.packPTH "Data.Int")))
                                  (Database.Persist.TH.packPTH "Int64")))))
                        [Database.Persist.TH.packPTH "sql=note_to_action"])
                       [((((((Database.Persist.Types.Base.FieldDef
                                (Database.Persist.Types.Base.HaskellName
                                   (Database.Persist.TH.packPTH "note_id")))
                               (Database.Persist.Types.Base.DBName
                                  (Database.Persist.TH.packPTH "note_id")))
                              ((Database.Persist.Types.Base.FTTypeCon GHC.Maybe.Nothing)
                                 (Database.Persist.TH.packPTH "Record_NoteId")))
                             (Database.Persist.Sql.Class.sqlType
                                (Data.Proxy.Proxy :: Data.Proxy.Proxy GHC.Int.Int64)))
                            [])
                           GHC.Types.True)
                          ((Database.Persist.Types.Base.ForeignRef
                              (Database.Persist.Types.Base.HaskellName
                                 (Database.Persist.TH.packPTH "Record_Note")))
                             ((Database.Persist.Types.Base.FTTypeCon
                                 (GHC.Maybe.Just (Database.Persist.TH.packPTH "Data.Int")))
                                (Database.Persist.TH.packPTH "Int64"))),
                        ((((((Database.Persist.Types.Base.FieldDef
                                (Database.Persist.Types.Base.HaskellName
                                   (Database.Persist.TH.packPTH "action_id")))
                               (Database.Persist.Types.Base.DBName
                                  (Database.Persist.TH.packPTH "action_id")))
                              ((Database.Persist.Types.Base.FTTypeCon GHC.Maybe.Nothing)
                                 (Database.Persist.TH.packPTH "Record_ActionId")))
                             (Database.Persist.Sql.Class.sqlType
                                (Data.Proxy.Proxy :: Data.Proxy.Proxy GHC.Int.Int64)))
                            [])
                           GHC.Types.True)
                          ((Database.Persist.Types.Base.ForeignRef
                              (Database.Persist.Types.Base.HaskellName
                                 (Database.Persist.TH.packPTH "Record_Action")))
                             ((Database.Persist.Types.Base.FTTypeCon
                                 (GHC.Maybe.Just (Database.Persist.TH.packPTH "Data.Int")))
                                (Database.Persist.TH.packPTH "Int64")))])
                      [])
                     [])
                    [Database.Persist.TH.packPTH "Show",
                     Database.Persist.TH.packPTH "Typeable"])
                   (Data.Map.Internal.fromList []))
                  GHC.Types.False,
                (((((((((Database.Persist.Types.Base.EntityDef
                           (Database.Persist.Types.Base.HaskellName
                              (Database.Persist.TH.packPTH "Record_Note_to_Type")))
                          (Database.Persist.Types.Base.DBName
                             (Database.Persist.TH.packPTH "note_to_type")))
                         (((((((Database.Persist.Types.Base.FieldDef
                                  (Database.Persist.Types.Base.HaskellName
                                     (Database.Persist.TH.packPTH "Id")))
                                 (Database.Persist.Types.Base.DBName
                                    (Database.Persist.TH.packPTH "id")))
                                ((Database.Persist.Types.Base.FTTypeCon GHC.Maybe.Nothing)
                                   (Database.Persist.TH.packPTH "Record_Note_to_TypeId")))
                               Database.Persist.Types.Base.SqlInt64)
                              [])
                             GHC.Types.True)
                            ((Database.Persist.Types.Base.ForeignRef
                                (Database.Persist.Types.Base.HaskellName
                                   (Database.Persist.TH.packPTH "Record_Note_to_Type")))
                               ((Database.Persist.Types.Base.FTTypeCon
                                   (GHC.Maybe.Just (Database.Persist.TH.packPTH "Data.Int")))
                                  (Database.Persist.TH.packPTH "Int64")))))
                        [Database.Persist.TH.packPTH "sql=note_to_type"])
                       [((((((Database.Persist.Types.Base.FieldDef
                                (Database.Persist.Types.Base.HaskellName
                                   (Database.Persist.TH.packPTH "note_id")))
                               (Database.Persist.Types.Base.DBName
                                  (Database.Persist.TH.packPTH "note_id")))
                              ((Database.Persist.Types.Base.FTTypeCon GHC.Maybe.Nothing)
                                 (Database.Persist.TH.packPTH "Record_NoteId")))
                             (Database.Persist.Sql.Class.sqlType
                                (Data.Proxy.Proxy :: Data.Proxy.Proxy GHC.Int.Int64)))
                            [])
                           GHC.Types.True)
                          ((Database.Persist.Types.Base.ForeignRef
                              (Database.Persist.Types.Base.HaskellName
                                 (Database.Persist.TH.packPTH "Record_Note")))
                             ((Database.Persist.Types.Base.FTTypeCon
                                 (GHC.Maybe.Just (Database.Persist.TH.packPTH "Data.Int")))
                                (Database.Persist.TH.packPTH "Int64"))),
                        ((((((Database.Persist.Types.Base.FieldDef
                                (Database.Persist.Types.Base.HaskellName
                                   (Database.Persist.TH.packPTH "type_id")))
                               (Database.Persist.Types.Base.DBName
                                  (Database.Persist.TH.packPTH "type_id")))
                              ((Database.Persist.Types.Base.FTTypeCon GHC.Maybe.Nothing)
                                 (Database.Persist.TH.packPTH "Record_TypeId")))
                             (Database.Persist.Sql.Class.sqlType
                                (Data.Proxy.Proxy :: Data.Proxy.Proxy GHC.Int.Int64)))
                            [])
                           GHC.Types.True)
                          ((Database.Persist.Types.Base.ForeignRef
                              (Database.Persist.Types.Base.HaskellName
                                 (Database.Persist.TH.packPTH "Record_Type")))
                             ((Database.Persist.Types.Base.FTTypeCon
                                 (GHC.Maybe.Just (Database.Persist.TH.packPTH "Data.Int")))
                                (Database.Persist.TH.packPTH "Int64")))])
                      [])
                     [])
                    [Database.Persist.TH.packPTH "Show",
                     Database.Persist.TH.packPTH "Typeable"])
                   (Data.Map.Internal.fromList []))
                  GHC.Types.False,
                (((((((((Database.Persist.Types.Base.EntityDef
                           (Database.Persist.Types.Base.HaskellName
                              (Database.Persist.TH.packPTH "Record_Poker")))
                          (Database.Persist.Types.Base.DBName
                             (Database.Persist.TH.packPTH "data")))
                         (((((((Database.Persist.Types.Base.FieldDef
                                  (Database.Persist.Types.Base.HaskellName
                                     (Database.Persist.TH.packPTH "Id")))
                                 (Database.Persist.Types.Base.DBName
                                    (Database.Persist.TH.packPTH "id")))
                                ((Database.Persist.Types.Base.FTTypeCon GHC.Maybe.Nothing)
                                   (Database.Persist.TH.packPTH "Record_PokerId")))
                               Database.Persist.Types.Base.SqlInt64)
                              [])
                             GHC.Types.True)
                            ((Database.Persist.Types.Base.ForeignRef
                                (Database.Persist.Types.Base.HaskellName
                                   (Database.Persist.TH.packPTH "Record_Poker")))
                               ((Database.Persist.Types.Base.FTTypeCon
                                   (GHC.Maybe.Just (Database.Persist.TH.packPTH "Data.Int")))
                                  (Database.Persist.TH.packPTH "Int64")))))
                        [Database.Persist.TH.packPTH "sql=data"])
                       [((((((Database.Persist.Types.Base.FieldDef
                                (Database.Persist.Types.Base.HaskellName
                                   (Database.Persist.TH.packPTH "date")))
                               (Database.Persist.Types.Base.DBName
                                  (Database.Persist.TH.packPTH "date")))
                              ((Database.Persist.Types.Base.FTTypeCon GHC.Maybe.Nothing)
                                 (Database.Persist.TH.packPTH "String")))
                             Database.Persist.Types.Base.SqlString)
                            [])
                           GHC.Types.True)
                          Database.Persist.Types.Base.NoReference,
                        ((((((Database.Persist.Types.Base.FieldDef
                                (Database.Persist.Types.Base.HaskellName
                                   (Database.Persist.TH.packPTH "amount")))
                               (Database.Persist.Types.Base.DBName
                                  (Database.Persist.TH.packPTH "amount")))
                              ((Database.Persist.Types.Base.FTTypeCon GHC.Maybe.Nothing)
                                 (Database.Persist.TH.packPTH "Int")))
                             Database.Persist.Types.Base.SqlInt64)
                            [])
                           GHC.Types.True)
                          Database.Persist.Types.Base.NoReference,
                        ((((((Database.Persist.Types.Base.FieldDef
                                (Database.Persist.Types.Base.HaskellName
                                   (Database.Persist.TH.packPTH "note_id")))
                               (Database.Persist.Types.Base.DBName
                                  (Database.Persist.TH.packPTH "note_id")))
                              ((Database.Persist.Types.Base.FTTypeCon GHC.Maybe.Nothing)
                                 (Database.Persist.TH.packPTH "Record_NoteId")))
                             (Database.Persist.Sql.Class.sqlType
                                (Data.Proxy.Proxy :: Data.Proxy.Proxy GHC.Int.Int64)))
                            [])
                           GHC.Types.True)
                          ((Database.Persist.Types.Base.ForeignRef
                              (Database.Persist.Types.Base.HaskellName
                                 (Database.Persist.TH.packPTH "Record_Note")))
                             ((Database.Persist.Types.Base.FTTypeCon
                                 (GHC.Maybe.Just (Database.Persist.TH.packPTH "Data.Int")))
                                (Database.Persist.TH.packPTH "Int64")))])
                      [])
                     [])
                    [Database.Persist.TH.packPTH "Show",
                     Database.Persist.TH.packPTH "Typeable"])
                   (Data.Map.Internal.fromList []))
                  GHC.Types.False]
       (Database.Persist.Sql.Migration.migrate defs_atRd)
         ((((((((((Database.Persist.Types.Base.EntityDef
                     (Database.Persist.Types.Base.HaskellName
                        (Database.Persist.TH.packPTH "Record_Action")))
                    (Database.Persist.Types.Base.DBName
                       (Database.Persist.TH.packPTH "actions")))
                   (((((((Database.Persist.Types.Base.FieldDef
                            (Database.Persist.Types.Base.HaskellName
                               (Database.Persist.TH.packPTH "Id")))
                           (Database.Persist.Types.Base.DBName
                              (Database.Persist.TH.packPTH "id")))
                          ((Database.Persist.Types.Base.FTTypeCon GHC.Maybe.Nothing)
                             (Database.Persist.TH.packPTH "Record_ActionId")))
                         Database.Persist.Types.Base.SqlInt64)
                        [])
                       GHC.Types.True)
                      ((Database.Persist.Types.Base.ForeignRef
                          (Database.Persist.Types.Base.HaskellName
                             (Database.Persist.TH.packPTH "Record_Action")))
                         ((Database.Persist.Types.Base.FTTypeCon
                             (GHC.Maybe.Just (Database.Persist.TH.packPTH "Data.Int")))
                            (Database.Persist.TH.packPTH "Int64")))))
                  [Database.Persist.TH.packPTH "sql=actions"])
                 [((((((Database.Persist.Types.Base.FieldDef
                          (Database.Persist.Types.Base.HaskellName
                             (Database.Persist.TH.packPTH "action")))
                         (Database.Persist.Types.Base.DBName
                            (Database.Persist.TH.packPTH "action")))
                        ((Database.Persist.Types.Base.FTTypeCon GHC.Maybe.Nothing)
                           (Database.Persist.TH.packPTH "String")))
                       Database.Persist.Types.Base.SqlString)
                      [])
                     GHC.Types.True)
                    Database.Persist.Types.Base.NoReference])
                [])
               [])
              [Database.Persist.TH.packPTH "Show",
               Database.Persist.TH.packPTH "Typeable"])
             (Data.Map.Internal.fromList []))
            GHC.Types.False)
       (Database.Persist.Sql.Migration.migrate defs_atRd)
         ((((((((((Database.Persist.Types.Base.EntityDef
                     (Database.Persist.Types.Base.HaskellName
                        (Database.Persist.TH.packPTH "Record_Note")))
                    (Database.Persist.Types.Base.DBName
                       (Database.Persist.TH.packPTH "notes")))
                   (((((((Database.Persist.Types.Base.FieldDef
                            (Database.Persist.Types.Base.HaskellName
                               (Database.Persist.TH.packPTH "Id")))
                           (Database.Persist.Types.Base.DBName
                              (Database.Persist.TH.packPTH "id")))
                          ((Database.Persist.Types.Base.FTTypeCon GHC.Maybe.Nothing)
                             (Database.Persist.TH.packPTH "Record_NoteId")))
                         Database.Persist.Types.Base.SqlInt64)
                        [])
                       GHC.Types.True)
                      ((Database.Persist.Types.Base.ForeignRef
                          (Database.Persist.Types.Base.HaskellName
                             (Database.Persist.TH.packPTH "Record_Note")))
                         ((Database.Persist.Types.Base.FTTypeCon
                             (GHC.Maybe.Just (Database.Persist.TH.packPTH "Data.Int")))
                            (Database.Persist.TH.packPTH "Int64")))))
                  [Database.Persist.TH.packPTH "sql=notes"])
                 [((((((Database.Persist.Types.Base.FieldDef
                          (Database.Persist.Types.Base.HaskellName
                             (Database.Persist.TH.packPTH "note")))
                         (Database.Persist.Types.Base.DBName
                            (Database.Persist.TH.packPTH "note")))
                        ((Database.Persist.Types.Base.FTTypeCon GHC.Maybe.Nothing)
                           (Database.Persist.TH.packPTH "String")))
                       Database.Persist.Types.Base.SqlString)
                      [])
                     GHC.Types.True)
                    Database.Persist.Types.Base.NoReference])
                [])
               [])
              [Database.Persist.TH.packPTH "Show",
               Database.Persist.TH.packPTH "Typeable"])
             (Data.Map.Internal.fromList []))
            GHC.Types.False)
       (Database.Persist.Sql.Migration.migrate defs_atRd)
         ((((((((((Database.Persist.Types.Base.EntityDef
                     (Database.Persist.Types.Base.HaskellName
                        (Database.Persist.TH.packPTH "Record_Type")))
                    (Database.Persist.Types.Base.DBName
                       (Database.Persist.TH.packPTH "types")))
                   (((((((Database.Persist.Types.Base.FieldDef
                            (Database.Persist.Types.Base.HaskellName
                               (Database.Persist.TH.packPTH "Id")))
                           (Database.Persist.Types.Base.DBName
                              (Database.Persist.TH.packPTH "id")))
                          ((Database.Persist.Types.Base.FTTypeCon GHC.Maybe.Nothing)
                             (Database.Persist.TH.packPTH "Record_TypeId")))
                         Database.Persist.Types.Base.SqlInt64)
                        [])
                       GHC.Types.True)
                      ((Database.Persist.Types.Base.ForeignRef
                          (Database.Persist.Types.Base.HaskellName
                             (Database.Persist.TH.packPTH "Record_Type")))
                         ((Database.Persist.Types.Base.FTTypeCon
                             (GHC.Maybe.Just (Database.Persist.TH.packPTH "Data.Int")))
                            (Database.Persist.TH.packPTH "Int64")))))
                  [Database.Persist.TH.packPTH "sql=types"])
                 [((((((Database.Persist.Types.Base.FieldDef
                          (Database.Persist.Types.Base.HaskellName
                             (Database.Persist.TH.packPTH "type")))
                         (Database.Persist.Types.Base.DBName
                            (Database.Persist.TH.packPTH "type")))
                        ((Database.Persist.Types.Base.FTTypeCon GHC.Maybe.Nothing)
                           (Database.Persist.TH.packPTH "String")))
                       Database.Persist.Types.Base.SqlString)
                      [])
                     GHC.Types.True)
                    Database.Persist.Types.Base.NoReference])
                [])
               [])
              [Database.Persist.TH.packPTH "Show",
               Database.Persist.TH.packPTH "Typeable"])
             (Data.Map.Internal.fromList []))
            GHC.Types.False)
       (Database.Persist.Sql.Migration.migrate defs_atRd)
         ((((((((((Database.Persist.Types.Base.EntityDef
                     (Database.Persist.Types.Base.HaskellName
                        (Database.Persist.TH.packPTH "Record_Note_to_Action")))
                    (Database.Persist.Types.Base.DBName
                       (Database.Persist.TH.packPTH "note_to_action")))
                   (((((((Database.Persist.Types.Base.FieldDef
                            (Database.Persist.Types.Base.HaskellName
                               (Database.Persist.TH.packPTH "Id")))
                           (Database.Persist.Types.Base.DBName
                              (Database.Persist.TH.packPTH "id")))
                          ((Database.Persist.Types.Base.FTTypeCon GHC.Maybe.Nothing)
                             (Database.Persist.TH.packPTH "Record_Note_to_ActionId")))
                         Database.Persist.Types.Base.SqlInt64)
                        [])
                       GHC.Types.True)
                      ((Database.Persist.Types.Base.ForeignRef
                          (Database.Persist.Types.Base.HaskellName
                             (Database.Persist.TH.packPTH "Record_Note_to_Action")))
                         ((Database.Persist.Types.Base.FTTypeCon
                             (GHC.Maybe.Just (Database.Persist.TH.packPTH "Data.Int")))
                            (Database.Persist.TH.packPTH "Int64")))))
                  [Database.Persist.TH.packPTH "sql=note_to_action"])
                 [((((((Database.Persist.Types.Base.FieldDef
                          (Database.Persist.Types.Base.HaskellName
                             (Database.Persist.TH.packPTH "note_id")))
                         (Database.Persist.Types.Base.DBName
                            (Database.Persist.TH.packPTH "note_id")))
                        ((Database.Persist.Types.Base.FTTypeCon GHC.Maybe.Nothing)
                           (Database.Persist.TH.packPTH "Record_NoteId")))
                       (Database.Persist.Sql.Class.sqlType
                          (Data.Proxy.Proxy :: Data.Proxy.Proxy GHC.Int.Int64)))
                      [])
                     GHC.Types.True)
                    ((Database.Persist.Types.Base.ForeignRef
                        (Database.Persist.Types.Base.HaskellName
                           (Database.Persist.TH.packPTH "Record_Note")))
                       ((Database.Persist.Types.Base.FTTypeCon
                           (GHC.Maybe.Just (Database.Persist.TH.packPTH "Data.Int")))
                          (Database.Persist.TH.packPTH "Int64"))),
                  ((((((Database.Persist.Types.Base.FieldDef
                          (Database.Persist.Types.Base.HaskellName
                             (Database.Persist.TH.packPTH "action_id")))
                         (Database.Persist.Types.Base.DBName
                            (Database.Persist.TH.packPTH "action_id")))
                        ((Database.Persist.Types.Base.FTTypeCon GHC.Maybe.Nothing)
                           (Database.Persist.TH.packPTH "Record_ActionId")))
                       (Database.Persist.Sql.Class.sqlType
                          (Data.Proxy.Proxy :: Data.Proxy.Proxy GHC.Int.Int64)))
                      [])
                     GHC.Types.True)
                    ((Database.Persist.Types.Base.ForeignRef
                        (Database.Persist.Types.Base.HaskellName
                           (Database.Persist.TH.packPTH "Record_Action")))
                       ((Database.Persist.Types.Base.FTTypeCon
                           (GHC.Maybe.Just (Database.Persist.TH.packPTH "Data.Int")))
                          (Database.Persist.TH.packPTH "Int64")))])
                [])
               [])
              [Database.Persist.TH.packPTH "Show",
               Database.Persist.TH.packPTH "Typeable"])
             (Data.Map.Internal.fromList []))
            GHC.Types.False)
       (Database.Persist.Sql.Migration.migrate defs_atRd)
         ((((((((((Database.Persist.Types.Base.EntityDef
                     (Database.Persist.Types.Base.HaskellName
                        (Database.Persist.TH.packPTH "Record_Note_to_Type")))
                    (Database.Persist.Types.Base.DBName
                       (Database.Persist.TH.packPTH "note_to_type")))
                   (((((((Database.Persist.Types.Base.FieldDef
                            (Database.Persist.Types.Base.HaskellName
                               (Database.Persist.TH.packPTH "Id")))
                           (Database.Persist.Types.Base.DBName
                              (Database.Persist.TH.packPTH "id")))
                          ((Database.Persist.Types.Base.FTTypeCon GHC.Maybe.Nothing)
                             (Database.Persist.TH.packPTH "Record_Note_to_TypeId")))
                         Database.Persist.Types.Base.SqlInt64)
                        [])
                       GHC.Types.True)
                      ((Database.Persist.Types.Base.ForeignRef
                          (Database.Persist.Types.Base.HaskellName
                             (Database.Persist.TH.packPTH "Record_Note_to_Type")))
                         ((Database.Persist.Types.Base.FTTypeCon
                             (GHC.Maybe.Just (Database.Persist.TH.packPTH "Data.Int")))
                            (Database.Persist.TH.packPTH "Int64")))))
                  [Database.Persist.TH.packPTH "sql=note_to_type"])
                 [((((((Database.Persist.Types.Base.FieldDef
                          (Database.Persist.Types.Base.HaskellName
                             (Database.Persist.TH.packPTH "note_id")))
                         (Database.Persist.Types.Base.DBName
                            (Database.Persist.TH.packPTH "note_id")))
                        ((Database.Persist.Types.Base.FTTypeCon GHC.Maybe.Nothing)
                           (Database.Persist.TH.packPTH "Record_NoteId")))
                       (Database.Persist.Sql.Class.sqlType
                          (Data.Proxy.Proxy :: Data.Proxy.Proxy GHC.Int.Int64)))
                      [])
                     GHC.Types.True)
                    ((Database.Persist.Types.Base.ForeignRef
                        (Database.Persist.Types.Base.HaskellName
                           (Database.Persist.TH.packPTH "Record_Note")))
                       ((Database.Persist.Types.Base.FTTypeCon
                           (GHC.Maybe.Just (Database.Persist.TH.packPTH "Data.Int")))
                          (Database.Persist.TH.packPTH "Int64"))),
                  ((((((Database.Persist.Types.Base.FieldDef
                          (Database.Persist.Types.Base.HaskellName
                             (Database.Persist.TH.packPTH "type_id")))
                         (Database.Persist.Types.Base.DBName
                            (Database.Persist.TH.packPTH "type_id")))
                        ((Database.Persist.Types.Base.FTTypeCon GHC.Maybe.Nothing)
                           (Database.Persist.TH.packPTH "Record_TypeId")))
                       (Database.Persist.Sql.Class.sqlType
                          (Data.Proxy.Proxy :: Data.Proxy.Proxy GHC.Int.Int64)))
                      [])
                     GHC.Types.True)
                    ((Database.Persist.Types.Base.ForeignRef
                        (Database.Persist.Types.Base.HaskellName
                           (Database.Persist.TH.packPTH "Record_Type")))
                       ((Database.Persist.Types.Base.FTTypeCon
                           (GHC.Maybe.Just (Database.Persist.TH.packPTH "Data.Int")))
                          (Database.Persist.TH.packPTH "Int64")))])
                [])
               [])
              [Database.Persist.TH.packPTH "Show",
               Database.Persist.TH.packPTH "Typeable"])
             (Data.Map.Internal.fromList []))
            GHC.Types.False)
       (Database.Persist.Sql.Migration.migrate defs_atRd)
         ((((((((((Database.Persist.Types.Base.EntityDef
                     (Database.Persist.Types.Base.HaskellName
                        (Database.Persist.TH.packPTH "Record_Poker")))
                    (Database.Persist.Types.Base.DBName
                       (Database.Persist.TH.packPTH "data")))
                   (((((((Database.Persist.Types.Base.FieldDef
                            (Database.Persist.Types.Base.HaskellName
                               (Database.Persist.TH.packPTH "Id")))
                           (Database.Persist.Types.Base.DBName
                              (Database.Persist.TH.packPTH "id")))
                          ((Database.Persist.Types.Base.FTTypeCon GHC.Maybe.Nothing)
                             (Database.Persist.TH.packPTH "Record_PokerId")))
                         Database.Persist.Types.Base.SqlInt64)
                        [])
                       GHC.Types.True)
                      ((Database.Persist.Types.Base.ForeignRef
                          (Database.Persist.Types.Base.HaskellName
                             (Database.Persist.TH.packPTH "Record_Poker")))
                         ((Database.Persist.Types.Base.FTTypeCon
                             (GHC.Maybe.Just (Database.Persist.TH.packPTH "Data.Int")))
                            (Database.Persist.TH.packPTH "Int64")))))
                  [Database.Persist.TH.packPTH "sql=data"])
                 [((((((Database.Persist.Types.Base.FieldDef
                          (Database.Persist.Types.Base.HaskellName
                             (Database.Persist.TH.packPTH "date")))
                         (Database.Persist.Types.Base.DBName
                            (Database.Persist.TH.packPTH "date")))
                        ((Database.Persist.Types.Base.FTTypeCon GHC.Maybe.Nothing)
                           (Database.Persist.TH.packPTH "String")))
                       Database.Persist.Types.Base.SqlString)
                      [])
                     GHC.Types.True)
                    Database.Persist.Types.Base.NoReference,
                  ((((((Database.Persist.Types.Base.FieldDef
                          (Database.Persist.Types.Base.HaskellName
                             (Database.Persist.TH.packPTH "amount")))
                         (Database.Persist.Types.Base.DBName
                            (Database.Persist.TH.packPTH "amount")))
                        ((Database.Persist.Types.Base.FTTypeCon GHC.Maybe.Nothing)
                           (Database.Persist.TH.packPTH "Int")))
                       Database.Persist.Types.Base.SqlInt64)
                      [])
                     GHC.Types.True)
                    Database.Persist.Types.Base.NoReference,
                  ((((((Database.Persist.Types.Base.FieldDef
                          (Database.Persist.Types.Base.HaskellName
                             (Database.Persist.TH.packPTH "note_id")))
                         (Database.Persist.Types.Base.DBName
                            (Database.Persist.TH.packPTH "note_id")))
                        ((Database.Persist.Types.Base.FTTypeCon GHC.Maybe.Nothing)
                           (Database.Persist.TH.packPTH "Record_NoteId")))
                       (Database.Persist.Sql.Class.sqlType
                          (Data.Proxy.Proxy :: Data.Proxy.Proxy GHC.Int.Int64)))
                      [])
                     GHC.Types.True)
                    ((Database.Persist.Types.Base.ForeignRef
                        (Database.Persist.Types.Base.HaskellName
                           (Database.Persist.TH.packPTH "Record_Note")))
                       ((Database.Persist.Types.Base.FTTypeCon
                           (GHC.Maybe.Just (Database.Persist.TH.packPTH "Data.Int")))
                          (Database.Persist.TH.packPTH "Int64")))])
                [])
               [])
              [Database.Persist.TH.packPTH "Show",
               Database.Persist.TH.packPTH "Typeable"])
             (Data.Map.Internal.fromList []))
            GHC.Types.False)
