{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-|
Module      : Database.Persist.Sql.PokerExtras.Internal
Description : Executable main for poker
Copyright   : (c) Magnolia Heights R&D, 2019
License     : All rights reserved
Maintainer  : scott@magnolia-heights.com
Stability   : experimental

Internal module in the Poker extension to 'Database.Persist.Sql'.
Despite the name, this module uses nothing Poker-schema-specific.

The implementation of 'sum' is essentially a fork of the code for
'Database.Persist.Sql.count' in
@persistent-2.9.1:Database.Persist.Sql.Orphan.PersistQuery@.
-}

{-# LANGUAGE KindSignatures    #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TypeFamilies      #-}


module Database.Persist.Sql.PokerExtras.Internal
  ( noFilters
  , noSelectOpts
  , getSqlSingle
  , sum
  ) where

import           Data.List                 (concat, find, inits, tail,
                                            transpose, unzip)
import           Data.Text                 (unpack)
import qualified Data.Text                 as T
import           Database.Persist.Sql      (DBName (..), EntityDef (..),
                                            EntityField, Filter (..),
                                            PersistEntity, PersistField,
                                            PersistFilter (..),
                                            PersistValue (..), SelectOpt,
                                            Single (..), Sql, SqlBackend,
                                            SqlPersistT, compositeFields,
                                            connEscapeName, entityDB, entityDef,
                                            entityFields, entityPrimary,
                                            fieldDB, fieldHaskell,
                                            persistFieldDef, rawSql,
                                            toPersistValue, unHaskellName)
import           Database.Persist.Sql.Util (isIdField)
import           RIO                       (Bool (..), Either (..), Maybe (..),
                                            MonadIO, Proxy (..), Show, Text,
                                            ask, const, either, elem, error,
                                            filter, fst, id, length, map,
                                            mconcat, not, null, pure, replicate,
                                            return, show, zip, ($), (&&), (++),
                                            (.), (/=), (<>), (==))


-- Typed empty lists for type inferencing
-- | No SQL filters, polytypic by record type
--
noFilters :: [Filter record]
noFilters = []

-- | No SQL selection options, polytypic by record type
--
noSelectOpts :: [SelectOpt record]
noSelectOpts = []

-- | Prepare some SQL, expecting one row with one column (Single) as the result
--
getSqlSingle :: forall a (m :: * -> *). (PersistField a, Show a, MonadIO m) =>
     Sql
  -> [PersistValue]
  -> SqlPersistT m a
getSqlSingle sql params = do
  sis <- rawSql sql params
  case sis of
    [Single x] -> pure x
    []         -> error $ "No rows returned from DB for SQL " ++ show sql
    _          -> error $ "Unexpected response from DB: " ++ show sis ++ "\n" ++
                          "  for SQL: " ++ show sql

-- | The sum of the value of a field in all chosen records
sum :: ( MonadIO m, PersistEntity record, Show a, PersistField a) =>
        Text
     -- ^ Field name (Haskell version, not necessarily DB version)
     -> [Filter record]
     -- ^ filters
     -> SqlPersistT m a
sum fld filters =
        let
            mkProxy :: [Filter record] -> (Proxy record)
            mkProxy _ = (Proxy :: Proxy record)
            EntityDef { entityHaskell = haskellRecordName
                      , entityFields  = fields
                      , entityDB      = tableName
                      } = entityDef $ mkProxy filters
        in
          case find ((== fld) . unHaskellName . fieldHaskell) fields of
            Nothing ->
                error $ unpack $ "Could not find field " <> fld
                              <> " in " <> unHaskellName haskellRecordName
            Just field -> do
                conn <- ask
                let wher = if null filters
                             then ""
                             else filterClause False conn filters
                    sql = mconcat
                        [ "SELECT SUM("
                        , dbFieldName
                        , ") FROM "
                        , connEscapeName conn $ DBName dbTableName
                        , wher
                        ]
                getSqlSingle sql []
              where
                dbFieldName = (unDBName ( fieldDB field ))
                dbTableName = (unDBName tableName)


filterClause :: (PersistEntity val)
             => Bool -- ^ include table name?
             -> SqlBackend
             -> [Filter val]
             -> Text
filterClause b c = fst . filterClauseHelper b True c OrNullNo

fieldName ::  forall record typ.  (PersistEntity record) =>
    EntityField record typ
 -> DBName
fieldName f = fieldDB $ persistFieldDef f

dummyFromFilts :: [Filter v] -> Maybe v
dummyFromFilts _ = Nothing

data OrNull = OrNullYes | OrNullNo

filterClauseHelper :: (PersistEntity val)
             => Bool -- ^ include table name?
             -> Bool -- ^ include WHERE?
             -> SqlBackend
             -> OrNull
             -> [Filter val]
             -> (Text, [PersistValue])
filterClauseHelper includeTable includeWhere conn orNull filters =
    (if not (T.null sql) && includeWhere
        then " WHERE " <> sql
        else sql, vals)
  where
    (sql, vals) = combineAND filters
    combineAND = combine " AND "

    combine s fs =
        (T.intercalate s $ map wrapP a, mconcat b)
      where
        (a, b) = unzip $ map go fs
        wrapP x = T.concat ["(", x, ")"]

    go (BackendFilter _) = error "BackendFilter not expected"
    go (FilterAnd []) = ("1=1", [])
    go (FilterAnd fs) = combineAND fs
    go (FilterOr []) = ("1=0", [])
    go (FilterOr fs)  = combine " OR " fs
    go (Filter field value pfilter) =
        let t = entityDef $ dummyFromFilts [Filter field value pfilter]
        in case (isIdField field, entityPrimary t, allVals) of
                 (True, Just pdef, PersistList ys:_) ->
                    if length (compositeFields pdef) /= length ys
                       then error $ "wrong number of entries in compositeFields vs PersistList allVals=" ++ show allVals
                    else
                      case (allVals, pfilter, isCompFilter pfilter) of
                        ([PersistList xs], Eq, _) ->
                           let sqlcl=T.intercalate " and " (map (\a -> connEscapeName conn (fieldDB a) <> showSqlFilter pfilter <> "? ")  (compositeFields pdef))
                           in (wrapSql sqlcl,xs)
                        ([PersistList xs], Ne, _) ->
                           let sqlcl=T.intercalate " or " (map (\a -> connEscapeName conn (fieldDB a) <> showSqlFilter pfilter <> "? ")  (compositeFields pdef))
                           in (wrapSql sqlcl,xs)
                        (_, In, _) ->
                           let xxs = transpose (map fromPersistList allVals)
                               sqls=map (\(a,xs) -> connEscapeName conn (fieldDB a) <> showSqlFilter pfilter <> "(" <> T.intercalate "," (replicate (length xs) " ?") <> ") ") (zip (compositeFields pdef) xxs)
                           in (wrapSql (T.intercalate " and " (map wrapSql sqls)), concat xxs)
                        (_, NotIn, _) ->
                           let xxs = transpose (map fromPersistList allVals)
                               sqls=map (\(a,xs) -> connEscapeName conn (fieldDB a) <> showSqlFilter pfilter <> "(" <> T.intercalate "," (replicate (length xs) " ?") <> ") ") (zip (compositeFields pdef) xxs)
                           in (wrapSql (T.intercalate " or " (map wrapSql sqls)), concat xxs)
                        ([PersistList xs], _, True) ->
                           let zs = tail (inits (compositeFields pdef))
                               sql1 = map (\b -> wrapSql (T.intercalate " and " (map (\(i,a) -> sql2 (i==length b) a) (zip [1..] b)))) zs
                               sql2 islast a = connEscapeName conn (fieldDB a) <> (if islast then showSqlFilter pfilter else showSqlFilter Eq) <> "? "
                               sqlcl = T.intercalate " or " sql1
                           in (wrapSql sqlcl, concat (tail (inits xs)))
                        (_, BackendSpecificFilter _, _) -> error "unhandled type BackendSpecificFilter for composite/non id primary keys"
                        _ -> error $ "unhandled type/filter for composite/non id primary keys pfilter=" ++ show pfilter ++ " persistList="++show allVals
                 (True, Just pdef, []) ->
                     error $ "empty list given as filter value filter=" ++ show pfilter ++ " persistList=" ++ show allVals ++ " pdef=" ++ show pdef
                 (True, Just pdef, _) ->
                     error $ "unhandled error for composite/non id primary keys filter=" ++ show pfilter ++ " persistList=" ++ show allVals ++ " pdef=" ++ show pdef

                 _ ->   case (isNull, pfilter, length notNullVals) of
                            (True, Eq, _) -> (name <> " IS NULL", [])
                            (True, Ne, _) -> (name <> " IS NOT NULL", [])
                            (False, Ne, _) -> (T.concat
                                [ "("
                                , name
                                , " IS NULL OR "
                                , name
                                , " <> "
                                , qmarks
                                , ")"
                                ], notNullVals)
                            -- We use 1=2 (and below 1=1) to avoid using TRUE and FALSE, since
                            -- not all databases support those words directly.
                            (_, In, 0) -> ("1=2" <> orNullSuffix, [])
                            (False, In, _) -> (name <> " IN " <> qmarks <> orNullSuffix, allVals)
                            (True, In, _) -> (T.concat
                                [ "("
                                , name
                                , " IS NULL OR "
                                , name
                                , " IN "
                                , qmarks
                                , ")"
                                ], notNullVals)
                            (False, NotIn, 0) -> ("1=1", [])
                            (True, NotIn, 0) -> (name <> " IS NOT NULL", [])
                            (False, NotIn, _) -> (T.concat
                                [ "("
                                , name
                                , " IS NULL OR "
                                , name
                                , " NOT IN "
                                , qmarks
                                , ")"
                                ], notNullVals)
                            (True, NotIn, _) -> (T.concat
                                [ "("
                                , name
                                , " IS NOT NULL AND "
                                , name
                                , " NOT IN "
                                , qmarks
                                , ")"
                                ], notNullVals)
                            _ -> (name <> showSqlFilter pfilter <> "?" <> orNullSuffix, allVals)

      where
        isCompFilter Lt = True
        isCompFilter Le = True
        isCompFilter Gt = True
        isCompFilter Ge = True
        isCompFilter _  =  False

        wrapSql sqlcl = "(" <> sqlcl <> ")"
        fromPersistList (PersistList xs) = xs
        fromPersistList other = error $ "expected PersistList but found " ++ show other

        filterValueToPersistValues :: forall a.  PersistField a => Either a [a] -> [PersistValue]
        filterValueToPersistValues v = map toPersistValue $ either return id v

        orNullSuffix =
            case orNull of
                OrNullYes -> mconcat [" OR ", name, " IS NULL"]
                OrNullNo  -> ""

        isNull = PersistNull `elem` allVals
        notNullVals = filter (/= PersistNull) allVals
        allVals = filterValueToPersistValues value
        tn = connEscapeName conn $ entityDB
           $ entityDef $ dummyFromFilts [Filter field value pfilter]
        name =
            (if includeTable
                then ((tn <> ".") <>)
                else id)
            $ connEscapeName conn $ fieldName field
        qmarks = case value of
                    Left _ -> "?"
                    Right x ->
                        let x' = filter (/= PersistNull) $ map toPersistValue x
                         in "(" <> T.intercalate "," (map (const "?") x') <> ")"
        showSqlFilter Eq                        = "="
        showSqlFilter Ne                        = "<>"
        showSqlFilter Gt                        = ">"
        showSqlFilter Lt                        = "<"
        showSqlFilter Ge                        = ">="
        showSqlFilter Le                        = "<="
        showSqlFilter In                        = " IN "
        showSqlFilter NotIn                     = " NOT IN "
        showSqlFilter (BackendSpecificFilter s) = s
