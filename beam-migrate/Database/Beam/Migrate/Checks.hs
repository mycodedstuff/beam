{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE CPP #-}

-- | Defines common 'DatabasePredicate's that are shared among backends
module Database.Beam.Migrate.Checks where

import Database.Beam.Backend.SQL.SQL92
import Database.Beam.Migrate.SQL.SQL92
import Database.Beam.Migrate.SQL.Types
import Database.Beam.Migrate.Serialization
import Database.Beam.Migrate.Types.Predicates
import Database.Beam.Schema.Tables

import Data.Aeson ((.:), (.=), withObject, object)
import Data.Aeson.Types (Parser, Value)
import Data.Hashable (Hashable(..))
import Data.Text (Text, unpack, intercalate, pack)

import Data.Text.Encoding (encodeUtf8)

import qualified FlatParse.Basic as FP
import Data.Typeable (Typeable, cast)
#if !MIN_VERSION_base(4, 11, 0)
import Data.Semigroup
#endif

import GHC.Generics (Generic)

-- * Table checks

-- | Asserts that a table with the given name exists in a database
data TableExistsPredicate = TableExistsPredicate QualifiedName {-^ Table name -}
  deriving (Show, Eq, Ord, Typeable, Generic)
instance Hashable TableExistsPredicate
instance DatabasePredicate TableExistsPredicate where
  englishDescription (TableExistsPredicate t) =
    "Table " <> show t <> " must exist"

  serializePredicate (TableExistsPredicate t) =
    object [ "table-exists" .= t ]

  predicateSpecificity _ = PredicateSpecificityAllBackends

-- | A class that can check whether a particular data type is present
-- in a set of preconditions.
class HasDataTypeCreatedCheck dataType where
  dataTypeHasBeenCreated :: dataType -> (forall preCondition. Typeable preCondition => [ preCondition ]) -> Bool

-- | Asserts that the table specified has a column with the given data type. The
-- type paramater @syntax@ should be an instance of 'IsSql92ColumnSchemaSyntax'.
data TableHasColumn be where
  TableHasColumn
    :: ( HasDataTypeCreatedCheck (BeamMigrateSqlBackendDataTypeSyntax be) )
    => { hasColumn_table  :: QualifiedName {-^ Table name -}
       , hasColumn_column :: Text {-^ Column name -}
       , hasColumn_type   :: BeamMigrateSqlBackendDataTypeSyntax be {-^ Data type -}
       }
    -> TableHasColumn be
instance Hashable (BeamMigrateSqlBackendDataTypeSyntax be) => Hashable (TableHasColumn be) where
  hashWithSalt salt (TableHasColumn t c s) = hashWithSalt salt (t, c, s)
instance Eq (BeamMigrateSqlBackendDataTypeSyntax be) => Eq (TableHasColumn be) where
  TableHasColumn aTbl aCol aDt == TableHasColumn bTbl bCol bDt =
    aTbl == bTbl && aCol == bCol && aDt == bDt
instance ( Typeable be
         , BeamMigrateOnlySqlBackend be
         , Hashable (BeamMigrateSqlBackendDataTypeSyntax be) ) =>
  DatabasePredicate (TableHasColumn be) where
  englishDescription (TableHasColumn tbl col type_) =
    "Table " <> show tbl <> " must have a column " <> show col <> " of " <> displaySyntax type_

  predicateSpecificity _ = PredicateSpecificityAllBackends

  serializePredicate (TableHasColumn tbl col type_) =
    object [ "has-column" .= object [ "table" .= tbl, "column" .= col
                                    , "type" .= serializeDataType type_ ]]

  predicateCascadesDropOn (TableHasColumn tblNm _ _) p'
    | Just (TableExistsPredicate tblNm') <- cast p' = tblNm' == tblNm
    | otherwise = False

-- | Asserts that a particular column of a table has a given constraint. The
-- @syntax@ type parameter should be an instance of 'IsSql92ColumnSchemaSyntax'
data TableColumnHasConstraint be
  = TableColumnHasConstraint
  { hasConstraint_table  :: QualifiedName {-^ Table name -}
  , hasConstraint_column :: Text {-^ Column name -}
  , hasConstraint_defn   :: BeamSqlBackendColumnConstraintDefinitionSyntax be {-^ Constraint definition -}
  } deriving Generic
instance Hashable (BeamSqlBackendColumnConstraintDefinitionSyntax be) => Hashable (TableColumnHasConstraint be)
deriving instance Eq (BeamSqlBackendColumnConstraintDefinitionSyntax be) => Eq (TableColumnHasConstraint be)
instance ( Typeable be, BeamMigrateOnlySqlBackend be
         , Hashable (BeamSqlBackendColumnConstraintDefinitionSyntax be) ) =>
         DatabasePredicate (TableColumnHasConstraint be) where
  englishDescription (TableColumnHasConstraint tbl col cns) =
    "Column " <> show tbl <> "." <> show col <> " has constraint " <> displaySyntax cns

  predicateSpecificity _ = PredicateSpecificityAllBackends
  serializePredicate (TableColumnHasConstraint tbl col cns) =
    object [ "has-column-constraint" .= object [ "table" .= tbl, "column" .= col
                                               , "constraint" .= serializeConstraint cns ] ]

  predicateCascadesDropOn (TableColumnHasConstraint tblNm colNm _) p'
    | Just (TableExistsPredicate tblNm') <- cast p' = tblNm' == tblNm
    | Just (TableHasColumn tblNm' colNm' _ :: TableHasColumn be) <- cast p' = tblNm' == tblNm && colNm' == colNm
    | otherwise = False

-- | Asserts that the given table has a primary key made of the given columns.
-- The order of the columns is significant.
data TableHasPrimaryKey
  = TableHasPrimaryKey
  { hasPrimaryKey_table :: QualifiedName   {-^ Table name -}
  , hasPrimaryKey_cols  :: [Text] {-^ Column names -}
  } deriving (Show, Eq, Generic)
instance Hashable TableHasPrimaryKey
instance DatabasePredicate TableHasPrimaryKey where
  englishDescription (TableHasPrimaryKey tblName colNames) =
    "Table " <> show tblName <> " has primary key " <> show colNames

  predicateSpecificity _ = PredicateSpecificityAllBackends

  serializePredicate (TableHasPrimaryKey tbl cols) =
    object [ "has-primary-key" .= object [ "table" .= tbl
                                         , "columns" .= cols ] ]

  predicateCascadesDropOn (TableHasPrimaryKey tblNm _) p'
    | Just (TableExistsPredicate tblNm') <- cast p' = tblNm' == tblNm
    | otherwise = False

data TableHasIndex = TableHasIndex
  { hasIndex_table :: QualifiedName
  , hasIndex_name :: Text
  , hasIndex_constraint :: Maybe IndexConstraint
  , hasIndex_columns :: [Text]
  , hasIndex_predicate :: Maybe Text
  } deriving (Generic, Show, Eq)

instance Hashable TableHasIndex

instance DatabasePredicate TableHasIndex where
  englishDescription (TableHasIndex tblName name constraint clause predicate) =
    "Table "
      <> show tblName
      <> " has "
      <> maybe mempty show constraint
      <> " index "
      <> unpack name
      <> " on "
      <> unpack (intercalate ", " clause)
      <> maybe mempty ((" with predicate " <>) . unpack) predicate
  predicateSpecificity _ = PredicateSpecificityAllBackends
  serializePredicate (TableHasIndex tbl nm constraint columns predicate) =
    object
      [ "has-index"
          .= object
               [ "table" .= tbl
               , "name" .= nm
               , "constraint" .= constraint
               , "columns" .= columns
               , "predicate" .= predicate
               ]
      ]
  predicateCascadesDropOn (TableHasIndex tblNm _ _ _ _) p'
    | Just (TableHasIndex tblNm' _ _ _ _) <- cast p' = tblNm' == tblNm
    -- | Just (TableHasColumn tblNm' colNm' _ :: TableHasColumn be) <- cast p' = tblNm' == tblNm && elem colNm' clause -- Would need to add `be` in TableHasIndex definition
    | otherwise = False

-- * Deserialization

-- | 'BeamDeserializers' for all the predicates defined in this module
beamCheckDeserializers
  :: forall be
   . ( Typeable be, BeamMigrateOnlySqlBackend be
     , HasDataTypeCreatedCheck (BeamMigrateSqlBackendDataTypeSyntax be) )
  => BeamDeserializers be
beamCheckDeserializers = mconcat
  [ beamDeserializer (const deserializeTableExistsPredicate)
  , beamDeserializer (const deserializeTableHasPrimaryKeyPredicate)
  , beamDeserializer deserializeTableHasColumnPredicate
  , beamDeserializer deserializeTableColumnHasConstraintPredicate
  , beamDeserializer (const deserializeTableHasIndexPredicate)
  ]
  where
    deserializeTableExistsPredicate :: Value -> Parser SomeDatabasePredicate
    deserializeTableExistsPredicate =
      withObject "TableExistPredicate" $ \v ->
      SomeDatabasePredicate <$> (TableExistsPredicate <$> v .: "table-exists")

    deserializeTableHasPrimaryKeyPredicate :: Value -> Parser SomeDatabasePredicate
    deserializeTableHasPrimaryKeyPredicate =
      withObject "TableHasPrimaryKey" $ \v ->
      v .: "has-primary-key" >>=
      (withObject "TableHasPrimaryKey" $ \v' ->
       SomeDatabasePredicate <$> (TableHasPrimaryKey <$> v' .: "table" <*> v' .: "columns"))

    deserializeTableHasColumnPredicate :: BeamDeserializers be'
                                       -> Value -> Parser SomeDatabasePredicate
    deserializeTableHasColumnPredicate d =
      withObject "TableHasColumn" $ \v ->
      v .: "has-column" >>=
      (withObject "TableHasColumn" $ \v' ->
       SomeDatabasePredicate <$>
       fmap (id @(TableHasColumn be))
         (TableHasColumn <$> v' .: "table" <*> v' .: "column"
                         <*> (beamDeserialize d =<< v' .: "type")))

    deserializeTableColumnHasConstraintPredicate :: BeamDeserializers be'
                                                 -> Value -> Parser SomeDatabasePredicate
    deserializeTableColumnHasConstraintPredicate d =
      withObject "TableColumnHasConstraint" $ \v ->
      v .: "has-column-constraint" >>=
      (withObject "TableColumnHasConstraint" $ \v' ->
       SomeDatabasePredicate <$>
       fmap (id @(TableColumnHasConstraint be))
         (TableColumnHasConstraint <$> v' .: "table" <*> v' .: "column"
                                   <*> (beamDeserialize d =<< v' .: "constraint")))

    deserializeTableHasIndexPredicate :: Value -> Parser SomeDatabasePredicate
    deserializeTableHasIndexPredicate =
      withObject "TableHasIndex" $ \v ->
        v .: "has-index"
          >>= withObject "TableHasIndex" (\v' ->
                 SomeDatabasePredicate
                   <$> (TableHasIndex
                          <$> v' .: "table"
                          <*> v' .: "name"
                          <*> v' .: "constraint"
                          <*> v' .: "columns"
                          <*> v' .: "predicate"))

-- * Utilities

-- This function simplyfies predicate by removing extra brackets and type information
-- TODO: Improve value parser
simplifyIndexPredicate :: Text -> Text
simplifyIndexPredicate predicate =
  case FP.runParser parser $ encodeUtf8 predicate of
    FP.OK a _ -> pack a
    FP.Fail -> error $ "simplifyIndexPredicate: Parser failed for input " ++ unpack predicate
    FP.Err err -> error $ "simplifyIndexPredicate: Parser failed with err: " ++ err ++ " for input " ++ unpack predicate
  where
    parseEntity :: Char -> FP.Parser String String
    parseEntity quote = do
      FP.skipMany $ FP.skipSatisfy (== ' ')
      FP.skipMany $ FP.skipSatisfy (== '(')
      ch <- FP.lookahead FP.anyChar
      let isEntityQuoted = ch == quote
      FP.optional_ $ FP.skipSatisfy (== quote)
      entity <-
        FP.some $ do
          FP.satisfy FP.isLatinLetter
            FP.<|> FP.satisfy FP.isDigit
            FP.<|> FP.satisfy (== '_')
      if isEntityQuoted
        then FP.skipSatisfy (== quote)
        else FP.optional_ $ FP.skipSatisfy (== quote)
      FP.skipMany $ FP.skipSatisfy (== ')')
      FP.optional_ $ do
        FP.skipSatisfy (== ':')
        FP.skipSatisfy (== ':')
        FP.skipSome $ FP.skipSatisfy (/= ' ')
        FP.skipMany $ FP.skipSatisfy (== ')')
      return
        $ if quote == '"'
            then "\"" ++ entity ++ "\""
            else if isEntityQuoted
                   then [quote] ++ entity ++ [quote]
                   else entity
    parseClause :: FP.Parser String String
    parseClause = do
      columnName <- parseEntity '"'
      FP.skipMany $ FP.skipSatisfy (== ' ')
      operator <- FP.some $ FP.satisfy (/= ' ')
      FP.skipMany $ FP.skipSatisfy (== ' ')
      value <- parseEntity '\''
      FP.skipMany $ FP.skipSatisfy (== ' ')
      return $ columnName ++ " " ++ operator ++ " " ++ value
    andParser :: FP.Parser String String
    andParser = do
      FP.skipSatisfy (== 'a') FP.<|> FP.skipSatisfy (== 'A')
      FP.skipSatisfy (== 'n') FP.<|> FP.skipSatisfy (== 'N')
      FP.skipSatisfy (== 'd') FP.<|> FP.skipSatisfy (== 'D')
      return "AND"
    orParser :: FP.Parser String String
    orParser = do
      FP.skipSatisfy (== 'o') FP.<|> FP.skipSatisfy (== 'O')
      FP.skipSatisfy (== 'r') FP.<|> FP.skipSatisfy (== 'R')
      return "OR"
    parser :: FP.Parser String String
    parser = do
      cond1 <- parseClause
      rest <-
        FP.many
          $ FP.withOption
              (andParser FP.<|> orParser)
              (\operator -> do
                 cond2 <- parseClause
                 return $ operator ++ " " ++ cond2)
              FP.empty
      FP.eof
      return
        $ case rest of
            [] -> cond1
            _rest -> cond1 ++ " " ++ unwords _rest
