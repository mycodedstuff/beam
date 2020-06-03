module Database.Beam.Query.Extra
  ( update'
  , save'
  ) where

import           Control.Monad.Identity          (Identity)
import           Control.Monad.Writer            (execWriter, tell)
import           Database.Beam.Backend.SQL
import           Database.Beam.Query             (SqlUpdate (..), setFieldsTo)
import           Database.Beam.Query.Combinators (SqlValableTable, current_,
                                                  references_', val_, (<-.))
import           Database.Beam.Query.Internal
import           Database.Beam.Query.Operator    (SqlBool)
import           Database.Beam.Query.Ord         (HasTableEquality)
import           Database.Beam.Schema.Tables
import           Lens.Micro                      ((^.))

-- SqlBool variants of updates
-- these functions are copies of non-asterisk ones from Beam.Query
-- All changes are in type signature only, except
-- use of a new references_' combinator which returns SqlBool

update' :: ( BeamSqlBackend be, Beamable table )
       => DatabaseEntity be db (TableEntity table)
          -- ^ The table to insert into
       -> (forall s. table (QField s) -> QAssignment be s)
          -- ^ A sequence of assignments to make.
       -> (forall s. table (QExpr be s) -> QExpr be s SqlBool)
          -- ^ Build a @WHERE@ clause given a table containing expressions
       -> SqlUpdate be table
update' (DatabaseEntity dt@(DatabaseTable {})) mkAssignments mkWhere =
  case assignments of
    [] -> SqlIdentityUpdate
    _  -> SqlUpdate (dbTableSettings dt)
                    (updateStmt (tableNameFromEntity dt)
                       assignments (Just (where_ "t")))
  where
    QAssignment assignments = mkAssignments tblFields
    QExpr where_ = mkWhere tblFieldExprs

    tblFields = changeBeamRep (\(Columnar' fd) -> Columnar' (QField False (dbTableCurrentName dt) (fd ^. fieldName)))
                              (dbTableSettings dt)
    tblFieldExprs = changeBeamRep (\(Columnar' (QField _ _ nm)) -> Columnar' (QExpr (pure (fieldE (unqualifiedField nm))))) tblFields

save' :: forall table be db.
        ( Table table
        , BeamSqlBackend be

        , SqlValableTable be (PrimaryKey table)
        , SqlValableTable be table

        , HasTableEquality be (PrimaryKey table)
        )
     => DatabaseEntity be db (TableEntity table)
        -- ^ Table to update
     -> table Identity
        -- ^ Value to set to
     -> SqlUpdate be table
save' tbl v =
  updateTableRow' tbl v
    (setFieldsTo (val_ v))

updateTableRow' :: ( BeamSqlBackend be, Table table
                  , HasTableEquality be (PrimaryKey table)
                  , SqlValableTable be (PrimaryKey table) )
               => DatabaseEntity be db (TableEntity table)
                  -- ^ The table to update
               -> table Identity
                  -- ^ The row to update
               -> table (QFieldAssignment be table)
                  -- ^ Updates to be made (use 'set' to construct an empty field)
               -> SqlUpdate be table
updateTableRow' tbl row assignments =
  updateTable' tbl assignments (references_' (val_ (pk row)))

updateTable' :: forall table db be
             . ( BeamSqlBackend be, Beamable table )
            => DatabaseEntity be db (TableEntity table)
               -- ^ The table to update
            -> table (QFieldAssignment be table)
               -- ^ Updates to be made (use 'set' to construct an empty field)
            -> (forall s. table (QExpr be s) -> QExpr be s SqlBool)
            -> SqlUpdate be table
updateTable' tblEntity assignments mkWhere =
  let mkAssignments :: forall s. table (QField s) -> QAssignment be s
      mkAssignments tblFields =
        let tblExprs = changeBeamRep (\(Columnar' fd) -> Columnar' (current_ fd)) tblFields
        in execWriter $
           zipBeamFieldsM
             (\(Columnar' field :: Columnar' (QField s) a)
               c@(Columnar' (QFieldAssignment mkAssignment)) ->
                case mkAssignment tblExprs of
                  Nothing -> pure c
                  Just newValue -> do
                    tell (field <-. newValue)
                    pure c)
             tblFields assignments

  in update' tblEntity mkAssignments mkWhere
