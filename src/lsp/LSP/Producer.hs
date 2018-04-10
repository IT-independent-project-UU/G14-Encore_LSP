module LSP.Producer (
    preCheckProgramTable,
    typeCheckProgramTable
) where

{- Run a pre-check on the program table before the type-checking pass

Param: Program table to run pre-check on
-}
preCheckProgramTable :: ProgramTable -> IO ProgramTable
precheckProgramTable table = do
let lookupTableTable = fmap buildLookupTable table
    mapM (precheckAndShowWarnings lookupTableTable) table
    where
    precheckAndShowWarnings table p = do
        (precheckedAST, precheckingWarnings) <-
            case precheckProgram table p of
            (Right ast, warnings)  -> return (ast, warnings)
            (Left error, warnings) -> do
                showWarnings warnings
                abort $ show error
        showWarnings precheckingWarnings
        return precheckedAST

{- Run type checking on a ProgramTable to produce a type-checked ProgramTable

Param: Program table to run type-checking on
-}
typeCheckProgramTable :: ProgramTable -> IO ProgramTable
typecheckProgramTable table = do
    let lookupTableTable = fmap buildLookupTable table
    mapM (typecheckAndShowWarnings lookupTableTable) table
    where
        typecheckAndShowWarnings table p = do
        (typecheckedAST, typecheckingWarnings) <-
            case typecheckProgram table p of
                (Right (newEnv, ast), warnings) -> return (ast, warnings)
                (Left error, warnings) -> do
                showWarnings warnings
                abort $ show error
        showWarnings typecheckingWarnings
        return typecheckedAST
