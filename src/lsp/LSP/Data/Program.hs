module LSP.Data.Program (
    Program(..),
    ProgramInfo(..),
    makeBlankProgram,
    makeBlankAST,
    getProgramInfoDescription,
    getProgramInfoRange,
    getProgramInfoForPos,

    dumpProgramErrors
 ) where

-- ###################################################################### --
-- Section: Imports
-- ###################################################################### --

-- Standard
import System.Exit
import Debug.Trace as Debug
import Data.List as List

-- Encore
import qualified AST.AST as AST
import qualified AST.Meta as ASTMeta
import qualified Typechecker.Util as TypeUtil
import qualified Types

-- LSP
import LSP.Data.Error
import qualified LSP.Data.Position as LSP

-- ###################################################################### --
-- Section: Data
-- ###################################################################### --

{- Data that holds information about a single program (compilation unit). This
    can later be used to lookup information.

    The error and warning-list will contain all the errors and warnings,
    respectively, that was generated during the compilation that generated the
    AST. If the program compiled correctly the list of errors will be empty.
    Even during successfull compilation the warnings list may contain items.
-}
data Program = Program {
    ast         :: AST.Program,     -- The "AST" of the program
    errors      :: [Error],         -- List of all errors
    warnings    :: [Error]          -- List of warnings.
} deriving (Show)

{- Data that holds information about the result of a lookup in a program. This
    is a String containing the description of the error as well as the
    range-position in the source file that the info describes.

    The range position is used by LSP to know in what range it the client does
    not need to request new information during, for example, hovering the cursor
    over the source.
-}
data ProgramInfo = ProgramInfo {
    pDesc       :: String,          -- Description of looked-up info.
    pRange      :: LSP.Range        -- Range of the info in source.
}

-- ###################################################################### --
-- Section: Functions
-- ###################################################################### --

{- Make a blank program data. The makeBlankAST will be used to create an empty
    AST and the error and warning lists will be set to empty
-}
makeBlankProgram :: FilePath -> Program
makeBlankProgram path = Program {
    ast = makeBlankAST path,
    errors = [],
    warnings = []
}

{- Make a blank AST.  -}
makeBlankAST :: FilePath -> AST.Program
makeBlankAST path = AST.Program {
    AST.source = path,
    AST.moduledecl = AST.NoModule,
    AST.etl = [],
    AST.imports = [],
    AST.typedefs = [],
    AST.functions = [],
    AST.traits = [],
    AST.classes = []
}

{- Make a program info data from the description of the info as well as the
    position range in the source that will yield the same info

    Param: Description of the information.
    Param: Range position in source that yields same info.
-}
makeProgramInfo :: String -> LSP.Range -> ProgramInfo
makeProgramInfo desc range = ProgramInfo{pDesc = desc, pRange = range}

{- Returns the description of a ProgramInfo data.

    Param: Program info.
    Return: Description of program info.
-}
getProgramInfoDescription :: ProgramInfo -> String
getProgramInfoDescription info = (pDesc info)

{- Returns the range position from a ProgramInfo data.

    Param: Program info
    Return: Range position of program info.
-}
getProgramInfoRange :: ProgramInfo -> LSP.Range
getProgramInfoRange info = (pRange info)

{- Utility function that can be used when a SingletonPos is encountered in the
    AST. It will print a message that warns about the SingletonPos. This is used
    because there should not exists any SingletonPos in the AST.

    Return: Always returns Nothing but also prints error to output.
-}
handleSingletonPos :: (Maybe ProgramInfo)
handleSingletonPos = (trace "#Error: cannot handle singleton pos" Nothing)

{- Retrieve information from the cursor position in the specified program. This
    will traverse the AST until it finds the most specific information about the
    position that is specified. The function may also fail to find any
    information if an error occurs or if the cursor position is on a whitespace.

    Param: Cursor position to look at.
    Param: Program to search in.
    Return: Program information for position or "Nothing"
-}
getProgramInfoForPos :: LSP.Position -> Program -> (Maybe ProgramInfo)
getProgramInfoForPos pos program = do
    -- Try to get function info
    let functionInfo = (getProgramInfoFunction pos (AST.functions $ ast program))
    case functionInfo of
        Just info   -> Just info
        Nothing     -> do
            -- Try to get class info
            let classInfo = (getProgramInfoClass pos (AST.classes $ ast program))
            case classInfo of
                Just info   -> Just info
                Nothing     -> Nothing

{-


-}
getProgramInfoFunction :: LSP.Position -> [AST.Function] -> (Maybe ProgramInfo)
getProgramInfoFunction _ [] = Nothing
getProgramInfoFunction pos (x:xs) =
    -- Check if singleton or range pos
    case (ASTMeta.getPos (AST.funmeta x)) of
        ASTMeta.SingletonPos _      -> handleSingletonPos
        ASTMeta.RangePos start end  -> do
            -- Check if pos is in function
            case LSP.inRange pos (LSP.fromSourcePosRange start end) of
                False   -> getProgramInfoFunction pos xs
                True    -> do
                    -- Check if pos is on paramDecl
                    let paramDeclInfo = getProgramInfoParamDecl pos (AST.hparams $ AST.funheader x)
                    case paramDeclInfo of
                        Just info   -> Just info
                        Nothing     -> do
                            -- Check if pos is in body
                            let bodyInfo = getProgramInfoExpr pos False (AST.funbody x)
                            case bodyInfo of
                                Just info   -> Just info
                                Nothing     -> do
                                    -- Check if pos is in local function
                                    let functionInfo = getProgramInfoFunction pos (AST.funlocals x)
                                    case functionInfo of
                                        Just info   -> Just info
                                        Nothing     -> Nothing

{-  -}
getProgramInfoClass :: LSP.Position -> [AST.ClassDecl] -> (Maybe ProgramInfo)
getProgramInfoClass _ [] = Nothing
getProgramInfoClass pos (x:xs) = do
    -- Check if pos is singleton or range pos
    case (ASTMeta.getPos (AST.cmeta x)) of
        ASTMeta.SingletonPos _      -> handleSingletonPos
        ASTMeta.RangePos start end  -> do
            -- Check if pos is in class
            case LSP.inRange pos (LSP.fromSourcePosRange start end) of
                False   -> getProgramInfoClass pos xs
                True    -> do
                    -- Check if pos is in field decl
                    let fieldInfo = getProgramInfoFieldDecl pos (AST.cfields x)
                    case fieldInfo of
                        Just info   -> Just info
                        Nothing     -> do
                            -- Check if pos is in method decl
                            let methodInfo = getProgramInfoMethodDecl pos (AST.cmethods x)
                            case methodInfo of
                                Just info   -> Just info
                                Nothing     -> Nothing

{-  -}
getProgramInfoFieldDecl :: LSP.Position -> [AST.FieldDecl] -> (Maybe ProgramInfo)
getProgramInfoFieldDecl _ [] = Nothing
getProgramInfoFieldDecl pos (x:xs) = Nothing

{-  -}
getProgramInfoMethodDecl :: LSP.Position -> [AST.MethodDecl] -> (Maybe ProgramInfo)
getProgramInfoMethodDecl _ [] = Nothing
getProgramInfoMethodDecl pos (x:xs) = do
    case AST.isImplicitMethod x of
        True    -> getProgramInfoMethodDecl pos xs
        False   -> do
            -- Check if pos is singleton or range pos
            case (ASTMeta.getPos (AST.mmeta x)) of
                ASTMeta.SingletonPos _      -> handleSingletonPos
                ASTMeta.RangePos start end  -> do
                    -- Check if pos is in method
                    case LSP.inRange pos (LSP.fromSourcePosRange start end) of
                        False   -> getProgramInfoMethodDecl pos xs
                        True    -> do
                            -- Check if pos is on paramDecl
                            let paramDeclInfo = getProgramInfoParamDecl pos (AST.hparams $ AST.mheader x)
                            case paramDeclInfo of
                                Just info   -> Just info
                                Nothing     -> do
                                    -- Check if pos is in body
                                    let bodyInfo = getProgramInfoExpr pos False (AST.mbody x)
                                    case bodyInfo of
                                        Just info   -> Just info
                                        Nothing     -> do
                                            -- Check if pos is in local function
                                            let localFunInfo = getProgramInfoFunction pos (AST.mlocals x)
                                            case localFunInfo of
                                                Just info   -> Just info
                                                Nothing -> Nothing

getProgramInfoParamDecl :: LSP.Position -> [AST.ParamDecl] -> (Maybe ProgramInfo)
getProgramInfoParamDecl _ [] = Nothing
getProgramInfoParamDecl pos (x:xs) =
    -- Check if singleton or range pos
    case (ASTMeta.getPos (AST.pmeta x)) of
        ASTMeta.SingletonPos _      -> handleSingletonPos
        ASTMeta.RangePos start end  -> do
            -- Check if pos is on parameter
            case LSP.inRange pos (LSP.fromSourcePosRange start end) of
                False -> getProgramInfoParamDecl pos xs
                True  -> do
                    let desc =  "Hovering over parameter " ++ show (AST.pname x)
                    let range = (LSP.fromSourcePosRange start end)
                    Just $ makeProgramInfo desc range

{-
@param range Contains range information about the entire declaration
-}
getProgramInfoDecls :: LSP.Position -> LSP.Range -> [([AST.VarDecl], AST.Expr)] -> (Maybe ProgramInfo)
getProgramInfoDecls _ _ [] = Nothing
getProgramInfoDecls pos range (_decl:[]) = do
  let expr = snd _decl
  case ASTMeta.getPos (AST.emeta expr) of
    ASTMeta.SingletonPos _     -> handleSingletonPos
    ASTMeta.RangePos start end -> do
      let exprInfo = getProgramInfoExpr pos False expr
      case exprInfo of
        Just info -> do
          let exprRange = getProgramInfoRange info
          case LSP.inRange pos exprRange of
            True -> exprInfo
            False -> do
              let complimentRange = LSP.rangeCompliment range (getProgramInfoRange info)
              Just $ makeProgramInfo (getProgramInfoDecl pos  _decl) complimentRange
        Nothing -> do
          let complimentRange = LSP.rangeCompliment range (LSP.fromSourcePosRange start end)
          Just $ makeProgramInfo (getProgramInfoDecl pos  _decl) complimentRange
getProgramInfoDecls pos _ _decls = getProgramInfoDeclsInner pos _decls

getProgramInfoDeclsInner :: LSP.Position -> [([AST.VarDecl], AST.Expr)] -> (Maybe ProgramInfo)
getProgramInfoDeclsInner _ [] = Nothing
getProgramInfoDeclsInner pos (_decl:_decls) = do
  let expr = snd _decl
  case ASTMeta.getPos (AST.emeta expr) of
    ASTMeta.SingletonPos _     -> handleSingletonPos
    ASTMeta.RangePos start end -> do
      case LSP.inRange pos (LSP.fromSourcePosRange start end) of
        False -> getProgramInfoDeclsInner pos _decls
        True  -> getProgramInfoExpr pos False expr

getProgramInfoDecl :: LSP.Position -> ([AST.VarDecl], AST.Expr) -> String
getProgramInfoDecl pos _decl@(varDecls, expr) = do
  (List.intercalate ", " (fmap (\x -> show (AST.varName x)) varDecls)) ++ " :: " ++ (show (AST.getType expr))

getProgramInfoExpr :: LSP.Position -> Bool -> AST.Expr -> (Maybe ProgramInfo)
getProgramInfoExpr pos ignorePos expr = do
    -- Check if singleton or range pos
    case (ASTMeta.getPos (AST.getMeta expr)) of
        ASTMeta.SingletonPos _      -> handleSingletonPos
        ASTMeta.RangePos start end  -> do
            case expr of
                AST.Skip meta
                    -> Nothing --Just $ makeProgramInfo "Skip" (LSP.fromSourcePosRange start end)
                AST.Break meta
                    -> Just $ makeProgramInfo "Break" (LSP.fromSourcePosRange start end)
                AST.Continue meta
                    -> Just $ makeProgramInfo "Continue" (LSP.fromSourcePosRange start end)
                AST.TypedExpr meta body ty
                    -> Just $ makeProgramInfo "TypedExpr" (LSP.fromSourcePosRange start end)
                AST.MethodCall meta tyArgs target name args
                    ->  -- Check if pos is in message send
                        case LSP.inRange pos (LSP.fromSourcePosRange start end) of
                            False   -> Nothing
                            True    -> do
                                let desc =  (buildSignature (show name) False args (AST.getType expr))
                                let range = (LSP.fromSourcePosRange start end)
                                Just $ makeProgramInfo desc range
                AST.MessageSend meta tyArgs target name args
                    ->  -- Check if pos is in message send
                        case LSP.inRange pos (LSP.fromSourcePosRange start end) of
                            False   -> Nothing
                            True    -> do
                                let desc = (buildSignature (show name) True args (AST.getType expr))
                                let range = (LSP.fromSourcePosRange start end)
                                Just $ makeProgramInfo desc range
                AST.Optional meta optTag
                    -> Just $ makeProgramInfo "Optional" (LSP.fromSourcePosRange start end)
                AST.ExtractorPattern meta ty name arg
                    -> Just $ makeProgramInfo "Extractorp attern" (LSP.fromSourcePosRange start end)
                AST.FunctionCall meta tyArgs name args
                    ->  -- Check if pos is in message send
                        case LSP.inRange pos (LSP.fromSourcePosRange start end) of
                            False   -> Nothing
                            True    -> do
                                let desc = (buildSignature (show name) False args (AST.getType expr))
                                let range = (LSP.fromSourcePosRange start end)
                                Just $ makeProgramInfo desc range
                AST.FunctionAsValue meta tyArgs name
                    -> Just $ makeProgramInfo "Function as value" (LSP.fromSourcePosRange start end)
                AST.Closure meta params maybeType body
                    -> Just $ makeProgramInfo "Closure" (LSP.fromSourcePosRange start end)
                AST.PartySeq meta par seqFun
                    -> Just $ makeProgramInfo "PartySeq" (LSP.fromSourcePosRange start end)
                AST.PartyPar meta parl parr
                    -> Just $ makeProgramInfo "PartyPar" (LSP.fromSourcePosRange start end)
                AST.PartyReduce meta seqFun init par runassoc
                    -> Just $ makeProgramInfo "PartyReduce" (LSP.fromSourcePosRange start end)
                AST.Async meta body
                    -> Just $ makeProgramInfo "Async" (LSP.fromSourcePosRange start end)
                AST.Return meta value
                    -> Just $ makeProgramInfo "Return" (LSP.fromSourcePosRange start end)
                AST.MaybeValue meta container
                    -> Just $ makeProgramInfo "Maybe value" (LSP.fromSourcePosRange start end)
                AST.Tuple meta args
                    -> Just $ makeProgramInfo "Tuple" (LSP.fromSourcePosRange start end)
                AST.Let meta mutability _decls _body
                        ->  do
                  -- Check if pos is in message send
                  let range = (LSP.fromSourcePosRange start end)
                  case LSP.inRange pos range of
                    False   -> getProgramInfoExpr pos False _body
                    True    -> do
                      let declInfo = getProgramInfoDecls pos range _decls
                      case declInfo of
                        Nothing   -> getProgramInfoExpr pos False _body
                        Just programInfo -> Just programInfo

                AST.MiniLet meta mutability decl
                    ->  Just $ makeProgramInfo "MiniLet" (LSP.fromSourcePosRange start end)
                AST.Seq meta eseq
                    ->  do
                    -- Get body info
                    let innerInfo = getProgramInfoBodySeq pos eseq
                    case (Debug.trace ("SEQ - INNER INFO: " ++ show innerInfo ++ ", expr count: " ++ show (length eseq)) innerInfo) of
                        Just info   -> Just info
                        Nothing     -> Nothing
                AST.IfThenElse meta cond thn els
                    -> Just $ makeProgramInfo "IfThenElse" (LSP.fromSourcePosRange start end)
                AST.IfThen meta cond thn
                    -> Just $ makeProgramInfo "IfThen" (LSP.fromSourcePosRange start end)
                AST.Unless meta cond thn
                    -> Just $ makeProgramInfo "Unless" (LSP.fromSourcePosRange start end)
                AST.While meta cond body
                    -> Just $ makeProgramInfo "While" (LSP.fromSourcePosRange start end)
                AST.DoWhile meta cond body
                    -> Just $ makeProgramInfo "DoWhile" (LSP.fromSourcePosRange start end)
                AST.Repeat meta name times body
                    -> Just $ makeProgramInfo "Repeat" (LSP.fromSourcePosRange start end)
                AST.For meta name step src body
                    -> Just $ makeProgramInfo "For" (LSP.fromSourcePosRange start end)
                AST.Match meta arg clauses
                    -> Just $ makeProgramInfo "Match" (LSP.fromSourcePosRange start end)
                AST.Borrow meta target name body
                    -> Just $ makeProgramInfo "Borrow" (LSP.fromSourcePosRange start end)
                AST.Get meta value
                    -> Just $ makeProgramInfo "Get" (LSP.fromSourcePosRange start end)
                AST.Forward meta forwardExpr
                    -> Just $ makeProgramInfo "Forward" (LSP.fromSourcePosRange start end)
                AST.Yield meta value
                    -> Just $ makeProgramInfo "Yield" (LSP.fromSourcePosRange start end)
                AST.Eos meta
                    -> Just $ makeProgramInfo "EOS" (LSP.fromSourcePosRange start end)
                AST.IsEos meta target
                    -> Just $ makeProgramInfo "IsEOS" (LSP.fromSourcePosRange start end)
                AST.StreamNext meta target
                    -> Just $ makeProgramInfo "StreamNext" (LSP.fromSourcePosRange start end)
                AST.Await meta value
                    -> Just $ makeProgramInfo "Await" (LSP.fromSourcePosRange start end)
                AST.Suspend meta
                    -> Just $ makeProgramInfo "Suspend" (LSP.fromSourcePosRange start end)
                AST.FutureChain meta future chain
                    -> Just $ makeProgramInfo "FutureChain" (LSP.fromSourcePosRange start end)
                AST.FieldAccess meta target name
                    -> Just $ makeProgramInfo "FieldAccess" (LSP.fromSourcePosRange start end)
                AST.ArrayAccess meta target name
                    -> Just $ makeProgramInfo "ArrayAccess" (LSP.fromSourcePosRange start end)
                AST.ArraySize meta target
                    -> Just $ makeProgramInfo "ArraySize" (LSP.fromSourcePosRange start end)
                AST.ArrayNew meta ty size
                    -> Just $ makeProgramInfo "ArrayNew" (LSP.fromSourcePosRange start end)
                AST.ArrayLiteral meta args
                    -> Just $ makeProgramInfo "ArrayLiteral" (LSP.fromSourcePosRange start end)
                AST.Assign emeta rhs lhs
                    -> Just $ makeProgramInfo "Assign" (LSP.fromSourcePosRange start end)
                AST.VarAccess meta qname
                    ->  do
                    -- Check if pos is in var access
                    let ty = (AST.getType expr)
                    case (LSP.inRange pos (LSP.fromSourcePosRange start end)) || ignorePos of
                        False   -> do
                            let desc = "ERROR WITH VARIABLE ACCESS"
                            let range = (LSP.fromSourcePosRange start end)
                            Just $ makeProgramInfo desc range
                        True    -> do
                            let desc = (getTypeInfo ty)
                            let range = (LSP.fromSourcePosRange start end)
                            Just $ makeProgramInfo desc range
                AST.TupleAccess meta target compartment
                    -> Just $ makeProgramInfo "TupleAccess" (LSP.fromSourcePosRange start end)
                AST.Consume meta target
                    -> Just $ makeProgramInfo "Consume" (LSP.fromSourcePosRange start end)
                AST.Null meta
                    -> Just $ makeProgramInfo "Null" (LSP.fromSourcePosRange start end)
                AST.BTrue meta
                    -> Just $ makeProgramInfo "True" (LSP.fromSourcePosRange start end)
                AST.BFalse meta
                    -> Just $ makeProgramInfo "False" (LSP.fromSourcePosRange start end)
                AST.NewWithInit meta ty args
                    -> Just $ makeProgramInfo "NewWithInit" (LSP.fromSourcePosRange start end)
                AST.New meta ty
                    -> Just $ makeProgramInfo "New" (LSP.fromSourcePosRange start end)
                AST.Print meta file args
                    -> do
                        let ty = (AST.getType expr)
                        case (LSP.inRange pos (LSP.fromSourcePosRange start end)) || ignorePos of
                            False   -> Nothing
                            True    -> do
                                let desc = "Print" --(getTypeInfo ty)
                                let range = (LSP.fromSourcePosRange start end)
                                Just $ makeProgramInfo desc range
                AST.Exit meta args
                    -> Just $ makeProgramInfo "Exit" (LSP.fromSourcePosRange start end)
                AST.Abort meta args
                    -> Just $ makeProgramInfo "Abort" (LSP.fromSourcePosRange start end)
                AST.StringLiteral meta literal
                    -> Just $ makeProgramInfo "StringLiteral" (LSP.fromSourcePosRange start end)
                AST.CharLiteral meta literal
                    -> Just $ makeProgramInfo "CharLiteral" (LSP.fromSourcePosRange start end)
                AST.RangeLiteral meta rstart rstop step
                    -> Just $ makeProgramInfo "RangeLiteral" (LSP.fromSourcePosRange start end)
                AST.IntLiteral meta literal
                    -> Just $ makeProgramInfo "IntLiteral" (LSP.fromSourcePosRange start end)
                AST.UIntLiteral meta literal
                    -> Just $ makeProgramInfo "UIntLiteral" (LSP.fromSourcePosRange start end)
                AST.RealLiteral meta literal
                    -> Just $ makeProgramInfo "RealLiteral" (LSP.fromSourcePosRange start end)
                AST.Embed meta ty embedded
                    -> Just $ makeProgramInfo "Embed" (LSP.fromSourcePosRange start end)
                AST.Unary meta op operand
                    -> Just $ makeProgramInfo "Unary op" (LSP.fromSourcePosRange start end)
                AST.Binop meta op loper roper
                    -> do
                  case ASTMeta.getPos (AST.getMeta loper) of
                    ASTMeta.SingletonPos _      -> handleSingletonPos
                    ASTMeta.RangePos lstart lend  -> do
                      -- are we in loper?
                      let lrange = LSP.fromSourcePosRange lstart lend
                      case LSP.inRange pos lrange of
                        True -> getProgramInfoExpr pos False loper
                        False ->
                          case ASTMeta.getPos (AST.getMeta roper) of
                            ASTMeta.SingletonPos _      -> handleSingletonPos
                            ASTMeta.RangePos rstart rend  -> do
                              -- are we in roper?
                              let rrange = LSP.fromSourcePosRange rstart rend
                              case LSP.inRange pos rrange of
                                True -> getProgramInfoExpr pos False roper
                                False -> do
                                  let binopRange = LSP.fromSourcePosRange start end
                                  let wholeRange = LSP.widestRange binopRange [lrange, rrange]
                                  Just $ makeProgramInfo "Binary op" wholeRange

{-  -}
getProgramInfoBodySeq :: LSP.Position -> [AST.Expr] -> (Maybe ProgramInfo)
getProgramInfoBodySeq _ [] = Nothing
getProgramInfoBodySeq pos (x:xs) =
    -- Check if singleton or range pos
    case (ASTMeta.getPos (AST.getMeta x)) of
        ASTMeta.SingletonPos _      -> handleSingletonPos
        ASTMeta.RangePos start end  -> do
            let exprInfo = getProgramInfoExpr pos False x
            case (Debug.trace ("BODY - EXPR INFO: " ++ show exprInfo ++ show (LSP.fromSourcePosRange start end)) exprInfo) of
                Just info   -> Just info
                Nothing     -> getProgramInfoBodySeq pos xs

-- ###################################################################### --
-- Section: Type functions
-- ###################################################################### --

{-  -}
getTypeInfo :: Types.Type -> String
getTypeInfo ty =
    -- Pattern-match the inner type
    case Types.getInnerType ty of
        Types.Unresolved refInfo
            -> "Unresolved type"
        Types.ClassType refInfo
            ->  case (Types.getRefInfoMode refInfo) of
                    Nothing -> "class" ++ (Types.getId ty)
                    Just m  -> (show m) ++ " class " ++ (Types.getId ty)
        _   -> show ty ++ " - NO INFO FOR TYPE"

{-  -}
buildSignatureParamType :: AST.Expr -> String
buildSignatureParamType expr = show (AST.getType expr)

{-  -}
buildSignatureParamList :: [AST.Expr] -> String
buildSignatureParamList [] = ""
buildSignatureParamList (x:[]) = (buildSignatureParamType x)
buildSignatureParamList (x:xs) =
    (buildSignatureParamType x) ++ ", " ++ (buildSignatureParamList xs)

{-  -}
buildSignatureReturnType :: Bool -> Types.Type -> String
buildSignatureReturnType isMsg ret =
    -- Check if message or standard call
    case isMsg of
        False   -> show ret
        True    -> do
            -- Get the type contained in future
            case Types.getInnerType ret of
                Types.FutureType resType    -> show resType
                _                           -> "[INVALID MESSAGE RESULT]"

{-  -}
buildSignature :: String -> Bool -> [AST.Expr] -> Types.Type -> String
buildSignature name isMsg argTypes ret =
    name ++ "(" ++ (buildSignatureParamList argTypes) ++ "): " ++ (buildSignatureReturnType isMsg ret)

-- ###################################################################### --
-- Section: Debug functions
-- ###################################################################### --

{-  -}
dumpProgramErrors :: Program -> IO ()
dumpProgramErrors program = do
    case (length $ errors program) > 0 || (length $ warnings program) > 0 of
        True -> do
            putStrLn $ "Errors and warnings for " ++ (AST.source (ast program))
            mapM_ (\x -> putStrLn $ show x) (errors program)
            mapM_ (\x -> putStrLn $ show x) (warnings program)
            putStrLn ""
        False -> return ()

-- ###################################################################### --
-- Section: Debug functions
-- ###################################################################### --

instance Show ProgramInfo where
    show (ProgramInfo pDesc pRange) = pDesc ++ (show pRange)
