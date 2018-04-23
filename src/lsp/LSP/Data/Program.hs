module LSP.Data.Program (
    Program(..),
    makeBlankProgram,
    makeBlankAST,
    getProgramInfoDescription,
    getProgramInfoForPos,

    dumpProgramErrors
 ) where

-- ###################################################################### --
-- Section: Imports
-- ###################################################################### --

-- Standard
import System.Exit
import Debug.Trace

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

{-  -}
data Program = Program {
    ast         :: AST.Program,
    errors      :: [Error],
    warnings    :: [Error]
} deriving (Show)

{-  -}
data ProgramInfo = ProgramInfo {
    pInfo       :: String
}

-- ###################################################################### --
-- Section: Functions
-- ###################################################################### --

{-  -}
makeBlankProgram :: FilePath -> Program
makeBlankProgram path = Program {
    ast = makeBlankAST path,
    errors = [],
    warnings = []
}

{- -}
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

getProgramInfoDescription :: ProgramInfo -> String
getProgramInfoDescription info = (pInfo info)

{-  -}
handleSingletonPos :: (Maybe ProgramInfo)
handleSingletonPos = (trace "#Error: cannot handle singleton pos" Nothing)

{- -}
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

{- -}
getProgramInfoFunction :: LSP.Position -> [AST.Function] -> (Maybe ProgramInfo)
getProgramInfoFunction _ [] = Nothing
getProgramInfoFunction pos (x:xs) =
    -- Check if singleton or range pos
    case (ASTMeta.getPos (AST.funmeta x)) of
        ASTMeta.SingletonPos _      -> handleSingletonPos
        ASTMeta.RangePos start end  -> do
            -- Check if pos is in function
            case LSP.inRange pos (LSP.fromSourcePos start, LSP.fromSourcePos end) of
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
            case LSP.inRange pos (LSP.fromSourcePos start, LSP.fromSourcePos end) of
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
                    case LSP.inRange pos (LSP.fromSourcePos start, LSP.fromSourcePos end) of
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
                    
{-  -}
getProgramInfoParamDecl :: LSP.Position -> [AST.ParamDecl] -> (Maybe ProgramInfo)
getProgramInfoParamDecl _ [] = Nothing
getProgramInfoParamDecl pos (x:xs) =
    -- Check if singleton or range pos
    case (ASTMeta.getPos (AST.pmeta x)) of
        ASTMeta.SingletonPos _      -> handleSingletonPos
        ASTMeta.RangePos start end  -> do
            -- Check if pos is on parameter
            case LSP.inRange pos (LSP.fromSourcePos start, LSP.fromSourcePos end) of
                False   -> getProgramInfoParamDecl pos xs
                True    -> Just $ ProgramInfo{pInfo = "Hovering over parameter " ++ show (AST.pname x)}

{- -}
getProgramInfoExpr :: LSP.Position -> Bool -> AST.Expr -> (Maybe ProgramInfo)
getProgramInfoExpr pos ignorePos expr = do
    -- Check if singleton or range pos
    case (ASTMeta.getPos (AST.getMeta expr)) of
        ASTMeta.SingletonPos _      -> handleSingletonPos
        ASTMeta.RangePos start end  -> do
            case expr of
                AST.Skip meta 
                    -> Just $ ProgramInfo{pInfo = "Skip"}
                AST.Break meta 
                    -> Just $ ProgramInfo{pInfo = "Break"}
                AST.Continue meta 
                    -> Just $ ProgramInfo{pInfo = "Continue"}
                AST.TypedExpr meta body ty 
                    -> Just $ ProgramInfo{pInfo = "Typed expression"}
                AST.MethodCall meta tyArgs target name args 
                    ->  -- Check if pos is in message send
                        case LSP.inRange pos (LSP.fromSourcePos start, LSP.fromSourcePos end) of
                            False   -> Nothing
                            True    -> Just ProgramInfo{pInfo = (buildSignature (show name) False args (AST.getType expr))}
                AST.MessageSend meta tyArgs target name args 
                    ->  -- Check if pos is in message send
                        case LSP.inRange pos (LSP.fromSourcePos start, LSP.fromSourcePos end) of
                            False   -> Nothing
                            True    -> Just ProgramInfo{pInfo = (buildSignature (show name) True args (AST.getType expr))}
                AST.Optional meta optTag 
                    -> Just $ ProgramInfo{pInfo = "Optional"}
                AST.ExtractorPattern meta ty name arg 
                    -> Just $ ProgramInfo{pInfo = "ExtractorPattern"}
                AST.FunctionCall meta tyArgs name args 
                    ->  -- Check if pos is in message send
                        case LSP.inRange pos (LSP.fromSourcePos start, LSP.fromSourcePos end) of
                            False   -> Nothing
                            True    -> Just ProgramInfo{pInfo = (buildSignature (show name) False args (AST.getType expr))}
                AST.FunctionAsValue meta tyArgs name 
                    -> Just $ ProgramInfo{pInfo = "Function as value"}
                AST.Closure meta params maybeType body 
                    -> Just $ ProgramInfo{pInfo = "Closure"}
                AST.PartySeq meta par seqFun 
                    -> Just $ ProgramInfo{pInfo = "PartySeq"}
                AST.PartyPar meta parl parr 
                    -> Just $ ProgramInfo{pInfo = "PartyPar"}
                AST.PartyReduce meta seqFun init par runassoc 
                    -> Just $ ProgramInfo{pInfo = "PartyReduce"}
                AST.Async meta body 
                    -> Just $ ProgramInfo{pInfo = "Async"}
                AST.Return meta value 
                    -> Just $ ProgramInfo{pInfo = "Return"}
                AST.MaybeValue meta container 
                    -> Just $ ProgramInfo{pInfo = "MaybeValue"}
                AST.Tuple meta args 
                    -> Just $ ProgramInfo{pInfo = "Tuple"}
                AST.Let meta mutability decl body 
                    ->  do
                    -- Check let body
                    let bodyInfo = getProgramInfoExpr pos False body
                    case bodyInfo of
                        Just info   -> Just info
                        Nothing     -> Nothing
                AST.MiniLet meta mutability decls 
                    ->  Just $ ProgramInfo{pInfo = "MiniLet"}
                AST.Seq meta eseq 
                    ->  do
                    -- Get body info
                    let innerInfo = getProgramInfoBodySeq pos eseq
                    case innerInfo of
                        Just info   -> Just info
                        Nothing     -> Nothing
                AST.IfThenElse meta cond thn els 
                    -> Just $ ProgramInfo{pInfo = "IfThenElse"}
                AST.IfThen meta cond thn 
                    -> Just $ ProgramInfo{pInfo = "IfThen"}
                AST.Unless meta cond thn 
                    -> Just $ ProgramInfo{pInfo = "Unless"}
                AST.While meta cond body 
                    -> Just $ ProgramInfo{pInfo = "While"}
                AST.DoWhile meta cond body 
                    -> Just $ ProgramInfo{pInfo = "DoWhile"}
                AST.Repeat meta name times body 
                    -> Just $ ProgramInfo{pInfo = "Repeat"}
                AST.For meta name step src body 
                    -> Just $ ProgramInfo{pInfo = "For"}
                AST.Match meta arg clauses 
                    -> Just $ ProgramInfo{pInfo = "Match"}
                AST.Borrow meta target name body 
                    -> Just $ ProgramInfo{pInfo = "Borrow"}
                AST.Get meta value 
                    -> Just $ ProgramInfo{pInfo = "Get"}
                AST.Forward meta forwardExpr 
                    -> Just $ ProgramInfo{pInfo = "ForwardExpr"}
                AST.Yield meta value 
                    -> Just $ ProgramInfo{pInfo = "Yield"}
                AST.Eos meta 
                    -> Just $ ProgramInfo{pInfo = "EOS"}
                AST.IsEos meta target 
                    -> Just $ ProgramInfo{pInfo = "IsEOS"}
                AST.StreamNext meta target 
                    -> Just $ ProgramInfo{pInfo = "StreamNext"}
                AST.Await meta value 
                    -> Just $ ProgramInfo{pInfo = "Await"}
                AST.Suspend meta 
                    -> Just $ ProgramInfo{pInfo = "Suspend"}
                AST.FutureChain meta future chain 
                    -> Just $ ProgramInfo{pInfo = "FutureChain"}
                AST.FieldAccess meta target name 
                    -> Just $ ProgramInfo{pInfo = "FieldAccess"}
                AST.ArrayAccess meta target name 
                    -> Just $ ProgramInfo{pInfo = "ArrayAccess"}
                AST.ArraySize meta target 
                    -> Just $ ProgramInfo{pInfo = "ArraySize"}
                AST.ArrayNew meta ty size
                    -> Just $ ProgramInfo{pInfo = "ArrayNew"}
                AST.ArrayLiteral meta args 
                    -> Just $ ProgramInfo{pInfo = "ArrayLiteral"}
                AST.Assign emeta rhs lhs 
                    -> Just $ ProgramInfo{pInfo = "Assign"}
                AST.VarAccess meta qname
                    ->  do
                    -- Check if pos is in var access
                    let ty = (AST.getType expr)
                    case (LSP.inRange pos (LSP.fromSourcePos start, LSP.fromSourcePos end)) || ignorePos of
                        False   -> Just $ ProgramInfo{pInfo = "Somehow fucked up"}
                        True    -> Just $ ProgramInfo{pInfo = (getTypeInfo ty)}
                AST.TupleAccess meta target compartment 
                    -> Just $ ProgramInfo{pInfo = "TupleAccess"}
                AST.Consume meta target 
                    -> Just $ ProgramInfo{pInfo = "Consume"}
                AST.Null meta
                    -> Just $ ProgramInfo{pInfo = "Null"}
                AST.BTrue meta 
                    -> Just $ ProgramInfo{pInfo = "True"}
                AST.BFalse meta 
                    -> Just $ ProgramInfo{pInfo = "False"}
                AST.NewWithInit meta ty args 
                    -> Just $ ProgramInfo{pInfo = "NewWithInit"}
                AST.New meta ty 
                    -> Just $ ProgramInfo{pInfo = "New"}
                AST.Print meta file args 
                    -> Just $ ProgramInfo{pInfo = "Print"}
                AST.Exit meta args 
                    -> Just $ ProgramInfo{pInfo = "Exit"}
                AST.Abort meta args 
                    -> Just $ ProgramInfo{pInfo = "Abort"}
                AST.StringLiteral meta literal 
                    -> Just $ ProgramInfo{pInfo = "String literal"}
                AST.CharLiteral meta literal
                    -> Just $ ProgramInfo{pInfo = "Char literal"}
                AST.RangeLiteral meta rstart rstop step 
                    -> Just $ ProgramInfo{pInfo = "Range literal"}
                AST.IntLiteral meta literal 
                    -> Just $ ProgramInfo{pInfo = "Int literal"}
                AST.UIntLiteral meta literal 
                    -> Just $ ProgramInfo{pInfo = "UInt literal"}
                AST.RealLiteral meta literal 
                    -> Just $ ProgramInfo{pInfo = "Real literal"}
                AST.Embed meta ty embedded 
                    -> Just $ ProgramInfo{pInfo = "Embed"}
                AST.Unary meta op operand 
                    -> Just $ ProgramInfo{pInfo = "Unary operation"}
                AST.Binop meta op loper roper 
                    -> Just $ ProgramInfo{pInfo = "Binary operation"}

            {-
getProgramInfoSeq :: LSP.Position -> [AST.Expr] -> IO (Maybe String)
getProgramInfoSeqExpr _ [] = return Nothing
getProgramInfoSeqExpr pos (x:xs) =
    case -}

getProgramInfoBodySeq :: LSP.Position -> [AST.Expr] -> (Maybe ProgramInfo)
getProgramInfoBodySeq _ [] = Nothing
getProgramInfoBodySeq pos (x:xs) =
    -- Check if singleton or range pos
    case (ASTMeta.getPos (AST.getMeta x)) of
        ASTMeta.SingletonPos _      -> handleSingletonPos
        ASTMeta.RangePos start end  -> do
            let exprInfo = getProgramInfoExpr pos False x
            case exprInfo of
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


buildSignatureParamType :: AST.Expr -> String
buildSignatureParamType expr = show (AST.getType expr)

buildSignatureParamList :: [AST.Expr] -> String
buildSignatureParamList [] = ""
buildSignatureParamList (x:[]) = (buildSignatureParamType x)
buildSignatureParamList (x:xs) = 
    (buildSignatureParamType x) ++ ", " ++ (buildSignatureParamList xs)

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

dumpProgramErrors :: Program -> IO ()
dumpProgramErrors program = do
    case (length $ errors program) > 0 || (length $ warnings program) > 0 of
        True -> do
            putStrLn $ "Errors and warnings for " ++ (AST.source (ast program))
            mapM_ (\x -> putStrLn $ show x) (errors program)
            mapM_ (\x -> putStrLn $ show x) (warnings program)
            putStrLn ""
        False -> return ()