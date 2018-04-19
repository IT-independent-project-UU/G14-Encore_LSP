 module LSP.Data.Program (
    Program(..),
    makeBlankProgram,
    makeBlankAST,
    getProgramInfoForPos
 ) where

-- ###################################################################### --
-- Section: Imports
-- ###################################################################### --

-- Standard
import System.Exit

-- Encore
import qualified AST.AST as AST
import qualified AST.Meta as ASTMeta

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
    message     :: String
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

{- -}
getProgramInfoForPos :: LSP.Position -> Program -> IO ()
getProgramInfoForPos pos program = do
    -- Try to get function info
    functionInfo <- (getProgramInfoFunction pos (AST.functions $ ast program))
    case functionInfo of
        Just s -> print $ s
        Nothing -> do
            -- Try to get class info
            classInfo <- (getProgramInfoClass pos (AST.classes $ ast program))
            case classInfo of
                Just s -> print $ s
                Nothing -> die "NO PROGRAM INFO FOUND"
            

{- -}
handleSingletonPos :: String
handleSingletonPos = "############ BAD ################"

{- -}
getProgramInfoFunction :: LSP.Position -> [AST.Function] -> IO (Maybe String)
getProgramInfoFunction _ [] = return (Nothing)
getProgramInfoFunction pos (x:xs) =
    -- Check if singleton or range pos
    case (ASTMeta.getPos (AST.funmeta x)) of
        ASTMeta.SingletonPos _      -> return (Just handleSingletonPos)
        ASTMeta.RangePos start end  -> do
            -- Check if pos is in function
            case LSP.inRange pos (LSP.fromSourcePos start, LSP.fromSourcePos end) of
                False   -> getProgramInfoFunction pos xs
                True    -> do
                    -- Check if pos is on paramDecl
                    paramDeclResult <- getProgramInfoParamDecl pos (AST.hparams $ AST.funheader x)
                    case paramDeclResult of
                        Just s  -> return (Just s)
                        Nothing -> do
                            -- Check if pos is in body
                            bodyResult <- getProgramInfoExpr pos (AST.funbody x)
                            case bodyResult of
                                Just s  -> return (Just s)
                                Nothing -> do
                                    -- Check if pos is in local function
                                    funResult <- getProgramInfoFunction pos (AST.funlocals x)
                                    case funResult of
                                        Just s  -> return $ Just s
                                        Nothing -> return Nothing

{-  -}
getProgramInfoClass :: LSP.Position -> [AST.ClassDecl] -> IO (Maybe String)
getProgramInfoClass _ [] = return Nothing
getProgramInfoClass pos (x:xs) = do
    -- Check if pos is singleton or range pos
    case (ASTMeta.getPos (AST.cmeta x)) of
        ASTMeta.SingletonPos _      -> return (Just handleSingletonPos)
        ASTMeta.RangePos start end  -> do
            -- Check if pos is in class
            case LSP.inRange pos (LSP.fromSourcePos start, LSP.fromSourcePos end) of
                False   -> getProgramInfoClass pos xs
                True    -> do
                    -- Check if pos is in field decl
                    fieldInfo <- getProgramInfoFieldDecl pos (AST.cfields x)
                    case fieldInfo of
                        Just s  -> return $ Just s
                        Nothing -> do
                            -- Check if pos is in method decl
                            methodInfo <- getProgramInfoMethodDecl pos (AST.cmethods x)
                            case methodInfo of
                                Just s  -> return $ Just s
                                Nothing -> return Nothing

{-  -}
getProgramInfoFieldDecl :: LSP.Position -> [AST.FieldDecl] -> IO (Maybe String)
getProgramInfoFieldDecl _ [] = return Nothing
getProgramInfoFieldDecl pos (x:xs) = return Nothing

{-  -}
getProgramInfoMethodDecl :: LSP.Position -> [AST.MethodDecl] -> IO (Maybe String)
getProgramInfoMethodDecl _ [] = return Nothing
getProgramInfoMethodDecl pos (x:xs) = do
    -- Check if pos is singleton or range pos
    case (ASTMeta.getPos (AST.mmeta x)) of
        ASTMeta.SingletonPos _      -> return (Just handleSingletonPos)
        ASTMeta.RangePos start end  -> do
            -- Check if pos is in method
            case LSP.inRange pos (LSP.fromSourcePos start, LSP.fromSourcePos end) of
                False   -> getProgramInfoMethodDecl pos xs
                True    -> do
                    -- Check if pos is on paramDecl
                    paramDeclResult <- getProgramInfoParamDecl pos (AST.hparams $ AST.mheader x)
                    case paramDeclResult of
                        Just s  -> return $ Just s
                        Nothing -> do
                            -- Check if pos is in body
                                bodyInfo <- getProgramInfoExpr pos (AST.mbody x)
                                case bodyInfo of
                                    Just s  -> return $ Just s
                                    Nothing -> do
                                        -- Check if pos is in local function
                                        localFunInfo <- getProgramInfoFunction pos (AST.mlocals x)
                                        case localFunInfo of
                                            Just s  -> return $ Just s
                                            Nothing -> return Nothing
                    
{-  -}
getProgramInfoParamDecl :: LSP.Position -> [AST.ParamDecl] -> IO (Maybe String)
getProgramInfoParamDecl _ [] = return Nothing
getProgramInfoParamDecl pos (x:xs) =
    -- Check if singleton or range pos
    case (ASTMeta.getPos (AST.pmeta x)) of
        ASTMeta.SingletonPos _ -> return $ Just handleSingletonPos
        ASTMeta.RangePos start end -> do
            -- Check if pos is on parameter
            case LSP.inRange pos (LSP.fromSourcePos start, LSP.fromSourcePos end) of
                False   -> getProgramInfoParamDecl pos xs
                True    -> return $ Just $ "Hovering over parameter " ++ show (AST.pname x)

{- -}
getProgramInfoExpr :: LSP.Position -> AST.Expr -> IO (Maybe String)
getProgramInfoExpr pos expr = do
    case expr of
        AST.Skip meta 
            -> return $ Just "Skip"
        AST.Break meta 
            -> return $ Just "Break"
        AST.Continue meta 
            -> return $ Just "Continue"
        AST.TypedExpr meta body ty 
            -> return $ Just "Typed expression"
        AST.MethodCall meta tyArgs target name args 
            -> return $ Just $ "Method call: " ++ show name
        AST.MessageSend meta tyArgs target name args 
            -> return $ Just $ "MessageSend: " ++ show name
        AST.Optional meta optTag 
            -> return $ Just "Optional"
        AST.ExtractorPattern meta ty name arg 
            -> return $ Just "ExtractorPattern"
        AST.FunctionCall meta typeArgs name args 
            -> return $ Just $ "Function call: " ++ show name
        AST.FunctionAsValue meta tyArgs name 
            -> return $ Just "Function as value"
        AST.Closure meta params maybeType body 
            -> return $ Just "Closure"
        AST.PartySeq meta par seqFun 
            -> return $ Just "PartySeq"
        AST.PartyPar meta parl parr 
            -> return $ Just "PartyPar"
        AST.PartyReduce meta seqFun init par runassoc -> return $ Just "PartyReduce"
        AST.Async meta body 
            -> return $ Just "Async"
        AST.Return meta value 
            -> return $ Just "Return"
        AST.MaybeValue meta container 
            -> return $ Just "MaybeValue"
        AST.Tuple meta args 
            -> return $ Just "Tuple"
        AST.Let meta mutability decl body 
            -> return $ Just "Let"
        AST.MiniLet meta mutability decls 
            -> return $ Just "MiniLet"
        AST.Seq meta seq 
            -> return $ Just "Seq"
        AST.IfThenElse meta cond thn els 
            -> return $ Just "IfThenElse"
        AST.IfThen meta cond thn 
            -> return $ Just "IfThen"
        AST.Unless meta cond thn 
            -> return $ Just "Unless"
        AST.While meta cond body 
            -> return $ Just "While"
        AST.DoWhile meta cond body 
            -> return $ Just "DoWhile"
        AST.Repeat meta name times body 
            -> return $ Just "Repeat"
        AST.For meta name step src body 
            -> return $ Just "For"
        AST.Match meta arg clauses 
            -> return $ Just "Match"
        AST.Borrow meta target name body 
            -> return $ Just "Borrow"
        AST.Get meta value 
            -> return $ Just "Get"
        AST.Forward meta forwardExpr 
            -> return $ Just "ForwardExpr"
        AST.Yield meta value 
            -> return $ Just "Yield"
        AST.Eos meta 
            -> return $ Just "EOS"
        AST.IsEos meta target 
            -> return $ Just "IsEOS"
        AST.StreamNext meta target 
            -> return $ Just "StreamNext"
        AST.Await meta value 
            -> return $ Just "Await"
        AST.Suspend meta 
            -> return $ Just "Suspend"
        AST.FutureChain meta future chain 
            -> return $ Just "FutureChain"
        AST.FieldAccess meta target name 
            -> return $ Just "FieldAccess"
        AST.ArrayAccess meta target name 
            -> return $ Just "ArrayAccess"
        AST.ArraySize meta target 
            -> return $ Just "ArraySize"
        AST.ArrayNew meta ty size
            -> return $ Just "ArrayNew"
        AST.ArrayLiteral meta args 
            -> return $ Just "ArrayLiteral"
        AST.Assign emeta rhs lhs 
            -> return $ Just "Assign"
        AST.VarAccess meta name 
            -> return $ Just "VarAccess"
        AST.TupleAccess meta target compartment 
            -> return $ Just "TupleAccess"
        AST.Consume meta target 
            -> return $ Just "Consume"
        AST.Null meta
            -> return $ Just "Null"
        AST.BTrue meta 
            -> return $ Just "True"
        AST.BFalse meta 
            -> return $ Just "False"
        AST.NewWithInit meta ty args 
            -> return $ Just "NewWithInit"
        AST.New meta ty 
            -> return $ Just "New"
        AST.Print meta file args 
            -> return $ Just "Print"
        AST.Exit meta args 
            -> return $ Just "Exit"
        AST.Abort meta args 
            -> return $ Just "Abort"
        AST.StringLiteral meta literal 
            -> return $ Just "String literal"
        AST.CharLiteral meta literal
            -> return $ Just "Char literal"
        AST.RangeLiteral meta rstart rstop step 
            -> return $ Just "Range literal"
        AST.IntLiteral meta literal 
            -> return $ Just "Int literal"
        AST.UIntLiteral meta literal 
            -> return $ Just "UInt literal"
        AST.RealLiteral meta literal 
            -> return $ Just "Real literal"
        AST.Embed meta ty embedded 
            -> return $ Just "Embed"
        AST.Unary meta op operand 
            -> return $ Just "Unary operation"
        AST.Binop meta op loper roper 
            -> return $ Just "Binary operation"