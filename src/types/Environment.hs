{-| 

The environment used by "Typechecker". Contains a class table and
a list of local name-type bindings for doing lookups, as well as a
stack used for the backtrace of the typechecker (See
'TypeError.Backtrace').

-}

module Environment(Environment, 
                   EnvironmentTransformer,
                   buildClassTable, 
                   classLookup, 
                   methodLookup, 
                   fieldLookup, 
                   varLookup,
                   extendEnvironment,
                   backtrace,
                   pushBT) where

-- Module dependencies
import Types
import AST
import TypeError

data Environment = Env {ctable :: [ClassType], locals :: [VarType], bt :: Backtrace}
type EnvironmentTransformer = Environment -> Environment

buildClassTable :: Program -> Environment
buildClassTable (Program classes) = Env (map getClassType classes) [] emptyBT
    where
      getClassType (Class name fields methods) = (name, (fields', methods'))
          where
            fields' = map getFieldType fields
            methods' = map getMethodType methods
            getFieldType fld = (fname fld, ftype fld)
            getMethodType mtd = (mname mtd, (rtype mtd, mparams mtd))

pushBT :: Pushable a => a -> EnvironmentTransformer
pushBT x env = env {bt = push x (bt env)}

backtrace = bt

fieldLookup :: Type -> Name -> Environment -> Maybe Type
fieldLookup cls f env = do (fields, _) <- lookup cls (ctable env)
                           lookup f fields

methodLookup :: Type -> Name -> Environment -> Maybe (Type, [ParamDecl])
methodLookup cls m env = do (_, methods) <- lookup cls (ctable env)
                            lookup m methods

classLookup :: Type -> Environment -> Maybe ([FieldType], [MethodType])
classLookup cls env = lookup cls (ctable env)

varLookup :: Name -> Environment -> Maybe Type
varLookup x env = lookup x (locals env)

extendEnvironment :: [(Name, Type)] -> Environment -> Environment
extendEnvironment [] env = env
extendEnvironment ((name, ty):newTypes) env = 
    extendEnvironment newTypes $ env {locals = extend (locals env) name ty}
    where
      extend [] name' ty' = [(name', ty')]
      extend ((name, ty):bindings) name' ty'
          | name == name' = (name', ty'):bindings
          | otherwise     = (name, ty):(extend bindings name' ty')