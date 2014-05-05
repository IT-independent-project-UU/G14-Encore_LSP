-- Defines how things will be called in the CCode generated by CodeGen.hs
-- Provides mappings from class/method names to their C-name

module CodeGen.CCodeNames where

import qualified AST as A
import CCode.Main
import Data.Char

char = Typ "char"
int = Typ "int"
void = Typ "void"
pony_actor_t = Typ "pony_actor_t"

-- each method is implemented as a function with a `this`
-- pointer. This is the name of that function
method_impl_name :: A.Type -> A.Name -> CCode Name
method_impl_name clazz mname =
    Nam $ (show clazz) ++ "_" ++ (show mname)

-- each class, in C, provides a dispatch function that dispatches
-- messages to the right method calls. This is the name of that
-- function.
class_dispatch_name :: A.Type -> CCode Name
class_dispatch_name clazz = Nam $ (show clazz) ++ "_dispatch"

class_message_type_name :: A.Type -> CCode Name
class_message_type_name clazz = Nam $ (show clazz) ++ "_message_type"

class_trace_fn_name :: A.Type -> CCode Name
class_trace_fn_name clazz = Nam $ show clazz ++ "_trace"

method_message_type_name :: A.Type -> A.Name -> CCode Lval --fixme should be a name
method_message_type_name clazz mname = Var $ "m_"++show clazz++"_"++show mname

-- for each method, there's a corresponding message, this is its name
method_msg_name :: A.Type -> A.Name -> CCode Name
method_msg_name clazz mname = Nam $ "MSG_"++show clazz++"_"++show mname

-- the name of the record type in which a class stores its state
data_rec_name :: A.Type -> CCode Ty
data_rec_name clazz = Typ $ show clazz ++ "_data"

data_rec_ptr :: A.Type -> CCode Ty
data_rec_ptr = Ptr . data_rec_name

actor_rec_name :: A.Type -> CCode Name
actor_rec_name clazz = Nam $ show clazz ++ "_actor"

pony_actor_t_Type :: A.Type -> CCode Ty
pony_actor_t_Type (A.Type ty) =
    Typ $ if isLower $ head ty
          then ty
          else ty++"_actor_t*"

temp_name :: String -> CCode Name
temp_name s = Nam $ "__backend__" ++ s
