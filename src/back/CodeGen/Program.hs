{-# LANGUAGE MultiParamTypeClasses,FlexibleInstances #-}

module CodeGen.Program(translate) where

import CodeGen.Typeclasses
import CodeGen.ClassDecl
import CodeGen.CCodeNames

import CCode.Main
import qualified AST as A
import Control.Monad.Reader hiding (void)
import qualified CodeGen.Context as Ctx

instance Translatable A.Program (CCode FIN) where
  translate (A.Program cs) =
    Program $ 
    ConcatTL $
    (Includes ["pony/pony.h",
               --"stdlib.h",
               --"string.h",
               --"inttypes.h",
               --"assert.h",
               "stdio.h"
              ]) :
    (fwd_decls (A.Program cs)) :
    (map fwd_decls cs) ++
    (map translate_class_here cs) ++
    [(Function
      (Static void) (Nam "dispatch")
      [(Ptr . Typ $ "pony_actor_t", Var "this"),
       (Ptr void, Var "p"),
       (Typ "uint64_t", Var "id"),
       (int, Var "argc"),
       (Ptr . Typ $ "pony_arg_t", Var "argv")]
      (Switch (Var "id")
       [(Nam "PONY_MAIN",
         Concat $ map Statement
                    [Assign
                     (Decl (translate (A.Type "Main"), Var "d"))
                     (Call 
                      (Var "pony_alloc")
                      [(Call (Nam "sizeof")
                        [AsExpr . Embed . show $ (translate (A.Type "Main") :: CCode Ty)])]),
                     Call (Nam "pony_set") [Var "d"],
                     Call (method_impl_name (A.Type "Main") (A.Name "main")) [Var "d"]])]
       (Embed "printf(\"error, got invalid id: %llu\",id);"))),
     (Function
      int (Nam "main")
      [(int, Var "argc"), (Ptr . Ptr $ char, Var "argv")]
      (Embed "return pony_start(argc, argv, pony_create(&Main_actor));"))]
    where
      translate_class_here :: A.ClassDecl -> CCode Toplevel
      translate_class_here cdecl = runReader (translate cdecl) $ Ctx.mk (A.Program cs)

instance FwdDeclaration A.Program (CCode Toplevel) where
  fwd_decls (A.Program cs) = ConcatTL $ [create_and_send_fn,
                                         msg_alloc_decl,
                                         msg_enum (A.Program cs),
                                         class_ids_enum (A.Program cs)]
    where
      msg_alloc_decl =
          Embed $ "static pony_msg_t m_MSG_alloc = {0, {{NULL, 0, PONY_PRIMITIVE}}};"
      create_and_send_fn =
          Embed $
                    "pony_actor_t* create_and_send(pony_actor_type_t* type, uint64_t msg_id) {\n" ++
--                    "  printf(\"creating:\\n\");\n" ++
                    "  pony_actor_t* ret = pony_create(type);\n" ++
                    "  pony_send(ret, msg_id);\n" ++
--                    "  printf(\"created and sent!\\n\");\n" ++
                    "  \n" ++
                    "  return ret;\n" ++
                    "}"
      msg_enum :: A.Program -> CCode Toplevel
      msg_enum (A.Program cs) =
        let
          meta = concat $ map (\cdecl -> zip (repeat $ A.cname cdecl) (A.methods cdecl)) cs
          lines = map (\ (cname, mdecl) -> "MSG_" ++ show cname ++ "_" ++ (show $ A.mname mdecl)) meta
        in
         Enum $ map Nam $ "MSG_alloc":lines

      class_ids_enum :: A.Program -> CCode Toplevel
      class_ids_enum (A.Program cs) =
        let
          names = map (("ID_"++) . show . A.cname) cs
        in
         Enum $ map Nam $ names