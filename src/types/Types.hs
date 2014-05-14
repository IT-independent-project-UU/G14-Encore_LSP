{-|

The types used by the "Typechecker".

-}

module Types(VarType, FieldType, MethodType, ClassType, isPrimitive) where

import AST

type VarType = (Name, Type)
type FieldType = (Name, Type)
type MethodType = (Name, (Type, [ParamDecl]))
type ClassType = (Type, ([FieldType], [MethodType]))

primitives :: [Type]
primitives = [Type "void", Type "int", Type "bool", Type "string"]

isPrimitive :: Type -> Bool
isPrimitive = flip elem primitives