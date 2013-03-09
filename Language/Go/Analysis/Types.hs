-- | 
-- Module      : Language.Go.Analysis.Types
-- Copyright   : (c) 2013 Rémy Oudompheng
-- License     : GPLv3 (see COPYING)
--
-- This module defines the algebra of Go types and
-- the rules for assignment and conversion.

module Language.Go.Analysis.Types (
  -- * Types
  Type(..),
  ChanDir(..),
  Signature(..),
  Method(..),
  StructField(..),
  NumType(..),
  typeString,
  -- * Predeclared types
  builtinByte,
  builtinRune,
  -- * Method sets
  methodSet,
  -- * Assignment and conversion rules
  assignableTo,
  convertibleTo,
  implements,
) where

import Data.List (intersperse)
import Data.Maybe (catMaybes)
import qualified Data.Map as M

-- | Go types. Equality of types is equivalent to Go's type identity.
data Type =
       NamedType String String Type [Method]
    -- ^ A named type with package name and type name, and underlying
    -- type, and methods.
     | BoolType
    -- ^ The (unnamed) boolean type
     | NumericType NumType
    -- ^ The (unnamed) numeric types
     | ComplexType CmplxType
    -- ^ The (unnamed) complex numeric types
     | StringType
    -- ^ The (unnamed) string type
     | ArrayType Integer Type
    -- ^ Array type, with length and element type
     | SliceType Type
     | PtrType Type
     | UnsafePtr
     | ChanType ChanDir Type
     | MapType Type Type
     | StructType [StructField]
     | IfaceType (M.Map String Signature)
   -- ^ Interface type, determined by its method set.
     | FuncType Signature
   -- ^ Function type, with in/out arguments and variadic.
  deriving (Show, Eq)

data ChanDir = ChanBi | ChanRecv | ChanSend
  deriving (Show, Eq)

data NumType = Int8 | Int16 | Int32 | Int64
             | Uint8 | Uint16 | Uint32 | Uint64
             | GoInt | GoUint | Uintptr
             | Float32 | Float64
  deriving (Show, Eq)

data CmplxType = Complex64 | Complex128
  deriving (Show, Eq)

-- | A @StructField name type tag@ represents
-- a field in a struct type.
data StructField = NamedField String Type (Maybe String)
                 | AnonField Type (Maybe String)
  deriving (Show, Eq)

data Signature = Signature [Type] [Type] Bool
  deriving (Show, Eq)

-- Method is a name, whether it has ptr receiver,
-- and a signature.
data Method = Method String Bool Signature
  deriving (Show, Eq)

type MethodSet = M.Map String Signature

builtinByte = NamedType "" "byte" (NumericType Uint8) []
builtinRune = NamedType "" "rune" (NumericType Int32) []

methodSet :: Type -> MethodSet
methodSet (IfaceType mset) = mset
methodSet (NamedType _ _ t meths) = M.union (M.fromList valueMeths) (methodSet t)
  where valueMeths = catMaybes [ if ptr then Nothing else Just (name, sig)
                               | Method name ptr sig <- meths ]
methodSet (PtrType (NamedType _ _ t meths)) = M.union (M.fromList meths') (methodSet (PtrType t))
  where meths' = [(name, sig) | Method name _ sig <- meths ]
methodSet (StructType _) = undefined -- TODO
-- other unnamed types
methodSet _ = M.empty

-- | @assignableTo t u@ says whether a value of type t can be assigned
-- to a value of type u.
assignableTo :: Type -> Type -> Bool
assignableTo left@(NamedType pkg1 name1 t1 _) (NamedType pkg2 name2 t2 _) =
  case t2 of
    IfaceType mset -> implements mset left
    _ -> pkg1 == pkg2 && name1 == name2 && t1 == t2
-- assignment between named channels is special
assignableTo (NamedType _ _ t1@(ChanType _ _) _)
             t2@(ChanType _ _) = assignableTo t1 t2
assignableTo t1@(ChanType _ _)
             (NamedType _ _ t2@(ChanType _ _) _) = assignableTo t1 t2
-- non-channel named types
assignableTo (NamedType _ _ t _) u = t == u
assignableTo t (NamedType _ _ u _) = t == u
assignableTo t (IfaceType mset) = implements mset t
assignableTo (ChanType ChanBi t1) (ChanType _ t2) = t1 == t2
assignableTo t u = t == u

-- | @convertibleTo t u@ says whether a value of type t can be
-- converted to type u.
convertibleTo :: Type -> Type -> Bool
convertibleTo (NamedType _ _ t _) (NamedType _ _ u _) = case (t, u) of
  (NumericType _, NumericType _) -> True
  (ComplexType _, ComplexType _) -> True
  (t, u) -> t == u
-- now when one of them is not named.
convertibleTo (PtrType t) (PtrType u) = underlying t == underlying u
convertibleTo (NumericType _) (NumericType _) = True
convertibleTo (ComplexType _) (ComplexType _) = True
convertibleTo t u = assignableTo t u

implements :: MethodSet -> Type -> Bool
implements mset t = methodSet t `M.isSubmapOf` mset

-- | @typeString t@ is the Go syntax representation
-- of t.
typeString :: Type -> String
typeString (NamedType pkg name _ _) = case pkg of
  "" -> name
  _  -> pkg ++ "." ++ name
typeString (ArrayType n t) = "[" ++ show n ++ "]" ++ typeString t
typeString (SliceType t) = "[]" ++ typeString t
typeString (ChanType dir t) = case dir of
  ChanBi   -> "chan " ++ typeString t
  ChanSend -> "chan<- " ++ typeString t
  ChanRecv -> "<-chan " ++ typeString t
typeString (FuncType (Signature arg out var)) = "func " ++ sarg ++ sout
  where sarg = if null arg then "()" else sarg1
        sarg1 = "(" ++ comma (init arg) ++ ", " ++ v ++ typeString (last arg) ++ ")"
        sout = if null out then "" else " (" ++ comma out ++ ")"
        v = if var then "..." else ""
        comma l = concat $ intersperse ", " $ map typeString l
typeString (MapType k v) = "map[" ++ typeString k ++ "]" ++ typeString v
typeString (PtrType t) = '*' : (typeString t)
-- TODO
typeString (StructType flds) = "struct " ++ show flds
typeString (IfaceType flds) = "interface " ++ show flds
-- should be unnamed
typeString BoolType = "bool"
typeString UnsafePtr = "unsafe.Pointer"
typeString StringType = "string"
typeString (NumericType n) = show n
typeString (ComplexType n) = show n

underlying :: Type -> Type
underlying (NamedType _ _ t _) = t
underlying t = t
