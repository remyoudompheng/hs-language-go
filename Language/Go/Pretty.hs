-- | 
-- Module      : Language.Go.Pretty
-- Copyright   : (c) 2013 Rémy Oudompheng
-- License     : GPLv3 (see COPYING)
--
-- This module provides a pretty printer for Go code similar
-- to the gofmt tool, though not equivalent.

module Language.Go.Pretty (
  Pretty(..),
) where

import Data.List
import Data.Maybe (maybe)
import Text.PrettyPrint

import Language.Go.Syntax.AST

class Pretty p where
  pretty :: p -> Doc

-- | @vsep blocks@ joins vertically blocks separating
-- them by blanks.
vsep :: [Doc] -> Doc
vsep blocks = foldl ($+$) empty $ intersperse (text "") blocks

indent  = nest 8
quote s = char '"' <> text s <> char '"'

commajoin :: Pretty a => [a] -> Doc
commajoin = hsep . punctuate comma . map pretty

-- | @prettySpecs keyword specs@ formats a sequence of declarations
-- with a given keyword (import, var, const, type...).
prettySpecs :: Pretty a => String -> [a] -> Doc
prettySpecs kw specs = case specs of
  []     -> text kw <+> lparen <> rparen
  [spec] -> text kw <+> pretty spec
  _      -> (text kw <+> lparen) $+$ indent (vcat (map pretty specs)) $+$ rparen

prettyFields :: Pretty a => String -> [a] -> Doc
prettyFields kw specs = case specs of
  [] -> text kw <+> lbrace <> rbrace
  _  -> (text kw <+> lbrace) $+$ indent (vcat (map pretty specs)) $+$ rbrace

qual :: Maybe GoId -> GoId -> Doc
qual Nothing name = pretty name
qual (Just pkg) name = pretty pkg <> char '.' <> pretty name

prettyBlock :: Doc -> GoBlock -> Doc
prettyBlock prefix GoNoBlock = prefix
prettyBlock prefix (GoBlock stmts) =
      (prefix <+> lbrace)
  $+$ vcat (map pretty stmts)
  $+$ rbrace

prettyMaybe :: (Pretty a) => Maybe a -> Doc
prettyMaybe = maybe empty pretty

instance Pretty GoSource where
  pretty src = pkg $+$ text "" $+$ imports $+$ text "" $+$ decls
    where pkg = text "package" <+> pretty (getPackage src)
          imports = vsep $ map pretty $ getTopLevelPrel src
          decls = vsep $ map pretty $ getTopLevelDecl src

instance Pretty GoId where
  pretty (GoId s) = text s

--
-- | Declarations
--

instance Pretty GoDecl where
  pretty (GoConst specs) = prettySpecs "const" specs
  pretty (GoType specs)  = prettySpecs "type" specs
  pretty (GoVar specs)   = prettySpecs "var" specs
  pretty (GoFunc (GoFuncDecl name sig block)) =
    prettyBlock (text "func" <+> pretty name <+> pretty sig) block
  pretty (GoMeth (GoMethDecl recv name sig block)) =
    prettyBlock (text "func" <+> pretty recv <+> pretty name <+> pretty sig) block

instance Pretty GoPrel where
  pretty (GoImportDecl specs) = prettySpecs "import" specs

instance Pretty GoImpSpec where
  pretty (GoImpSpec GoImp s)          = quote s
  pretty (GoImpSpec GoImpDot s)       = text "." <+> quote s
  pretty (GoImpSpec (GoImpQual id) s) = pretty id <+> quote s

instance Pretty GoCVSpec where
  pretty (GoCVSpec lhs typ rhs) = commajoin lhs
                              <+> prettyMaybe typ
                              <+> commajoin rhs

instance Pretty GoTypeSpec where
  pretty _ = empty -- FIXME

--
-- | Types
--

instance Pretty GoType where
  pretty (GoTypeName pkg name) = qual pkg name
  pretty (GoArrayType len elem) = lbrack <> pretty len <> rbrack <> pretty elem
  pretty (GoEllipsisType elem) = text "[...]" <> pretty elem
  pretty (GoSliceType elem) = text "[]" <> pretty elem
  pretty (GoPointerType elem) = char '*' <> pretty elem
  pretty (GoMapType key elem) = text "map[" <> pretty key <> rbrack <> pretty elem
  pretty (GoChannelType dir elem) = text chan <+> pretty elem
    where chan = case dir of
            GoIChan ->  "<-chan"
            GoOChan ->  "chan<-"
            GoIOChan -> "chan"
  pretty (GoFunctionType sig) = text "func" <> pretty sig
  pretty (GoInterfaceType specs) = prettyFields "interface" specs
  pretty (GoStructType specs) = prettyFields "struct" specs

-- methods in interfaces
instance Pretty GoMethSpec where
  pretty (GoMethSpec name sig) = pretty name <> pretty sig
  pretty (GoIfaceName pkg name) = qual pkg name

-- fields in structs
instance Pretty GoFieldType where
  pretty (GoFieldType tag names typ) = ids <+> pretty typ <+> quote tag
    where ids = commajoin names
  pretty (GoFieldAnon tag ptr typ) = p <> pretty typ <+> quote tag
    where p = if ptr then char '*' else empty

instance Pretty GoRec where
  pretty (GoRec ptr name typ) = prettyMaybe name <+> p <+> pretty typ
    where p = if ptr then char '*' else empty

-- function signatures
instance Pretty GoSig where
  pretty (GoSig ins []) = parens $ commajoin ins
  pretty (GoSig ins outs) = pins <+> pouts
    where pins = parens $ commajoin ins
          pouts = parens $ commajoin outs

instance Pretty GoParam where
  pretty (GoParam ids typ) = commajoin ids <+> pretty typ

--
-- | Statements
--

instance Pretty GoStmt where
  pretty (GoStmtDecl dcl) = pretty dcl
  pretty (GoStmtLabeled label stmt) = (pretty label <> colon) $+$ pretty stmt
  pretty (GoStmtSimple stmt) = pretty stmt

  pretty (GoStmtGo call) = text "go" <+> pretty call
  pretty (GoStmtDefer call) = text "defer" <+> pretty call

  pretty (GoStmtReturn args) = text "return" <+> commajoin args
  pretty (GoStmtBreak label) = text "break" <+> prettyMaybe label
  pretty (GoStmtContinue label) = text "continue" <+> prettyMaybe label
  pretty (GoStmtGoto label) = text "goto" <+> pretty label
  pretty GoStmtFallthrough = text "fallthrough"

  pretty (GoStmtIf cond blk stmt) = prettyBlock (text "if" <+> pretty cond) blk
    <+> text "else" <+> prettyMaybe stmt
  pretty (GoStmtFor clause blk) = empty -- FIXME
  pretty (GoStmtSwitch cond cases) = empty -- FIXME
  pretty (GoStmtTypeSwitch cond cases var) = empty -- FIXME
  pretty (GoStmtSelect cases) = empty -- FIXME

  pretty (GoStmtBlock blk) = prettyBlock empty blk

instance Pretty GoSimp where
  pretty _ = empty -- FIXME

instance Pretty GoCond where
  pretty _ = empty -- FIXME

--
-- | Expressions
--

instance Pretty GoExpr where
  pretty _ = empty -- FIXME
