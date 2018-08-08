-- | 
-- Module      : Language.Go.Pretty
-- Copyright   : (c) 2013 Rémy Oudompheng
-- License     : GPLv3 (see COPYING)
--
-- This module provides a pretty printer for Go code similar
-- to gofmt -tabs=false -tabwidth=4 -comments=false.

module Language.Go.Pretty (
  Pretty(..),
) where

-- TODO: tabwriter-like alignment of fields.

import Prelude hiding ((<>))
import Data.List
import Data.Maybe (isJust)
import Text.PrettyPrint

import Language.Go.Syntax.AST

class Pretty p where
  pretty :: p -> Doc

-- | @vsep blocks@ joins vertically blocks separating
-- them by blanks.
vsep :: [Doc] -> Doc
vsep blocks = foldl ($+$) empty $ intersperse (text "") blocks

indent  = nest 4
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
    prettyBlock (text "func" <+> pretty name <> pretty sig) block
  pretty (GoMeth (GoMethDecl recv name sig block)) =
    prettyBlock (text "func" <+> parens (pretty recv) <+> pretty name <> pretty sig) block

instance Pretty GoPrel where
  pretty (GoImportDecl specs) = prettySpecs "import" specs

instance Pretty GoImpSpec where
  pretty (GoImpSpec GoImp s)          = quote s
  pretty (GoImpSpec GoImpDot s)       = text "." <+> quote s
  pretty (GoImpSpec (GoImpQual id) s) = pretty id <+> quote s

instance Pretty GoCVSpec where
  pretty (GoCVSpec lhs typ rhs) = commajoin lhs
                              <+> prettyMaybe typ
                              <+> (if not (null rhs) then equals else empty)
                              <+> commajoin rhs

instance Pretty GoTypeSpec where
  pretty (GoTypeSpec name typ) = pretty name <+> pretty typ

--
-- | Types
--

instance Pretty GoType where
  pretty (GoTypeName pkg name) = qual pkg name
  pretty (GoArrayType len elem) = lbrack <> pretty len <> rbrack <> pretty elem
  pretty (GoEllipsisType elem) = text "[...]" <> pretty elem
  pretty (GoVariadicType elem) = text "..." <> pretty elem
  pretty (GoSliceType elem) = text "[]" <> pretty elem
  pretty (GoPointerType elem) = char '*' <> pretty elem
  pretty (GoMapType key elem) = text "map[" <> pretty key <> rbrack <> pretty elem
  pretty (GoChannelType dir elem) = text chan <+> par (pretty elem)
    where chan = case dir of
            GoIChan ->  "<-chan"
            GoOChan ->  "chan<-"
            GoIOChan -> "chan"
          par = case (dir, elem) of
            (GoOChan, (GoChannelType GoIOChan _)) -> parens
            (GoIOChan, (GoChannelType GoIChan _)) -> parens
            _ -> id
  pretty (GoFunctionType sig) = text "func" <> pretty sig
  pretty (GoInterfaceType specs) = prettyFields "interface" specs
  pretty (GoStructType specs) = prettyFields "struct" specs

-- methods in interfaces
instance Pretty GoMethSpec where
  pretty (GoMethSpec name sig) = pretty name <> pretty sig
  pretty (GoIfaceName pkg name) = qual pkg name

-- fields in structs
instance Pretty GoFieldType where
  pretty (GoFieldType tag names typ) = ids <+> pretty typ <+> prettyMaybe tag
    where ids = commajoin names
  pretty (GoFieldAnon tag ptr typ) = p <> pretty typ <+> prettyMaybe tag
    where p = if ptr then char '*' else empty

instance Pretty GoRec where
  pretty (GoRec ptr name typ) = prettyMaybe name <+> (p <> pretty typ)
    where p = if ptr then char '*' else empty

-- function signatures
instance Pretty GoSig where
  -- (a, b, c T, d U)
  pretty (GoSig ins []) = parens $ commajoin ins
  -- (a, b, c T, d U) V
  pretty (GoSig ins [GoParam [] typ]) = parens (commajoin ins) <+> pretty typ
  -- (a, b, c T, d U) (v V, w W)
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
    <> if stmt == GoStmtSimple GoSimpEmpty then semi else empty
  pretty (GoStmtSimple stmt) = pretty stmt

  pretty (GoStmtGo call) = text "go" <+> pretty call
  pretty (GoStmtDefer call) = text "defer" <+> pretty call

  pretty (GoStmtReturn args) = text "return" <+> commajoin args
  pretty (GoStmtBreak label) = text "break" <+> prettyMaybe label
  pretty (GoStmtContinue label) = text "continue" <+> prettyMaybe label
  pretty (GoStmtGoto label) = text "goto" <+> pretty label
  pretty GoStmtFallthrough = text "fallthrough"

  pretty (GoStmtIf cond blk stmt) = prettyBlock (text "if" <+> pretty cond) blk
    <+> maybe empty (\s -> text "else" <+> pretty s) stmt
  pretty (GoStmtFor clause blk) = prettyBlock (text "for" <+> pretty clause) blk
  pretty (GoStmtSwitch cond cases) = prettySwitch (text "switch" <+> pretty cond) cases
  pretty (GoStmtTypeSwitch cond cases var) = prettySwitch prefix cases
    where GoCond initstmt expr = cond
          lhs = maybe empty (\v -> pretty v <+> text ":=") var
          rhs = prettyMaybe expr <> text ".(type)"
          maybesemi = if isJust initstmt then semi else empty
          prefix = text "switch" <+> prettyMaybe initstmt <> maybesemi <+> lhs <+> rhs
  pretty (GoStmtSelect cases) = prettySwitch (text "select") cases

  pretty (GoStmtBlock blk) = prettyBlock empty blk

prettySwitch :: Pretty a => Doc -> [a] -> Doc
prettySwitch prefix cases = (prefix <+> lbrace) $+$ vcat (map pretty cases) $+$ rbrace

instance Pretty GoSimp where
  pretty GoSimpEmpty = text ""
  pretty (GoSimpSend left right) = pretty left <+> text "<-" <+> pretty right
  pretty (GoSimpExpr expr) = pretty expr
  pretty (GoSimpInc expr) = pretty expr <> text "++"
  pretty (GoSimpDec expr) = pretty expr <> text "--"
  pretty (GoSimpAsn lhs (GoOp op) rhs) = commajoin lhs <+> text op <+> commajoin rhs
  pretty (GoSimpVar lhs rhs) = commajoin lhs <+> text ":=" <+> commajoin rhs

instance Pretty GoCond where
  pretty (GoCond stmt expr) = prettyMaybe stmt <> maybesemi <+> prettyMaybe expr
    where maybesemi = if isJust stmt then semi else empty

instance Pretty GoForClause where
  pretty (GoForWhile expr) = prettyMaybe expr
  pretty (GoForThree init cond incr) = pretty init <> semi <+> prettyMaybe cond <> semi <+> pretty incr
  pretty (GoForRange lhs rhs isdecl) = commajoin lhs <+> op <+> text "range" <+> pretty rhs
    where op = if isdecl then text ":=" else equals

instance Pretty a => Pretty (GoCase a) where
  pretty (GoCase items stmts) = p1 $+$ indent p2
    where p1 = text "case" <+> commajoin items <> colon
          p2 = vcat $ map pretty stmts
  pretty (GoDefault stmts) = (text "default" <> colon) $+$ indent p2
    where p2 = vcat $ map pretty stmts

instance Pretty GoChan where
  -- CommClause
  pretty (GoChanRecv Nothing expr) = pretty expr
  pretty (GoChanRecv (Just (v, ok, GoOp eq)) expr) = lhs <+> text eq <+> pretty expr
    where lhs = pretty v <> maybe empty (\x -> comma <+> pretty x) ok
  pretty (GoChanSend left right) = pretty left <+> text "<-" <+> pretty right

--
-- | Expressions
--

instance Pretty GoExpr where
  pretty (GoPrim prim) = pretty prim
  pretty (Go1Op (GoOp op) expr) = text op <> pretty expr
  pretty (Go2Op (GoOp op) expr1 expr2) = pretty expr1 <+> text op <+> pretty expr2

instance Pretty GoPrim where
  pretty (GoLiteral lit)   = pretty lit
  pretty (GoQual pkg name) = qual pkg name
  pretty (GoMethod rec name) = t <> char '.' <> pretty name
    where GoRec isptr _ typ = rec
          t = if isptr then parens (char '*' <> pretty typ) else pretty typ
  pretty (GoSelect left right) = pretty left <> char '.' <> pretty right
  pretty (GoParen expr)    = parens $ pretty expr
  pretty (GoCast typ expr) = conv typ <> parens (pretty expr)
    where conv typ@(GoPointerType _) = parens $ pretty typ
          -- "if the type starts with the keyword func and has no result
          -- list, it must be parenthesized"
          conv typ@(GoFunctionType (GoSig _ [])) = parens $ pretty typ
          conv typ@(GoChannelType GoIChan _) = parens $ pretty typ
          conv typ@(_) = pretty typ
  pretty (GoNew typ)       = text "new" <> parens (pretty typ)
  pretty (GoMake typ exprs) = case exprs of
    [] -> text "make" <> parens (pretty typ)
    _  -> text "make" <> parens (pretty typ <> comma <+> commajoin exprs)
  pretty (GoIndex expr idx) = pretty expr <> brackets (pretty idx)
  pretty (GoCall func args variadic) = pretty func <> parens a
    where a = commajoin args <> (if variadic then text "..." else empty)
  pretty (GoTA expr typ) = pretty expr <> char '.' <> parens (pretty typ)
  pretty (GoSlice expr lo hi) = pretty expr <> lbrack <> prettyMaybe lo <> colon <> prettyMaybe hi <> rbrack

instance Pretty GoLit where
  pretty (GoLitInt s _)  = text s
  pretty (GoLitReal s _) = text s
  pretty (GoLitImag s _) = text s
  pretty (GoLitChar s _) = text s
  pretty (GoLitStr s _)  = text s
  pretty (GoLitFunc (GoFuncExpr sig blk)) = prettyBlock (text "func" <> pretty sig) blk
  pretty (GoLitComp typ comp) = case comp of
    GoComp [] -> pretty typ <> lbrace <> rbrace
    GoComp (x@(GoElement GoKeyNone (GoValueExpr _)):xs) -> pretty typ <> braces (commajoin (x:xs))
    GoComp elems -> (pretty typ <> lbrace)
                $+$ indent (vcat [pretty e <> comma | e <-  elems])
                $+$ rbrace

instance Pretty GoElement where
  pretty (GoElement k v) = key k <+> value v
    where key GoKeyNone = empty
          key (GoKeyField fld) = pretty fld <> colon
          key (GoKeyIndex expr) = pretty expr <> colon
          value (GoValueExpr expr) = pretty expr
          value (GoValueComp (GoComp elems)) = braces $ commajoin elems

