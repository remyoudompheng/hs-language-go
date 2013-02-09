{
-- |
-- Module: Language.Go.Parser.Lexer
module Language.Go.Parser.Lexer (alexScanTokens) where
import Language.Go.Parser.Tokens
import Text.Parsec.Pos
}

%wrapper "posn"

-- SS. 3.1. Characters
@unicode_char   = [\x00-\x7F]|[\x80-\xFF]+
@unicode_nobr   = [\x00-\x5C\x5E-\x7F]|[\x80-\xFF]+
@unicode_nobq   = $printable # `
@unicode_nosq   = $printable # [\' \n \\]
@unicode_nodq   = $printable # \"
@unicode_letter = [A-Za-z] -- should be [\p{L}]
@unicode_digit  = [0-9]    -- should be [\p{N}]

-- SS. 3.2 Letters and digits
@letter         = _|@unicode_letter
$decimal_digit  = [0-9]
$octal_digit    = [0-7]
$hex_digit      = [0-9A-Fa-f]
$sign           = [\+\-]

-- SS. 4.1. Comments
@ol_char    = [^\n]
@ml_char    = [\x00-\x29\x2B-\xFF]|\*[\x00-\x2E\x30-\xFF]
@ol_comment = "//"(@ol_char)*\n
@ml_comment = "/*"(@ml_char)*"*/"

-- SS. 4.2. Tokens
$whitespace = [\ \t\f\v\r]
$whiteline  = [\n]

-- SS. 4.4. Identifiers
@identifier = @letter(@letter|@unicode_digit)*

-- SS. 4.6. Operators and Delimiters
$add_op     = [\+\-\|\^]
@mul_op     = [\*\\\/\%\&] | "<<" | ">>" | "&^"
@assignop   = $add_op"=" | @mul_op"="

-- SS. 4.7. Integer literals
@decimallit = [1-9]($decimal_digit)*
@octal_lit  = 0($octal_digit)*
@hex_lit    = 0[xX]($hex_digit)+
@int_lit    = (@decimallit|@octal_lit|@hex_lit)

-- SS. 4.8. Floating-point literals
@decimals   = ($decimal_digit)+
@exponent   = [eE]($sign)?(@decimals)
@float1lit  = (@decimals)\.(@decimals)?(@exponent)?
@float2lit  = (@decimals)(@exponent)
@float3lit  = \.(@decimals)(@exponent)?
@float_lit  = (@float1lit|@float2lit|@float3lit)

-- SS. 4.9. Imaginary literals
@imaginary_lit  = (@decimals|@float_lit)i
@hex_byte_value = \\x($hex_digit){2}
@oct_byte_value = \\($octal_digit){3}
@little_u_value = \\u($hex_digit){4}
@big_u_value    = \\U($hex_digit){8}
@escaped_char   = \\[abfnrtv\`\'\"]
@unicode_value  = (@unicode_nodq|@little_u_value|@big_u_value|@escaped_char)
@byte_value     = (@oct_byte_value|@hex_byte_value)

-- SS. 4.10. Character literals
@char_lit   = \'(@unicode_nosq|@little_u_value|@big_u_value|@escaped_char|@byte_value)\'

-- SS. 4.11. String literals
@raw_string_lit = \`(@unicode_nobq)*\`
@int_string_lit = \"(@unicode_value|@byte_value)*\"
@string_lit = (@raw_string_lit|@int_string_lit)

--
--
--

tokens :-

  $whitespace+    { \p s -> posify p $ GoTokNone }
  $whiteline      { \p s -> posify p $ GoTokSemicolonAuto }
  @ol_comment     { \p s -> posify p $ tokenFromComment False s }
  @ml_comment     { \p s -> posify p $ tokenFromComment True s }
  @int_lit        { \p s -> posify p $ tokenFromInt s }
  @float_lit      { \p s -> posify p $ tokenFromReal s }
  @imaginary_lit  { \p s -> posify p $ tokenFromImag s }
  @char_lit       { \p s -> posify p $ tokenFromChar s }
  @raw_string_lit { \p s -> posify p $ tokenFromRawStr s }
  @int_string_lit { \p s -> posify p $ tokenFromString s }
  "("             { \p _ -> posify p $ GoTokLParen }
  ")"             { \p _ -> posify p $ GoTokRParen }
  "{"             { \p _ -> posify p $ GoTokLBrace }
  "}"             { \p _ -> posify p $ GoTokRBrace }
  "["             { \p _ -> posify p $ GoTokLBracket }
  "]"             { \p _ -> posify p $ GoTokRBracket }

  ";"             { \p _ -> posify p $ GoTokSemicolon }
  ":"             { \p _ -> posify p $ GoTokColon }
  ":="            { \p _ -> posify p $ GoTokColonEq }
  "="             { \p _ -> posify p $ GoTokEqual }
  ","             { \p _ -> posify p $ GoTokComma }
  "."             { \p _ -> posify p $ GoTokFullStop }
  "..."           { \p _ -> posify p $ GoTokElipsis }
  "_"             { \p _ -> posify p $ GoTokId "_" }

-- BEGIN operators
  "||"            { \p _ -> posify p $ GoTokLOR }
  "&&"            { \p _ -> posify p $ GoTokLAND }
  "=="            { \p _ -> posify p $ GoTokEQ }
  "!="            { \p _ -> posify p $ GoTokNE }
  "<"             { \p _ -> posify p $ GoTokLT }
  "<="            { \p _ -> posify p $ GoTokLE }
  ">"             { \p _ -> posify p $ GoTokGT }
  ">="            { \p _ -> posify p $ GoTokGE }
  "+"             { \p _ -> posify p $ GoTokPlus }
  "-"             { \p _ -> posify p $ GoTokMinus }
  "|"             { \p _ -> posify p $ GoTokIOR }
  "^"             { \p _ -> posify p $ GoTokXOR }
  "*"             { \p _ -> posify p $ GoTokAsterisk }
  "/"             { \p _ -> posify p $ GoTokSolidus }
  "%"             { \p _ -> posify p $ GoTokPercent }
  "<<"            { \p _ -> posify p $ GoTokSHL }
  ">>"            { \p _ -> posify p $ GoTokSHR }
  "&"             { \p _ -> posify p $ GoTokAND }
  "&^"            { \p _ -> posify p $ GoTokBUT }
  "!"             { \p _ -> posify p $ GoTokExclaim }
  "<-"            { \p _ -> posify p $ GoTokArrow }
  "--"            { \p _ -> posify p $ GoTokDec }
  "++"            { \p _ -> posify p $ GoTokInc }
-- END operators
  @assignop       { \p s -> posify p $ GoTokOp s }

-- BEGIN keywords
  break           { \p _ -> posify p $ GoTokBreak }
  case            { \p _ -> posify p $ GoTokCase }
  chan            { \p _ -> posify p $ GoTokChan }
  const           { \p _ -> posify p $ GoTokConst }
  continue        { \p _ -> posify p $ GoTokContinue }
  default         { \p _ -> posify p $ GoTokDefault }
  defer           { \p _ -> posify p $ GoTokDefer }
  else            { \p _ -> posify p $ GoTokElse }
  fallthrough     { \p _ -> posify p $ GoTokFallthrough }
  for             { \p _ -> posify p $ GoTokFor }
  func            { \p _ -> posify p $ GoTokFunc }
  go              { \p _ -> posify p $ GoTokGo }
  goto            { \p _ -> posify p $ GoTokGoto }
  if              { \p _ -> posify p $ GoTokIf }
  import          { \p _ -> posify p $ GoTokImport }
  interface       { \p _ -> posify p $ GoTokInterface }
  map             { \p _ -> posify p $ GoTokMap }
  package         { \p _ -> posify p $ GoTokPackage }
  range           { \p _ -> posify p $ GoTokRange }
  return          { \p _ -> posify p $ GoTokReturn }
  select          { \p _ -> posify p $ GoTokSelect }
  struct          { \p _ -> posify p $ GoTokStruct }
  switch          { \p _ -> posify p $ GoTokSwitch }
  type            { \p _ -> posify p $ GoTokType }
  var             { \p _ -> posify p $ GoTokVar }
-- END keywords
  @identifier     { \p s -> posify p $ GoTokId s }

{
posAlex2Parsec :: String -> AlexPosn -> SourcePos
posAlex2Parsec filename (AlexPn o l c) = newPos filename l c

posify :: AlexPosn -> GoToken -> GoTokenPos
posify pos tok = GoTokenPos (posAlex2Parsec "" pos) tok
}
