-- | 
-- Module      : Language.Go.Parser
-- Copyright   : (c) 2011 Andrew Robbins
-- License     : GPLv3 (see COPYING)
--
-- This module contains analysis functions that parse Go source code into an
-- abstract syntax tree (AST). For more information, see one of the submodules.

module Language.Go.Parser where
import Language.Go.Parser.Lexer
import Language.Go.Parser.Parser
import Language.Go.Parser.Tokens
