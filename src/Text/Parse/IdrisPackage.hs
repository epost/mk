-- references:
--
-- - idris ipkg parser https://github.com/idris-lang/Idris-dev/blob/a9b30adea20cab85e61ab6b8919f86b3d7efd47d/src/Idris/Package/Parser.hs
-- - megaparsec tutorial https://markkarpov.com/megaparsec/parsing-simple-imperative-language.html
module Text.Parse.IdrisPackage where

import           Data.Void
import           Text.Megaparsec
import           Text.Megaparsec.Char
import           Text.Megaparsec.Expr
import qualified Text.Megaparsec.Char.Lexer as Lexer

-- -- https://ghc.haskell.org/trac/ghc/wiki/Records/OverloadedRecordFields
-- {-# LANGUAGE OverloadedRecordFields #-}
-- {-# LANGUAGE DuplicateRecordFields #-}
-- {-# LANGUAGE OverloadedLabels #-}
data Pkg = Pkg
  { _name       :: String
  , _sourcedir  :: String
  , _main       :: [String]
  , _executable :: String
  , _modules    :: [[String]]
  , _tests      :: [[String]]
  }
  deriving Show

pkg :: Parser Pkg
pkg = do
  pkgName'    <- pkgNameDecl
  sourcedir'  <- sourcedir
  executable' <- executable
  main'       <- mainModule
  modules'    <- modules
  tests'      <- tests
  pure $ Pkg { _name       = pkgName'
             , _sourcedir  = sourcedir'
             , _main       = main'
             , _executable = executable'
             , _modules    = modules'
             , _tests      = tests'
             }

pkgNameDecl :: Parser String
pkgNameDecl = (keyword "package") *> pkgNameStr

sourcedir :: Parser String
sourcedir = keyVal "sourcedir" executableNameStr

executable :: Parser String
executable = keyVal "executable" executableNameStr

mainModule :: Parser [String]
mainModule = keyVal "main" moduleNameStr

modules :: Parser [[String]]
modules = keyVal "modules" (moduleNameStr `sepBy1` comma)

tests :: Parser [[String]]
tests = keyVal "tests" (moduleNameStr `sepBy1` comma)

--------------------------------------------------------------------------------

keyVal :: String -> Parser a -> Parser a
keyVal kp vp = do
  keyword kp
  symbol "="
  vp

--------------------------------------------------------------------------------

integer :: Parser Integer
integer = lexeme Lexer.decimal

comma :: Parser String
comma = symbol ","

dot :: Parser String
dot = symbol "."

--------------------------------------------------------------------------------

keyword :: String -> Parser ()
keyword w = (lexeme . try) (string w *> notFollowedBy alphaNumChar)

-- TODO some of the keywords here likely *will* be identifiers, so this approach needs to change
keywords :: [String]
keywords = ["package", "modules", "tests", "sourcedir", "main", "executable", "pkgs", "author"]

identifier :: Parser String
identifier = (lexeme . try) (p >>= check)
  where
    p       = (:) <$> letterChar <*> many alphaNumChar
    check x = if x `elem` keywords
                then fail $ "keyword " ++ show x ++ " cannot be an identifier"
                else return x

pkgNameStr = identifier
moduleNameStr = identifier `sepBy1` dot
executableNameStr = identifier

--------------------------------------------------------------------------------

-- | space consumer
sc :: Parser ()
sc = Lexer.space space1 lineCmnt blockCmnt
  where
    lineCmnt  = Lexer.skipLineComment "--"
    blockCmnt = Lexer.skipBlockComment "{-" "-}"

symbol :: String -> Parser String
symbol = Lexer.symbol sc

lexeme :: Parser a -> Parser a
lexeme = Lexer.lexeme sc

--------------------------------------------------------------------------------

type Parser = Parsec Void String
