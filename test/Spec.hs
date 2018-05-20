import Text.Megaparsec
import Text.Parse.IdrisPackage

main :: IO ()
main = do
  parseTest' pkg $
    "package mypackage\n" ++
    "\n" ++
    "sourcedir = src\n" ++
    "\n" ++
    "executable = examples\n" ++
    "\n" ++
    "main = Examples\n" ++
    "\n" ++
    "modules = Types\n" ++
    "        , Token\n" ++
    "        , Lex\n" ++
    "        , AST\n" ++
    "        , Parse\n" ++
    "        , Types\n" ++
    "\n" ++
    "tests   = Test.Types\n" ++
    "        , Test.Token\n" ++
    "        , Test.Lex\n" ++
    "        , Test.AST\n" ++
    "        , Test.Parse\n" ++
    ""
