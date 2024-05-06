# blog-to-lazyppl
Translator from Stuart Russel et al.'s BLOG language to Sam Staton et al.'s LazyPPL Haskell library.
BLOG can be found here: https://bayesianlogic.github.io/
LazyPPL can be found here: https://lazyppl-team.github.io/
The lexer is generated by Alex: https://haskell-alex.readthedocs.io/en/latest/
The parser is generated by Happy: https://haskell-happy.readthedocs.io/en/latest/

The translator is composed of three functions:
- lexer :: String -> [Token]
- parser :: [Token] -> Program
- translator :: Program -> String
(translator.parser.lexer) expects a BLOG source file and returns a Haskell source file.

For example a typical prompt at the command line would be `(translator.parser.lexer) "random Real a ~ Gaussian(0, 10); query a"`

Note that several grammatical features of BLOG are currently untranslated:
- IF-THEN statements (as opposed to IF-THEN-ELSE statements)
- matrices, vectors, and distributions which support them
- time steps, and language features which support them
- constants (EG pi)
- some inbuilt functions
this may not be an exhaustive list. Where possible, the translator reports when a program contains a language feature which isn't translated yet.
