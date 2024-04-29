module BLOGTranslate (translator) where
import BLOGParse (parser, Program(..), Statement(..), Declaration(..), Expr(..), Type(..))
import BLOGLex (lexer, Token(..))
import Prelude hiding (GT, LT, EQ)  -- clashes with Token type
import Data.Map
import Data.List (intercalate, find)
import Data.Maybe

-- translates BLOG program to LazyPPL code
-- last part of the translator.parser.lexer pipeline
translator :: Program -> String
translator p = unlines $ Prelude.map (\x -> unlines $ x p) [header, helpers, userTypes, userOrigins, model, mainPart, footer]

-- the header features the readme comment and imports
header :: Program -> [String]
header = const ["-- This file was produced by LazyBLOG translator",
                "-- Sam Staton, Sky Shoesmith, 2024",
                "-- special thanks to UC Berkeley", "",
                "module LazyBLOG where",
                "import LazyPPL",
                "import Distr",
                "import Distr.Memoization",
                "import System.Random"]

-- the userTypes section translates user-defined BLOG types into Haskell types
userTypes :: Program -> [String]
userTypes p = if (types p == [])
              then ["-- user-defined type declarations would go here",
                    "-- (none found in source file)"] 
              else "-- user-defined type declarations" : Prelude.map (declare p) (types p)

-- the userOrigins section translates origin functions into Haskell functions
-- (keeps a copy of the whole program around)
userOrigins' :: Program -> Program -> [String]
userOrigins' p [] = []
userOrigins' p (DECSTMT (OFUDECL (SIMPLETYPE t, s) (SIMPLETYPE t')) : p') = typeSignature : def : userOrigins' p p'
  where typeSignature = ("f"++s ++ " :: "++t'++" -> Maybe "++t)
        def = "f"++s++" ("++t'++concat [if t'' == t then " x" else " _" | (SIMPLETYPE t'',s) <- ofus p t']++" i) = x"
userOrigins' p (stmt:p') = userOrigins' p p'

userOrigins p = userOrigins' p p

-- Haskell declaration of a type
declare :: Program -> String -> String
declare p x = "data " ++ x ++ " = " ++ x ++ " " ++ (unwords $ Prelude.map f (ofus p x)) ++ "Int deriving (Show,Eq,Ord)"
  where f (SIMPLETYPE t,s) = "(Maybe "++t++") "

-- the list of all user-defined types in the source program
types :: Program -> [String]
types ((DECSTMT (TYPDECL x)) : xs) = x : types xs
types (x:xs) = types xs
types [] = []

-- list of all origin *functions* (and their types) of a type (could be fused with origins?)
ofus :: Program -> String -> [(Type,String)]
ofus ((DECSTMT (OFUDECL (SIMPLETYPE t,s') (SIMPLETYPE t'))):p) s = if s == t' 
                                                                      then (SIMPLETYPE t,s'):ofus p s 
                                                                      else ofus p s
ofus (stmt:p) s = ofus p s
ofus [] s = []

-- find the (user-defined) type of an origin function
typeOrigin :: Program -> String -> String
typeOrigin (DECSTMT (OFUDECL (SIMPLETYPE t,s) (SIMPLETYPE t')) : p) ofu = if ofu == s then t else typeOrigin p ofu
typeOrigin (stmt:p) ofu = typeOrigin p ofu
typeOrigin [] ofu = "BUG!!!"

-- these helper functions feature in all translations
helpers :: Program -> [String]
helpers = const ["-- helper functions (used in all translations)",
                 "distribution :: Ord a => [a] -> [(a,Int)]",
                 "distribution = foldr ins []",
                 "",
                 "ins :: Ord a => a -> [(a,Int)] -> [(a, Int)]",
                 "ins a [] = (a,1):[]",
                 "ins a ((b,n):xs) = if a == b then (b,n+1):xs ",
                 "                   else if a > b ",
                 "                        then ((b,n) : ins a xs)",
                 "                        else (a,1) : (b,n) : xs",
                 "",
                 "pretty :: Show a => Int -> (a,Int) -> String",
                 "pretty n (x,y) = \"        \" ++ show x ++ \"   \" ++ show (fromIntegral y / fromIntegral n)",
                 "",
                 "uniformChoice :: [a] -> Prob a",
                 "uniformChoice xs = do",
                 "    choice <- uniformdiscrete (length xs)",
                 "    return (xs !! choice)",
                 "",
                 "rejectionSampler :: Int -> [Maybe a] -> ([a],Int)",
                 "rejectionSampler 0 _ = ([],0)",
                 "rejectionSampler n (Nothing:xs) = let (xs',r) = rejectionSampler  n    xs in (xs',r+1)",
                 "rejectionSampler n (Just x :xs) = let (xs',r) = rejectionSampler (n-1) xs in (x:xs',r)"]

-- the statistical model of the BLOG program, in LazyPPL's Prob monad
model :: Program -> [String]
model p = ["-- model from source BLOG file",
           "model :: Prob (Maybe " ++ (tuplefy $ queryTypes p) ++ ")",
           "model = do"] ++ 
           Prelude.map ("    "++) ((userTypeInits p) ++
           [""] ++
           modelBody p ++
           [""] ++
           returnStmt p)

-- does not explicitly type, nor leave comments (perhaps it should)
modelBody :: Program -> [String]
modelBody p = modelBody' p p 

-- keeps a copy of the whole program for reference (first arg)
-- translation is ad-hoc (pattern matching non-exhaustive)
modelBody' :: Program -> Program -> [String]
-- the query is handled by returnStmt
modelBody' p (QRYSTMT e : p')                  = modelBody' p p'

-- evidence statements are handled by returnStmt
modelBody' p (EVDSTMT (e1,e2) : p') = ("let obs"++(show $ obsNum p (EVDSTMT (e1,e2)))++" = (" ++ (transExpr p (context p) e1) ++ ") == (" ++ (transExpr p (context p) e2) ++ ")") : modelBody' p p' 

-- nullary random functions (where RHS is built-in distribution)
modelBody' p (DECSTMT (RFUDECL (SIMPLETYPE "Real") s [] (CALL "UnivarGaussian" [arg1,arg2])) : p') = (s++" <- normal ("++(transExpr p (context p) arg1)++") ("++(transExpr p (context p) arg2)++")"):modelBody' p p'

-- nullary random functions (where RHS is general)
modelBody' p (DECSTMT (RFUDECL _ s [] e):p') = if isRand e
                                               then error "can only translate built-in distributions"
                                               else ("let "++s++" = "++(transExpr p (context p) e)) : modelBody' p p'

-- random functions on arguments
modelBody' p (DECSTMT (FFUDECL t s args e) : p') = ("let "++s++(unwords (Prelude.map snd args)) ++ " = " ++ (transExpr p (context p) e)) : modelBody' p p'

modelBody' p (DECSTMT (RFUDECL t s args e) : p') = (("f"++s++" <- generalmemoize (\\"++(unwords $ Prelude.map snd args)++" -> "++(transExpr p (context p) e))++")"):modelBody' p p'
modelBody' p (stmt:p') = modelBody' p p'
modelBody' p [] = []

-- helper function of modelBody'
obsNum :: Program -> Statement -> Int
obsNum [] _ = 1
obsNum (EVDSTMT (e1,e2):p') stmt = if stmt==(EVDSTMT (e1,e2)) then 1 else 1 + obsNum p' stmt
obsNum (stmt':p') stmt = obsNum p' stmt

-- counts the number of obs statements in a program
obsCount :: Program -> Int
obsCount [] = 0
obsCount (EVDSTMT (e1,e2):p') = 1 + obsCount p'
obsCount (stmt:p) = obsCount p

-- detects whether the translation of the expression will require random sampling
isRand :: Expr -> Bool
isRand (CALL "UnivarGaussian" _)   = True
isRand (CALL "Bernoulli" _)        = True
isRand (CALL "Beta" _)             = True
isRand (CALL "Binomial" _)         = True
isRand (CALL "BooleanDistrib" _)   = True
isRand (CALL "Dirichlet" _)        = True
isRand (CALL "Discrete" _)         = True
isRand (CALL "Exponential" _)      = True
isRand (CALL "Gamma" _)            = True
isRand (CALL "Gaussian" _)         = True
isRand (CALL "Geometric" _)        = True
isRand (CALL "Laplace" _)          = True
isRand (CALL "Multinomial" _)      = True
isRand (CALL "MultivarGaussian" _) = True
isRand (CALL "NegativeBinomial" _) = True
isRand (CALL "Poisson" _)          = True
isRand (CALL "UniformChoice" _)    = True
isRand (CALL "UniformInt" _)       = True
isRand (CALL "UniformReal" _)      = True
isRand (CALL "UniformVector" _)    = True
isRand (INT _)    = False
isRand (STRING _) = False
isRand (CHAR _)   = False
isRand (DOUBLE _) = False
isRand (BOOL _)   = False
isRand (BLOGParse.ID _) = False
isRand (BLOGParse.PLUS e1 e2)  = isRand e1 || isRand e2
isRand (BLOGParse.MINUS e1 e2) = isRand e1 || isRand e2
isRand (BLOGParse.MULT e1 e2)  = isRand e1 || isRand e2
isRand (BLOGParse.DIV e1 e2)   = isRand e1 || isRand e2
isRand (BLOGParse.MOD e1 e2)   = isRand e1 || isRand e2
isRand (BLOGParse.POWER e1 e2) = isRand e1 || isRand e2
isRand (BLOGParse.LT e1 e2)    = isRand e1 || isRand e2
isRand (BLOGParse.GT e1 e2)    = isRand e1 || isRand e2
isRand (BLOGParse.LEQ e1 e2)   = isRand e1 || isRand e2
isRand (BLOGParse.GEQ e1 e2)   = isRand e1 || isRand e2
isRand (BLOGParse.EQEQ e1 e2)  = isRand e1 || isRand e2
isRand (BLOGParse.NEQ e1 e2)   = isRand e1 || isRand e2
isRand (BLOGParse.AND e1 e2)   = isRand e1 || isRand e2
isRand (BLOGParse.OR e1 e2)    = isRand e1 || isRand e2
isRand (IMPLIES e1 e2) = isRand e1 || isRand e2
isRand (APPLY e1 e2)   = isRand e1 || isRand e2
isRand (NEG e) = isRand e
isRand (BLOGParse.NOT e) = isRand e
isRand (BLOGParse.AT e)  = isRand e --not sure what this means
isRand (IFELSE e1 e2 e3) = isRand e1 || isRand e2 || isRand e3
isRand (CALL _ args) = False --not sure on this one

returnStmt :: Program -> [String]
returnStmt p = ["return $ if "++observations++" then Just " ++ tuplefy (Prelude.map (transExpr p $ context p) (queries p))++" else Nothing"]
  where observations = intercalate " && " ["obs"++show n | n <- [1..obsCount p]]

userTypeInits :: Program -> [String]
userTypeInits p = if ts == [] 
                  then ["-- user-defined type population would go here",
                        "-- (no user-defined types in source file)"]
                  else intercalate [""] (Prelude.map (userTypeInit p) ts)
    where ts = types p

-- writes the (Haskell) type initialisation of a user-defined BLOG type
userTypeInit :: Program -> String -> [String]
userTypeInit p t = ("---- "++t++" population") :
                   concat (Prelude.map (dntStmtTrans p t) dntStmts) ++
                   concat (Prelude.map (numStmtTrans p) numStmts) ++
                   universeDecl p t
  where dntStmts = distinctStmts p t
        numStmts = numberStmts p t

-- helper function of userTypeInit
distinctStmts :: Program -> String -> [(String,Int)]
distinctStmts (DECSTMT (DNTDECL s' [(member,count)]):p) s = if s' == s 
                                                            then (member,count) : distinctStmts p s
                                                            else distinctStmts p s
distinctStmts (DECSTMT (DNTDECL s' ((member,count):ms)):p) s = if s' == s 
                                                               then (member,count) : distinctStmts (DECSTMT (DNTDECL s' ms):p) s
                                                               else distinctStmts (DECSTMT (DNTDECL s' ms):p) s
distinctStmts (stmt:p) s = distinctStmts p s
distinctStmts []       s = []

dntStmtTrans :: Program -> String -> (String,Int) -> [String]
dntStmtTrans p t (obj,count) = ["-- distinct "++t++" "++obj++if count == -1 then "" else "["++(show count)++"]",
                                "let v"++obj++" = "++if count == -1 
                                                 then t++nothings++" "++(show identity)
                                                 else "["++t++nothings++" ("++(show identity)++s++(show $ count-1)++"]]"]
  where nothings = (concat [" Nothing" | ofu <- ofus p t])
        identity = offset p t obj
        s = " + i) | i <- [0.."

-- helper function of dntStmtTrans
-- determines the identity of a distinct member of a user-defined type
offset :: Program -> String -> String -> Int
offset [] t obj = 0
offset (DECSTMT (DNTDECL t' [(obj',n)]):p) t obj = if t == t'
                                                   then if obj' == obj 
                                                        then 0
                                                        else abs n + offset p t obj
                                                   else offset p t obj
offset (DECSTMT (DNTDECL t' ((obj',n):objs)):p) t obj = if t == t'
                                                        then if obj' == obj 
                                                             then 0
                                                             else abs n + offset (DECSTMT (DNTDECL t' objs):p) t obj
                                                        else offset p t obj
offset (stmt:p) t obj = offset p t obj

-- find the distinct members of a type
members :: Program -> String -> [(String,Int)]
members [] t = []
members (DECSTMT (DNTDECL t' objs):p) t  = if t == t'
                                           then objs ++ members p t
                                           else members p t
members (stmt:p) t = members p t

-- trick abusing the fact that no member has identifier ""
countDistincts :: Program -> String -> Int
countDistincts p t = offset p t ""

-- translate a number statement, EG "#Blip(Source=a) ~ Poisson(1.0)"
numStmtTrans :: Program -> Statement -> [String]
numStmtTrans p (DECSTMT (NUMDECL s' os e)) = ["-- "++s'++(describeOrigins os)++" ~ "++(show e),
                                              numStmtCalc p stmt,
                                              numStmtGround p stmt]
  where stmt = (DECSTMT (NUMDECL s' os e))
        maybeSum = if os == [] then "" else "sum "

describeOrigins :: [(String,String)] -> String
describeOrigins [] = ""
describeOrigins xs = "("++(intercalate "," $ Prelude.map (\x->fst x ++"="++snd x) xs)++")"

-- declare a list containing all members of a user-defined type
universeDecl :: Program -> String -> [String]
universeDecl p t = ["-- universe of all "++t++" members",
                    "let universe"++t++" = "++(intercalate " ++ " $ memberStrings p t)]

memberStrings :: Program -> String -> [String]
memberStrings [] t = []
memberStrings (DECSTMT (NUMDECL t'   os e):p) t = if t' == t then ("lst"++name) : memberStrings p t else memberStrings p t
  where name = numStmtName $ DECSTMT (NUMDECL t' os e)
memberStrings (DECSTMT (DNTDECL t' [m]):p) t = if t' == t then (memberString m) : memberStrings p t else memberStrings p t
memberStrings (DECSTMT (DNTDECL t' (m:ms)):p) t = if t' == t 
                                                  then (memberString m) : memberStrings (DECSTMT (DNTDECL t' ms):p) t
                                                  else memberStrings p t
memberStrings (stmt:p) t = memberStrings p t

-- helper function of memberStrings
memberString :: (String,Int) -> String
memberString (s,-1) = "[v"++s++"]"
memberString (s,_)  = "v"++s

-- gives a unique identifier to a number statement
-- (NB: output is a valid Haskell variable identifier.)
numStmtName :: Statement -> String
numStmtName (DECSTMT (NUMDECL s' os e)) = s' ++ concat (Prelude.map fst os)

-- writes a line of Haskell to calculate the RHS of a number statement
-- assumes identifier "i" is not already bound
numStmtCalc :: Program -> Statement -> String
numStmtCalc p (DECSTMT (NUMDECL s' [] e)) = "len"++numStmtName (DECSTMT (NUMDECL s' [] e))++" <- "++(transExpr p (context p) e)
numStmtCalc p (DECSTMT (NUMDECL s' os e)) = "len"++numStmtName (DECSTMT (NUMDECL s' os e))++" <- sequence [do {i <- "++rhs++";return (i"++vars++")} | "++originLoop++"]"
  where rhs = (transExpr p (context p) e)
        originLoop = intercalate ", " $ Prelude.map (\(ofu,arg) -> arg ++ " <- universe" ++ (typeOrigin p ofu)) os
        vars = concat ["," ++ (originVar os (fst o)) | o <- os]

numStmtGround :: Program -> Statement -> String
numStmtGround p (DECSTMT (NUMDECL s' [] e)) = "let lst"++name++" = ["++s'++sources++lastBit++name++" - 1]]"
  where name    = numStmtName (DECSTMT (NUMDECL s' [] e))
        sources = numStmtSources p (DECSTMT (NUMDECL s' [] e))
        lastBit = " ("++(show $ offset p s' "")++" + i) | i <- [0..fromIntegral len"
numStmtGround p (DECSTMT (NUMDECL s' os e)) = "let lst"++name++" = concat [["++s'++sources++" (fromIntegral i) | i <- [0..n-1]] | (n"++vars++") <- len"++name++"]"
  where name    = numStmtName (DECSTMT (NUMDECL s' os e))
        sources = numStmtSources p (DECSTMT (NUMDECL s' os e))
        vars    = concat ["," ++ (originVar os ofu) | (ofu,arg) <- os]

numStmtSources :: Program -> Statement -> String
numStmtSources p (DECSTMT (NUMDECL s' os e)) = concat [if os `contains` ofu 
                                                       then " (Just "++(originVar os ofu)++")"
                                                       else " Nothing"
                                                      |ofu <- Prelude.map snd $ ofus p s']

--helper function of numStmtSources
contains :: [(String,String)] -> String -> Bool
contains [] s             = False
contains ((ofu,arg):os) s = (ofu == s) || contains os s
--helper function of numStmtSources
originVar :: [(String,String)] -> String -> String
originVar [] s             = "ORIGINVAR BUG"
originVar ((ofu,arg):os) s = if (ofu == s) then arg else originVar os s

-- translates a BLOG expression into a Haskell one
-- source program -> program's type context -> expression to translate -> output
transExpr :: Program -> Map String ([Type],Type) -> Expr -> String
transExpr p c (CALL "Poisson" [n]) = "poisson " ++ transExpr p c n
-- other BLOG inbuilt distributions go here.
transExpr p c (INT n)    = show (n :: Int)
transExpr p c (STRING s) = show s
transExpr p c (DOUBLE n) = show (n :: Double)
transExpr p c (BOOL n)   = show (n :: Bool)
transExpr p c BLOGParse.NULL = error "why am I translating null?"
transExpr p c (BLOGParse.ID s) = s
transExpr p c (BLOGParse.PLUS  e1 e2) = op p c "+" e1 e2
transExpr p c (BLOGParse.MINUS e1 e2) = op p c "-" e1 e2
transExpr p c (BLOGParse.MULT  e1 e2) = op p c "*" e1 e2
transExpr p c (BLOGParse.DIV   e1 e2) = op p c "/" e1 e2
transExpr p c (BLOGParse.MOD   e1 e2) = op p c "`mod`" e1 e2 -- will cause type issues; FIX!
transExpr p c (BLOGParse.POWER e1 e2) = op p c "^" e1 e2     -- may cause type issues; INVESTIGATE!
transExpr p c (BLOGParse.LT    e1 e2) = op p c "<" e1 e2
transExpr p c (BLOGParse.GT    e1 e2) = op p c ">" e1 e2
transExpr p c (BLOGParse.LEQ   e1 e2) = op p c "<=" e1 e2
transExpr p c (BLOGParse.GEQ   e1 e2) = op p c ">=" e1 e2
transExpr p c (BLOGParse.EQEQ  e1 e2) = op p c "==" e1 e2
transExpr p c (BLOGParse.NEQ   e1 e2) = op p c "/=" e1 e2
transExpr p c (BLOGParse.AND   e1 e2) = op p c "&&" e1 e2
transExpr p c (BLOGParse.OR    e1 e2) = op p c "||" e1 e2
transExpr p c (BLOGParse.IMPLIES e1 e2) = op p c "<=" e1 e2 -- will confuse people; CLARIFY!
transExpr p c (BLOGParse.APPLY e1 e2) = error "APPLY not implemented"
transExpr p c (BLOGParse.NEG e) = "(*-1) $ " ++ transExpr p c e
transExpr p c (BLOGParse.NOT e) = "not $ " ++ transExpr p c e
transExpr p c (BLOGParse.AT e) = error "AT may be out-of-scope for this translator (timesteps not implemented)"
transExpr p c (IFELSE e1 e2 e3) = "if "++(transExpr p c e1)++" then "++(transExpr p c e2)++" else "++(transExpr p c e3)
transExpr p c (IFTHEN _ _) = error "IFTHEN may be deprecated syntax"
transExpr p c (CALL s args) = transCall p c (CALL s args)
transExpr p c (MAPCONSTRUCT _) = error "MAPCONSTRUCT not implemented"
transExpr p c (COMPREHENSION [e'] args e) = "["++(transExpr p (argContext args `union` c) e')++" | "++(intercalate ", " $ Prelude.map (\(SIMPLETYPE t,s) -> s++" <- universe"++t) args)++"]"
transExpr p c (BLOGParse.EXISTS (SIMPLETYPE t) s e) = "any id [" ++ transExpr p (insert s ([],SIMPLETYPE t) c) e ++ " | " ++ s ++ " <- universe" ++ t ++ "]"
transExpr p c (BLOGParse.FORALL (SIMPLETYPE t) s e) = "all id [" ++ transExpr p (insert s ([],SIMPLETYPE t) c) e ++ " | " ++ s ++ " <- universe" ++ t ++ "]"
transExpr p c e = error $ "I don't know how to translate " ++ show e

-- helper function of transExpr (creates type context for args)
argContext :: [(Type,String)] -> Map String ([Type],Type)
argContext = fromList . (Prelude.map (\(t,s) -> (s,([],t))))

-- helper function of transExpr (inserts operator between expressions)
op :: Program -> Map String ([Type],Type) -> String -> Expr -> Expr -> String
op p c s e1 e2 = "(" ++ transExpr p c e1 ++ ") "++s++" (" ++ transExpr p c e2 ++ ")"

-- translates a BLOG function call into a Haskell one
-- (note BLOG models variables as 0-ary functions)
transCall :: Program -> Map String ([Type],Type) -> Expr -> String
transCall p c (CALL "abs" [e]) = "abs (" ++ (transExpr p c e) ++ ")"
transCall p c (CALL "UnivarGaussian" [e1,e2]) = "normal ("++(transExpr p c e1)++") ("++(transExpr p c e2)++")"
transCall p c (CALL "UniformChoice" [e]) = "uniformChoice ("++(transExpr p c e)++")"
transCall p c (CALL "UniformReal" [e1,e2]) = "do {i <- LazyPPL.uniform; return "++e1'++" + i*(("++e2'++") - ("++e1'++"))}"
  where e1' = transExpr p c e1
        e2' = transExpr p c e2
transCall p c (CALL s [])  = case (Data.Map.lookup s $ c) of
                               Nothing -> s -- lambda-bound argument
                               Just ([],SIMPLETYPE "Bool") -> s
                               Just ([],SIMPLETYPE "Real") -> s
                               Just ([],SIMPLETYPE userType) -> s
                               Just ([], t)   -> error $ "transCall error #1"
                               Just (args, t) -> error $ "transCall error #2"
transCall p c (CALL "size" [e]) = "length "++(transExpr p c e)
transCall p c (CALL s [e]) = case (Data.Map.lookup s $ c) of
                               Nothing -> error $ "unable to find function " ++ s
                               Just ([],SIMPLETYPE t') ->  s ++ " !! " ++  "???" ++ " + " ++ (transExpr p c e)
                               Just ((arg:args),t) -> "f"++ s ++ " (" ++ transExpr p c e ++ ")"
                               _ -> error $ "cannot call (" ++ (show s) ++ ") as a function"

-- retrieves the number statements of a user-defined type
numberStmts :: Program -> String -> [Statement]
numberStmts [] s = []
numberStmts (DECSTMT (NUMDECL s' origins e) : p) s = if s == s' 
                                                    then DECSTMT (NUMDECL s' origins e):numberStmts p s 
                                                    else numberStmts p s
numberStmts (x:p) s = numberStmts p s

-- counts the inhabitants of a user-defined type
countType :: Program -> String -> Int
countType (DECSTMT (DNTDECL s' ls) : p) s = (countType p s) + if s' == s then (sum $ (Prelude.map (abs.snd) ls)) else 0
countType [] s = 0
countType (x:p) s = countType p s

tuplefy :: [String] -> String
tuplefy []  = "()"
tuplefy [t] = t
tuplefy xs  = "(" ++ (intercalate "," xs) ++ ")"

-- maps identifiers to their types
-- (BLOG models constants as 0-ary functions)
context :: Program -> Map String ([Type],Type)
context [] = fromList [("size",([SIMPLETYPE "a set of some sort"],SIMPLETYPE "Int"))] -- UNFINISHED
context ((DECSTMT (FFUDECL t s args e)) : p) = insert s (Prelude.map fst args,t) (context p)
context ((DECSTMT (RFUDECL t s args e)) : p) = insert s (Prelude.map fst args,t) (context p)
context ((DECSTMT (DNTDECL s members)) : p) = (mapify s members) `union` (context p)
context ((DECSTMT (OFUDECL (SIMPLETYPE t, s) (SIMPLETYPE t'))) : p) = insert s ([SIMPLETYPE t'],SIMPLETYPE t) (context p)
context (stmt : p) = context p

-- helper function of context
-- creates the type map of a Distinct statement
mapify :: String -> [(String,Int)] -> Map String ([Type],Type)
mapify s [] = fromList []
mapify s ((name,count):xs) = insert name ([],(SIMPLETYPE s)) (mapify s xs)

-- the expressions in query statements (in source file order)
queries :: Program -> [Expr]
queries ((QRYSTMT e) : xs) = e : (queries xs)
queries (x:xs) = queries xs
queries [] = []

-- the types of those queried expressions (in source file order)
-- (DANGEROUS: ASSUMES THEY ARE 0-ary EXPRESSIONS)
queryTypes :: Program -> [String]
queryTypes p = Prelude.map (typeString.(\e -> snd $ typeIt e (context p))) (queries p)

-- translates an AST type into Haskell
typeString :: Type -> String
typeString (SIMPLETYPE x) = if x == "Real" then "Double" else x
typeString t = error "cannot translate type "++(show t)

-- infers the type of an expression (in a context)
-- assumes the source file type-checks correctly
typeIt :: Expr -> Map String ([Type],Type) -> ([Type],Type)
typeIt (INT n) _       = ([],SIMPLETYPE "Int")
typeIt (STRING s) _    = ([],SIMPLETYPE "String")
typeIt (CHAR c) _      = ([],SIMPLETYPE "Char")
typeIt (DOUBLE n) _    = ([],SIMPLETYPE "Double")
typeIt (BOOL b) _      = ([],SIMPLETYPE "Bool")
typeIt BLOGParse.NULL _          = error "why am I type checking null?"
typeIt (BLOGParse.ID s) m        = let (Just t) = Data.Map.lookup s m in t 
typeIt (BLOGParse.PLUS e1 e2) m  = typeIt e1 m
typeIt (BLOGParse.MINUS e1 e2) m = typeIt e1 m
typeIt (BLOGParse.MULT e1 e2) m  = typeIt e1 m
typeIt (BLOGParse.DIV e1 e2) m   = typeIt e1 m
typeIt (BLOGParse.MOD e1 e2) m   = typeIt e1 m
typeIt (BLOGParse.POWER e1 e2) _  = ([],SIMPLETYPE "Double")
typeIt (BLOGParse.LT e1 e2) _     = ([],SIMPLETYPE "Bool")
typeIt (BLOGParse.GT e1 e2) _     = ([],SIMPLETYPE "Bool")
typeIt (BLOGParse.LEQ e1 e2) _    = ([],SIMPLETYPE "Bool")
typeIt (BLOGParse.GEQ e1 e2) _    = ([],SIMPLETYPE "Bool")
typeIt (BLOGParse.EQEQ e1 e2) _   = ([],SIMPLETYPE "Bool")
typeIt (BLOGParse.NEQ e1 e2) _    = ([],SIMPLETYPE "Bool")
typeIt (BLOGParse.AND e1 e2) _    = ([],SIMPLETYPE "Bool")
typeIt (BLOGParse.OR e1 e2) _     = ([],SIMPLETYPE "Bool")
typeIt (IMPLIES e1 e2) _ = ([],SIMPLETYPE "Bool")
typeIt (APPLY e1 e2) _  = error "what's an apply?"
typeIt (NEG e1) m       = typeIt e1 m
typeIt (BLOGParse.NOT e1) _      = ([],SIMPLETYPE "Bool")
typeIt (BLOGParse.AT e1) _       = error "I can't type an @ statement"
typeIt (IFELSE e1 e2 e3) m = if ((typeIt e2 m) == (typeIt e3 m)) then typeIt e2 m else error "IfElse type error"
typeIt (IFTHEN e1 e2) _  = error "How can an if-then have no else?"
typeIt (CALL s es) m     = let l = Data.Map.lookup s m in 
                           if isNothing l
                           then error $ s++" not found" 
                           else let (Just (argTypes, returnType)) = l in ([],returnType)
typeIt (MAPCONSTRUCT e1e2s) _   = error "Not sure how to do maps...?"
typeIt (COMPREHENSION es tss e) _ = error "How to do comprehensions??"
typeIt (BLOGParse.EXISTS t s e) _ = ([],SIMPLETYPE "Bool")
typeIt (BLOGParse.FORALL t s e) _ = ([],SIMPLETYPE "Bool")

-- main method (not quite finished)
mainPart :: Program -> [String]
mainPart p = ["main :: IO ()","main = do"] ++ Prelude.map ("    "++) ([
              "putStrLn \"Using a fixed random seed for repeatability.\"",
              "putStrLn \"..............................................\"",
              "putStrLn \"Evidence: [???]\"", -- EVIDENCE statements not implemented.
              "putStrLn \"Query: " ++ (escapeQuotes (show $ queries p)) ++ "\"",
              "putStrLn \"Running for 10000 samples...\"\n",
              "-- establish random seed",
              "let tree = randomTree (mkStdGen 0)\n",
              "-- loop to yield data points",
              "let (answers,rejections) = (rejectionSampler 10000) $ runProb (sequence (repeat model)) tree\n",
              "let consistentWorlds = 10000 / (fromIntegral (10000 + rejections))",
              "let worlds = length (distribution answers)",
              "putStrLn \"======  LW Trial Stats  ======\"",
              "putStrLn $ \"Log of average likelihood weight (this trial): \" ++ show (log $ 1 / fromIntegral worlds)",
              "putStrLn $ \"Average likelihood weight (this trial): \"        ++ show (1 / fromIntegral worlds)",
              "putStrLn $ \"Fraction of consistent worlds (this trial): \"    ++ show consistentWorlds",
              "putStrLn $ \"Fraction of consistent worlds (running avg, all trials): \" ++ show consistentWorlds",
              "putStrLn \"======== Query Results ========\"",
              "putStrLn \"Number of samples: 10000\""] ++
              results p ++
              ["putStrLn \"======== Done ========\""])

results :: Program -> [String]
results p = concat [["putStrLn \"Distribution of samples for "++(escapeQuotes $ show (queries p !! (q-1)))++"\"",
                     "mapM (putStrLn.pretty 10000) (distribution $ map " ++ pull q (length $ queries p) ++ " answers)"] 
                    | q <- [1..length $ queries p]]

escapeQuotes :: String -> String
escapeQuotes [] = []
escapeQuotes ('"':xs) = '\\':'"':(escapeQuotes xs)
escapeQuotes (x:xs) = x:escapeQuotes xs

-- returns the Haskell code to pull the k-th item of a n-tuple
pull :: Int -> Int -> String
pull 1 1 = "id"
pull k n = "(\\(" ++ intercalate "," [if q == k then "x" else "_" | q <- [1..n]] ++ ") -> x)"

-- the footer marks the end of the file.
footer :: Program -> [String]
footer p = ["-- EOF"]

-- main method for testing
main :: IO ()
main = do
    fileName <- getLine
    string <- readFile ("example/"++fileName++".blog")
    putStrLn $ (translator.parser.lexer) string

debug :: IO ()
debug = do
    string <- readFile "example/aircraft-static.blog"
    putStrLn $ (show $ ofus ((parser.lexer) string) "Blip")
    
