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
translator p = unlines $ Prelude.map (\x -> unlines $ x p)
     [header, helpers, userTypes, userOrigins, model, mainPart, footer]

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
userOrigins' p (DECSTMT (OFUDECL (SIMPLETYPE t, s) (SIMPLETYPE t')) : p') = signature : def : userOrigins' p p'
  where signature = ("f"++s ++ " :: "++t'++" -> Maybe "++t)
        def = "f"++s++" ("++t'++originFields++" i) = x"
        originFields = concat [if t'' == t then " x" else " _" | (SIMPLETYPE t'',s) <- ofus p t']
userOrigins' p (stmt:p') = userOrigins' p p'

userOrigins p = userOrigins' p p

-- Haskell declaration of a type
declare :: Program -> String -> String
declare p x = "data " ++ x ++ " = " ++ x ++ " " ++ originTypes ++ "Int deriving (Show,Eq,Ord)"
  where f (SIMPLETYPE t,s) = "(Maybe "++t++") "
        originTypes = (unwords $ Prelude.map f (ofus p x))

-- the list of all user-defined types in the source program
types :: Program -> [String]
types ((DECSTMT (TYPDECL x)) : xs) = x : types xs
types (x:xs) = types xs
types [] = []

-- list of all origin *functions* (and their types) of a type
ofus :: Program -> String -> [(Type,String)]
ofus ((DECSTMT (OFUDECL (SIMPLETYPE t,s') (SIMPLETYPE t'))):p) s = if s == t' 
                                                                      then (SIMPLETYPE t,s'):ofus p s 
                                                                      else ofus p s
ofus (stmt:p) s = ofus p s
ofus [] s = []

-- find the (user-defined) type of an origin function
typeOrigin :: Program -> String -> String
typeOrigin (DECSTMT (OFUDECL (SIMPLETYPE t,s) (SIMPLETYPE t')) : p) ofu = if ofu == s 
                                                                          then t 
                                                                          else typeOrigin p ofu
typeOrigin (stmt:p) ofu = typeOrigin p ofu
typeOrigin [] ofu = error "cannot search the empty program for origin functions"

-- these helper functions feature in all translations
helpers :: Program -> [String]
helpers = const 
    ["-- helper functions (used in all translations)",
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

-- the declarations and observations of the source BLOG program
modelBody :: Program -> [String]
modelBody p = modelBody' p p 

-- keeps a copy of the whole program for reference (first arg)
modelBody' :: Program -> Program -> [String]

-- evidence statements
modelBody' p (EVDSTMT (e1,e2) : p') = ("let "++obsString++" = " ++ lhs ++ " == " ++ rhs) : modelBody' p p' 
  where obsString = "obs" ++ (show $ obsNum p (EVDSTMT (e1,e2)))
        lhs = transExpr p (context p) e1
        rhs = transExpr p (context p) e2

-- nullary random functions (random variables)
modelBody' p (DECSTMT (RFUDECL _ s [] e):p') =
    if isRand e
    then ("v"++s++" <- "++e')    : modelBody' p p'
    else ("let v"++s++" = "++e') : modelBody' p p'
  where e' = transExpr p (context p) e

-- random functions on arguments (which can be sampled right away using memoization)
modelBody' p (DECSTMT (RFUDECL t s args e) : p') = 
    if isRand e
    then ("f"++s++" <- "++lambdaFunc) : modelBody' p p'
    else ("let f"++s++" "++lambdaArgs++" = "++lambdaBody) : modelBody' p p'
  where c' = Prelude.foldr addToContext (context p) args
        addToContext (t,s) c = insert s ([],t) c
        lambdaArgs = unwords $ Prelude.map (("v"++).snd) args
        lambdaBody = (transExpr p c' e)
        lambdaFunc = "generalmemoize (\\"++lambdaArgs++" -> "++lambdaBody++")"

-- nullary fixed functions (fixed variables)
modelBody' p (DECSTMT (FFUDECL t s [] e) : p') = ("let v"++s++" = " ++ body) : modelBody' p p'
  where body = transExpr p (context p) e

-- fixed functions on arguments
modelBody' p (DECSTMT (FFUDECL t s args e) : p') = ("let f"++s++" "++strArgs++" = " ++ body) : modelBody' p p'
  where addToContext (t,s) c = insert s ([],t) c
        c' = Prelude.foldr addToContext (context p) args
        body = transExpr p c' e
        strArgs = unwords $ Prelude.map (("v"++).snd) args

-- other lines are not considered part of the model body and are passed over
modelBody' p (stmt:p') = modelBody' p p'
modelBody' p [] = []

-- detects whether an expression is random w.r.t its free variables
-- (IE whether it should be translated as a Prob or not)
isRand :: Expr -> Bool
isRand (INT n)    = False
isRand (STRING s) = False
isRand (DOUBLE n) = False
isRand (BOOL n)   = False
isRand BLOGParse.NULL = False
isRand (BLOGParse.ID s) = False
isRand (CALL "UnivarGaussian" _) = True
isRand (CALL "Gaussian" _) = True
isRand (CALL "UniformInt" _) = True
isRand (CALL "BooleanDistrib" _) = True
isRand (CALL "Bernoulli" _) = True
isRand (CALL "Geometric" _) = True
isRand (CALL "UniformReal" _) = True
isRand (CALL "UniformChoice" _) = True
isRand (CALL "Categorical" _) = True
isRand (CALL "Beta" _) = True
isRand (CALL "Exponential" _) = True
isRand (CALL "Binomial" _) = True
isRand (CALL "Laplace" _) = True
isRand (CALL "Gamma" _) = True
isRand (CALL "Poisson" _) = True
isRand (CALL "abs" _) = False
isRand (CALL "exp" _) = False
isRand (CALL "sin" _) = False
isRand (BLOGParse.DIV e1 e2) = False
isRand (IFELSE e1 e2 e3) = isRand e2
isRand (BLOGParse.CASE e1 (MAPCONSTRUCT ((e2,e3):ess))) = isRand e3
isRand (CALL s []) = False -- assume this is a variable or user-defined constant
isRand (CALL s es) = False -- assume this is a user-defined function
isRand x = error $ "cannot check for randomness in " ++ show x

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

returnStmt :: Program -> [String]
returnStmt p = [if obsCount p == 0
                then "return $ " ++ returnValue
                else "return $ if "++observations++" then " ++ returnValue ++" else Nothing"]
  where returnValue  = "Just " ++ if length (queries p) == 1 then "("++returnTuple++")" else returnTuple
        returnTuple = tuplefy (Prelude.map (transExpr p $ context p) (queries p))
        observations = intercalate " && " ["obs"++show n | n <- [1..obsCount p]]

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

distinctStmts (DECSTMT (DNTDECL s' ((member,count):[])):p) s = if s' == s 
                                                               then (member,count) : distinctStmts p s
                                                               else distinctStmts p s

distinctStmts (DECSTMT (DNTDECL s' ((member,count):ms)):p) s = if s' == s 
                                                               then (member,count) : distinctStmts shrunkStmt s
                                                               else distinctStmts p s
  where shrunkStmt = (DECSTMT (DNTDECL s' ms):p)

distinctStmts (stmt:p) s = distinctStmts p s
distinctStmts []       s = []

dntStmtTrans :: Program -> String -> (String,Int) -> [String]
dntStmtTrans p t (obj,count) = ["-- distinct "++t++" "++obj++optionalIndex,
                                "let v"++obj++" = "++
                                  if count == -1 
                                  then t++nothings++" "++(show identity)
                                  else "["++t++nothings++" ("++(show identity)++s++(show $ count-1)++"]]"]
  where nothings = (concat [" Nothing" | ofu <- ofus p t])
        optionalIndex = if count == -1 then "" else "["++(show count)++"]"
        identity = offset p t obj
        s = " + i) | i <- [0.."

-- helper function of dntStmtTrans
-- determines the identity of a distinct member of a user-defined type
offset :: Program -> String -> String -> Int
offset [] t obj = 0

offset (DECSTMT (DNTDECL t' [(obj',n)]):p) t obj = 
    if t == t'
    then if obj' == obj 
         then 0
         else abs n + offset p t obj
    else offset p t obj

offset (DECSTMT (DNTDECL t' ((obj',n):objs)):p) t obj = 
    if t == t'
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

memberStrings (DECSTMT (NUMDECL t'   os e):p) t = if t' == t 
                                                  then ("lst"++name) : memberStrings p t 
                                                  else memberStrings p t
  where name = numStmtName $ DECSTMT (NUMDECL t' os e)

memberStrings (DECSTMT (DNTDECL t' [m]):p) t = if t' == t 
                                               then (memberString m) : memberStrings p t 
                                               else memberStrings p t

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
numStmtCalc p (DECSTMT (NUMDECL s' [] e)) = "len"++name++" <- "++(transExpr p (context p) e)
  where name = numStmtName (DECSTMT (NUMDECL s' [] e))
numStmtCalc p (DECSTMT (NUMDECL s' os e)) = "len"++name++" <- sequence [do {i <- "++rhs++suffix
  where name = numStmtName (DECSTMT (NUMDECL s' os e))
        rhs = (transExpr p (context p) e)
        stringifyOrigins = Prelude.map (\(ofu,arg) -> arg ++ " <- universe" ++ (typeOrigin p ofu))
        originLoop = intercalate ", " $ stringifyOrigins os
        vars = concat ["," ++ (originVar os (fst o)) | o <- os]
        suffix = ";return (i"++vars++")} | "++originLoop++"]"

numStmtGround :: Program -> Statement -> String
numStmtGround p (DECSTMT (NUMDECL s' [] e)) = "let lst"++name++" = ["++s'++sources++lastBit++name++" - 1]]"
  where name    = numStmtName (DECSTMT (NUMDECL s' [] e))
        sources = numStmtSources p (DECSTMT (NUMDECL s' [] e))
        lastBit = " ("++(show $ offset p s' "")++" + i) | i <- [0..fromIntegral len"
numStmtGround p (DECSTMT (NUMDECL s' os e)) = line
  where name    = numStmtName (DECSTMT (NUMDECL s' os e))
        sources = numStmtSources p (DECSTMT (NUMDECL s' os e))
        vars    = concat ["," ++ (originVar os ofu) | (ofu,arg) <- os]
        str     = " (fromIntegral i) | i <- [0..n-1]] | (n"
        line    = "let lst"++name++" = concat [["++s'++sources++str++vars++") <- len"++name++"]"

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
transExpr p c (INT n)    = show (n :: Int)
transExpr p c (STRING s) = show s
transExpr p c (DOUBLE n) = show (n :: Double)
transExpr p c (BOOL n)   = show (n :: Bool)
transExpr p c (SET es)   = "["++intercalate ", " (Prelude.map (transExpr p c) es)++"]"
transExpr p c BLOGParse.NULL = "Nothing" -- corresponds to a null origin 
transExpr p c (BLOGParse.ID s) = "v"++s
transExpr p c (BLOGParse.PLUS  e1 e2) = op p c "+" e1 e2
transExpr p c (BLOGParse.MINUS e1 e2) = op p c "-" e1 e2
transExpr p c (BLOGParse.MULT  e1 e2) = op p c "*" e1 e2
transExpr p c (BLOGParse.DIV   e1 e2) = op p c "/" e1 e2
transExpr p c (BLOGParse.MOD   e1 e2) = op p c "`mod`" e1 e2 -- may cause subtle type issues in some cases
transExpr p c (BLOGParse.POWER e1 e2) = if typeIt e1 c == ([],SIMPLETYPE "Integer")
                                        then op p c "^" e1 e2
                                        else op p c "**" e1 e2
transExpr p c (BLOGParse.LT    e1 e2) = op p c "<" e1 e2
transExpr p c (BLOGParse.GT    e1 e2) = op p c ">" e1 e2
transExpr p c (BLOGParse.LEQ   e1 e2) = op p c "<=" e1 e2
transExpr p c (BLOGParse.GEQ   e1 e2) = op p c ">=" e1 e2
transExpr p c (BLOGParse.EQEQ  e1 e2) = op p c "==" e1 e2
transExpr p c (BLOGParse.NEQ   e1 e2) = op p c "/=" e1 e2
transExpr p c (BLOGParse.AND   e1 e2) = op p c "&&" e1 e2
transExpr p c (BLOGParse.OR    e1 e2) = op p c "||" e1 e2
transExpr p c (BLOGParse.IMPLIES e1 e2) = op p c "<=" e1 e2 -- this is an anti-pun. (<=) is "implies" on Bool
transExpr p c (APPLY (CALL s []) (INT n)) = "(v"++s++" !! "++show n++")"
transExpr p c (BLOGParse.NEG e) = "(-" ++ transExpr p c e ++ ")"
transExpr p c (BLOGParse.NOT e) = "(not " ++ transExpr p c e ++ ")"
transExpr p c (BLOGParse.AT e) = error "AT is out-of-scope for this translator (timesteps not implemented)"
transExpr p c (IFELSE e1 e2 e3) = "if "++transExpr p c e1++" then "++transExpr p c e2++" else "++transExpr p c e3
transExpr p c (IFTHEN _ _) = error "IFTHEN not implemented (null values are out-of-scope for this translator)"
transExpr p c (CALL s args) = transCall p c (CALL s args)
transExpr p c (MAPCONSTRUCT _) = error "MAPCONSTRUCT not implemented"
transExpr p c (BLOGParse.CASE e1 (MAPCONSTRUCT ess)) = "case " ++ transExpr p c e1 ++ " of {" ++ mapify ess ++ "}"
  where mapify = (intercalate "; ". (Prelude.map (\(e1,e2) -> (transExpr p c e1) ++ " -> " ++ (transExpr p c e2))))
transExpr p c (COMPREHENSION [e'] args e) = "["++body++" | "++sources++", "++transExpr p c e++"]"
  where body    = (transExpr p (argContext args `union` c) e')
        sources = intercalate ", " $ Prelude.map (\(SIMPLETYPE t,s) -> "v"++s++" <- universe"++t) args
transExpr p c (BLOGParse.LIST es) = "[" ++ intercalate ", " (Prelude.map (transExpr p c) es) ++ "]"
transExpr p c (BLOGParse.EXISTS (SIMPLETYPE t) s e) = "(any id [" ++ e' ++ " | v" ++ s ++ " <- universe" ++ t ++ "])"
  where e' = transExpr p (insert s ([],SIMPLETYPE t) c) e
transExpr p c (BLOGParse.FORALL (SIMPLETYPE t) s e) = "(all id [" ++ e' ++ " | v" ++ s ++ " <- universe" ++ t ++ "])"
  where e' = transExpr p (insert s ([],SIMPLETYPE t) c) e
transExpr p c e = error $ "I don't know how to translate " ++ show e

-- helper function of transExpr (creates type context of arguments)
argContext :: [(Type,String)] -> Map String ([Type],Type)
argContext = fromList . (Prelude.map (\(t,s) -> (s,([],t))))

-- helper function of transExpr (inserts operator between expressions)
op :: Program -> Map String ([Type],Type) -> String -> Expr -> Expr -> String
op p c s e1 e2 = "(" ++ transExpr p c e1 ++" "++s++" "++ transExpr p c e2 ++ ")"

-- translates a BLOG function call into a Haskell one
-- (note BLOG models variables as 0-ary functions)
transCall :: Program -> Map String ([Type],Type) -> Expr -> String
transCall p c (CALL "UniformChoice" [e]) = "(uniformChoice "++transExpr p c e++")"
transCall p c (CALL "UniformReal" [e1,e2]) = "do {i <- LazyPPL.uniform; return $ "++e1'++" + i*("++e2'++" - "++e1'++")}"
  where e1' = transExpr p c e1
        e2' = transExpr p c e2
transCall p c (CALL "UniformInt" [e1,e2]) = "(uniformChoice ["++e1'++".."++e2'++"])"
  where e1' = transExpr p c e1
        e2' = transExpr p c e2
transCall p c (CALL "MultivarGaussian" [e1,e2]) = error "Matrices are out-of-scope for this translator"
transCall p c (CALL "Poisson" [e]) = "(do {i <- poisson " ++ transExpr p c e ++ ";return $ fromIntegral i})"
transCall p c (CALL "BooleanDistrib" [e]) = "(bernoulli " ++ transExpr p c e ++ ")"
transCall p c (CALL "Bernoulli" [e]) = "(categorical [" ++ transExpr p c e ++ ",1])"
transCall p c (CALL "Geometric" [e]) = "let geometric = do {i <- LazyPPL.uniform;if i < "++transExpr p c e++suffix
  where suffix = " then 0 else 1 + geometric} in geometric"
transCall p c (CALL "Categorical" [MAPCONSTRUCT ess]) = "do {i <- categorical "++probs++";return $ "++vals++" !! i}"
  where probs = "[" ++ (intercalate ", " $ Prelude.map (normalise.transExpr p c.snd) ess) ++ "]"
        vals  = "[" ++ (intercalate ", " $ Prelude.map (transExpr p c.fst) ess) ++ "]"
        normalise x = "("++x++")/"++totalWeight
        totalWeight = "("++(intercalate " + " $ Prelude.map (transExpr p c.snd) ess)++")"
transCall p c (CALL "Beta" _) = error "Beta distribution not implemented"
transCall p c (CALL "Binomial" _) = error "Binomial distribution not implemented"
transCall p c (CALL "Exponential" _) = error "Exponential distribution not implemented"
transCall p c (CALL "Gamma" [e1,e2]) = "(gamma "++transExpr p c e1++" "++transExpr p c e2++")"
transCall p c (CALL "Gaussian" [e1,e2]) = "(normal "++transExpr p c e1++" "++transExpr p c e2++")"
transCall p c (CALL "UnivarGaussian" [e1,e2]) = "(normal "++transExpr p c e1++" "++transExpr p c e2++")"
transCall p c (CALL "Laplace" _) = error "Laplace distribution not implemented"
transCall p c (CALL "Dirichlet" _)        = error "Matrices are out-of-scope for this translator"
transCall p c (CALL "Discrete" _)         = error "Matrices are out-of-scope for this translator"
transCall p c (CALL "Multinomial" _)      = error "Matrices are out-of-scope for this translator"
transCall p c (CALL "NegativeBinomial" _) = error "Matrices are out-of-scope for this translator"
transCall p c (CALL "UniformVector" _)    = error "Matrices are out-of-scope for this translator"
transCall p c (CALL "size"  [e]) = "(length "++transExpr p c e++")"
transCall p c (CALL "round" [e]) = "(round " ++transExpr p c e++")"
transCall p c (CALL "sin"   [e]) = "(sin "   ++transExpr p c e++")"
transCall p c (CALL "cos"   [e]) = "(cos "   ++transExpr p c e++")"
transCall p c (CALL "tan"   [e]) = "(tan "   ++transExpr p c e++")"
transCall p c (CALL "atan"  [e]) = "(atan "   ++transExpr p c e++")"
transCall p c (CALL "atan2" [e1,e2]) = "(atan2 "++transExpr p c e1++" "++transExpr p c e2++")"
transCall p c (CALL "abs"   [e]) = "(abs "   ++transExpr p c e++")"
transCall p c (CALL "min"   [e]) = "(minimum "   ++transExpr p c e++")"
transCall p c (CALL "max"   [e]) = "(maximum "   ++transExpr p c e++")"
transCall p c (CALL "exp"   [e]) = "(exp "   ++if typeIt e c == ([],SIMPLETYPE "Integer") 
                                               then "(fromIntegral "++transExpr p c e++")"
                                               else (transExpr p c e)++")"
transCall p c (CALL s [e]) = case (Data.Map.lookup s $ c) of
                               Nothing -> error $ "unable to find function " ++ s
                               Just ((arg:args),t) -> "(f"++ s ++ " " ++ transExpr p c e ++ ")"
                               _ -> error $ "cannot call " ++ (show s) ++ " as a function"
transCall p c (CALL "pi" []) = "pi"
transCall p c (CALL s [])  = case (Data.Map.lookup s $ c) of
                               Nothing -> "v"++s -- lambda-bound argument
                               Just ([],SIMPLETYPE "Boolean") -> "v"++s
                               Just ([],SIMPLETYPE "Real")    -> "v"++s
                               Just ([],SIMPLETYPE userType)  -> "v"++s
                               Just ([], t)   -> error $ "transCall error #1"
                               Just (args, t) -> error $ "transCall error #2"

transCall p c (CALL s es) = "(f"++s++" "++unwords (Prelude.map (transExpr p c) es)++")"

-- retrieves the number statements of a user-defined type
numberStmts :: Program -> String -> [Statement]
numberStmts [] s = []
numberStmts (DECSTMT (NUMDECL s' origins e) : p) s = if s == s' 
                                                    then DECSTMT (NUMDECL s' origins e):numberStmts p s 
                                                    else numberStmts p s
numberStmts (x:p) s = numberStmts p s

-- counts the inhabitants of a user-defined type
countType :: Program -> String -> Int
countType (DECSTMT (DNTDECL s' ls) : p) s = (countType p s) + if s' == s 
                                                              then sum $ Prelude.map (abs.snd) ls
                                                              else 0
countType [] s = 0
countType (x:p) s = countType p s

tuplefy :: [String] -> String
tuplefy []  = "()"
tuplefy [t] = t
tuplefy xs  = "(" ++ (intercalate "," xs) ++ ")"

-- maps identifiers to their types
-- (note BLOG models constants as 0-ary functions)
context :: Program -> Map String ([Type],Type)
context [] = fromList []
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
queries ((QRYSTMT e) : p) = e : (queries p)
queries (stmt:p) = queries p
queries [] = []

-- the expressions in obs statements (in source file order)
observations :: Program -> [(Expr,Expr)]
observations ((EVDSTMT (e1,e2)):p) = (e1,e2) : observations p
observations (stmt:p) = observations p
observations [] = []

-- the types of those queried expressions (in source file order)
-- (assumes these expressions take no arguments)
queryTypes :: Program -> [String]
queryTypes p = Prelude.map (typeString.(\e -> snd $ typeIt e (context p))) (queries p)

-- translates a BLOG type into a Haskell one
typeString :: Type -> String
typeString (SIMPLETYPE "Real")    = "Double"
typeString (SIMPLETYPE "Boolean") = "Bool"
typeString (SIMPLETYPE "Integer") = "Int"
typeString (SIMPLETYPE x)         = x
typeString t = error "cannot translate type "++(show t)

-- infers the type of an expression (in a context)
-- assumes the source file type-checks correctly
typeIt :: Expr -> Map String ([Type],Type) -> ([Type],Type)
typeIt (INT n) _       = ([],SIMPLETYPE "Integer")
typeIt (STRING s) _    = ([],SIMPLETYPE "String")
typeIt (CHAR c) _      = ([],SIMPLETYPE "Char")
typeIt (DOUBLE n) _    = ([],SIMPLETYPE "Double")
typeIt (BOOL b) _      = ([],SIMPLETYPE "Boolean")
typeIt BLOGParse.NULL _  = error "why am I type checking null?"
typeIt (CALL "round" _) _  = ([],SIMPLETYPE "Integer")
typeIt (BLOGParse.ID s) c        = let (Just t) = Data.Map.lookup s c in t 
typeIt (BLOGParse.PLUS e1 e2) c  = typeIt e1 c
typeIt (BLOGParse.MINUS e1 e2) c = typeIt e1 c
typeIt (BLOGParse.MULT e1 e2) c  = typeIt e1 c
typeIt (BLOGParse.DIV e1 e2) c   = typeIt e1 c
typeIt (BLOGParse.MOD e1 e2) c   = typeIt e1 c
typeIt (BLOGParse.POWER e1 e2) _ = ([],SIMPLETYPE "Real")
typeIt (BLOGParse.LT e1 e2) _    = ([],SIMPLETYPE "Boolean")
typeIt (BLOGParse.GT e1 e2) _    = ([],SIMPLETYPE "Boolean")
typeIt (BLOGParse.LEQ e1 e2) _   = ([],SIMPLETYPE "Boolean")
typeIt (BLOGParse.GEQ e1 e2) _   = ([],SIMPLETYPE "Boolean")
typeIt (BLOGParse.EQEQ e1 e2) _  = ([],SIMPLETYPE "Boolean")
typeIt (BLOGParse.NEQ e1 e2) _   = ([],SIMPLETYPE "Boolean")
typeIt (BLOGParse.AND e1 e2) _   = ([],SIMPLETYPE "Boolean")
typeIt (BLOGParse.OR e1 e2) _    = ([],SIMPLETYPE "Boolean")
typeIt (IMPLIES e1 e2) _ = ([],SIMPLETYPE "Boolean")
typeIt (CALL "size" _) _ = ([],SIMPLETYPE "Integer")
typeIt (APPLY (CALL s []) (INT n)) c  = let l = Data.Map.lookup s c in
                                          if isNothing l 
                                          then error $ "no user-declared value "++s
                                          else let Just (_,t) = l in ([],t)
typeIt (NEG e1) m       = typeIt e1 m
typeIt (BLOGParse.NOT e1) _ = ([],SIMPLETYPE "Boolean")
typeIt (BLOGParse.AT e1) _  = error "Timesteps are out-of-scope for this translator"
typeIt (IFELSE e1 e2 e3) c = typeIt e2 c
typeIt (IFTHEN e1 e2) _  = error "IFTHEN not implemented"
typeIt (CALL s es) c     = let l = Data.Map.lookup s c in 
                             if isNothing l
                             then error $ s++" not found in context" 
                             else let (Just (argTypes, returnType)) = l in ([],returnType)
typeIt (MAPCONSTRUCT e1e2s) _   = error "Maps cannot be typed"
typeIt (COMPREHENSION es tss e) _ = error "Comprehensions cannot be typed"
typeIt (BLOGParse.EXISTS t s e) _ = ([],SIMPLETYPE "Boolean")
typeIt (BLOGParse.FORALL t s e) _ = ([],SIMPLETYPE "Boolean")
typeIt x _ = error $ "I dont know how to type" ++ show x

-- main method
mainPart :: Program -> [String]
mainPart p = ["main :: IO ()","main = do"] ++ Prelude.map ("    "++) ([
              "putStrLn \"Using a fixed random seed for repeatability.\"",
              "putStrLn \"............................................\"",
              "putStrLn \"Constructing inference engine\"",
              "putStrLn \"Constructing sampler of type Int -> [Maybe a] -> ([a],Int)\"",
              "putStrLn \"Evidence: ["++(escapeQuotes $ (intercalate ", " $ Prelude.map showObs $ observations p))++"]\"",
              "putStrLn \"Query: " ++ (escapeQuotes (show $ queries p)) ++ "\"",
              "putStrLn \"Running for 10000 samples...\"\n",
              "putStrLn \"Query reporting interval is 1000\"",
              "putStrLn \"Samples done: 1000.   Time elapsed: ??? s.\"",
              "putStrLn \"Samples done: 2000.   Time elapsed: ??? s.\"",
              "putStrLn \"Samples done: 3000.   Time elapsed: ??? s.\"",
              "putStrLn \"Samples done: 4000.   Time elapsed: ??? s.\"",
              "putStrLn \"Samples done: 5000.   Time elapsed: ??? s.\"",
              "putStrLn \"Samples done: 6000.   Time elapsed: ??? s.\"",
              "putStrLn \"Samples done: 7000.   Time elapsed: ??? s.\"",
              "putStrLn \"Samples done: 8000.   Time elapsed: ??? s.\"",
              "putStrLn \"Samples done: 9000.   Time elapsed: ??? s.\"",
              "putStrLn \"Samples done: 10000.   Time elapsed: ??? s.\"",
              "-- establish random seed",
              "let tree = randomTree (mkStdGen 0)\n",
              "-- loop to yield data points",
              "let (answers,rejections) = (rejectionSampler 10000) $ runProb (sequence (repeat model)) tree\n",
              "let consistentWorlds = 10000 / (fromIntegral (10000 + rejections))",
              "putStrLn \"=== Rejection Sampler Trial Stats ===\"",
              "putStrLn $ \"Fraction of worlds accepted (this trial): \" ++ show consistentWorlds",
              "putStrLn \"======== Query Results =========\"",
              "putStrLn \"Number of samples: 10000\""] ++
              results p ++
              ["putStrLn \"======== Done ========\""])
  where showObs (e1,e2) = (show e1) ++ " = " ++ (show e2)

results :: Program -> [String]
results p = concat [[msg++(escapeQuotes $ show (queries p !! (q-1)))++"\"",
                     prettyPrint ++ " (distribution $ map " ++ pull q (length $ queries p) ++ " answers)"] 
                    | q <- [1..length $ queries p]]
  where prettyPrint = "mapM (putStrLn.pretty 10000)"
        msg = "putStrLn \"Distribution of values for "

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
    filename <- getLine
    string <- readFile filename
    putStrLn $ (translator.parser.lexer) string
    
