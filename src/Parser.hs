module Parser where

import Control.Monad (MonadPlus(..))
import GHC.Base (Alternative(..))
import Control.Monad.Trans.State.Lazy (StateT(..))
import Data.Char (ord)

import AST (Com(..), Exp(..))

type Parser a = StateT String [] a

runParser :: Parser a -> String -> [(a, String)]
runParser = runStateT

item :: Parser Char
item = StateT $ \inp ->
    case inp of
        [] -> mzero
        (x:xs) -> [(x, xs)]

sat :: (Char -> Bool) -> Parser Char
sat p = do { x <- item; if (p x) then return x else mzero }

char :: Char -> Parser Char
char x = sat (== x)

digit :: Parser Char
digit = sat (\x -> '0' <= x && x <= '9')

lower :: Parser Char
lower = sat (\x -> 'a' <= x && x <= 'z')

upper :: Parser Char
upper = sat (\x -> 'A' <= x && x <= 'Z')

letter :: Parser Char
letter = lower +++ upper

alphanum :: Parser Char
alphanum = letter +++ digit

word :: Parser String
word = many' letter 

many' :: Parser a -> Parser [a]
many' p = force (ne +++ return [])
    where
        ne = do { x <- p; xs <- many' p; return (x:xs) }

string :: String -> Parser String
string "" = return ""
string (x:xs) = do { _ <- char x; _ <- string xs; return (x:xs) }

ident :: Parser String
ident = do { x <- lower; xs <- many' alphanum; return (x:xs) }

many1 :: Parser a -> Parser [a]
many1 p = do { x <- p; xs <- many' p; return (x:xs) }

nat :: Parser Int
nat = chainl1 (do { x <- digit; return (ord x - ord '0') })
             (return op)
    where
        x `op` y = x * 10 + y

int :: Parser Int
int = do { _ <- char '-'; n <- nat; return (-n) } +++ nat

bracket :: Parser a -> Parser b -> Parser c -> Parser b
bracket open p close = do { _ <- open; x <- p; _ <- close; return x }

chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
p `chainl1` op = p >>= rest
  where
    rest x = do { f <- op; y <- p; rest (f x y) }
         +++ return x

chainr1 :: Parser a -> Parser (a -> a -> a) -> Parser a
p `chainr1` op = do { x <- p
                    ; (do { f <- op
                          ; y <- p `chainr1` op
                          ; return (f x y) }) +++ return x }

ops :: [(Parser a, b)] -> Parser b
ops xs = foldr1 (+++) [do { _ <- p; return x } | (p, x) <- xs]

chainl :: Parser a -> Parser (a -> a -> a) -> a -> Parser a
chainl p op v = (p `chainl1` op) +++ return v

chainr :: Parser a -> Parser (a -> a -> a) -> a -> Parser a
chainr p op v = (p `chainr1` op) +++ return v

force :: Parser a -> Parser a
force p =
  StateT $ \inp ->
    let x = runStateT p inp
    in (fst (head x), snd (head x)) : tail x

first :: Parser a -> Parser a
first p = 
  StateT $ \inp ->
    case runStateT p inp of
    [] -> []
    (x:_) -> [x]

(+++) :: Parser a -> Parser a -> Parser a
p +++ q = first (p <|> q)

spaces :: Parser ()
spaces = do { _ <- many1 (sat isSpace); return () }
  where
    isSpace x = x == ' ' || x == '\n' || x == '\t' || x == '\r'

junk :: Parser ()
junk = many' spaces *> return ()

parse :: Parser a -> Parser a
parse = (*>) junk

token :: Parser a -> Parser a
token = flip (<*) junk

integer :: Parser Int
integer = token int

symbol :: String -> Parser String
symbol xs = token (string xs)

identifier :: [String] -> Parser String
identifier ks = token (do { x <- ident; if notElem x ks then return x else mzero })

factor :: Parser Exp
factor = var +++ constant +++ paren expr

term :: Parser Exp
term = factor `chainl1` mulop

expr :: Parser Exp
expr = term `chainl1` addop

rexp :: Parser Exp
rexp = expr `chainl1` relop

addop :: Parser (Exp -> Exp -> Exp)
addop = ops [ (symbol "+", Plus)
            , (symbol "-", Minus)
            ]

mulop :: Parser (Exp -> Exp -> Exp)
mulop = ops [ (symbol "*", Times)
            , (symbol "/", Div)
            ]

relop :: Parser (Exp -> Exp -> Exp)
relop = ops [ (symbol ">", Greater)
            , (symbol "<", Smaller)
            , (symbol ">=", GreaterOrEq)
            , (symbol "<=", SmallerOrEq)
            , (symbol "==", Eq)
            , (symbol "!=", NotEq)
            ]

com :: Parser Com
com = assign +++ seqv +++ cond +++ while +++ decl +++ printe

assign :: Parser Com
assign = do
  x <- variable
  _ <- symbol ":="
  e <- expr

  return $ Assign x e

decl :: Parser Com
decl = do
  _ <- symbol "declare"
  x <- variable
  _ <- symbol "="
  v <- rexp
  _ <- symbol "in"
  body <- com

  return $ Declare x v body
  

seqv :: Parser Com
seqv = bracket (symbol "{")
               (Seq <$> com <*> (symbol ";" *> com))
               (symbol "}")

cond :: Parser Com
cond = do
  _ <- symbol "if"
  e <- rexp
  _ <- symbol "then"
  b <- com
  _ <- symbol "else"
  b' <- com

  return $ Cond e b b'

while :: Parser Com
while = do
  _ <- symbol "while"
  e <- rexp
  _ <- symbol "do"
  body <- com

  return $ While e body

printe :: Parser Com
printe = Print <$> (symbol "print" *> rexp)

constant :: Parser Exp
constant = Constant <$> integer

var :: Parser Exp
var = Variable <$> variable

paren :: Parser a -> Parser a
paren p = bracket (symbol "(") p (symbol ")")

variable :: Parser String
variable = identifier ["if", "then", "else", "declare", "in", "while", "do", "print"]

