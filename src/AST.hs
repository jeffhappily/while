module AST where

data Exp
    = Constant Int
    | Variable String
    | Plus Exp Exp
    | Minus Exp Exp
    | Greater Exp Exp
    | Smaller Exp Exp
    | Eq Exp Exp
    | NotEq Exp Exp
    | GreaterOrEq Exp Exp
    | SmallerOrEq Exp Exp
    | Times Exp Exp
    | Div Exp Exp
    deriving Show

data Com
    = Assign String Exp
    | Seq Com Com
    | Cond Exp Com Com
    | While Exp Com
    | Declare String Exp Com
    | Print Exp
    deriving Show

type Location = Int
type Index = [String]
type Stack = [Int]

position :: String -> Index -> Location
position name index =
    let
        pos _ []       = -1
        pos n (nm:nms) = if name == nm
                            then n
                            else pos (n+1) nms
    in pos 1 index

fetch :: Location -> Stack -> Int
fetch _ []     = -1
fetch n (v:vs) = if n == 1 then v else fetch (n-1) vs

put :: Location -> Int -> Stack -> Stack
put _ x []     = [x]
put n x (v:vs) = if n == 1
                --­­ if the replacement of the head is required
                    then x:vs
                    -- the old stack with a replaced head is returned
                    else v:(put (n-1) x vs)
                    -- otherwise we must put the value in the list's tail

newtype M a = StOut { unStOut :: (Stack -> (a, Stack, String)) }

instance Functor M where
    fmap f (StOut g) = StOut $ \s -> let (a, stack, str) = g s in (f a, stack, str)

instance Applicative M where
    pure = return

    f <*> v = do
      f' <- f
      f' <$> v

instance Monad M where
  return x = StOut $ \n -> (x,n, "")
  e >>= f = StOut $ \n ->
    let
        (a,n1,s1) = (unStOut e) n
        (b,n2,s2) = unStOut (f a) n1
    in (b,n2,s1++s2)

getfrom :: Location -> M Int
getfrom i = StOut $ \ns -> (fetch i ns, ns, "")

write :: Location -> Int -> M ()
write i v = StOut $ \ns -> ( (), put i v ns, "")

push :: Int -> M ()
push x = StOut $ \ns -> ((), x:ns, "")

-- ­­ Please note the simultaneous effects of the function from the
-- ­­ monadic  capsule: the returned value is the  0­tuple () and the
-- ­­ stack have just lost it's head.
pop :: M ()
pop = StOut $ \m ->
    let ns = case m of
                (_:xs) -> xs
                _      -> [] 
    in (() , ns ,"")

output :: Show a => a -> M ()
output v = StOut $ \n -> ((), n, show v)

eval1 :: Exp -> Index -> M Int
eval1 expr index = 
    case expr of
        Constant n ->  return n
        Variable x ->  let loc = position x index
                       in getfrom loc
        Plus x y ->    do { a <- eval1 x index ;
                            b <- eval1 y index ;
                            return (a + b) }
        Minus x y ->   do { a <- eval1 x index ;
                            b <- eval1 y index ;
                            return (a - b) }
        Greater x y -> do { a <- eval1 x index ;
                            b <- eval1 y index ;
                            return (if a > b
                                    then 1
                                    else 0) }
        Smaller x y -> do { a <- eval1 x index ;
                            b <- eval1 y index ;
                            return (if a < b
                                    then 1
                                    else 0) }
        Eq x y ->      do { a <- eval1 x index ;
                            b <- eval1 y index ;
                            return (if a == b
                                    then 1
                                    else 0) }
        NotEq x y ->   do { a <- eval1 x index ;
                            b <- eval1 y index ;
                            return (if a /= b
                                    then 1
                                    else 0) }
        GreaterOrEq x y -> do { a <- eval1 x index ;
                                b <- eval1 y index ;
                                return (if a >= b
                                        then 1
                                        else 0) }
        SmallerOrEq x y -> do { a <- eval1 x index ;
                                b <- eval1 y index ;
                                return (if a <= b
                                        then 1
                                        else 0) }
        Times x y ->   do { a <- eval1 x index ;
                            b <- eval1 y index ;
                            return ( a * b )  }
        Div x y ->     do { a <- eval1 x index ;
                            b <- eval1 y index ;
                            return ( a `div` b )  }

interpret1 :: Com -> Index -> M ()
interpret1 stmt index = case stmt of
    Assign name e -> let loc = position name index
                     in do { v <- eval1 e index ;
                             write loc v }
    Seq s1 s2 -> do { _ <- interpret1 s1 index ;
                      _ <- interpret1 s2 index ;
                      return () }
    Cond e s1 s2 -> do { x <- eval1 e index ;
                         if x == 1
                            then interpret1 s1 index
                            else interpret1 s2 index }
    While e b -> let loop () = do { v <- eval1 e index ;
                                    if v == 0 
                                        then return ()
                                        else do { interpret1 b index ;
                                                  loop () } }
                 in loop ()
    Declare nm e stmt' -> do { v <- eval1 e index ;
                              push v ;
                              interpret1 stmt' (nm:index) ;
                              pop }
    Print e -> do { v <- eval1 e index ;
                    output v }

s1' :: Com
s1' = Declare "x" (Constant 150)
       (Declare "y" (Constant 200)
          (Seq (While (Greater (Variable "x") (Constant 0)
                      )
                      (Seq (Assign "x" (Minus (Variable "x")
                                              (Constant 1)
                                       )
                           )
                           (Assign "y" (Minus (Variable "y")
                                              (Constant 1)
                                       )
                           )
                      )
               )
               (Print (Variable "y"))
          )
       )

test :: Exp -> (Int, Stack, String)
test a = unStOut (eval1 a []) []

interp :: Com -> ((), Stack, String)
interp a = unStOut (interpret1 a []) []
