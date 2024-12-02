-------------------------------------------------------------------------------------
-- PRACTICA 2
-------------------------------------------------------------------------------------
first :: (a, b) -> a
first (x, y) = x

apply :: (a -> b) -> (a -> b)
apply f = g
    where g x = f x

twice :: (a -> a) -> (a -> a)
twice f = g
    where g x = f (f x)

doble :: Int -> Int
doble x = x + x

swap :: (a, b) -> (b, a)
swap (x, y) = (y, x)

uflip :: ((b, a) -> c) -> (a, b) -> c
uflip f = g
    where g p = f (swap p)

-------------------------------------------------------------------------------------
-- PRACTICA 3
-------------------------------------------------------------------------------------
curry f a b = f (a, b)
uncurry f (a, b) = f a b

suma x = \y -> x + y
-- otras
appDup f = g where g x = f (x, x)
appFork (f, g) = h where h x = (f x, g x)
appPar (f, g) = h where h (x, y) = (f x, g y)
appDist f = g where g (x, y) = (f x, f y)
subst f = h where h g = k where k x = (f x) (g x)

lambdaDup = \f -> \x -> f(x, x)
lambdaFork = \pf -> \x -> (first pf x, first (swap pf) x)
lambdaPar = \pf -> \px -> (first pf (first px), first (swap pf) (first (swap px)))
lambdaDist = \f -> \px -> (f (first px), f (first (swap px)))
lambdaSubst = \f -> \g -> \x -> f x (g x)

-- ej. 7
many :: Int -> (a -> a) -> a -> a
many 0 f x = identidad x
many n f x = compose f (many (n-1) f) x

identidad x = x
compose = \f -> (\g -> (\x -> f (g x)))

-- ej. 9
cuadruple = doble . doble 
fourTimes = many 4
timesTwoPlusThree = (suma 3) . doble

-------------------------------------------------------------------------------------
-- PRACTICA 5
-------------------------------------------------------------------------------------
data Gusto = Chocolate | DDL | Frutilla | Sambayon
data Helado = Vasito Gusto
    | Cucurucho Gusto Gusto
    | Pote Gusto Gusto Gusto
chocoHelate consH = consH Chocolate

-- Vasito :: gusto -> helado
-- Chocolate :: gusto
-- Cucurucho :: gusto -> gusto -> helado
-- Sambayon :: gusto
-- Pote :: g -> g -> g -> h
-- chocoHelate :: (g -> a) -> a
-- chocoHelate Vasito :: helado
-- chocoHelate Cucurucho :: gusto -> helado
-- chocoHelate (Cucurucho Sambayon) :: helado
-- chocoHelate (chocoHelate Cucurucho) :: helado
-- chocoHelate (Vasito DulceDeLeche) :: BOOM!
-- chocoHelate Pote :: g -> g -> h
-- chocoHelate (chocoHelate (Pote Frutilla)) :: helado

data DigBin = O | I
dbAsInt :: DigBin -> Int
dbAsInt O = 0
dbAsInt I = 1

dbAsBool O = False
dbAsBool I = True

dbOfBool False = O
dbOfBool True = I

negDB O = I
negDB I = O

data DigDec = D0 | D1 | D2 | D3 | D4 | D5 | D6 | D7 | D8 | D9
ddAsInt :: DigDec -> Int
ddAsInt D0 = 0
ddAsInt D1 = 1
ddAsInt D2 = 2
ddAsInt D3 = 3
ddAsInt D4 = 4
ddAsInt D5 = 5
ddAsInt D6 = 6
ddAsInt D7 = 7
ddAsInt D8 = 8
ddAsInt D9 = 9

ddOfInt :: Int -> DigDec
ddOfInt 0 = D0
ddOfInt 1 = D1
ddOfInt 2 = D2
ddOfInt 3 = D3
ddOfInt 4 = D4
ddOfInt 5 = D5
ddOfInt 6 = D6
ddOfInt 7 = D7
ddOfInt 8 = D8
ddOfInt 9 = D9

-- ej 5.
data Shape = Circle Float | Rect Float Float
construyeShNormal :: (Float -> Shape) -> Shape
construyeShNormal c = c 1.0

-- uncurry Rect :: (Float, Float) -> Shape
-- construyeShNormal (flip Rect 5.0) :: Shape
-- compose (uncurry Rect) swap :: (Float, Float) -> Shape
-- uncurry Cucurucho :: (Gusto, Gusto) -> Helado
-- uncurry Rect swap :: BOOM!
-- compose uncurry Pote :: Gusto -> (Gusto, Gusto) -> Helado
-- compose Just :: 
-- compose uncurry (Pote Chocolate) :: preguntar