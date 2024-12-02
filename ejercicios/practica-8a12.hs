-------------------------------------------------------------------------------------
-- PRACTICA 8
-------------------------------------------------------------------------------------
len :: [a] -> Int
len [] = 0
len (x:xs) = 1 + length xs

sumatoria :: [Int] -> Int
sumatoria [] = 0
sumatoria (x:xs) = x + sum xs

myProd :: [Int] -> Int
myProd [] = 1
myProd (x:xs) = x * product xs

concatenar :: [[a]] -> [a]
concatenar []		= []
concatenar (xs:xss) = xs ++ concat xss

pertenece :: Eq a => a -> [a] -> Bool
pertenece x [] = False
pertenece x (e:es) = x == e || pertenece x es

todos :: (a -> Bool) -> [a] -> Bool
todos f []      = False
todos f (x:xs)  = if null xs
    then f x
    else todos f xs && f x

alguno :: (a -> Bool) -> [a] -> Bool
alguno f []      = False
alguno f (x:xs)  = if null xs
    then f x
    else alguno f xs || f x

cuantos :: (a -> Bool) -> [a] -> Int
cuantos f []      = 0
cuantos f (x:xs)  = if f x then 1 + cuantos f xs else cuantos f xs

subset :: Eq a => [a] -> [a] -> Bool
subset [] ys     = True
subset (x:xs) ys = subset xs ys && pertenece x ys

subsetF :: Eq a => [a] -> [a] -> Bool
subsetF = \s1 s2 -> foldr (\e r -> r && pertenece e s2) True s1

miAppend :: [a] -> [a] -> [a]
miAppend [] ys      = ys
miAppend (x:xs) ys  = x : miAppend xs ys

sever :: [a] -> [a]
sever []     = []
sever (x:xs) = miAppend (sever xs) [x]

ziip :: [a] -> [b] -> [(a,b)]
ziip [] ys         = []
ziip xs []         = []
ziip (x:xs) (y:ys) = (x, y) : zip xs ys

uunzip :: [(a,b)] -> ([a],[b])
uunzip []     = ([], [])
uunzip (p:ps) = (fst p : (fst (uunzip ps)), snd p : (snd (uunzip ps)))

data N = Z | S N
evalN :: N -> Int
evalN Z = 0
evalN (S n) = 1 + evalN n

addN :: N -> N -> N
addN Z m = m
addN (S n) m = S (addN n m)

prodN :: N -> N -> N
prodN Z m = Z
prodN (S n) m = addN m (prodN n m)

data ExpA = Cte Int
    | Suma ExpA ExpA
    | Prod ExpA ExpA

evalExpA :: ExpA -> Int
evalExpA (Cte n) = n
evalExpA (Suma n m) = evalExpA n + evalExpA m
evalExpA (Prod n m) = evalExpA n * evalExpA m

simplificarExpA :: ExpA -> ExpA
simplificarExpA (Suma n m) = case n of
    (Cte 0) -> simplificarExpA m
    _ -> case m of
        (Cte 0) -> simplificarExpA n
        _ -> (Suma (simplificarExpA n) (simplificarExpA m))
simplificarExpA (Prod n m) = case n of
    (Cte 0) -> (Cte 0)
    (Cte 1) -> simplificarExpA m
    _ -> case m of
        (Cte 0) -> (Cte 0)
        (Cte 1) -> simplificarExpA n
        _ -> (Prod (simplificarExpA n) (simplificarExpA m))
simplificarExpA c = c

cantidadDeSumaCero :: ExpA -> Int
cantidadDeSumaCero (Cte n) = 0
cantidadDeSumaCero (Suma n m) = case n of
    (Cte 0) -> 1 + cantidadDeSumaCero m
    _ -> case m of
        (Cte 0) -> 1 + cantidadDeSumaCero n
        _ -> cantidadDeSumaCero n + cantidadDeSumaCero m
cantidadDeSumaCero (Prod n m) = cantidadDeSumaCero n + cantidadDeSumaCero m

masCeroTriple = Suma (Cte 0) (Suma (Cte 0) (Prod (Cte 1) (Suma (Cte 3) (Cte 1))))

-------------------------------------------------------------------------------------
-- PRACTICA 9
-------------------------------------------------------------------------------------

data EA = Const Int | BOp BinOp EA EA
data BinOp = Sum | Mul

evalEA :: EA -> Int
evalEA (Const n) = n
evalEA (BOp op se1 se2) = case op of
    Sum -> evalEA se1 + evalEA se2
    Mul -> evalEA se1 * evalEA se2

ea2ExpA :: EA -> ExpA
ea2ExpA (Const n) = Cte n
ea2ExpA (BOp op se1 se2) = case op of
    Sum -> Suma (ea2ExpA se1) (ea2ExpA se2)
    Mul -> Prod (ea2ExpA se1) (ea2ExpA se2)

expA2ea :: ExpA -> EA
expA2ea (Cte n) = Const n
expA2ea (Suma se1 se2) = BOp Sum (expA2ea se1) (expA2ea se2)
expA2ea (Prod se1 se2) = BOp Mul (expA2ea se1) (expA2ea se2)



data Arbol a b = Hoja b | Nodo a (Arbol a b) (Arbol a b)

cantidadDeHojas :: Arbol a b -> Int
cantidadDeHojas (Hoja _) = 1
cantidadDeHojas (Nodo _ ti td) = cantidadDeHojas ti + cantidadDeHojas td

cantidadDeNodos :: Arbol a b -> Int
cantidadDeNodos (Hoja _) = 0
cantidadDeNodos (Nodo _ ti td) = 1 + cantidadDeNodos ti + cantidadDeNodos td

cantidadDeConstructores :: Arbol a b -> Int
cantidadDeConstructores (Hoja _) = 1
cantidadDeConstructores (Nodo _ ti td) = 1 + cantidadDeConstructores ti + cantidadDeConstructores td

ea2Arbol :: EA -> Arbol BinOp Int
ea2Arbol (Const n) = Hoja n
ea2Arbol (BOp op se1 se2) = Nodo op (ea2Arbol se1) (ea2Arbol se2)



data Tree a = EmptyT | NodeT a (Tree a) (Tree a)

sumarT :: Tree Int -> Int
sumarT EmptyT = 0
sumarT (NodeT n ti td) = n + sumarT ti + sumarT td

sizeT :: Tree a -> Int
sizeT EmptyT = 1
sizeT (NodeT _ ti td) = 1 + sizeT ti + sizeT td

anyT :: (a -> Bool) -> Tree a -> Bool
anyT f EmptyT = False
anyT f (NodeT e ti td) = f e || anyT f ti || anyT f td

countT :: (a -> Bool) -> Tree a -> Int
countT f EmptyT = 0
countT f (NodeT e ti td) = if f e
    then 1 + countT f ti + countT f td
    else countT f ti + countT f td

countLeaves :: Tree a -> Int
countLeaves EmptyT = 1
countLeaves (NodeT e ti td) = countLeaves ti + countLeaves td

heightT :: Tree a -> Int
heightT EmptyT = 1
heightT (NodeT e ti td) = 1 + max (heightT ti) (heightT td)

inOrder :: Tree a -> [a]
inOrder EmptyT = []
inOrder (NodeT e ti td) = (inOrder ti) ++ e:(inOrder td)



data AppList a = Single a | Append (AppList a) (AppList a)
milista = Append (Append (Append (Single "Iancho") (Single "Orne")) (Append (Single "Chiara") (Single "Doug"))) (Append (Single "Franco") (Single "Cande"))
pibesOrga = Append (Single "Doug") (Append (Single "Iancho") (Single "Franco"))
pibasOrga = Append (Single "Chiara") (Append (Single "Orne") (Single "Cande"))

lenAL :: AppList a -> Int
lenAL (Single e) = 1
lenAL (Append l1 l2) = lenAL l1 + lenAL l2

consAL :: a -> AppList a -> AppList a
consAL e l = Append (Single e) l

headAL :: AppList a -> a
headAL (Single e) = e
headAL (Append l1 l2) = headAL l1

tailAL :: AppList a -> AppList a
tailAL (Append (Single _) xs) = xs
tailAL (Append xs ys) = Append (tailAL xs) ys

snocAL :: AppList a -> a -> AppList a
snocAL (Single e) x = Append (Single e) (Single x)
snocAL (Append sl1 sl2) x = Append sl1 (Append sl2 (Single x))

lastAL :: AppList a -> a
lastAL (Single e) = e
lastAL (Append sl1 sl2) = lastAL sl2

initAL :: AppList a -> AppList a
initAL (Append l1 l2) = if esSingle l2 then l1 else Append l1 (initAL l2)

esSingle (Single _) = True
esSingle _ = False

reverseAL :: AppList a -> AppList a
reverseAL (Append l1 l2) = Append (reverseAL l2) (reverseAL l1)
reverseAL l = l

elemAL :: Eq a => a -> AppList a -> Bool
elemAL x (Single e) = e == x
elemAL x (Append l1 l2) = elemAL x l1 || elemAL x l2

appendAL :: AppList a -> AppList a -> AppList a
appendAL l1 l2 = Append l1 l2

appListToList :: AppList a -> [a]
appListToList (Single e) = [e]
appListToList (Append l1 l2) = appListToList l1 ++ appListToList l2


-------------------------------------------------------------------------------------
-- PRACTICA 11 (creo)
-------------------------------------------------------------------------------------
data Pizza = Prepizza | Capa Ingrediente Pizza
    deriving Show
data Ingrediente = Aceitunas Int | Jamon | Queso | Salsa | Cebolla | Tomate
    deriving (Eq, Show)

foldPz f b Prepizza = b
foldPz f b (Capa i p) = f i (foldPz f b p)

cuantasCumplen f = foldPz (\i r -> if f i then 1 + r else r) 0
cumplen' = \f -> foldPz (\i r -> if f i then 1 + r else r) 0
transformada = \f -> foldPz (\i r -> Capa (f i) r) Prepizza
soloSi = \f -> foldPz (\i r -> if (f i) then Capa i r else r) Prepizza
sinLactosa = soloSi (\i -> i /= Queso)
aptaIntolerantes = foldPz (\i r -> r && i /= Queso) True
quesos = foldPz (\i r -> if (i == Queso) then 1 + r else r) 0
dobleAce = foldPz (\i r -> case i of
                            (Aceitunas n) -> Capa (Aceitunas (2*n)) r
                            i -> Capa i r) Prepizza
aceitunas = foldPz (\i r -> case i of
    (Aceitunas n) -> n + r
    _ -> r) 0
cumplen = \f -> foldPz (\i r -> if f i then i:r else r) []

mejoradasEstr (Prepizza) = Prepizza
mejoradasEstr (Capa i p) = case i of
    (Aceitunas n) -> juntarAceitunas n (mejoradasEstr p)
    i -> Capa i (mejoradasEstr p)

mejoradasF = foldPz (\i r -> case i of
    (Aceitunas n) -> juntarAceitunas n r
    i -> Capa i r) Prepizza

mixPizzas Prepizza p = p
mixPizzas (Capa i p) p2 = Capa i (mixPizzas p p2)

mixF = \p1 p2 -> foldPz Capa p2 p1
mixF' = \p1 p2 -> foldPz (\i r -> Capa i r) p2 p1
-- foldPz :: (Ingrediente -> b -> b), y Capa :: Ingrediente -> Pizza -> Pizza
-- b = Pizza
-- p2 es base
-- p1 la pizza a recorrer

primeras 0 _ = Prepizza
primeras _ (Prepizza) = Prepizza
primeras n (Capa i p) = Capa i (primeras (n-1) p)

primerasF :: Int -> Pizza -> Pizza
primerasF n p = primerasF' p n

primerasF' :: Pizza -> Int -> Pizza
primerasF' = foldPz (\i r -> \n -> if n > 0 then Capa i (r (n-1)) else r n) (\_ -> Prepizza)

-- Ejercicio 9.
sumF = foldr (+) 0
lengthF = foldr (\i r -> 1 + r) 0
mapF f = foldr (\i r -> f i : r) []
filterF p = foldr (\i r -> if p i then i : r else r) []
findF p = foldr (\e r -> if p e then Just e else r) Nothing
anyF p = foldr (\e r -> p e || r) False
allF p = foldr (\e r -> p e && r) True
countByF p = lengthF . (filterF p)
partitionF p = foldr (\e r -> if p e then (e:fst r, snd r) else (fst r, e:snd r) ) ([], [])
zipWithF f = foldr (\e r -> \ys -> if null ys then r ys else (f e (head ys)) : r (tail ys)) (\_ -> [])
scanrF f b = foldr (\e r -> f e (head r) : r) [b]
takeWhileF f = foldr (\e r -> if f e then e:r else []) []

takeWFold n xs = takeFlipped xs n
-- takeFlipped :: [a] -> Int -> [a]
takeFlipped = foldr (\e r -> \n -> if n == 0 then r n else e : (r (n-1))) (\n -> [])

drop = flip (foldr (\e r -> \n -> if n == 0 then e : r n else r (n - 1)) (\n -> []))

elemAtF = flip (foldr (\e r -> \n -> if n == 0 then e else r (n-1)) (\n -> error "out of limit"))

-- AUXS
juntarAceitunas n Prepizza = Prepizza
juntarAceitunas n (Capa i p) = case i of
    (Aceitunas m) -> Capa (Aceitunas (n + m)) p
    _ -> Capa (Aceitunas n) (Capa i p)

-------------------------------------------------------------------------------------
-- PRACTICA 12
-------------------------------------------------------------------------------------
foldT :: (a -> b -> b -> b) -> b -> Tree a -> b
foldT fn b EmptyT          = b
foldT fn b (NodeT a ti td) = fn a (foldT fn b ti) (foldT fn b td)

ejemploArbol :: Tree Int
ejemploArbol = NodeT 1
               (NodeT 2
                   EmptyT
                   (NodeT 4 EmptyT EmptyT)
               )
               (NodeT 3
                   (NodeT 5 (NodeT 6 EmptyT EmptyT) EmptyT)
                   EmptyT
               )

mapTF :: (a -> b) -> Tree a -> Tree b
mapTF f = foldT (\e ri rd -> NodeT (f e) ri rd) EmptyT

sumTF :: Tree Int -> Int
sumTF = foldT (\e ri rd -> e + ri + rd) 0

sizeTF :: Tree a -> Int
sizeTF = foldT (\e ri rd -> 1 + ri + rd) 0

heightTF :: Tree a -> Int
heightTF = foldT (\e ri rd -> if ri > rd then ri + 1 else rd + 1) 0

preOrderF :: Tree a -> [a]
preOrderF = foldT (\e ri rd -> [e] ++ ri ++ rd) []

inOrderF :: Tree a -> [a]
inOrderF = foldT (\e ri rd -> ri ++ [e] ++ rd) []

postOrderF :: Tree a -> [a]
postOrderF = foldT (\e ri rd -> ri ++ rd ++ [e]) []

mirrorT :: Tree a -> Tree a
mirrorT = foldT (\e ri rd -> NodeT e rd ri) EmptyT

countByTF :: (a -> Bool) -> Tree a -> Int
countByTF p = foldT (\e ri rd -> if p e then 1 + ri + rd else ri + rd) 0

partitionTF :: (a -> Bool) -> Tree a -> ([a], [a])
partitionTF p = foldT (\e ri rd -> if p e 
    then (e : fst ri ++ fst rd, snd ri ++ snd rd)
    else (fst ri ++ fst rd, e : snd ri ++ snd rd)) ([], [])

appDist f = g where g (x, y) = (f x, f y)

-------------------------------------------------------------------------------------
-- Mapa
data Dir = Izq | Der | Recto
    deriving (Eq, Show)
data Mapa a = Cofre [a]
            | Nada (Mapa a)
            | Bifurcacion [a] (Mapa a) (Mapa a)

myMap = Bifurcacion ["Espada", "Vial", "Monedas"] 
			(
				Cofre ["Monedas", "Botas"]
			)
			(
				Bifurcacion []
				(
					Nada (
						Cofre ["Pergamino", "Pociones"]
					)
				)
				(
					Cofre ["Estoque"]
				)
			)

--			Bifurcacion [3]
--			/		  \
--		Cofre [2]	Bifurcacion []
--					/		  \
--				  Nada		 Cofre [1]
--				   |
--				  Cofre [2]

foldM :: ([a] -> b) -> (b -> b) -> ([a] -> b -> b -> b) -> Mapa a -> b
foldM fc fn fb (Cofre as) = fc as
foldM fc fn fb (Nada m) = fn (foldM fc fn fb m)
foldM fc fn fb (Bifurcacion as mi md) = fb as (foldM fc fn fb mi) (foldM fc fn fb md)

recM :: ([a] -> b) -> (b -> Mapa a -> b) -> ([a] -> b -> b -> Mapa a -> Mapa a -> b) -> Mapa a -> b
recM fc fn fb (Cofre as) = fc as
recM fc fn fb (Nada m) = fn (recM fc fn fb m) m
recM fc fn fb (Bifurcacion as mi md) = fb as (recM fc fn fb mi) (recM fc fn fb md) mi md

objects :: Mapa a -> [a]
objects = foldM (id) (id) (\os ri rd -> os ++ ri ++ rd)

mapM :: (a -> b) -> Mapa a -> Mapa b
mapM f = foldM (\os -> Cofre (map f os)) (id) (\os ri rd -> Bifurcacion (map f os) ri rd)

has :: (a -> Bool) -> Mapa a -> Bool
has f = foldM (\os -> any f os) (\r -> r) (\os ri rd -> any f os || ri || rd)

{-
    has f (Cofre os) = any f os
    has f (Nada m) = has f m
    has f (Bifurcacion os mi md) = any f os || (has f mi) || (has f md)
-}

hasObjectAt :: (a -> Bool) -> Mapa a -> ([Dir] -> Bool)
hasObjectAt f = foldM (\os -> \ds -> if null ds then any f os else False) 
                      (\r -> \ds -> case ds of
                                        (Recto:c) -> r c
                                        _ -> False)
                      (\os ri rd -> \ds -> case ds of
                                        (Izq:c) -> ri c
                                        (Der:c) -> rd c
                                        _ -> False)

longestPath :: Mapa a -> [Dir]
longestPath = foldM (\_ -> []) 
                   (\r -> Recto:r)
                   (\_ ri rd -> if length ri >= length rd then Izq:ri else Der:rd)

objectsOfLongestPath :: Mapa a -> [a]
objectsOfLongestPath m = objectsInPath m (longestPath m)

objectsInPath :: Mapa a -> ([Dir] -> [a])
objectsInPath = foldM (\os -> \_ -> os) 
                      (\r -> \ds -> case ds of
                                        (Recto:c) -> r c
                                        _ -> []) 
                      (\os ri rd -> \ds -> case ds of
                                        (Izq:c) -> os ++ ri c
                                        (Der:c) -> os ++ rd c
                                        _ -> os)

allPaths :: Mapa a -> [[Dir]]
allPaths = foldM (\os -> [[]])
                 (\r -> addAll Recto r)
                 (\os ri rd -> addAll Izq ri ++ addAll Der rd)

addAll :: Dir -> [[Dir]] -> [[Dir]]
addAll d = foldr (\e r -> (d:e) : r) [] -- Idem: map (d:), ya q se añade en todos

objectsPerLevel :: Mapa a -> [[a]]
objectsPerLevel = foldM (\os -> [os])
                        (\r -> []:r)
                        (\os ri rd -> (os : mergeLevels ri rd))

{-
    objectsPerLevel (Cofre os) = os
    objectsPerLevel (Nada m) = [] : objectsPerLevel m
    objectsPerLevel (Bifurcacion os mi md) = os : (mergeLevels (objectsPerLevel mi) (objectsPerLevel md))
-}

mergeLevels :: [[a]] -> [[a]] -> [[a]]
mergeLevels [] ys = ys
mergeLevels xs [] = xs
mergeLevels (x:xs) (y:ys) = (x ++ y) : mergeLevels xs ys

-------------------------------------------------------------------------------------
-- DEMO
-------------------------------------------------------------------------------------
{-
    ¿para todo x. has (==x) = any (elem x) . objectsPerLevel?
                                                        -- por ppio de ext.
    ¿para todo x. para todo m :: Mapa a.
        has (==x) = any (elem x) . objectsPerLevel?
                                                        -- por ppio de ind sobre Mapa a.
    -- Caso base
        has (==x) (Cofre os) = (any (elem x) . objectsPerLevel) (Cofre os)
    -- LI
        has (==x) (Cofre os)
    =                                                   -- has.1
        any (==x) os
    =                                                   -- elem = any . (==) | Guia 8
        elem x os

    -- LD
        (any (elem x) . objectsPerLevel) (Cofre os)
    =                                                   -- (.)
        any (elem x) (objectsPerLevel (Cofre os))
    =                                                   -- objectsPerLevel.1
        any (elem x) [os]
    =                                                   -- : == []
        any (elem x) (os:[])
    =                                                   -- any.2       
        elem x os || any (elem x) []
    =                                                   -- any.1
        elem x os || False
    =                                                   -- (||)
        elem x os


    -- Caso ind.1
        HI) ¡has (==x) m = (any (elem x) . objectsPerLevel) m!
        
        has (==x) (Nada m) = (any (elem x) . objectsPerLevel) (Nada m)
    -- LI
        has (==x) (Nada m)
    =                                                   -- has.2
        has (==x) m

    -- LD
        (any (elem x) . objectsPerLevel) (Nada m)
    =                                                   -- (.)
        any (elem x) (objectsPerLevel (Nada m))
    =                                                   -- objectsPerLevel.2
        any (elem x) ([]:(objectsPerLevel m))
    =                                                   -- any.2
        any (elem x) [] || any (elem x) (objectsPerLevel m)
    =                                                   -- elem con []
        False || any (elem x) (objectsPerLevel m)
    =                                                   -- False ||
        any (elem x) (objectsPerLevel m)
    =                                                   -- HI
        has (==x) m


    -- Caso ind.2
        HI) ¡has (==x) mi = (any (elem x) . objectsPerLevel) mi!
        HI) ¡has (==x) md = (any (elem x) . objectsPerLevel) md!

        has (==x) (Bifurcacion os mi md) = (any (elem x) . objectsPerLevel) (Bifurcacion os mi md)
    -- LI
        has (==x) (Bifurcacion os mi md)
    =                                                   -- has.3
        any (==x) os || (has (==x) mi) || (has (==x) md)
    =                                                   -- HI
        any (==x) os || (any (elem x) . objectsPerLevel) mi || (any (elem x) . objectsPerLevel) md
    =                                                   -- (.)
        any (==x) os || any (elem x) (objectsPerLevel mi) || any (elem x) (objectsPerLevel md)
    =                                                   -- por LEMA: elemx-merge
        any (==x) os || any (elem x) (mergeLevels (objectsPerLevel mi) (objectsPerLevel md))
    =                                                   -- elem = any . (==) | Guia 8
        elem x os || any (elem x) (mergeLevels (objectsPerLevel mi) (objectsPerLevel md))

        {   -- LEMA: elemx-merge
            ¿para toda l1 :: [[a]]. para toda l2 :: [[a]].
                any (elem x) l1 || any (elem x) l2 = any (elem x) (mergeLevels l1 l2)?

                -- por ppio. de ind. sobre [[a]]
                -- CASO BASE; l1 = []
                    ¿any (elem x) [] || any (elem x) l2 = any (elem x) (mergeLevels [] l2)?
                -- LI
                    any (elem x) [] || any (elem x) l2
                =                                       -- por any.1
                    False || any (elem x) l2
                =                                       -- (||)
                    any (elem x) l2

                -- LD
                    any (elem x) (mergeLevels [] l2)
                =                                       -- mergeLevels.1
                    any (elem x) l2

                -------------------------------------------------------------------------------------
                -- CASO IND; l1 = (l:ls)
                    HI) ¡any (elem x) ls || any (elem x) l2 = any (elem x) (mergeLevels ls l2)!
                    ¿any (elem x) (l:ls) || any (elem x) l2 = any (elem x) (mergeLevels (l:ls) l2)?
                -- LI
                    any (elem x) (l:ls) || any (elem x) l2
                =                                       -- any.2
                    (elem x) l || any (elem x) ls || any (elem x) l2
                    
                    -- CASO A: l2 = []
                            (elem x) l || any (elem x) ls || any (elem x) []
                        =                               -- any.1
                            (elem x) l || any (elem x) ls || False
                        =                               -- (||)
                            (elem x) l || any (elem x) ls
                        =                               -- any.2
                            any (elem x) (l:ls)

                    -- CASO B: l2 /= []
                            (elem x) l || any (elem x) ls || any (elem x) (y:ys)        
                    
                -- LD
                    any (elem x) (mergeLevels (l:ls) l2)
                    
                    -- CASO A: l2 = []
                            any (elem x) (mergeLevels (l:ls) [])
                        =                               -- merge.2
                            any (elem x) (l:ls)

                    -- CASO B: l2 /= []
                            any (elem x) (mergeLevels (l:ls) (y:ys))
                        =                               -- merge.3
                            any (elem x) ((l ++ y) : mergeLevels ls ys)
                        =                               -- any
                            (elem x) (l ++ y) || any (elem x) (mergeLevels ls ys)
                        =                               -- por HI)
                            (elem x) (l ++ y) || any (elem x) ls || any (elem x) ys
                        =                               -- prop (++) con (elem x)
                            (elem x) l || (elem x) y || any (elem x) ls || any (elem x) ys
                        =                               -- por asociatividad de (||); por any.2
                            (elem x) l || any (elem x) ls || any (elem x) (y:ys)              
        }

    -- LD
        (any (elem x) . objectsPerLevel) (Bifurcacion os mi md)
    =                                                   -- (.)
        any (elem x) (objectsPerLevel (Bifurcacion os mi md))
    =                                                   -- objectsPerLevel.3
        any (elem x) (os : (mergeLevels (objectsPerLevel mi) (objectsPerLevel md)))
    =                                                   -- any
        elem x os || any (elem x) (mergeLevels (objectsPerLevel mi) (objectsPerLevel md))

    -- any :: (a->Bool) -> [a] -> Bool
    -- any p [] = False
    -- any p (e:es) = p e || any p es
-}
-------------------------------------------------------------------------------------
-- Arbol general
-------------------------------------------------------------------------------------
data GTree a = GNode a [GTree a]

-- a) fold y rec
foldGT :: (a -> [b] -> b) -> GTree a -> b
foldGT f (GNode a ts) = f a (map (foldGT f) ts)

recGT :: (a -> [b] -> [GTree a] -> b) -> GTree a -> b
recGT f (GNode a ts) = f a (map (recGT f) ts) ts

-- b)
mapGT f = foldGT (\a tsb -> GNode (f a) tsb)

sumGT = foldGT (\a ns -> a + sum ns)

sizeGT = foldGT (\_ ss -> 1 + sum ss)

heightGT = foldGT (\_ hs -> 1 + maximum (0:hs))

preOrderGT = foldGT (\a as -> a : foldr (++) [] as)

postOrderGT = foldGT (\a as -> foldr (++) [] (reverse as) ++ [a])

mirrorGT = foldGT (\a ts -> GNode a (reverse ts))

countByGT f = foldGT (\e ns -> if f e then (sum ns) + 1 else sum ns)

partitionGT :: (a -> Bool) -> GTree a -> ([a], [a])
partitionGT f = foldGT (\e ps -> let (tes, fes) = flatPairs ps in if f e then (e:tes, fes) else (tes, e:fes))

flatPairs = foldr (\(xs, ys) (tes, fes) -> (xs ++ tes, ys ++ fes)) ([], [])

-- ejemplo
numbers :: GTree Integer
numbers =
	GNode 1 [
		GNode 3 [GNode 9 [], GNode 15 [], GNode 30 []],
		GNode 7 [GNode 0 [], GNode 21 [], GNode 49 []],
		GNode 10 [GNode 10 [], GNode 20 [GNode 0 []], GNode 100 []]
	]

-------------------------------------------------------------------------------------

type Name = String
type Content = String
type Path = [Name]
data FileSystem = File Name Content
                | Folder Name [FileSystem]

miSO = (Folder "UNQ" [
    (Folder "funcio" [
        (File "demos" "..."), (File "folds" "..."), (File "bottom" "...")
    ]), 
    (Folder "TIP" [
        (File "executors" "..."), (File "parse" "...")
    ]), 
    (Folder "new" []), (File "historia" "casi recibido, falta funcio y TIP")])

foldFS :: (Name -> Content -> b) -> (Name -> [b] -> b) -> FileSystem -> b
foldFS ffile ffolder (File n c)     = ffile n c
foldFS ffile ffolder (Folder n fss) = ffolder n (map (foldFS ffile ffolder) fss)

recFS :: (Name -> Content -> b) -> (Name -> [b] -> [FileSystem] -> b) -> FileSystem -> b
recFS ffile ffolder (File n c)     = ffile n c
recFS ffile ffolder (Folder n fss) = ffolder n (map (recFS ffile ffolder) fss) fss

amountOfFiles :: FileSystem -> Int
amountOfFiles = foldFS (\_ _ -> 1) (\_ ns -> sum ns)

find :: Name -> FileSystem -> Maybe Content
find name = foldFS (\n c -> if n == name then Just c else Nothing)
                   (\_ rs -> foldr (\mb r -> case mb of
                                                (Just c) -> mb
                                                _ -> r) Nothing rs)

-- pathOf :: Name -> FileSystem -> Path

pathWith :: Name -> [Path] -> Path
pathWith n = foldr (\p ps -> if elem n p then p else ps) []

mapContent :: (Content -> Content) -> FileSystem -> FileSystem
mapContent f = foldFS (\n c -> File n (f c)) (\n r -> Folder n r)

-- targetedMapContents :: [(Name, Content -> Content)] -> FileSystem -> FileSystem
