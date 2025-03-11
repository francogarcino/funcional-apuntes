type Partition a = ([a], [a]) -- tupla de elementos que cumplen o no un criterio respectivamente
data Criteria a b = C (a -> Bool) (a -> b) (a -> b) -- un criterio que, de cumplirse un predicado, aplica la primer funcion; la segunda en caso contrario
data Funnel a b = Initial (Criteria a b) | Step (Criteria a b) (Funnel a b) {-- 
    una estructura linear no vacia que representa los criterios a utilizar que se aplican desde el ultimo: -}
    -- Ej: Step c3 (Step c2 (Initial c1)) aplica primero c1, luego c2, por ultimo c3

partition :: Criteria a b -> ([b] -> b) -> [a] -> (Partition a, [b])
partition (C p f g) h [] = (([], []), [])
partition (C p f g) h (x:xs) = let ((ss, ns), bs) = partition (C p f g) h xs
                                in if p x
                                    then ((x:ss, ns), (f x):bs)
                                    else ((ss, x:ns), (g x):bs)

step :: Criteria a b -> ([b] -> b) -> (Partition a, [b]) -> (Partition a, [b])
step c f ((ss, ns), bs) = let ((ss', ns'), bs') = partition c f ss 
                            in ((ss', ns ++ ns'), f bs' : bs)

composeC :: Criteria a b -> Criteria b c -> Criteria a c
composeC (C p f g) (C q h k) = C (\a -> p a && q (f a)) (h . f) (k . g)

-- EJ 1) Definir estructuralmente
appF :: Funnel a b -> ([b] -> b) -> [a] -> (Partition a, [b])
appF (Initial c) f xs = let (ps, rs) = partition c f xs
                        in (ps, [f rs])
appF (Step c fn) f xs = step c f (appF fn f xs)

complementF :: Funnel a b -> Funnel a b
complementF (Initial c) = Initial (negarC c)
complementF (Step c fn) = Step (negarC c) (complementF fn)

negarC (C p f g) = C (not . p) g f

reverseF :: Funnel a b -> Funnel a b
reverseF (Initial c) = Initial c
reverseF (Step c fn) = snocF c (reverseF fn)

snocF c (Initial c') = Step c' (Initial c)
snocF c (Step c' fn) = Step c' (snocF c fn)

mapF :: (b -> c) -> Funnel a b -> Funnel a c
mapF f (Initial c) = Initial (mergeCriteria f c)
mapF f (Step c fn) = Step (mergeCriteria f c) (mapF f fn)

mergeCriteria :: (b -> c) -> Criteria a b -> Criteria a c
mergeCriteria h (C p f g) = C p (h . f) (h . g)

zipF :: Funnel a b -> Funnel b c -> Funnel a c
zipF (Initial c) (Initial c')  = Initial (composeC c c')
zipF (Initial c) (Step c' fn)  = Initial (composeC c c')
zipF (Step c fn) (Initial c')  = Initial (composeC c c')
zipF (Step c fn) (Step c' fn') = Step (composeC c c') (zipF fn fn')

-- alternativa
zipFST :: Funnel a b -> Funnel b c -> Funnel a c
zipFST (Initial c) fnl = Initial (composeC c (criteria fnl))
zipFST (Step c fn) fnl = case fnl of
                        (Initial c') -> Initial (composeC c c')
                        (Step c' fn') -> Step (composeC c c') (zipFST fn fn')

criteria (Initial c) = c
criteria (Step c fn) = c

-- EJ 2) Demostrar que:
-- ¿para todo fn. para todo f. para todo xs.
--      appF fn f xs = appF (complementF (complementF fn)) f xs?

-- LEMA idempotencia-funnel
-- ¿para todo fn :: Funnel a b.
--      el lema a sacar es que fn = complementF (complementF fn)?

-- EJ 3) Definir foldF y recF, los esquemas sobre Funnel a b
foldF :: (Criteria a b -> c) -> (Criteria a b -> c -> c) -> Funnel a b -> c
foldF fi fs (Initial c) = fi c
foldF fi fs (Step c fn) = fs c (foldF fi fs fn)

recF :: (Criteria a b -> c) -> (Criteria a b -> c -> Funnel a b -> c) -> Funnel a b -> c
recF fi fs (Initial c) = fi c
recF fi fs (Step c fn) = fs c (recF fi fs fn) fn

-- EJ 4) Reescribir el (EJ 1) con los esquemas
partitionFold (C p f g) h = foldr (\x r -> let ((ss, ns), bs) = r
                                in if p x
                                    then ((x:ss, ns), (f x):bs)
                                    else ((ss, x:ns), (g x):bs)) (([], []), [])

appF' fn f xs = foldF (\c -> let (ps, rs) = partition c f xs in (ps, [f rs])) (\c r -> step c f r)
complementF' = foldF (\c -> Initial (negarC c)) (\c r -> Step (negarC c) r)
-- complementF' = foldF (Initial . negarC) (Step . negarC) // tirando magia
reverseF' = foldF (\c -> Initial c) (\c r -> snocF' c r)
-- reverseF' = foldF Initial snocF' // tirando magia
snocF' cr = foldF (\c -> Step c (Initial cr)) (\c r -> Step c (r))
mapF' f = foldF (\c -> Initial (mergeCriteria f c)) (\c r -> Step (mergeCriteria f c) r)
-- mapF' f = foldF (Initial . (mergeCriteria f)) (Step . (mergeCriteria f))
zipF' = foldF (\c -> \fn -> Initial (composeC c (criteria fn))) (\c r -> \fn -> case fn of
                                                                            (Initial c') -> Initial (composeC c c')
                                                                            (Step c' fn') -> Step (composeC c c') (r fn'))