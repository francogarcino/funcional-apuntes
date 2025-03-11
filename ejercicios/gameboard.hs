data Nim = Empty | Heap Int Nim 

type Move = (Int,Int)
    -- jugada: de la fila i remover k fichas

data GameTree = Nil -- árbol vacío
              | Node (Move, GameTree) -- (jugada, hijos - jugadas del contrincante)
                     GameTree -- hermanos (otras jugadas propias)
p :: Move
p = (0,0)

tree :: GameTree
tree = Node (p, (Node (p, Nil) Nil)) (Node (p, (Node (p, (Node (p, Nil) Nil)) (Node (p, (Node (p, (Node (p, Nil) Nil)) Nil)) Nil))) (Node (p, Nil) Nil))

-- Observar que el GameTree tiene dos casos recursivos, el primero hace referencia al primero de los hijos
-- de la jugada almacenada en el nodo (jugadas del contrincante), mientras que el segundo referencia al primero
-- de los hermanos de la jugada (otras jugadas propias – del jugador actual).

foldNim :: b -> (Int -> b -> b) -> Nim -> b 
foldNim e fn (Empty) = e
foldNim e fn (Heap i n) = fn i (foldNim e fn n)

recNim :: b -> (Int -> b -> Nim -> b) -> Nim -> b
recNim e fn (Empty) = e
recNim e fn (Heap i n) = fn i (recNim e fn n) n

foldGame :: b -> ((Move, b) -> b -> b) -> GameTree -> b
foldGame b fn (Nil) = b
foldGame b fn (Node (m, t) gt) = fn (m, foldGame b fn t) (foldGame b fn gt)

recGame :: b -> ((Move, b) -> GameTree -> b -> GameTree -> b) -> GameTree -> b
recGame b fn (Nil) = b
recGame b fn (Node (m, t) gt) = fn (m, recGame b fn t) t (recGame b fn gt) gt

------------------------------------------------------
heaps :: Nim -> Int
heaps = foldNim 0 (\_ r -> 1 + r)

chips :: Nim -> Int
chips = foldNim 0 (+)

maxHeap :: Nim -> Int
maxHeap = foldNim 0 (max)

alongside :: Nim -> Nim -> Nim
alongside n1 n2 = foldNim n2 (Heap) n1

-- gameHeight' :: GameTree -> Int
-- gameHeight' Nil = 0
-- gameHeight' (Node (_, br) sides) = max (1 + gameHeight br) (gameHeight sides)

gameHeightF :: GameTree -> Int
gameHeightF = foldGame 0 (\(_, rbranch) rside -> (max (rbranch + 1) rside))

-- map (m:) para todos

-- branches :: GameTree -> [[(Int, Int)]]
-- branches Nil                       = 
-- branches (Node (m, otro) posibles) = map (m:) (branches otro) ... (branches posibles)