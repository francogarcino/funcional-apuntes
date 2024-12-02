data BST23 a = Cero
    | Dos Int Int (BST23 a) a (BST23 a)
    | Tres Int Int (BST23 a) a (BST23 a) a (BST23 a)

-- INVARIANTES:
-- en t = Dos h n t1 x t2
    -- h es la altura de t
    -- n es el size
    -- t1 y t2 tienen la misma altura
    -- todos los e de t1 son < x
    -- todos los e de t2 son > x
    -- t1 y t2 cumplen con los inv.
-- en t = Tres h n t1 x t2 y t3
    -- h es la altura de t
    -- n es el size
    -- t1, t2 y t3 tienen la misma altura
    -- todos los elementos de t1 son < x
    -- todos los de t2 son > x; < y (entre x e y)
    -- x < y
    -- t1, t2 y t3 cumplen con los inv.

-- def. fold23 y rec23
fold23 :: (Int -> Int -> a -> b -> b -> b) -> 
    (Int -> Int -> a -> a -> b -> b -> b -> b) -> 
    b -> BST23 a -> b
fold23 fd ft b (Cero)                  = b
fold23 fd ft b (Dos h s t1 x t2)       = fd h s x (fold23 fd ft b t1) (fold23 fd ft b t2)
fold23 fd ft b (Tres h s t1 x t2 y t3) = ft h s x y (fold23 fd ft b t1) (fold23 fd ft b t2) (fold23 fd ft b t3)

rec23 :: (Int -> Int -> a -> b -> BST23 a -> b -> BST23 a -> b) -> 
    (Int -> Int -> a -> a -> b -> BST23 a -> b -> BST23 a -> b -> BST23 a -> b) -> 
    b -> BST23 a -> b
rec23 fd ft b (Cero)                  = b
rec23 fd ft b (Dos h s t1 x t2)       = fd h s x (rec23 fd ft b t1) t1 (rec23 fd ft b t2) t2
rec23 fd ft b (Tres h s t1 x t2 y t3) = ft h s x y (rec23 fd ft b t1) t1 (rec23 fd ft b t2) t2 (rec23 fd ft b t3) t3

-- Definir sin explicitas
inOrder :: BST23 a -> [a]
inOrder = fold23 (\_ _ e r1 r2 -> r1 ++ e:r2)
                 (\_ _ e1 e2 r1 r2 r3 -> r1 ++ e1:r2 ++ e2:r3)
                 []
cantElem :: BST23 a -> Int
cantElem = fold23 (\_ _ _ r1 r2 -> 1 + r1 + r2) 
                  (\_ _ _ _ r1 r2 r3 -> 2 + r1 + r2 + r3)
                  0
height :: BST23 a -> Int
height = fold23 (\_ _ _ r1 r2 -> 1 + max r1 r2) 
                (\_ _ _ _ r1 r2 r3 -> 1 + maximum [r1, r2, r3])
                0
minElem :: BST23 a -> a
minElem = rec23 (\_ _ e1 r1 t1 _ _ -> case t1 of
                                        Cero -> e1 
                                        _ -> r1) 
                (\_ _ e1 _ r1 t1 _ _ _ _ -> case t1 of
                                                Cero -> e1 
                                                _ -> r1) 
                (error "404 min")
maxElem :: BST23 a -> a
maxElem = rec23 (\_ _ e _ _ r2 t2 -> case t2 of
                                        Cero -> e 
                                        _ -> r2) 
                (\_ _ _ e _ _ _ _ r3 t3 -> case t3 of
                                                Cero -> e
                                                _ -> r3) 
                (error "404 max")
search :: Ord a => a -> BST23 a -> Bool
search x = fold23 (\h s e r1 r2 -> e == x || if x < e then r1 else r2)
                  (\h s e1 e2 r1 r2 r3 -> e1 == x || e2 == x || 
                    if x < e1 then r1
                    else if x < e2 then r2
                    else r3
                  )
                  False

insert :: Ord a => a -> BST23 a -> BST23 a
insert x = fold23 (\h s e r1 r2 -> if x == e then (Dos h s x r1 r2) 
                                else if x < e then 
                                              else ) 
                  (\h s e1 e2 r1 r2 r3 -> ) 
                  (Dos 1 1 Cero x Cero)

ejemplo23 :: BST23 Int
ejemplo23 = Tres 0 0 (Dos 0 0 (Dos 0 0 Cero 1 Cero) 3 (Dos 0 0 Cero 6 (Dos 0 0 Cero 7 Cero))) 8 Cero 10 (Dos 0 0 Cero 12 Cero)