-- representación de conjuntos como una función
-- la función que responde si un elemento está en el conjunto (función característica)
type Set a = (a -> Bool)
conjuntoPares = \x -> x `mod` 2 == 0
multiplosTres = \x -> x `mod` 3 == 0

-- interfaz
belongs :: Eq a => a -> Set a -> Bool
belongs x s = s x

-- empty :: Set a
-- empty = \x -> False

-- universe :: Set a
-- universe = \x -> True

add :: Eq a => a -> Set a -> Set a
add e s = \x -> e == x || s x
union :: Set a -> Set a -> Set a
union s1 s2 = \x -> s1 x || s2 x
intersection :: Set a -> Set a -> Set a
intersection s1 s2 = \x -> s1 x && s2 x
complement :: Set a -> Set a
complement s = \x -> not (s x)

-- ¿se puede? justificar
size :: Set a -> Int