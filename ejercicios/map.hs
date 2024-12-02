-- representación de conjuntos como una función
-- ???
type Map k v = (k -> Maybe v)

mapBL = (\n -> if (n == 8) then (Just "Bachira") else Nothing)

lookupM :: Eq k => k -> Map k v -> Maybe v
lookupM k m = m k
emptyM :: Map k v
emptyM = \_ -> Nothing
assocM :: Eq k => k -> v -> Map k v -> Map k v
assocM k v m = (\x -> if (x == k) then (Just v) else m x)
deleteM :: Eq k => k -> Map k v -> Map k v
deleteM k m = (\x -> if (x == k) then (Nothing) else m x)

-- ¿se puede? justificar
-- keys :: Map k v -> [k]