```hs
-- v1
recEnListas :: [a] -> b
recEnListas []     = ... valor ...
recEnListas (x:xs) = ... x ... recEnListas xs ...

-- v1.5
recEnListas :: [a] -> b
recEnListas []     = ... valor ...
recEnListas (x:xs) = let r = recEnListas xs in
                     ... x ... r ...

-- v2
recEnListas :: [a] -> b
recEnListas = foldr (\x r -> ... x ... r ...) valor

```
Esto es la base que hace fold y como surge, ahora vamos a la [Segunda parte: Fold de estructuras](/apuntes/wip.md)

[:back: Al indice del repo](/README.md)