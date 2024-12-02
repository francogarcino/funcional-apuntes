-- demos
¿para todo xs. para todo ys.
    length (xs ++ ys) = length xs + length ys?
ppio inducciones en [a].
Caso Base) xs = []
¿length ([] ++ ys) = length [] + length ys?

    length ([] ++ ys)
=                                       (++).1
    length ys

    length [] + length ys
=                                       length.1
    0 + length ys
=                                       arit.
    length ys

Caso Inductivo)
xs = (z:zs); HI) ¡length (zs ++ ys) = length zs + length ys!
¿length ((z:zs) ++ ys) = length (z:zs) + length ys?

    length (z:zs) + length ys
=                                       length.2
    1 + length zs + length ys
=                                       por HI
    1 + length (zs ++ ys)
=                                       length.2
    length (z:(zs ++ ys))
=                                       (++)
    length ((z:zs) ++ ys)
-----------------------------------------------------------------------------
¿count (const True) = length?
ppio. extensionalidad
¿para todo l :: [a]
    count (const True) l = length l?
ppio inducciones en [a].

Caso Base) l = []
¿count (const True) [] = length []?

    count (const True) []
=                                       count.1
    0
=                                       length.1
    length []

Caso Inductivo)
l = (z:zs); HI) ¡count (const True) zs = length zs!
¿count (const True) (z:zs) = length (z:zs)?

    count (const True) (z:zs)
=                                       count.2
    if (const True) z then 1 + count (const True) zs else count (const True) zs
=                                       lema const True a = True
    1 + count (const True) zs
=                                       HI
    1 + length zs
=                                       length.2
    length (z:zs)

-- LEMA const True a = True
¿para todo a :: a
    const True a = True?

    const True a
=                                       const
    True

-----------------------------------------------------------------------------
elem = any . (==)
ppio de extensionalidad.
¿para todo e :: a. para todo xs :: [a]
    elem e xs = (any . (==)) e xs?
ppio de induccion en [a]

Caso Base) xs = []
¿elem e [] = (any . (==)) e []?

    elem e []
=                                       elem.1
    False

    (any . (==)) e []
=                                       (.)
    any ((==) e) []
=                                       any.1
    False

Caso Inductivo) xs = (z:zs) ¡elem e zs = (any . (==)) e zs!
¿elem e (z:zs) = (any . (==)) e (z:zs)?

    elem e (z:zs)
=                                       elem.2
    e == z || elem e zs

    (any . (==)) e (z:zs)
=                                       (.)
    any ((==) e) (z:zs)
=                                       any.2
    if null zs then (==) e z else any ((==) e) zs || (==) e z

caso a. zs = null
    if null [] then (==) e z else any ((==) e) [] || (==) e z
=                                       if-then
    (==) e z
=                                       ¿ orden ?
    e == z

    e == z || elem e []
=                                       elem.1
    e == z || False
=                                       (||) False
    e == z

caso b. zs != null
    if null zs then (==) e z else any ((==) e) zs || (==) e z
=                                       if-else
    any ((==) e) zs || (==) e z
=                                       (.)
    (any . (==)) e zs || (==) e z
=                                       HI
    elem e zs || (==) e z
=                                       conmutatividad (||) y ¿ orden ?
    e == z || elem e zs




-----------------------------------------------------------------------------
¿evalExpA . simplificarExpA = evalExpA?
ppio de extensionalidad.
¿para todo e :: ExpA. 
    (evalExpA . simplificarExpA) e = evalExpA e?
ppio de induccion sobre ExpA.
Caso Base) ¿(evalExpA . simplificarExpA) (Cte n) = evalExpA (Cte n)?

Caso Inductivo I)
    HI1) ¡(evalExpA . simplificarExpA) se1 = evalExpA se1!
    HI2) ¡(evalExpA . simplificarExpA) se2 = evalExpA se2!
    TI) ¿(evalExpA . simplificarExpA) (Suma se1 se2) = evalExpA (Suma se1 se2)?

Caso Inductivo II)
    HI1) ¡(evalExpA . simplificarExpA) se1 = evalExpA se1!
    HI2) ¡(evalExpA . simplificarExpA) se2 = evalExpA se2!
    TI) ¿(evalExpA . simplificarExpA) (Prod se1 se2) = evalExpA (Prod se1 se2)?

Caso Base) ¿(evalExpA . simplificarExpA) (Cte n) = evalExpA (Cte n)?

    (evalExpA . simplificarExpA) (Cte n)
=                                       def (.)
    evalExpA (simplificarExpA (Cte n))
=                                       simplificarExpA.1
    evalExpA (Cte n) -- ¡CASO BASE DEMOSTRADO!

Caso Inductivo I) ¿(evalExpA . simplificarExpA) (Suma se1 se2) = evalExpA (Suma se1 se2)?

-- Lado Izq
    (evalExpA . simplificarExpA) (Suma se1 se2)
=                                       def (.)
    evalExpA (simplificarExpA (Suma se1 se2))

-- Lado Der
    evalExpA (Suma se1 se2)

caso a. se1 = (Cte 0) -- planteo por casos
-- Lado Izq
    evalExpA (simplificarExpA (Suma (Cte 0) se2))
=                                       simplificarExpA.1.1
    evalExpA (simplificarExpA se2)
=                                       por HI2
    evalExpA se2

-- Lado Der
    evalExpA (Suma (Cte 0) se2)
=                                       evalExpA.2
    evalExpA (Cte 0) + evalExpA se2
=                                       evalExpA.1
    0 + evalExpA se2
=                                       arit
    evalExpA se2

caso b. como a, pero con se2 = (Cte 0)

caso c. se1 y se2 != (Cte 0) -- planteo por casos

    evalExpA (simplificarExpA (Suma se1 se2))
=                                       simplificarExpA.1.3
    evalExpA (Suma (simplificarExpA se1) (simplificarExpA se2))
=                                       evalExpA.2
    evalExpA (simplificarExpA se1) + evalExpA (simplificarExpA se2)
=                                       def (.)
    (evalExpA . simplificarExpA) se1 + evalExpA (simplificarExpA se2)
=                                       def (.)
    (evalExpA . simplificarExpA) se1 + (evalExpA . simplificarExpA) se2
=                                       por HI1
    evalExpA se1 + (evalExpA . simplificarExpA) se2
=                                       por HI2
    evalExpA se1 + evalExpA se2
=                                       evalExpA.2
    evalExpA (Suma se1 se2)
