# :curry: ¿Currificación? :basketball:
---

## Partamos de un ejemplo

```hs
mult :: Int -> Int -> Int
mult a b = a * b
```

Si tuvieramos que analizar este código:
- ¿Cuantas funciones se estan definiendo?
    - ¿Cuantos parametros tiene/n? 
    - ¿Que retorna la función/es?

Un acercamiento como el visto en Estructuras nos haria decir que:
- Se esta definiendo **unicamente** una sola función, la cual es `mult`
    - La cual tiene 2 parametros, que son dos numeros
    - y retorna otro numero

Entonces, si tenemos que describir el código diriamos que:

```hs
-- mult es una función que toma 2 numeros y retorna otro numero
-- y acá SOLO definimos mult
mult :: Int -> Int -> Int
mult a b = a * b
```

---

## ¿...es verdad...?

Omitamos tooooda la parte teorica donde conocemos a Haskell Curry _(el matematico del video)_ y vayamos a lo importante.

#### ¿Que es la currificación?

De manera formal, es una relacion uno a uno entre funciones **equivalentes** donde una toma **un par de valores** como parametros y la otra toma esos parametros por separados.

Vamos en español ahora, planteemos `mult` y su alternativa:

```hs
mult :: Int -> Int -> Int
mult a b = a * b

multPar :: (Int, Int) -> Int
multPar (a, b) = a * b
```

En este código, vemos que `mult` y `multPar` hacen lo mismo (multiplicar a por b) pero reciben esos valores de maneras formas:

- `multPar` toma **un par** de numeros y retorna otro numero y vemos que definimos **una sola función**
    - ¿Donde? En las flechas:
    
    ```hs
    --                   (aca)
    multPar :: (Int, Int) -> Int
    multPar (a, b) = a * b
    ```

#### ¿Y en `mult`? Bueno...

- `mult` es una función que toma **UN numero** y retorna **otra función** que toma un numero y retorna otro numero
    - **¡¿COMO?! ¡¿DOS FUNCIONES?! ¡¿DONDE?!** De nuevo, en las flechas...

    ```hs
    --            (aca)  (y aca)
    mult :: Int -> Int -> Int
    mult a b = a * b
    ```

De cierto modo, cada flecha en el tipado de una función nos indica que se esta definiendo una función en si. Por lo que en `mult` estamos definiendo 2 funciones como tal:
- La primera, donde le pasamos 2 numeros (como en Estructuras) y nos retorna un numero
    - Es decir:
    ```hs
    -- (2)    (5)
    -- Int -> Int -> Int
    mult 2 5 -- ie., 10 :: Int
    ```
- Y la segunda, donde solo pasamos el primer numero a `mult`
    - Es decir:
    ```hs
    -- (2)    (?)
    -- Int -> Int -> Int
    mult 2 -- ¿Y esto retorna...?
    ```
    `mult 2` retorna **una función** que toma _"el segundo numero"_ para multiplicar
    ```hs
    -- retorna este parentesis
    -- Int -> (Int -> Int)
    mult 2 -- mult 2 :: Int -> Int
    ```
Y ademas, gracias a lo visto en la clase 1 y 2 con las expresiones y el sistema de tipos, tambien definimos
    ```hs
    -- Int -> Int -> Int
    mult -- :: Int -> Int -> Int
    ```
---

## La posta de currificar :curry:
Basandonos en calculo lambda _(spoilers muuuuy a futuro)_, **toda función tiene UN SOLO parametro**. Por lo que decir que mult _"toma dos numeros"_ (como hacemos desde Estructuras) no es correcto.

> Aclaración: Decir que una función cualquiera toma más de un parametro como haciamos en Estructuras es lo que en PF se llama como :croissant: leer en frances :croissant: 

> Pero a terminos practicos, en PF leeremos cada tanto en frances entendiendo que es unicamente por comodidad

#### ¿Y todo esto para que?
Ademas de facilitarlos el refactor y la detección de errores en algunos casos (que no son relevantes para este resumen) la gran ventaja de la currificación es la **aplicación parcial** de las funciones, es decir, aprovechar todas esas funciones que definimos para expresar ideas con funciones que toman ciertos parametros y retornan otras funciones que hacen a nuestra idea. Veamos ejemplos:

- ¿Como escribiriamos una función que calcula el doble de un numero? ¿Y el triple? ¿Y el decuple _(por 10)_?
    - Un enfoque como el de Estructuras seria:
    ```hs
    doble n = n * 2
    triple n = n * 3
    decuple n = n * 10
    ```
    - Pero un enfoque en PF, con currificación nos permite escribir esto
    ```hs
    doble = mult 2 
    triple = mult 3 
    decuple = mult 10 
    ```
    - Recordemos que `mult` aplicado a un solo numero nos devuelve una función de tipo `Int -> Int` ([visto aca](#que-es-la-currificación)), por lo que:
    ```hs
    doble = mult 2    -- esto tipa en Int -> Int
    triple = mult 3   -- esto tipa en Int -> Int
    decuple = mult 10 -- esto tipa en Int -> Int
    ```
    O sea que podemos decir que:
    ```hs
    doble = mult 2    -- toma un numero, y retorna otro (su doble)
    triple = mult 3   -- toma un numero, y retorna otro (su triple)
    decuple = mult 10 -- toma un numero, y retorna otro (su decuple)
    ```

---

## Resumen del resumen:

- Al currificar, podemos tener _'varias funciones'_ al definir una sola _(atención con las ->)_
- Podemos escribir cosas de manera más humana, como decir que calcular el doble es multiplicar por 2, etc...

[:back: Al indice del repo](/README.md)