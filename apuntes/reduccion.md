# :lady_beetle: Reducción :lady_beetle:
---

## Planteemos LA pregunta: ¿Como hace Haskell para ejecutar los programas?

Así como tenemos un aspecto **denotacional** en la materia que nos permite decir si algunas funciones son equivalentes entre si o no, tambien tenemos un aspecto **operacional** donde recaen las partes mecanicas. ¿Y que entra dentro de estas partes mecanicas? La reducción de programas que hace Haskell para ejecutarlos.

---

## Redexes

Antes de meternos en lo complicado, definamos un termino importantisimo para esto: los **Redexes**.

Un Redex (reducible expression) es un lado izquierdo de una función definida que puede reemplazarse por el lado derecho. Poniendo ejemplos:

```hs
doble n = n + n
```

Dado este código, si escribimos `doble 10` podemos reemplazarlo por `10 + 10`, por lo que `doble 10` ES un redex.

```hs
-- doble n esta del lado IZQ
doble n = n + n
        -- Y n + n es el lado DER por el que reemplazamos
```

---

## Reducciones

Ahora si, ¿como hace Haskell para reducir?