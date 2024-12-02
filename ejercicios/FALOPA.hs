ii. reverse . inOrder = inOrder . mirrorT

ppio de ext.
        (reverse . inOrder) t = (inOrder . mirrorT) t

caso base, t = EmptyT

LI:

  (reverse . inOrder) EmptyT
=                                   (def compose)
  reverse (inOrder EmptyT)
=                                   (def inOrder.1)
  reverse []
=                                   (def reverse.1)
  []


LD:
  (inOrder . mirrorT) EmptyT
=                                   (def compose)
  inOrder (mirrorT EmptyT)
=                                   (def mirrorT.1)
  inOrder EmptyT
=                                   (def inOrder)
  []


Caso ind., t = NodeT x ti td

HI.1: � (reverse . inOrder) ti = (inOrder . mirrorT) ti !
HI.2: � (reverse . inOrder) td = (inOrder . mirrorT) td !
TI:   � (reverse . inOrder) (NodeT x ti td) = (inOrder . mirrorT) (NodeT x ti td) !


LI:
  (reverse . inOrder) (NodeT x ti td) 
=                                               (def compose)
  reverse (inOrder (NodeT x ti td))
=                                               (def inOrder.2)
  reverse (inOrder ti ++ [x] ++ inOrder td)
=                                               ++ unitario == (:)
  reverse (inOrder t1 ++ (x:(inOrder td)))
=                                               lema
  reverse (x:(inOrder td)) ++ reverse (inOrder ti)
=                                               def. reverse
  reverse (inOrder td) ++ [x] ++ reverse (inOrder ti)
=                                               por H1, H2
  (inOrder . mirrorT) td ++ [x] ++ (inOrder . mirrorT) ti
=
  inOrder (mirrorT td) ++ [x] ++ inOrder (mirrorT ti)


LD:
  (inOrder . mirrorT) (NodeT x ti td)
=                                                     (def compose)
  inOrder (mirrorT (NodeT x ti td))
=                                                     (def mirrorT.2)
  inOrder (NodeT x (mirrorT td) (mirrorT ti))
=                                                     (def inOrder.2)
  inOrder (mirrorT td) ++ [x] ++ inOrder (mirrorT ti)

-- LEMA reverse (l1 ++ l2) = reverse l2 ++ reverse l1
ppio. ind sobre [a]

-- CB; l1 = []
-- LI
reverse ([] ++ l2)
=
reverse l2

-- LD
reverse l2 ++ reverse []
=
reverse l2 ++ []
=
reverse l2


-- CI
-- LI
reverse ((x:xs) ++ l2)
=
reverse (xs ++ l2) ++ [x]

-- LD
reverse l2 ++ reverse ((x:xs))
=
reverse l2 ++ reverse xs ++ [x]