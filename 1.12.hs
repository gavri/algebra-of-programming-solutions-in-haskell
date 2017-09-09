import Test.HUnit
data ConsList a = Nil | Cons a (ConsList a) deriving (Show, Eq)

foldR _ c Nil = c
foldR f c (Cons x xs) = f x (foldR f c xs)

takeR l n = foldR (\x accFunc -> (\n -> if (n > 0) then (Cons x (accFunc (n - 1))) else Nil)) (\_ -> Nil) l n
dropR l n = foldR (\x accFunc -> (\n -> if (n > 0) then accFunc (n - 1) else (Cons x $ accFunc (n - 1)))) (\_ -> Nil) l n

main = runTestTT $ test $ [
  TestLabel "takeR" $ TestList [
  "list length terminal" ~:  (Nil :: ConsList Int) ~=? (takeR Nil 10)
  , "n terminal" ~: Nil ~=? (takeR (Cons 5 (Cons 6 Nil)) 0)
  , "n > list length" ~:  (Cons 5 (Cons 6 Nil)) ~=? (takeR (Cons 5 (Cons 6 Nil)) 10)
  , "n == list length" ~: (Cons 5 (Cons 6 Nil)) ~=? (takeR (Cons 5 (Cons 6 Nil)) 2)
  , "n < list length" ~: (Cons 5 (Cons 6 Nil)) ~=? (takeR (Cons 5 (Cons 6 (Cons 7 Nil))) 2)
  ],
  TestLabel "dropR" $ TestList [
  "list length terminal" ~:  (Nil :: ConsList Int) ~=? (dropR Nil 10)
  , "n terminal" ~: (Cons 5 (Cons 6 Nil)) ~=? (dropR (Cons 5 (Cons 6 Nil)) 0)
  , "n > list length" ~:  Nil ~=? (dropR (Cons 5 (Cons 6 Nil)) 10)
  , "n == list length" ~: Nil ~=? (dropR (Cons 5 (Cons 6 Nil)) 2)
  , "n < list length" ~: (Cons 7 Nil) ~=? (dropR (Cons 5 (Cons 6 (Cons 7 Nil))) 2)
  ]
  ]
