import Test.HUnit
data ConsList a = Nil | Cons a (ConsList a) deriving (Show, Eq)

foldR _ c Nil = c
foldR f c (Cons x xs) = f x (foldR f c xs)

foldL f c l = foldR (\x g c -> g (f c x)) id l c

main = runTestTT $ test $ [
  "Terminal" ~:  5 ~=? (foldL (-) 5 Nil)
  , "Nonterminal" ~:  -4 ~=? (foldL (-) 5 (Cons 4 (Cons 5 Nil)))
  ]
