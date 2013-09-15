import Test.HUnit
data ListR a = ListRNull | Cons a (ListR a) deriving (Show, Eq)

foldListR c _ ListRNull = c
foldListR c h (Cons x xs) = h x (foldListR c h xs)

cat l r = foldListR r Cons l

main = runTestTT $ test $ [
     "Terminal" ~:  (ListRNull :: ListR Int) ~=? (cat ListRNull ListRNull)
     , "First Arg Terminal" ~:  (Cons 5 ListRNull) ~=? (cat ListRNull (Cons 5 ListRNull))
     , "Second Arg Terminal" ~:  (Cons 4 ListRNull) ~=? (cat (Cons 4 ListRNull) ListRNull)
     , "Nonterminal" ~:  (Cons 4 (Cons 5 ListRNull)) ~=? (cat (Cons 4 ListRNull) (Cons 5 ListRNull))
       ]
