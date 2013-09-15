import Test.HUnit
data ListL a = ListLNull | Snoc (ListL a) a deriving (Show, Eq)

foldListL c _ ListLNull = c
foldListL c f (Snoc xs x) = f (foldListL c f xs) x

data ListR a = ListRNull | Cons a (ListR a) deriving (Show, Eq)

h ListRNull x = Cons x ListRNull
h (Cons a rest) x = Cons a (h rest x)

c = ListRNull

lToR = foldListL c h

main = runTestTT $ test $ [
  "Terminal" ~:  (ListRNull :: ListR Int) ~=? (lToR ListLNull),
  "Nonterminal" ~:  (Cons 5 (Cons 6 (Cons 7 (Cons 8 ListRNull)))) ~=? (lToR $ (Snoc (Snoc (Snoc (Snoc ListLNull 5) 6) 7) 8))
  ]
