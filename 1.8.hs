import Test.HUnit
data ListL a = ListLNull | Snoc (ListL a) a deriving (Show, Eq)

foldListL c _ ListLNull = c
foldListL c f (Snoc xs x) = f (foldListL c f xs) x

data ListR a = ListRNull | Cons a (ListR a) deriving (Show, Eq)

h ListRNull x = Cons x ListRNull
h (Cons a rest) x = Cons x (h rest a)

c = ListRNull

catConv :: ListL a -> ListR a -> ListR a
catConv l r = foldListL r h l

convert :: ListL a -> ListR a
convert l = catConv l ListRNull

main = runTestTT $ test $ [
  TestLabel "catConv" $ TestList [
     "Terminal" ~:  (ListRNull :: ListR Int) ~=? (catConv ListLNull ListRNull)
     , "First Arg Terminal" ~:  (Cons 5 ListRNull) ~=? (catConv ListLNull (Cons 5 ListRNull))
     , "Second Arg Terminal" ~:  (Cons 4 ListRNull) ~=? (catConv (Snoc ListLNull 4) ListRNull)
     , "Nonterminal" ~:  (Cons 4 (Cons 5 ListRNull)) ~=? (catConv (Snoc ListLNull 4) (Cons 5 ListRNull))
     ]
  , TestLabel "convert" $ TestList [
     "Terminal" ~:  (ListRNull :: ListR Int) ~=? (convert ListLNull)
     , "One cell" ~:  ((Cons 4 ListRNull) :: ListR Int) ~=? ((convert (Snoc ListLNull 4)) :: ListR Int)
     , "Multiple cells" ~:  ((Cons 3 (Cons 4 ListRNull)) :: ListR Int) ~=? ((convert (Snoc (Snoc ListLNull 4) 3)) :: ListR Int)
     ]
  ]

