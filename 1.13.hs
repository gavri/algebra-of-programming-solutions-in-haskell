import Test.HUnit
data Tree a = a :<< [Tree a] deriving Show

foldTree f (parent :<< children) = f parent (map (foldTree f) children)

sizeTree t = foldTree (\parent foldedChildren -> 1 + sum foldedChildren) t
depthTree t = foldTree (\_ foldedChildren -> 1 + (maximum (0 : foldedChildren))) t


main = runTestTT $ test $ [
  TestLabel "sizeTree" $ TestList [
     "Terminal" ~:  1 ~=? (sizeTree $ 5 :<< [])
     , "Nonterminal" ~:  8 ~=? (sizeTree (5 :<< [6 :<< [7 :<< []], 8 :<< [9 :<< [], 10 :<< [11 :<< [12 :<< []]]]]))
     ]
  , TestLabel "depthTree" $ TestList [
     "Terminal" ~:  1 ~=? (depthTree $ 5 :<< [])
     , "Nonterminal" ~:  5 ~=? (depthTree (5 :<< [6 :<< [7 :<< []], 8 :<< [9 :<< [], 10 :<< [11 :<< [12 :<< []]]]]))
     ]
  ]
