data Whole = WholeZero | WholeSucc Whole deriving Show
data Natural = NaturalOne | NaturalSucc Natural deriving Show

foldWhole h c WholeZero = c
foldWhole h c (WholeSucc w) = h (foldWhole h c w)

foldNatural h c NaturalOne = c
foldNatural h c (NaturalSucc n) = h (foldNatural h c n)

f w = foldWhole NaturalSucc NaturalOne w
g n = foldNatural WholeSucc WholeZero n

idWhole = g . f
idNatural = f . g
