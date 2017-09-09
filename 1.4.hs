data Nat = Zero | Succ Nat deriving Show

foldn f c Zero = c
foldn f c (Succ n) = f (foldn f c n)

addNat natOne natTwo = foldn Succ natOne natTwo
multNat natOne natTwo = foldn (addNat natOne) Zero natTwo
-- squareNat n = multNat n n
-- squareNat n = foldNat (addNat n) Zero n
squareNat n = foldn (foldn Succ n) Zero n
