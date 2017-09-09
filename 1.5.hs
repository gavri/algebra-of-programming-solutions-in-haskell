data Nat = Zero | Succ Nat deriving Show

foldn f c Zero = c
foldn f c (Succ n) = f (foldn f c n)

evenNat Zero = True
evenNat (Succ Zero) = False
evenNat (Succ (Succ n)) = evenNat n

lastNat p n = snd $ foldn append (Zero, Zero) n
           where append (previous, answer) | p current = (current, current)
                                           | otherwise = (current, answer)
                    where
                        current = Succ previous

-- f = snd
-- c = Zero
-- h = append
