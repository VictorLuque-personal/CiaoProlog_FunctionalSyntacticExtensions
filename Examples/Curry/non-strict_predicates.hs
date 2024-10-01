data NatPeano = Zero | S NatPeano deriving Show

dec :: NatPeano -> NatPeano
dec (S x) = x

const :: NatPeano -> NatPeano -> NatPeano
const x _ = x

-- Try the following:
-- Main.const Zero (dec Zero)
-- it does not fail due to the lazy evaluation.

app :: [a] -> [a] -> [a]
app [] ys = ys
app (x:xs) ys = x : app xs ys

app3 :: [a] -> [a] -> [a] -> [a]
app3 xs ys zs = app (app xs ys) zs