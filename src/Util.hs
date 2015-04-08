module Util (binToInt, binToDouble) where
import Genes

binToInt::Chromosome->Int
binToInt chromosome = binToInt' 0 base chromosome
    where base = 2^(length chromosome-1)

binToInt'::Int->Int->Chromosome->Int
binToInt' acc 0 [x] = acc+x
binToInt' acc base (x:xs) = binToInt' (acc+x*base) newBase xs
    where newBase = div base 2
binToInt' acc _ _ = acc

binToDouble::Double->Chromosome->Double
binToDouble n chromosome = binToDouble' 0 base chromosome
    where base = 2**(n-1) -- | n = 1 means 1 digit before "."; means (2^0)*digit

binToDouble'::Double->Double->Chromosome->Double
binToDouble' acc _ [] = acc
binToDouble' acc base (x:xs) = binToDouble' newAcc newBase xs
    where 
        newAcc = acc + (fromIntegral x)*base
        newBase = base / 2.0