module FractionalMultiplication where

type Vinculum = String -- The line seperating numerator and denominator

type Numerator = Integer -- Top Number 
type Denominator = Integer -- Bottom Number
type WholeNumber = Integer -- First Number in front of fraction

data Fraction = NormalFraction Numerator Vinculum Denominator | MixedFraction WholeNumber Numerator Vinculum Denominator
    deriving (Show)

fractionalMultiplication :: Fraction -> Fraction -> Fraction
fractionalMultiplication (NormalFraction n v d) (NormalFraction n2 v2 d2) = NormalFraction (n*n2) v (d*d2)
fractionalMultiplication (NormalFraction n v d) (MixedFraction w n2 v2 d2) = NormalFraction ((w * d2 + n2) * n) v (d*d2)
fractionalMultiplication (MixedFraction w n2 v2 d2) (NormalFraction n v d)  = NormalFraction ((w * d2 + n2) * n) v (d*d2)
fractionalMultiplication (MixedFraction w n v d) (MixedFraction w2 n2 v2 d2) = NormalFraction (normalFraction1 * normalFraction2) v (d*d2)
        where 
            normalFraction1 = w * d + n
            normalFraction2 = w2 * d2 + n2