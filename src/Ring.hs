module Ring (
    Ring (..),
    getNeirbours,
    ringLength
) where

newtype Ring a = Ring [a]
instance Functor Ring where
    fmap f (Ring xs) = Ring (fmap f xs)

ringLength :: Ring a -> Int
ringLength (Ring x) = length x

getNeirbours :: Ring a -> Int -> (a, a, a)
getNeirbours (Ring xs) index = let l = length xs in
                                   (xs !! ((index + l - 1) `mod` l), xs !! (index + l `mod` l), xs !! ((index + l + 1) `mod` l))
