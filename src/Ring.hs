module Ring (
    Ring (..),
    getNeirbours
) where

newtype Ring a = Ring [a]
instance Functor Ring where
    fmap f (Ring xs) = Ring (fmap f xs)

getNeirbours :: Ring a -> Int -> (a, a, a)
getNeirbours (Ring xs) index = let l = length xs in
                                   (xs !! ((index - 1) `mod` l), xs !! (index `mod` l), xs !! ((index + 1) `mod` l))
