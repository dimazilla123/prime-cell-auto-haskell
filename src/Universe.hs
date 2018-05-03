module Universe (
    Universe (..),
    update
) where

import Ring
import Rule

data Universe = Univ (Ring Cell) Rule
instance Show Universe where
    show (Univ (Ring xs) r) = show xs
rule :: Universe -> Rule
rule (Univ x r) = r

update :: Universe -> Universe
update (Univ ring r) = let l = ringLength ring in
                           Univ (Ring (fmap (applicateRule r . getNeirbours ring) [0..l - 1])) r
