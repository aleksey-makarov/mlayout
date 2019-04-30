src=tree.dhall

dhall format --inplace $src
dhall --explain < $src