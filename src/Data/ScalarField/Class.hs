module Data.ScalarField.Class where

  
class ScalarField f p g s | f -> p, f -> g, f -> s where
  valueAt         :: f -> p -> s
  gradientAt      :: f -> p -> g
