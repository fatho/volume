module Numeric.ScalarField where

-- | A scalar field f with a position type p, a gradient type g and a scalar value type s.
class ScalarField f p g s | f -> p, f -> g, f -> s where
  -- | Evaluates the scalar value of the field at the given position
  valueAt         :: f -> p -> s
  -- | Evaluates the gradient of the field at the given position
  gradientAt      :: f -> p -> g
