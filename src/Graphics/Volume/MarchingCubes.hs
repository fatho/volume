module Graphics.Volume.MarchingCubes where

import Graphics.Volume.MarchingCubesTables
import Numeric.ScalarField

import           Control.Lens
import           Data.Bits
import qualified Data.Vector as V
import           Linear

-- | Calculates the isosurface of a scalar field in three dimensional euclidian space.
marchingCubes :: (Enum a, Ord a, Epsilon a, Floating a, ScalarField s (V3 a) (V3 a) a)
              => s                -- ^ the isosurface
              -> a                -- ^ iso level
              -> V3 a             -- ^ region origin
              -> V3 Int           -- ^ number of cubes in each direction
              -> a                -- ^ cube size
              -> [[(V3 a, V3 a)]] -- ^ a list of triangle vertices consisting of position and normal
marchingCubes field isoLevel (V3 x0 y0 z0) (V3 nx ny nz) cubeSize
  = map handleCube positions
  where
    -- positions of all cubes in the specified region
    positions = 
      [V3 (x0 + cubeSize * realToFrac x) (y0 + cubeSize * realToFrac y) (z0 + cubeSize * realToFrac z)
      | x <- [0..nx-1]
      , y <- [0..ny-1]
      , z <- [0..nz-1]
      ]
    -- returns gradient and density in the scalar field as 4 dimensional vector
    valueAndGradientAt pos = 
      case gradientAt field pos of
        V3 x y z -> V4 x y z (valueAt field pos)
    -- calculate corners and values of cube
    handleCube pos = 
      let corners = cubeCorners cubeSize pos
          values  = V.map valueAndGradientAt corners
      in generateMesh isoLevel corners values


-- | Calculates the cornes of a cube
cubeCorners :: (Num a) => a -> V3 a -> V.Vector (V3 a)
cubeCorners width (V3 x y z) = V.fromList
  [ V3 x y z
  , V3 (x+width) y z
  , V3 (x+width) y (z+width)
  , V3 x y (z+width)
  , V3 x (y+width) z
  , V3 (x+width) (y+width) z
  , V3 (x+width) (y+width) (z+width)
  , V3 x (y+width) (z+width)
  ]

-- | Calculates the intersection of an edge with the iso-surface
-- and the corresponding normal vector.
interpolate :: (Floating a, Epsilon a) 
            => a            -- ^ isoLevel
            -> V3 a         -- ^ start point of edge
            -> V3 a         -- ^ end point of edge
            -> V4 a         -- ^ start value of edge
            -> V4 a         -- ^ end value of edge
            -> (V3 a, V3 a) -- ^ point of intersection and normal vector
interpolate isoLevel v0 v1 val0 val1
  | nearZero $ val0 ^. _w - isoLevel   = (v0, val1 ^. _xyz)
  | nearZero $ val1 ^. _w - isoLevel   = (v1, val1 ^. _xyz)
  | nearZero $ val1 ^. _w - val0 ^. _w = (v0, val0 ^. _xyz)
  | otherwise = 
      ( v0 ^+^ mu *^ (v1 ^-^ v0) 
      , normalize $ (val0 ^+^ mu *^ (val1 ^-^ val0)) ^. _xyz
      ) where
    mu = (isoLevel - val0 ^. _w) / (val1 ^. _w - val0 ^. _w) 

-- | Generates the mesh for one cube.
generateMesh :: (Ord a, Floating a, Epsilon a)
             => a                -- ^ iso level
             -> V.Vector (V3 a)  -- ^ cube corner positions
             -> V.Vector (V4 a)  -- ^ cube corner values (gradient + density)
             -> [(V3 a, V3 a)] -- ^ list of triangle vertices with corresponding normal vectors
generateMesh isoLevel corners values = concatMap (vectorToList . fmap (intersections V.!)) triangles where
  -- index of cube in the lookup tables
  cubeIndex = V.ifoldl' (\idx i v -> if v ^. _w >= isoLevel then idx .|. (1 `shiftL` i) else idx) 0 values
  triangles = mcTriangles V.! cubeIndex
  -- indices of corners participating in the respective edges
  edges     = [(0,1), (1,2), (2,3), (3,0), (4,5), (5,6), (6,7), (7,4), (0,4), (1,5), (2,6), (3,7)]
  -- lazy vector of interpolated intersections
  intersections = V.fromList 
    [ interpolate isoLevel (corners V.! i) (corners V.! j) (values V.! i) (values V.! j)
    | (i,j) <- edges ]

-- | returns the elements of the vector as list
vectorToList :: V3 a -> [a]
vectorToList (V3 x y z) = [x,y,z]