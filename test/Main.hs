module Main where

import qualified Data.Array.MArray as MA
import qualified Data.Array.IO as IOA
import qualified Data.Array.Unboxed as UA
import qualified Data.Array.Unsafe as UnsafeA
import Linear

--type VoxelData = UA.Array (V3 Int) Float

--loadRaw :: Int -> Int -> Int -> IO VoxelData
--loadRaw lx ly lz = do
--    arr <- newArray'



--    UnsafeA.unsafeFreeze arr
--  where
--    newArray' :: IO (IOA.IOUArray (V3 Int) Float)
--    newArray' = MA.newArray (V3 0 0 0, V3 (lx-1) (ly-1) (lz-1)) 0

main :: IO ()
main = do
  putStrLn "FOOBAR"
