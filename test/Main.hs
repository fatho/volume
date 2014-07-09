module Main where

import           Control.Monad
import           Control.Monad.Error
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Cont

import qualified Data.Array.MArray as MA
import qualified Data.Array.IO as IOA
import qualified Data.Array.Unboxed as UA
import qualified Data.Array.Unsafe as UnsafeA


import qualified Graphics.GLUtil as GLU
import qualified Graphics.GLUtil.Camera2D as GLU
import qualified Graphics.Rendering.OpenGL as GL
import           Graphics.Rendering.OpenGL (($=))
import qualified Graphics.UI.GLFW as GLFW

import Numeric.ScalarField
import Linear


data Radial f a = Radial { sphCenter :: f a, sphValue0 :: a, sphSlope :: a}

data LG f a = LG { lgnormal :: f a, lgoffset :: a }

instance (Metric f, Floating a) => ScalarField (LG f a) (f a) (f a) a where
  valueAt lg pos = dot pos (lgnormal lg) + lgoffset lg
  gradientAt lg _ = lgnormal lg

instance (Metric f, Floating a) => ScalarField (Radial f a) (f a) (f a) a where
  valueAt r pos = sphValue0 r - sphSlope r * qd pos (sphCenter r)
  gradientAt r pos = negated $ (2 * sphSlope r) *^ (pos ^-^ sphCenter r)


testRad :: Radial V3 Double
testRad = Radial zero 1 1

testLin :: LG V3 Double
testLin = LG (normalize $ V3 1 1 0) (-0.5)

--type VoxelData = UA.Array (V3 Int) Float

--loadRaw :: Int -> Int -> Int -> IO VoxelData
--loadRaw lx ly lz = do
--    arr <- newArray'



--    UnsafeA.unsafeFreeze arr
--  where
--    newArray' :: IO (IOA.IOUArray (V3 Int) Float)
--    newArray' = MA.newArray (V3 0 0 0, V3 (lx-1) (ly-1) (lz-1)) 0

keyCallback :: GLFW.KeyCallback
keyCallback wnd key _ state modifiers  = do
  putStrLn (show key)
  when (key == GLFW.Key'Escape && state == GLFW.KeyState'Pressed) $ GLFW.setWindowShouldClose wnd True

main :: IO ()
main = do
  void $ GLFW.init -- checking return status unnecessary

  Just wnd <- GLFW.createWindow 800 600 "Marching Cubes" Nothing Nothing

  GLFW.makeContextCurrent (Just wnd)
  GLFW.setKeyCallback wnd (Just keyCallback)
  -- set nice background color
  GL.clearColor $= GL.Color4 0.0 0.0 1.0 1.0
  -- enable alpha blending
  --GL.blendFunc $= (GL.SrcAlpha, GL.OneMinusSrcAlpha)
  --GL.blend $= GL.Enabled

  gameLoop wnd

  GLFW.destroyWindow wnd
  GLFW.terminate

gameLoop :: GLFW.Window -> IO ()
gameLoop wnd = GLFW.windowShouldClose wnd >>= 
  \close -> unless close $ do
    GL.clear [GL.ColorBuffer, GL.DepthBuffer]
    GL.flush
    GLFW.swapBuffers wnd
    GLFW.pollEvents
    gameLoop wnd


await :: Monad m => m Bool -> m ()
await action = action >>= flip unless (await action)
