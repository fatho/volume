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
  GL.clearColor $= GL.Color4 0.0 0.0 0.2 1.0
  -- enable alpha blending
  --GL.blendFunc $= (GL.SrcAlpha, GL.OneMinusSrcAlpha)
  --GL.blend $= GL.Enabled

  mainLoop wnd

  GLFW.destroyWindow wnd
  GLFW.terminate

mainLoop :: GLFW.Window -> IO ()
mainLoop wnd = GLFW.windowShouldClose wnd >>= 
  \close -> unless close $ do

    (width, height) <- GLFW.getFramebufferSize wnd
    let ratio = fromIntegral width / fromIntegral height

    GL.viewport $= (GL.Position 0 0, GL.Size (fromIntegral width) (fromIntegral height))
    GL.clear [GL.ColorBuffer, GL.DepthBuffer]

    GL.matrixMode $= GL.Projection
    GL.loadIdentity
    --GL.ortho (negate ratio) ratio (negate 1.0) 1.0 1.0 (negate 1.0)
    GL.perspective 80 ratio 0.1 100
    GL.matrixMode $= GL.Modelview 0

    GL.loadIdentity
    let
      eye    = GL.Vertex3 3.0 3.0 3.0
      center = GL.Vertex3 0.0 0.0 0.0
      up     = GL.Vector3 (-3.0) (-3.0) 6.0
    GL.lookAt eye center up
    -- this is bad, but keeps the logic of the original example I guess
    Just t <- GLFW.getTime
    --GL.rotate ((realToFrac t) * 50) $ (GL.Vector3 0 0 1 :: GL.Vector3 GL.GLdouble)

    GL.renderPrimitive GL.Triangles $ do
        GL.color  (GL.Color3 1 0 0 :: GL.Color3 GL.GLdouble)
        GL.vertex (GL.Vertex3 0 0 0 :: GL.Vertex3 GL.GLdouble)
        GL.color  (GL.Color3 0 1 0 :: GL.Color3 GL.GLdouble)
        GL.vertex (GL.Vertex3 0 1 0 :: GL.Vertex3 GL.GLdouble)
        GL.color  (GL.Color3 0 0 1 :: GL.Color3 GL.GLdouble)
        GL.vertex (GL.Vertex3 1 0 0 :: GL.Vertex3 GL.GLdouble)


    GL.flush
    GLFW.swapBuffers wnd
    GLFW.pollEvents
    mainLoop wnd


await :: Monad m => m Bool -> m ()
await action = action >>= flip unless (await action)
