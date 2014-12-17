{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE PackageImports #-}
module Main where

import Control.Concurrent.STM (TQueue,
                               atomically, newTQueueIO, tryReadTQueue, writeTQueue)
import Control.Applicative
import Control.Monad
import Control.Lens ((+~), (^.), contains)
import Data.Foldable (foldMap, traverse_)
import Data.Vinyl
import Data.Set (Set)
import Data.Vinyl.Universe ((:::), SField(..))
import Graphics.GLUtil
import qualified Graphics.UI.GLFW as GLFW
import Graphics.GLUtil.Camera2D
import Graphics.Rendering.OpenGL
import "GLFW-b" Graphics.UI.GLFW as GLFW
import Graphics.VinylGL
import Data.Vector.Storable (fromList)
import Linear (V1(..), V2(..), _x, M33)
import System.FilePath ((</>))
import System.Environment ( getArgs )
import FRP.Elerea.Simple
import Data.List (transpose)
import Data.Maybe
import Control.Concurrent (threadDelay)
import Control.Monad.RWS.Strict  (RWST,
                                  ask, asks, evalRWST, get, liftIO, modify, put)
import Data.Bool.Extras
import System.Random
import Graphics.Rendering.FontAtlas

import Window

--------------------------------------------------------------------------------
-- Graphics Stuff
--------------------------------------------------------------------------------

type GLInfo = PlainFieldRec '["cam" ::: M33 GLfloat]

type Point2D = V2 GLfloat
type UV      = Point2D

type VPos  = "vertexCoord" ::: Point2D
type Tex  = "texCoord"    ::: Point2D

vpos :: SField VPos
vpos = SField

tex :: SField Tex
tex = SField

square :: GLfloat -> GLfloat -> [[Point2D]]
square x y = [[V2 (x * cell_width) (y * cell_height + cell_height),
               V2 (x * cell_width + cell_width) (y * cell_height + cell_height),
               V2 (x * cell_width) (y * cell_height)],
              
              [V2 (x * cell_width) (y * cell_height),
               V2 (x * cell_width + cell_width) (y * cell_height),
               V2 (x * cell_width + cell_width) (y * cell_height + cell_height)
               ]
              ]
   where
     cell_width :: GLfloat
     cell_width = 1.0 

     cell_height :: GLfloat
     cell_height = 1.0

text' :: CharInfo -> Char -> [PlainFieldRec [VPos,Tex]]
text' offsets c = vt
  where (o,o',h) = charToOffsetWidthHeight offsets c
        vt :: [PlainFieldRec [VPos,Tex]]
        vt = concat $ concat ps 
        f (sq, t) = zipWith (zipWith (\pt uv -> vpos =: pt <+> tex =: uv)) sq t
        
        ps = map f [
          (square 0 0,[[V2 o 0, V2 o' 0, V2 o h],
                       [V2 o h, V2 o' h, V2 o' 0]])]

text :: CharInfo -> Char -> IO (BufferedVertices [VPos,Tex])
text offsets c = bufferVertices $ text' offsets c

isPress :: GLFW.KeyState -> Bool
isPress GLFW.KeyState'Pressed   = True
isPress GLFW.KeyState'Repeating = True
isPress _                       = False

isKeyPressed :: Char -> TQueue EventKey -> IO (Maybe Char)
isKeyPressed ch kq = do
  k <- atomically $ tryReadTQueue kq
  maybe (return $ Just ch)
        (\(EventKey win k scancode ks mk) ->
          if isPress ks
          then case k of
               Key'Escape -> return Nothing
               _          -> return $ Just $ ascii mk k 
          else return $ Just ch) k
  where ascii mk Key'Space = '.'
        ascii mk Key'Apostrophe = '\''
        ascii mk Key'Comma = ','
        ascii mk Key'Minus = '-'
        ascii mk Key'Period = '.'
        ascii mk Key'Slash = '/'
        ascii mk Key'0 = '0'
        ascii mk Key'1 = '1'
        ascii mk Key'2 = '2'
        ascii mk Key'3 = '3'
        ascii mk Key'4 = '4'
        ascii mk Key'5 = '5'
        ascii mk Key'6 = '6'
        ascii mk Key'7 = '7'
        ascii mk Key'8 = '8'
        ascii mk Key'9 = '9'
        ascii mk Key'Semicolon  = ';'
        ascii mk Key'Equal = '='
        ascii mk Key'A = s mk 'a' 'A'
        ascii mk Key'B = 'B'
        ascii mk Key'C = 'C'
        ascii mk Key'D = 'D'
        ascii mk Key'E = 'E'
        ascii mk Key'F = 'F'
        ascii mk Key'G = 'G'
        ascii mk Key'H = 'H'
        ascii mk Key'I = 'I'
        ascii mk Key'J = 'J'
        ascii mk Key'K = 'K'
        ascii mk Key'L = 'L'
        ascii mk Key'M = 'M'
        ascii mk Key'N = 'N'
        ascii mk Key'O = 'O'
        ascii mk Key'P = 'P'
        ascii mk Key'Q = 'Q'
        ascii mk Key'R = 'R'
        ascii mk Key'S = 'S'
        ascii mk Key'T = 'T'
        ascii mk Key'U = 'U'
        ascii mk Key'V = 'V'
        ascii mk Key'W = 'W'
        ascii mk Key'X = 'X'
        ascii mk Key'Y = 'Y'
        ascii mk Key'Z = 'Z'
        ascii _ _      = ch
        s mk f t = bool f t (modifierKeysShift mk)
  

renderer :: FilePath -> IO (GLInfo -> UI -> Char -> IO (Maybe Char))
renderer file = do
   ts <- simpleShaderProgram ("shaders"</>"text.vert") ("shaders"</>"text.frag")

   (chars, offsets)  <- createAtlas (file) (48*2) 1   
   setUniforms ts (texSampler =: 1)
   tverts   <- text offsets 'B'
   tindices <- bufferIndices [0 .. 2 * 3]
   tvao <- makeVAO $ do
     enableVertices' ts tverts
     bindVertices tverts
     bindBuffer ElementArrayBuffer $= Just tindices

   return $ \i ui ch -> do
     k <- isKeyPressed ch (keys ui)
     if isJust k
     then do  
        reloadVertices tverts (fromList $ text' offsets $ fromJust k)
        currentProgram $= Just (program ts)
        setUniforms ts i
        withVAO tvao . withTextures2D [chars] $ drawIndexedTris 2
     else return ()
     return k
   where
     texSampler = SField :: SField ("tex" ::: GLint)

loop :: FilePath -> IO UI -> IO ()
loop file tick = do
  clearColor $= Color4 0.0 0.0 0.0 1
  r <- Main.renderer file
  go camera2D '.' r
  where go :: Camera GLfloat ->
              Char ->
              (GLInfo -> UI -> Char -> IO (Maybe Char)) -> IO ()
        go c ch draw = do
          ui <- tick
          clear [ColorBuffer, DepthBuffer]
          let mCam  = camMatrix c
              info  = SField =: mCam
          q <- draw info ui ch
          if isNothing q
          then return ()
          else swapBuffers (window ui) >> go c (fromJust q) draw

main :: IO ()
main = do
  [filename] <- getArgs
  let width  = 300
      height = 300
  tick      <- initGL "Font Atlas" width height
  loop filename tick
  return ()
