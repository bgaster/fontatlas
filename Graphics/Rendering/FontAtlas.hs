{-
  | Simple module to create a font atlas as an OpenGL Texture

  Code was inspired by the blog:
       http://wiki.zyghost.com/Haskell%20font%20rendering%20with%20freetype2%20and%20opengl
-}
module Graphics.Rendering.FontAtlas (
       CharInfo,
       charToOffsetWidthHeight,
       createAtlas
  ) where 

import Control.Monad
import Graphics.Rendering.OpenGL hiding (bitmap)
import Graphics.Rendering.FreeType.Internal
import qualified Graphics.Rendering.FreeType.Internal.BitmapSize as BS
import Graphics.Rendering.FreeType.Internal.PrimitiveTypes
import Graphics.Rendering.FreeType.Internal.Library
import Graphics.Rendering.FreeType.Internal.FaceType
import Graphics.Rendering.FreeType.Internal.Face
import Graphics.Rendering.FreeType.Internal.GlyphSlot
import Graphics.Rendering.FreeType.Internal.Bitmap

import Foreign
import Foreign.C.String

import Data.Char
import Data.Bool.Extras

-- helper functions
runFreeType :: IO FT_Error -> IO ()
runFreeType m = do
    r <- m
    unless (r == 0) $ fail $ "FreeType Error:" ++ show r

freeType :: IO FT_Library
freeType = alloca $ \p -> do
    runFreeType $ ft_Init_FreeType p
    peek p

fontFace :: FT_Library -> FilePath -> IO FT_Face
fontFace ft fp = withCString fp $ \str ->
    alloca $ \ptr -> do
        runFreeType $ ft_New_Face ft str 0 ptr
        peek ptr

addPadding :: Int -> Int -> a -> [a] -> [a]
addPadding _ _ _ [] = []
addPadding amt w val xs = a ++ b ++ c
    where a = take w xs
          b = replicate amt val
          c = addPadding amt w val (drop w xs)

{-
  | For each character in the font there is an index:
       x_start_offset into texture (normalized for UV)
       x_end_offset into texture (normalized for UV)
       height of particular character (normalized for UV)
-}
newtype CharInfo = CharInfo [(GLfloat, GLfloat, GLfloat)]

charToOffsetWidthHeight :: CharInfo -> Char -> (GLfloat, GLfloat, GLfloat)
charToOffsetWidthHeight (CharInfo fs) c = fs !! ((ord c) - 32)

{-
  | Create a texture atlas from a FreeType.ttf

  We should probably don't need the texture unit binding but keep it for now.
-}
createAtlas :: FilePath -> -- ^ A filepath to the .ttf file
               Int ->      -- ^ Pixel size
               Int ->      -- ^ TextureUnit for the sampler binding
               IO (TextureObject, CharInfo) -- ^ OpenGL texture for atlas and char info
createAtlas path px texUnit = do
  ft <- freeType

  ff <- fontFace ft path
  runFreeType $ ft_Set_Pixel_Sizes ff (fromIntegral px) 0

  -- calulate width and height of atlas
  (ww,hh) <- foldM (\(ww,hh) i -> do

           chNdx <- ft_Get_Char_Index ff $ fromIntegral i
           runFreeType $ ft_Load_Glyph ff chNdx 0

           slot <- peek $ glyph ff
           runFreeType $ ft_Render_Glyph slot ft_RENDER_MODE_NORMAL

--           runFreeType $ ft_Load_Char ff i ft_LOAD_RENDER
--           slot <- peek $ glyph ff

           bmp <- peek $ bitmap slot           
           let w  = fromIntegral $ width bmp
               h  = fromIntegral $ rows bmp
               w' = fromIntegral w :: Integer
               h' = fromIntegral h
               p  = 4 - w `mod` 4
               nw = p + fromIntegral w'           
           
           return (ww + nw, max hh h)) (0,0) [32..127]

  -- now we know the size create the atlas texture
  -- Set the texture params on our bound texture.
  texture Texture2D $= Enabled

  -- Generate an opengl texture.
  [tex] <- genObjectNames 1
  texture Texture2D $= Enabled
  activeTexture $= TextureUnit (fromIntegral texUnit)
  textureBinding Texture2D $= Just tex

  -- generate non-populated texture
  texImage2D
          Texture2D
          NoProxy
          0
          R8
          (TextureSize2D (fromIntegral ww) (fromIntegral hh))
          0
          (PixelData Red UnsignedByte nullPtr)

  -- now we just populate texture and create list of (offset, width) for each 
  (_, ows) <- foldM (\(x, xs) i -> do

            chNdx <- ft_Get_Char_Index ff $ fromIntegral i
            runFreeType $ ft_Load_Glyph ff chNdx 0

            slot <- peek $ glyph ff
            runFreeType $ ft_Render_Glyph slot ft_RENDER_MODE_NORMAL

            bmp <- peek $ bitmap slot
            
            let w  = fromIntegral $ width bmp
                h  = fromIntegral $ rows bmp
                w' = fromIntegral w :: Integer
                r' = fromIntegral h
                p  = 4 - w `mod` 4
                nw = p + fromIntegral w'
            
            -- Get the raw bitmap data.
            bmpData <- peekArray (w*h) $ buffer bmp
            let data' = addPadding p w 0 bmpData
            
            withArray data' $ \ptr->
              texSubImage2D
                Texture2D
                0
                (TexturePosition2D (fromIntegral x) (fromIntegral 0)) 
                (TextureSize2D (fromIntegral nw) (fromIntegral r'))
                (PixelData Red UnsignedByte ptr)
            
            let ix = fromIntegral x   :: GLfloat
                iw = fromIntegral ww   :: GLfloat
                ih = fromIntegral hh   :: GLfloat
                iw' = fromIntegral nw :: GLfloat
                ir  = fromIntegral r' :: GLfloat                
            return (x + nw, (ix / iw, (ix + iw') / iw, ir / ih) : xs)) (0, []) [32..127]

  -- finally setup the textures sampler
  textureFilter   Texture2D   $= ((Linear', Nothing), Linear')
  textureWrapMode Texture2D S $= (Repeated, ClampToEdge)
  textureWrapMode Texture2D T $= (Repeated, ClampToEdge)

  return (tex, CharInfo $ reverse ows)
