Haskell Font Atlas
=========

Simple Haskell library to load a true type font using freetype and
produce a single OpenGL texture with corresponding offsets for UV access.

The library is very simply:

   import Graphics.Renderer.FontAtlas

which exports two functions:

   createAtlas :: FilePath -> -- ^ A filepath to the .ttf file
                  Int ->      -- ^ Pixel size
                  Int ->      -- ^ TextureUnit for the sampler binding
                  IO (TextureObject, CharInfo)


and 

   charToOffsetWidthHeight :: CharInfo -> Char -> (GLfloat, GLfloat, GLfloat)

The first creates an OpenGL texture, representing the font atlas,
given a filename, including path, of a TrueType font, and pixel size
and a texture unit to bind the texture. It also returns offsey
information that can be indexed for a specific character with the
second function, which returns the (normalized) x start and x finish
offsets into the texture and finally the height of the specific
character in the texture. These three are enough to generate UVs to
access the corresponding pixels for drawing a given character, see
Main.hs for a simple exaple of how this can be done.

The example application, defined in Main.hs, creates a font atlas from
a given TrueType file and displays an window with an OpenGL surface
that takes keyboard input to display the specific character, in the
range [0,..,9] and [A,..,Z] (with corresponding use of shift)
otherwise displays period.