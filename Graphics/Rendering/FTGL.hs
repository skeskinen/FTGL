{-# INCLUDE <FTGL/ftgl.h> #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# OPTIONS_GHC -O2 -fglasgow-exts #-}
-- | * Author: Jefferson Heard (jefferson.r.heard at gmail.com)
--
--   * Copyright 2008 Renaissance Computing Institute < http://www.renci.org > 
--   
--   * License: GNU LGPL 
--
--   * Compatibility GHC (I could change the data declarations to not be empty and that would make it more generally compatible, I believe)
--
--   * Description: 
--
--  Use FreeType 2 Fonts in OpenGL.  Requires the FTGL library and FreeType libraries.
--  available at < http://ftgl.wiki.sourceforge.net/ > . The most important functions for
--  everyday use are renderFont and the create*Font family of functions.  To render a 
--  simple string inside OpenGL, assuming you have OpenGL initialized and a current 
--  pen color, all you need is:
-- 
-- > do font <- createTextureFont "Font.ttf"
-- >   setFontFaceSize font 24 72
-- >   renderFont font "Hello world!"
--
-- Fonts are rendered so that a single point is an OpenGL unit, and a point is 1:72 of
-- an inch.
module Graphics.Rendering.FTGL 
where

import Foreign (unsafePerformIO)
import Foreign.C
import Foreign.Ptr
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Data.Bits 
import Data.Char (ord)

import qualified Graphics.Rendering.OpenGL.GL as GL

import Control.Applicative ((<$>))

foreign import ccall unsafe "ftglCreateBitmapFont" fcreateBitmapFont :: CString -> IO Font
-- | Create a bitmapped version of a TrueType font.  Bitmapped versions will not
-- | respond to matrix transformations, but rather must be transformed using the
-- | raster positioning functions in OpenGL
createBitmapFont :: String -> IO Font
createBitmapFont file = withCString file $ \p -> fcreateBitmapFont p


foreign import ccall unsafe "ftglCreateBufferFont" fcreateBufferFont :: CString -> IO Font
-- | Create a buffered version of a TrueType font. This stores the entirety of
-- | a string in a texture, "buffering" it before rendering.  Very fast if you
-- | will be repeatedly rendering the same strings over and over.
createBufferFont :: String -> IO Font
createBufferFont file = withCString file $ \p -> fcreateBufferFont p


foreign import ccall unsafe "ftglCreateOutlineFont" fcreateOutlineFont :: CString -> IO Font
-- | Create an outline version of a TrueType font. This uses actual geometry
-- | and will scale independently without loss of quality.  Faster than polygons
-- | but slower than texture or buffer fonts.
createOutlineFont :: String -> IO Font
createOutlineFont file = withCString file $ \p -> fcreateOutlineFont p


foreign import ccall unsafe "ftglCreatePixmapFont" fcreatePixmapFont  :: CString -> IO Font
-- | Create a pixmap version of a TrueType font.  Higher quality than the bitmap
-- | font without losing any performance.  Use this if you don't mind using
-- | set and get RasterPosition.
createPixmapFont :: String -> IO Font
createPixmapFont file = withCString file $ \p -> fcreatePixmapFont p


foreign import ccall unsafe "ftglCreatePolygonFont" fcreatePolygonFont :: CString -> IO Font
-- | Create polygonal display list fonts.  These scale independently without
-- | losing quality, unlike texture or buffer fonts, but can be impractical
-- | for large amounts of text because of the high number of polygons needed.
-- | Additionally, they do not, unlike the textured fonts, create artifacts
-- | within the square formed at the edge of each character.
createPolygonFont :: String -> IO Font
createPolygonFont file = withCString file $ \p -> fcreatePolygonFont p


foreign import ccall unsafe "ftglCreateTextureFont" fcreateTextureFont :: CString -> IO Font
-- | Create textured display list fonts.  These can scale somewhat well, 
-- | but lose quality quickly.  They are much faster than polygonal fonts, 
-- | though, so are suitable for large quantities of text.  Especially suited
-- | well to text that changes with most frames, because it doesn't incur the
-- | (normally helpful) overhead of buffering.
createTextureFont :: String -> IO Font
createTextureFont file = withCString file $ \p -> fcreateTextureFont p


foreign import ccall unsafe "ftglCreateExtrudeFont" fcreateExtrudeFont :: CString -> IO Font
-- | Create a 3D extruded font.  This is the only way of creating 3D fonts 
-- | within FTGL.  Could be fun to use a geometry shader to get different
-- | effects by warping the otherwise square nature of the font.  Polygonal.
-- | Scales without losing quality.  Slower than all other fonts.
createExtrudeFont :: String -> IO Font
createExtrudeFont file = withCString file $ \p -> fcreateExtrudeFont p




-- | Create a simple layout
foreign import ccall unsafe "ftglCreateSimpleLayout" createSimpleLayout :: IO Layout

-- | Set the layout's font.
foreign import ccall unsafe "ftglSetLayoutFont" setLayoutFont :: Layout -> Font -> IO ()


foreign import ccall unsafe "ftglGetLayoutFont" fgetLayoutFont :: Layout -> IO Font
-- | Get the embedded font from the Layout
getLayoutFont f = fgetLayoutFont f 


-- | Set the line length, I believe in OpenGL units, although I'm not sure.
foreign import ccall unsafe "ftglSetLayoutLineLength" setLayoutLineLength :: Layout -> CFloat -> IO ()


foreign import ccall unsafe "ftglGetLayoutLineLength" fgetLayoutLineLength :: Layout -> IO CFloat
-- | Get the line length in points (1:72in) of lines in the layout
getLayoutLineLength :: Layout -> IO Float
getLayoutLineLength f = realToFrac <$> fgetLayoutLineLength f


foreign import ccall unsafe "ftglSetLayoutAlignment" fsetLayoutAlignment :: Layout -> CInt -> IO ()
-- | Set the layout alignment
setLayoutAlignment layout alignment = fsetLayoutAlignment layout (marshalTextAlignment alignment)


foreign import ccall unsafe "ftglGetLayoutAlignement" fgetLayoutAlignment :: Layout -> IO CInt
-- | Get the alignment of text in this layout.
getLayoutAlignment f = readTextAlignment <$>  fgetLayoutAlignment f


foreign import ccall unsafe "ftglSetLayoutLineSpacing" fsetLayoutLineSpacing :: Layout -> CFloat -> IO ()
-- | Set layout line spacing in OpenGL units.
setLayoutLineSpacing :: Layout -> Float -> IO ()
setLayoutLineSpacing layout spacing = setLayoutLineSpacing layout (realToFrac spacing)


-- | Destroy a font
foreign import ccall unsafe "ftglDestroyFont" destroyFont :: Font -> IO ()


foreign import ccall unsafe "ftglAttachFile" fattachFile  :: Font -> CString -> IO ()
-- | Attach a metadata file to a font.
attachFile :: Font -> String -> IO () 
attachFile font str = withCString str $ \p -> fattachFile font p


-- | Attach some external data (often kerning) to the font
foreign import ccall unsafe "ftglAttachData" attachData :: Font -> Ptr () -> IO () 


-- | Set the font's character map
foreign import ccall unsafe "ftglSetFontCharMap" fsetFontCharMap :: Font -> CInt -> IO ()
setCharMap :: Font -> CharMap -> IO ()
setCharMap font charmap = fsetFontCharMap font (marshalCharMap charmap) 


foreign import ccall unsafe "ftglGetFontCharMapCount" fgetFontCharMapCount :: Font -> IO CInt
-- | Get the number of characters loaded into the current charmap for the font.
getFontCharMapCount :: Font -> Int
getFontCharMapCount f = fromIntegral . unsafePerformIO $ fgetFontCharMapCount f


foreign import ccall unsafe "ftglGetFontCharMapList" fgetFontCharMapList  :: Font -> IO (Ptr CInt)
-- | Get the different character mappings available in this font.
getFontCharMapList f = unsafePerformIO $ fgetFontCharMapList f


foreign import ccall unsafe "ftglSetFontFaceSize" fsetFontFaceSize  :: Font -> CInt -> CInt -> IO CInt
setFontFaceSize :: Font -> Int -> Int -> IO CInt
setFontFaceSize f s x = fsetFontFaceSize f (fromIntegral s) (fromIntegral x)

foreign import ccall unsafe "ftglGetFontFaceSize" fgetFontFaceSize :: Font -> IO CInt
-- | Get the current font face size in points.
getFontFaceSize :: Font -> IO Int
getFontFaceSize f = fromIntegral <$> fgetFontFaceSize f


foreign import ccall unsafe "ftglSetFontDepth" fsetFontDepth :: Font -> CFloat -> IO ()
setFontDepth :: Font -> Float -> IO ()
setFontDepth font depth = fsetFontDepth font (realToFrac depth)


foreign import ccall unsafe "ftglSetFontOutset" fsetFontOutset :: Font -> CFloat -> CFloat -> IO ()
setFontOutset :: Font -> Float -> Float -> IO ()
setFontOutset font d o = fsetFontOutset font (realToFrac d) (realToFrac o)


foreign import ccall unsafe "ftglGetFontBBox" fgetFontBBox :: Font -> CString -> Int -> Ptr CFloat -> IO () 
-- | Get the text extents of a string as a list of (llx,lly,lly,urx,ury,urz)
getFontBBox :: Font -> String -> IO [Float]
getFontBBox f s = allocaBytes 24 $ \pf -> 
                     withCString s $ \ps -> do 
                       fgetFontBBox f ps (-1) pf
                       map realToFrac <$> peekArray 6 pf

foreign import ccall unsafe "ftglGetFontAscender" fgetFontAscender :: Font -> CFloat
-- | Get the global ascender height for the face. 
getFontAscender :: Font -> Float
getFontAscender  = realToFrac . fgetFontAscender 

foreign import ccall unsafe "ftglGetFontDescender" fgetFontDescender :: Font -> CFloat
-- | Gets the global descender height for the face. 
getFontDescender :: Font -> Float
getFontDescender  = realToFrac . fgetFontDescender 


foreign import ccall unsafe "ftglGetFontLineHeight" fgetFontLineHeight :: Font -> CFloat
-- | Gets the global line spacing for the face. 
getFontLineHeight :: Font -> Float
getFontLineHeight  = realToFrac . fgetFontLineHeight

foreign import ccall unsafe "ftglGetFontAdvance" fgetFontAdvance :: Font -> CString -> IO CFloat
-- | Get the horizontal span of a string of text using the current font.  Input as the xcoord
-- | in any translate operation
getFontAdvance :: Font -> String -> IO Float
getFontAdvance font str = realToFrac <$> (withCString str $ \p -> fgetFontAdvance font p )


foreign import ccall unsafe "ftglRenderFont" frenderFont :: Font -> CString -> CInt -> IO ()
-- | Render a string of text in the current font.
renderFont :: Font -> String -> RenderMode -> IO ()
renderFont font str mode = withCString str $ \p -> do 
	frenderFont font p (marshalRenderMode mode)


foreign import ccall unsafe "ftglGetFontError" fgetFontError :: Font -> IO CInt
-- | Get any errors associated with loading a font. FIXME return should be a type, not an Int.
getFontError :: Font -> IO Int
getFontError f = fromIntegral <$> fgetFontError f




foreign import ccall unsafe "ftglDestroyLayout" destroyLayout :: Layout -> IO ()


foreign import ccall unsafe "ftglRenderLayout" frenderLayout :: Layout -> CString -> IO ()
-- | Render a string of text within a layout.
renderLayout layout str = withCString str $ \strPtr -> do frenderLayout layout strPtr


foreign import ccall unsafe "ftglGetLayoutError" fgetLayoutError :: Layout -> IO CInt
-- | Get any errors associated with a layout.
getLayoutError f = fgetLayoutError f






-- | Whether or not in polygonal or extrusion mode, the font will render equally front and back
data RenderMode = Front | Back | Side | All

-- | In a Layout directed render, the layout mode of the text
data TextAlignment = AlignLeft | AlignCenter | AlignRight | Justify

marshalRenderMode :: RenderMode -> CInt
marshalRenderMode Front = 0x0001
marshalRenderMode Back = 0x0002
marshalRenderMode Side = 0x004
marshalRenderMode All = 0xffff

marshalTextAlignment :: TextAlignment -> CInt
marshalTextAlignment AlignLeft = 0
marshalTextAlignment AlignCenter = 1
marshalTextAlignment AlignRight = 2 
marshalTextAlignment Justify = 3

readTextAlignment :: CInt -> TextAlignment
readTextAlignment 0 = AlignLeft
readTextAlignment 1 = AlignCenter
readTextAlignment 2 = AlignRight
readTextAlignment 3 = Justify

-- | An opaque type encapsulating a glyph in C.  Currently the glyph functions are unimplemented in Haskell.
data Glyph_Opaque 

-- | An opaque type encapsulating a font in C.
data Font_Opaque

-- | An opaque type encapsulating a layout in C
data Layout_Opaque

type Glyph = Ptr Glyph_Opaque
type Font = Ptr Font_Opaque
type Layout = Ptr Layout_Opaque


data CharMap = 
    EncodingNone 
  | EncodingMSSymbol 
  | EncodingUnicode 
  | EncodingSJIS 
  | EncodingGB2312 
  | EncodingBig5
  | EncodingWanSung
  | EncodingJohab
  | EncodingAdobeStandard
  | EncodingAdobeExpert
  | EncodingAdobeCustom
  | EncodingAdobeLatin1
  | EncodingOldLatin2
  | EncodingAppleRoman

encodeTag :: Char -> Char -> Char -> Char -> CInt 
encodeTag a b c d = 
    (fromIntegral (ord a) `shift` 24) 
    .|. (fromIntegral (ord b) `shift` 16) 
    .|. (fromIntegral (ord c) `shift` 8) 
    .|. (fromIntegral (ord d))

marshalCharMap EncodingNone = 0
marshalCharMap EncodingMSSymbol = encodeTag 's' 'y' 'm' 'b'
marshalCharMap EncodingUnicode =encodeTag 'u' 'n' 'i' 'c'
marshalCharMap EncodingSJIS = encodeTag 's' 'j' 'i' 's' 
marshalCharMap EncodingGB2312 = encodeTag 'g' 'b' ' ' ' ' 
marshalCharMap EncodingBig5= encodeTag 'b' 'i' 'g' '5' 
marshalCharMap EncodingWanSung= encodeTag 'w' 'a' 'n' 's' 
marshalCharMap EncodingJohab= encodeTag 'j' 'o' 'h' 'a' 
marshalCharMap EncodingAdobeStandard= encodeTag 'A' 'D' 'O' 'B' 
marshalCharMap EncodingAdobeExpert= encodeTag 'A' 'D' 'B' 'E' 
marshalCharMap EncodingAdobeCustom= encodeTag 'A' 'D' 'B' 'C' 
marshalCharMap EncodingAdobeLatin1= encodeTag 'l' 'a' 't' '1' 
marshalCharMap EncodingOldLatin2= encodeTag 'l' 'a' 't' '2' 
marshalCharMap EncodingAppleRoman= encodeTag 'a' 'r' 'm' 'n' 


