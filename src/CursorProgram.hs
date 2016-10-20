{-# LANGUAGE DataKinds, FlexibleContexts, TypeOperators #-}
module CursorProgram where

import Data.Vinyl
import Graphics.GLUtil
import Graphics.Rendering.OpenGL hiding (normal, normalize, light, Normal, Color)
import Linear (V2(..), V3(..))
import Graphics.VinylGL
import Foreign.Ptr (nullPtr)
import System.FilePath ((</>))
import Data.Word (Word16)

-----
-- Shader params
-----

type Pos = '("position", V2 GLfloat)

pos :: SField Pos
pos = SField

-----
-- Parameter data
-----

cursorNumSlices :: Num a => a
cursorNumSlices = 50

cursorNumIdxs :: GLsizei -- = NumArrayIndices
cursorNumIdxs = toEnum $ cursorNumSlices + 2

cursorpts :: [V2 GLfloat]
cursorpts = take (cursorNumSlices+2) $ (V2 0 0) : (zipWith V2 (map cos winding) (map sin winding))
	where
		winding :: [Float]
		winding = [0,(2*pi/(fromIntegral cursorNumSlices))..]

{-
Changing this from Word16 implies drawElements called w/ UnsignedInt instead of UnsignedShort and removing the import of (Word.Word16).
-}
cursorinds :: [Word16]
cursorinds = take (cursorNumSlices+2) [0,1..]

-----
-- Shader program
-----
type CursorCircleStyle = '[ '("offset", V2 GLfloat), '("color", V3 GLfloat) ]

cursorCircle :: (CursorCircleStyle <: i) => IO (FieldRec i -> IO ())
cursorCircle = do
		sprog <- simpleShaderProgram ("etc"</>"cursor.vert") ("etc"</>"cursor.frag")
		vb <- bufferVertices . map (pos =:) $ cursorpts
		eb <- makeBuffer ElementArrayBuffer cursorinds
		vao <- makeVAO $ do
			currentProgram $= Just (program sprog)
			enableVertices' sprog vb
			bindVertices vb
			bindBuffer ElementArrayBuffer $= Just eb
		let ss = setUniforms sprog
		return $ \appInfo -> withVAO vao $ do
			currentProgram $= Just (program sprog)
			ss (rcast appInfo :: FieldRec CursorCircleStyle)
			-- triangle fan drawing
			drawElements TriangleFan cursorNumIdxs UnsignedShort nullPtr
