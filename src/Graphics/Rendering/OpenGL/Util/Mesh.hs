module Graphics.Rendering.OpenGL.Util.Mesh (
    createMesh, 
    renderMesh,
    deleteMesh, 
    Mesh(..)
) where

import Data.Maybe (catMaybes)
import Graphics.Rendering.OpenGL (PrimitiveMode (..), deleteObjectNames)
import Graphics.Rendering.OpenGL.Util.Buffers

----------------------------------------------------------------------

data Mesh = Mesh { 
        mTris :: IndexBufferObject,
        mVbo  :: VertexBufferObject,
        mAttribVbos :: [VertexBufferObject] 
    }

renderMesh :: Mesh -> IO ()
renderMesh (Mesh ts v avs) = do
    displayVbos Triangles (v:avs) ts

createMesh :: [Float]    -- vertex position coords
           -> [Int]      -- triangle indices
           -> Maybe [Float]    -- texture coords
           -> Maybe [Float]    -- normal coords
           -> [String]   -- named attributes names
           -> IO Mesh
createMesh vs tis mts mns nas = do
    let n = length vs `div` 3
        f = Just . map realToFrac
        vldp = VertexListDescription VertexPosition n (f vs)
        mvldt = fmap (\ts -> VertexListDescription VertexTexCoord n(f ts)) mts
        mvldn = fmap (\ns -> VertexListDescription VertexNormal n (f ns)) mns

    vbo <- createVbo $ [vldp] ++ catMaybes [mvldt, mvldn] 

    avbos <- mapM createEmptyVbo' $ map (\a -> VertexListDescription (VertexNamedAttrib a 3) n Nothing) nas

    tibo <- createIbo tis
    
    return $ Mesh tibo vbo avbos


deleteMesh :: Mesh -> IO ()
deleteMesh (Mesh ts v avs) = do
  deleteObjectNames [getIndexBufferObject ts]
  deleteObjectNames (getVertexBufferObject v : map getVertexBufferObject avs)