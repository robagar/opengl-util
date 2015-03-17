module Graphics.Rendering.OpenGL.Util.Buffers
     ( VertexListDescription (..)
     , VertexListType (..)
     , VertexBufferObject (..)
     , IndexBufferObject (..)
     , createEmptyVbo, createEmptyVbo'
     , createVbo, createVbo'
     , fillVbo
     , createIbo
     , displayVbos
     ) where

import Foreign.Ptr
import Foreign.Storable
import Foreign.Marshal.Utils(copyBytes)
import Control.Monad (forM_)
import Data.Maybe (fromMaybe)
import Data.Array.Storable
import Data.Vector.Storable (Vector)
import qualified Data.Vector.Storable as V (length, unsafeWith)
import Graphics.Rendering.OpenGL

import Misc

data VertexListDescription = VertexListDescription { 
        vtype :: VertexListType,
        vlength :: Int,
        vlist :: Maybe [Float]
   }

data VertexListType
   = VertexPosition     
   | VertexNormal       
   | VertexTexCoord 
   | VertexColor3       
   | VertexColor4       
   | VertexNamedAttrib String Int

vsize :: VertexListDescription -> Int
vsize vd = case vtype vd of
    VertexPosition        -> 3
    VertexNormal          -> 3
    VertexTexCoord        -> 2
    VertexColor3          -> 3
    VertexColor4          -> 4
    VertexNamedAttrib _ i -> i

toArrayType :: VertexListDescription -> Maybe ClientArrayType
toArrayType vd = case vtype vd of
    VertexPosition        -> Just VertexArray
    VertexNormal          -> Just NormalArray
    VertexTexCoord        -> Just TextureCoordArray
    VertexColor3          -> Just ColorArray
    VertexColor4          -> Just ColorArray
    _                     -> Nothing

--vlength :: VertexListDescription -> Int
--vlength vd = length (vlist vd) `div` vsize vd

data VertexBufferObject = VertexBufferObject
   { getVertexBufferObject :: BufferObject
   , prepareVBO            :: IO ()
   , vboSize               :: Int
   , vboStride             :: Int
   , vboOffsets            :: [Int]
   , vboWithMapped         :: (Ptr Float -> IO ()) -> IO ()
   }
data IndexBufferObject = IndexBufferObject
   { getIndexBufferObject :: BufferObject
   , getSize              :: Int
   }

sizeOfFloat :: Int
sizeOfFloat = sizeOf (undefined :: Float)

sizeOfInt :: Int
sizeOfInt = sizeOf (undefined :: GLsizei)

createEmptyVbo' :: VertexListDescription -> IO VertexBufferObject
createEmptyVbo' vd = createEmptyVbo [vd]

createEmptyVbo :: [VertexListDescription] -> IO VertexBufferObject
createEmptyVbo vds = do
    forM_ vds $ \vd -> maybeDo_ (toArrayType vd) $ \t ->
        clientState t $= Enabled

    [array] <- genObjectNames 1
    bindBuffer ArrayBuffer $= Just array

    {- debug info }
    printf "sizeOfFlaot %d\n" $ sizeOfFloat
    printf "size        %d\n" $ size
    printf "stride      %d\n" $ stride
    printf "fstr        %d\n" $ fstr
    printf "size*stride %d\n" $ size*stride
    printf "size*fstr   %d\n" $ size*fstr
    printf "offsets     %s\n" $ show . init . scanl (+) 0
                              $ map vsize vds
    { -}

    let n = size*stride
    arr <- newListArray (0, n-1) $ take n (repeat (0 :: Float))
    withStorableArray arr $ \ptr -> bufferData ArrayBuffer $=
        (toEnum $ size*fstr, ptr, DynamicDraw)

    bindBuffer ArrayBuffer $= Nothing
    return $ VertexBufferObject array (prep array) size stride offsets (withMapped array)
  where
    size   = minimum $ map vlength vds
    stride = sum (map vsize vds)
    fstr   = sizeOfFloat * stride
    offsets = init $ scanl (+) 0 (map vsize vds)
    prep buf = let cvds = zip vds offsets
               in do
        bindBuffer ArrayBuffer $= Just buf      
        mprog <- get currentProgram
        maybeDo_ mprog $ \prog -> forM_ cvds $ \(vd, o) -> 
            let desc = VertexArrayDescriptor (toEnum $ vsize vd) 
                       Float (toEnum fstr)
                       $ plusPtr nullPtr $ o*sizeOfFloat
            in case toArrayType vd of
                Just t  -> arrayPointer t $= desc
                Nothing -> case vtype vd of
                    VertexNamedAttrib n _ -> do
                        loc <- get $ attribLocation prog n
                        vertexAttribArray   loc $= Enabled
                        vertexAttribPointer loc $= (ToFloat, desc)
                    _ -> return ()
        bindBuffer ArrayBuffer $= Nothing

    withMapped buf = \f -> do
        bindBuffer ArrayBuffer $= Just buf
        withMappedBuffer ArrayBuffer WriteOnly f reportMappingFailure
        bindBuffer ArrayBuffer $= Nothing 
 

createVbo' :: VertexListDescription -> IO VertexBufferObject
createVbo' vd = createVbo [vd]

createVbo :: [VertexListDescription] -> IO VertexBufferObject
createVbo vds = do
    forM_ vds $ \vd -> maybeDo_ (toArrayType vd) $ \t ->
        clientState t $= Enabled

    [array] <- genObjectNames 1
    bindBuffer ArrayBuffer $= Just array

    {- debug info }
    printf "sizeOfFlaot %d\n" $ sizeOfFloat
    printf "size        %d\n" $ size
    printf "stride      %d\n" $ stride
    printf "fstr        %d\n" $ fstr
    printf "size*stride %d\n" $ size*stride
    printf "size*fstr   %d\n" $ size*fstr
    printf "offsets     %s\n" $ show . init . scanl (+) 0
                              $ map vsize vds
    { -}

    arr <- newListArray (0, size*stride - 1) elems
    withStorableArray arr $ \ptr -> bufferData ArrayBuffer $=
        (toEnum $ size*fstr, ptr, DynamicDraw)

    bindBuffer ArrayBuffer $= Nothing
    return $ VertexBufferObject array (prep array) size stride offsets (withMapped array)
  where
    size   = minimum $ map vlength vds
    stride = sum (map vsize vds)
    fstr   = sizeOfFloat * stride
    elems  = take (size*stride) $ fvd vds
    offsets = init $ scanl (+) 0 (map vsize vds)
    prep buf = let cvds = zip vds offsets
               in do
        bindBuffer ArrayBuffer $= Just buf      
        mprog <- get currentProgram
        maybeDo_ mprog $ \prog -> forM_ cvds $ \(vd, o) -> 
            let desc = VertexArrayDescriptor (toEnum $ vsize vd) 
                       Float (toEnum fstr)
                       $ plusPtr nullPtr $ o*sizeOfFloat
            in case toArrayType vd of
                Just t  -> arrayPointer t $= desc
                Nothing -> case vtype vd of
                    VertexNamedAttrib n _ -> do
                        loc <- get $ attribLocation prog n
                        vertexAttribArray   loc $= Enabled
                        vertexAttribPointer loc $= (ToFloat, desc)
                    _ -> return ()
        bindBuffer ArrayBuffer $= Nothing

    fvd []  = []
    fvd ds = concat fs ++ fvd ls
        where
        (fs, ls) = unzip $ map fn ds
        fn vd = let (h, t) = splitAt (vsize vd) $ fromMaybe [] (vlist vd) 
                in  (h, VertexListDescription (vtype vd) (vlength vd - 1) (Just t))

    withMapped buf = \f -> do
        bindBuffer ArrayBuffer $= Just buf
        withMappedBuffer ArrayBuffer WriteOnly f reportMappingFailure
        bindBuffer ArrayBuffer $= Nothing 

reportMappingFailure :: MappingFailure -> IO ()
reportMappingFailure = putStrLn . show 

fillVbo :: VertexBufferObject -> Vector Float -> IO ()
fillVbo vbo fs = do
    let n = V.length fs
    V.unsafeWith fs $ \src -> 
        vboWithMapped vbo $ \dest -> copyBytes dest src (n * sizeOf(undefined :: Float))



createIbo :: [Int] -> IO IndexBufferObject
createIbo is = do
    [indexes] <- genObjectNames 1
    bindBuffer ElementArrayBuffer $= Just indexes

    let is' :: [GLsizei]
        is' = map fromIntegral is
    arr <- newListArray (0, size - 1) is'
    withStorableArray arr $ \ptr -> bufferData ElementArrayBuffer $=
        (toEnum $ size*sizeOfInt, ptr, StaticDraw)

    bindBuffer ElementArrayBuffer $= Nothing
    return $ IndexBufferObject indexes size
  where
    size = length is

displayVbos :: PrimitiveMode -> [VertexBufferObject] -> IndexBufferObject -> IO ()
displayVbos pm vbos idxs = do
    bindBuffer ElementArrayBuffer $= Just (getIndexBufferObject idxs)

    mapM_ prepareVBO vbos

    drawElements pm (toEnum $ getSize idxs) UnsignedInt nullPtr

    bindBuffer ElementArrayBuffer $= Nothing

