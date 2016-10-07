{-# LANGUAGE FlexibleContexts #-}
 
module Graphics.Rendering.OpenGL.Util.Textures 
    ( ImageObject(..)
    , loadPNGTexture
    , maybeLoadPNGTextureObject
    , createTexture
) where

import Prelude
import Codec.Picture
import Data.Vector.Storable hiding (null, (++))
import Control.Applicative ((<$>))
import Foreign.Ptr
import Foreign.Storable
import Foreign.Marshal.Array
import Graphics.Rendering.OpenGL hiding (imageWidth, imageHeight)
import System.Directory

----------------------------------------------------------------------

data ImageObject = ImageObject
   { imgWidth  :: Int
   , imgHeight :: Int
   , imgObject :: TextureObject }

----------------------------------------------------------------------

whenExists fn f = do
    exists <- doesFileExist fn
    if exists
        then f
        else return . Left $ "File does not exist: " ++ fn

loadPNGTexture :: FilePath -> IO (Either String ImageObject)
loadPNGTexture filename = whenExists filename $ do
    res <- readPng filename
    case res of
        Left ems -> return $ Left ems
        Right (ImageRGB8  img) -> loadWithFormat RGB  img
        Right (ImageRGBA8 img) -> loadWithFormat RGBA img
        -- TODO: Other formats.
        _ -> return $ Left "Unhandled image format."
    where 
    loadWithFormat pf img = do
        let iw = imageWidth  img
        let ih = imageHeight img
        let size = TextureSize2D (fromIntegral iw) (fromIntegral ih)
        -- cln <- cloneVector $ imageData img
        -- Ok I'm not sure about this
        let mkt = unsafeWith (imageData img) $
                  createTexturePtr RGBA' size pf UnsignedByte
        fmap (ImageObject (fromIntegral iw) (fromIntegral ih)) <$> mkt

maybeLoadPNGTextureObject :: FilePath -> IO (Maybe TextureObject)
maybeLoadPNGTextureObject p = do
    -- putStr $ "try load texture: " ++ p ++ "..."
    et <- loadPNGTexture p
    case et of
        Left _ -> do
            -- putStrLn "not found"
            return Nothing
        Right (ImageObject _ _ t) -> do
            -- putStrLn "OK"
            return $ Just t

----------------------------------------------------------------------

createTexture :: Storable a
    => PixelInternalFormat
    -> TextureSize2D
    -> PixelFormat
    -> DataType
    -> Maybe [a]
    -> IO (Either String TextureObject)
createTexture ipf size pf dt dat = do
    ndata <- maybe (return nullPtr) newArray dat
    createTexturePtr ipf size pf dt ndata

createTexturePtr :: Storable a
    => PixelInternalFormat
    -> TextureSize2D
    -> PixelFormat
    -> DataType
    -> Ptr a
    -> IO (Either String TextureObject)
createTexturePtr ipf size pf dt dat = do
    [tex] <- genObjectNames 1
    textureBinding Texture2D $= Just tex
    texture Texture2D $= Enabled

    generateMipmap Texture2D $= Enabled
    texImage2D Texture2D NoProxy 0 ipf size 0 $ 
        PixelData pf dt dat
    --textureFilter Texture2D $= ((Linear', Just Nearest), Linear')
    textureFilter Texture2D $= ((Linear', Nothing), Linear')
    textureWrapMode Texture2D S $= (Repeated, Repeat)
    textureWrapMode Texture2D T $= (Repeated, Repeat)

    textureBinding Texture2D $= Nothing
    e <- get errors
    if (not $ null e)
        then return (Left $ show e)
        else return (Right $ tex)

