{-# LANGUAGE ExistentialQuantification #-}
module Graphics.Rendering.OpenGL.Util.Shaders
    ( loadShader
    , loadShaderFromString
    , loadProgram
    , createProgram
    , ProgramEnvironment(..)
    , defaultProgramEnvironment
    , UniformSetting(..) 
    , uniformArray
    , Sampler(..)
    , setProgramEnv
    , setUniform
    , setUniformArray
    , setUniformValue
    , setUniformMatrix
    , setSamplers
) where

import Control.Monad
import Control.Applicative
import Foreign.Ptr
import Foreign.Storable
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import System.Directory
import System.FilePath ((</>))

import Graphics.Rendering.OpenGL
import Graphics.Rendering.OpenGL.GL.ByteString
import Graphics.Rendering.OpenGL.Raw.Core31

import Misc 

----------------------------------------------------------------------

type UniformName = String
data UniformSetting
    = forall a. Uniform a => UniformValue UniformName a
    | forall a. Uniform a => UniformArray UniformName GLsizei (Ptr a)

  -- 2D texture
data Sampler = Sampler UniformName TextureObject
-- = Sampler1D UniformName TextureObject
-- | Sampler2D UniformName TextureObject
-- | Sampler3D UniformName TextureObject

data ProgramEnvironment = ProgramEnvironment
    { peProgram   :: Maybe Program
    , peUniforms  :: [UniformSetting]
    , peSamplers  :: [Sampler] }

defaultProgramEnvironment :: ProgramEnvironment
defaultProgramEnvironment = ProgramEnvironment
    { peProgram   = Nothing
    , peUniforms  = []
    , peSamplers  = [] }

----------------------------------------------------------------------

setUniform :: UniformSetting -> IO ()
setUniform us = do
    let gl p = get . uniformLocation p
    mprog <- get currentProgram
    maybeDo_ mprog $ \prog -> case us of
        UniformValue n v   -> gl prog n >>= \l -> uniform l $= v
        UniformArray n s v -> gl prog n >>= \l -> uniformv l s v

setUniformArray :: (Storable a, Uniform a) => String -> [a] -> IO ()
setUniformArray n = setUniform <=< uniformArray n

setUniformValue :: (Storable a, Uniform a) => String -> a -> IO ()
setUniformValue = setUniform .: UniformValue

setUniformMatrix :: String -> GLmatrix GLfloat -> IO ()
setUniformMatrix n mx = do
    mprog <- get currentProgram
    maybeDo_ mprog $ \_ -> do
        -- programID is unexposed again (damn you Sven Panne!)
        pid <- getPID
        ul <- withGLstring n $ glGetUniformLocation pid
        ma <- getMatrixComponents ColumnMajor mx
        withArray ma $ glUniformMatrix4fv ul 1 (fromIntegral gl_FALSE)
    where
    getPID :: IO GLuint
    getPID = alloca $ \buf -> do
        glGetIntegerv gl_CURRENT_PROGRAM buf
        peekElemOff (castPtr buf) 0


setUniforms :: Program -> [UniformSetting] -> IO ()
setUniforms prog = mapM_ setu
    where
        setu (UniformValue n v) = gl n >>= \l -> uniform l $= v
        setu (UniformArray n s v) = gl n >>= \l -> uniformv l s v
        gl = get . uniformLocation prog

setSamplers :: Program -> [Sampler] -> IO ()
setSamplers prog = mapM_ setSampler . zip [0..]
    where setSampler (i, Sampler name tex) = do
            location <- get $ uniformLocation prog name
            activeTexture $= TextureUnit i
            textureBinding Texture2D $= Just tex
            uniform location $= TexCoord1 (fromIntegral i :: GLint)

setProgramEnv :: ProgramEnvironment -> IO ()
setProgramEnv env = maybeDo_ (peProgram env) $ \prog -> do
    currentProgram $= Just prog
    setUniforms prog (peUniforms env)
    setSamplers prog (peSamplers env)

uniformArray :: (Uniform a, Storable a)
             => String -> [a] -> IO UniformSetting
uniformArray name v = newArray v >>= \ptr ->
    return $ UniformArray name (fromIntegral $ length v) ptr

loadShader :: ShaderType ->  FilePath -> IO (Either String Shader)
loadShader t fn = do
    exists <- doesFileExist fn
    if exists
        then either (Left . (++ ("File: " ++ fn))) Right <$>
             (loadShaderFromString t =<< readFile fn)
        else return . Left $ "File does not exist: " ++ fn

loadShaderFromString :: ShaderType -> String -> IO (Either String Shader)
loadShaderFromString t src = do
    s <- createShader t
    shaderSource s $= map ((++ "\n") . stripComment) (lines src)
    compileShader s
    cs <- get $ compileStatus s
    if cs then return (Right s) else do
        l <- get $ shaderInfoLog s
        return . Left $ "Unable to compile shader. \n" ++ l
    where
        stripComment [] = []
        stripComment (a:[]) = [a]
        stripComment ('/':'/':_) = []
        stripComment (a:as) = a : stripComment as

loadProgram :: FilePath -> FilePath -> IO (Either String Program)
loadProgram vn fn = do
    vp <- loadShader VertexShader vn
    fp <- loadShader FragmentShader fn
    either (return . Left) (createProgramFrom) $ (,) <$> vp <*> fp

-- create program from pair (VertexShader, FragmentShader)
createProgramFrom :: (Shader, Shader) -> IO (Either String Program)
createProgramFrom (vp, fp) = do
    prog <- createProgram -- [prog] <- genObjectNames 1
    attachShader prog vp
    attachShader prog fp

    linkProgram prog
    ls <- get $ linkStatus prog
    if ls then return (Right prog) else do
        l <- get $ programInfoLog prog
        return . Left $ "Unable to link program. \n" ++ l
