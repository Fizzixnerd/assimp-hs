{-# LANGUAGE ScopedTypeVariables #-}

module Asset.AssImp.Types where

import Foreign
import Foreign.C.Types
import Foreign.C.String
import Foreign.ForeignPtr
import Foreign.Marshal.Utils
import Control.Monad
import qualified Data.Vector.Storable as V

{#context lib = "assimp" prefix = "ai"#}

#include <assimp/types.h>

maxNumberTextureCoords :: Int
maxNumberTextureCoords = 8

type AIReal = {#type ai_real#}

{#pointer *aiPlane as Plane newtype#}
{#pointer *aiRay as Ray newtype#}

data Color3D = Color3D
  { r3D :: Float
  , g3D :: Float
  , b3D :: Float
  } deriving (Eq, Ord, Show)

{#pointer *aiColor3D as Color3DPtr -> Color3D#}

instance Storable Color3D where
  sizeOf _ = 3 * sizeOf (0 :: Float)
  alignment _ = alignment (0 :: Float)
  peek ptr = do
    let ptr' = castPtr ptr :: Ptr Float
    r <- peek ptr'
    g <- peekElemOff ptr' 1
    b <- peekElemOff ptr' 2
    return $ Color3D r g b
  poke ptr (Color3D r g b) = do
    let ptr' = castPtr ptr :: Ptr Float
    poke ptr' r
    pokeElemOff ptr' 1 g
    pokeElemOff ptr' 2 b

data AIString = AIString
{#pointer *aiString as AIStringPtr -> AIString#}

aiStringLength :: AIStringPtr -> IO CULong
aiStringLength = {#get struct aiString->length#}

aiStringData :: AIStringPtr -> IO (Ptr CChar)
aiStringData = {#get struct aiString->data#}

peekAIString :: AIStringPtr -> IO String
peekAIString s = if s /= nullPtr
  then do
  d <- aiStringData s
  len <- aiStringLength s
  peekCStringLen (d, fromIntegral len)
  else return ""

peekAIStringFree :: AIStringPtr -> IO String
peekAIStringFree s = do
  s' <- peekAIString s
  free s
  return s'

allocaAIString = allocaBytes {#sizeof aiString#}

{#enum aiReturn as Return
 {aiReturn_SUCCESS as ReturnSuccess,
  aiReturn_FAILURE as ReturnFailure,
  aiReturn_OUTOFMEMORY as ReturnOutOfMemory}
 omit (_AI_ENFORCE_ENUM_SIZE)
 deriving (Eq, Ord, Show)
#}

{#enum aiOrigin as Origin
 {aiOrigin_SET as OriginSet,
  aiOrigin_CUR as OriginCur,
  aiOrigin_END as OriginEnd}
 omit (_AI_ORIGIN_ENFORCE_ENUM_SIZE)
 deriving (Eq, Ord, Show)
#}

{#enum aiDefaultLogStream as DefaultLogStream
 {aiDefaultLogStream_FILE as DefaultLogStreamFile,
  aiDefaultLogStream_STDOUT as DefaultLogStreamStdOut,
  aiDefaultLogStream_STDERR as DefaultLogStreamStdErr,
  aiDefaultLogStream_DEBUGGER as DefaultLogStreamDebugger}
 omit (_AI_DLS_ENFORCE_ENUM_SIZE)
 deriving (Eq, Ord, Show)
#}

data MemoryInfo = MemoryInfo
{#pointer *aiMemoryInfo as MemoryInfoPtr -> MemoryInfo#}

#include <assimp/vector2.h>

{#pointer *aiVector2D as Vector2D newtype#}

#include <assimp/vector3.h>

data Vector3D = Vector3D
{#pointer *aiVector3D as Vector3DPtr -> Vector3D#}

peekXVector3D :: Vector3DPtr -> IO AIReal
peekXVector3D v3d = peek $ castPtr v3d

peekYVector3D :: Vector3DPtr -> IO AIReal
peekYVector3D v3d = peekElemOff (castPtr v3d) 1

peekZVector3D :: Vector3DPtr -> IO AIReal
peekZVector3D v3d = peekElemOff (castPtr v3d) 2

#include <assimp/color4.h>

data Color4D = Color4D
{#pointer *aiColor4D as Color4DPtr -> Color4D#}

#include <assimp/quaternion.h>

data Quaternion = Quaternion
{#pointer *aiQuaternion as QuaternionPtr -> Quaternion#}

#include <assimp/matrix3x3.h>

data Matrix3x3 = Matrix3x3
{#pointer *aiMatrix3x3 as Matrix3x3Ptr -> Matrix3x3#}

#include <assimp/matrix4x4.h>

data Matrix4x4 = Matrix4x4
{#pointer *aiMatrix4x4 as Matrix4x4Ptr -> Matrix4x4#}

#include <assimp/mesh.h>

data Face = Face
{#pointer *aiFace as FacePtr -> Face#}

faceNumIndices :: FacePtr -> IO CUInt
faceNumIndices = {#get struct Face->mNumIndices#}

faceIndices :: FacePtr -> IO (Ptr CUInt)
faceIndices = {#get struct Face->mIndices#}

peekFace :: FacePtr -> IO (V.Vector CUInt)
peekFace f = do
  n <- faceNumIndices f
  indices <- faceIndices f
  indices' <- (newForeignPtr_ indices)
  return $ V.unsafeFromForeignPtr0 indices' (fromIntegral n)

sizeOfFace = {#sizeof Face#}

-- | Buffer the nth Face
bufferFace :: Int -> FacePtr -> Ptr CUInt -> IO ()
bufferFace nEle f buf = do
  fIndices <- faceIndices f
  copyBytes buf fIndices (nEle * sizeOf (0 :: CUInt))

-- | Assumes separation, so that a mesh contains only one type of
-- primitive.
bufferFaces :: MeshPtr -> IO (Ptr CUInt, Int)
bufferFaces m = do
  nFaces <- fromIntegral <$> meshNumFaces m
  faces  <- meshFaces m
  if nFaces > 0
    then do
    nEle <- fromIntegral <$> faceNumIndices faces
    let nTotal = fromIntegral $ nEle * nFaces
    buf :: Ptr CUInt <- mallocArray nTotal
    forM_ [0..(nFaces - 1)] $ \n -> do
      let bufOffset = n * nEle * sizeOf (0 :: CUInt)
          facesOffset = n * sizeOfFace
      bufferFace nEle (faces `plusPtr` facesOffset) (buf `plusPtr` bufOffset)
    return (buf, nTotal)
    else return (nullPtr, 0)

data VertexWeight = VertexWeight
{#pointer *aiVertexWeight as VertexWeightPtr -> VertexWeight#}

vertexWeightVertexID :: VertexWeightPtr -> IO CUInt
vertexWeightVertexID = {#get struct VertexWeight->mVertexId#}

vertexWeightWeight :: VertexWeightPtr -> IO Float
vertexWeightWeight vw = (\(CFloat x) -> x) <$> 
                        {#get struct VertexWeight->mWeight#} vw

data Bone = Bone
{#pointer *aiBone as BonePtr -> Bone#}

boneName :: BonePtr -> IO AIStringPtr
boneName b = castPtr <$> {#get struct Bone->mName#} b

boneNumWeights :: BonePtr -> IO CUInt
boneNumWeights = {#get struct Bone->mNumWeights#}

boneWeights :: BonePtr -> IO VertexWeightPtr
boneWeights b = castPtr <$> {#get struct Bone->mWeights#} b 

boneOffsetMatrix :: BonePtr -> IO Matrix4x4Ptr
boneOffsetMatrix b = castPtr <$> {#get struct Bone->mOffsetMatrix#} b

{#enum aiPrimitiveType as PrimitiveType
 {aiPrimitiveType_POINT as PrimitiveTypePoint,
  aiPrimitiveType_LINE as PrimitiveTypeLine,
  aiPrimitiveType_TRIANGLE as PrimitiveTypeTriangle,
  aiPrimitiveType_POLYGON as PrimitiveTypePolygon}
 omit (_aiPrimitiveType_Force32Bit)
 deriving (Eq, Ord, Show)
#}

{#pointer *aiAnimMesh as AnimMesh newtype#}

{#enum aiMorphingMethod as MorphingMethod
 {aiMorphingMethod_VERTEX_BLEND as MorphingMethodVertexBlend,
  aiMorphingMethod_MORPH_NORMALIZED as MorphingMethodMorphNormalized,
  aiMorphingMethod_MORPH_RELATIVE as MorphingMethodMorphRelative}
 omit (_aiMorphingMethod_Force32Bit)
 deriving (Eq, Ord, Show)
#}

data Mesh = Mesh
{#pointer *aiMesh as MeshPtr -> Mesh#}

meshPrimitiveTypes :: MeshPtr -> IO CUInt
meshPrimitiveTypes = {#get struct Mesh->mPrimitiveTypes#}

meshNumVertices :: MeshPtr -> IO CUInt
meshNumVertices = {#get struct Mesh->mNumVertices#}

meshVertices :: MeshPtr -> IO Vector3DPtr
meshVertices = {#get struct Mesh->mVertices#}

meshNormals :: MeshPtr -> IO Vector3DPtr
meshNormals = {#get struct Mesh->mNormals#}

meshTangents :: MeshPtr -> IO Vector3DPtr
meshTangents = {#get struct Mesh->mTangents#}

meshBitangents :: MeshPtr -> IO Vector3DPtr
meshBitangents = {#get struct Mesh->mBitangents#}

meshColors :: MeshPtr -> IO (Ptr Color4DPtr)
meshColors = {#get struct Mesh->mColors#}

meshTextureCoords :: MeshPtr -> IO (Ptr Vector3DPtr)
meshTextureCoords = {#get struct Mesh->mTextureCoords#}

meshNumUVComponents :: MeshPtr -> IO (Ptr CUInt)
meshNumUVComponents = {#get struct Mesh->mNumUVComponents#}

meshNumFaces :: MeshPtr -> IO CUInt
meshNumFaces = {#get struct Mesh->mNumFaces#}

meshFaces :: MeshPtr -> IO FacePtr
meshFaces = {#get struct Mesh->mFaces#}

meshNumBones :: MeshPtr -> IO CUInt
meshNumBones = {#get struct Mesh->mNumBones#}

meshBones :: MeshPtr -> IO (Ptr BonePtr)
meshBones = {#get struct Mesh->mBones#}

meshMaterialIndex :: MeshPtr -> IO CUInt
meshMaterialIndex = {#get struct Mesh->mMaterialIndex#}

meshName :: MeshPtr -> AIStringPtr
meshName m = castPtr $ m `plusPtr` {#offsetof Mesh->mName#}

#include <assimp/cimport.h>

data LogStream = LogStream
{#pointer *aiLogStream as LogStreamPtr -> LogStream#}

data PropertyStore = PropertyStore
{#pointer *aiPropertyStore as PropertyStorePtr -> PropertyStore#}

#include <assimp/scene.h>

data Node = Node
{#pointer *aiNode as NodePtr -> Node#}

nodeName :: NodePtr -> AIStringPtr
nodeName n = castPtr $ n `plusPtr` {#offsetof Node->mName#}

nodeTransformation :: NodePtr -> Matrix4x4Ptr
nodeTransformation n = castPtr $ n `plusPtr` {#offsetof Node->mTransformation#}

nodeParent :: NodePtr -> IO NodePtr
nodeParent = {#get struct Node->mParent#}

nodeNumChildren :: NodePtr -> IO CUInt
nodeNumChildren = {#get struct Node->mNumChildren#}

nodeChildren :: NodePtr -> IO (Ptr NodePtr)
nodeChildren = {#get struct Node->mChildren#}

nodeNumMeshes :: NodePtr -> IO CUInt
nodeNumMeshes = {#get struct Node->mNumMeshes#}

nodeMeshes :: NodePtr -> IO (Ptr CUInt)
nodeMeshes = {#get struct Node->mMeshes#}

nodeMetadata :: NodePtr -> IO MetadataPtr
nodeMetadata n = castPtr <$> {#get struct Node->mMetaData#} n

data Scene = Scene
{#pointer *aiScene as ScenePtr -> Scene#}

sceneFlags :: ScenePtr -> IO CUInt
sceneFlags = {#get struct Scene->mFlags#}

sceneRootNode :: ScenePtr -> IO NodePtr
sceneRootNode = {#get struct Scene->mRootNode#}

sceneNumMeshes :: ScenePtr -> IO CUInt
sceneNumMeshes = {#get struct Scene->mNumMeshes#}

sceneMeshes :: ScenePtr -> IO (Ptr MeshPtr)
sceneMeshes = {#get struct Scene->mMeshes#}

sceneNumMaterials :: ScenePtr -> IO CUInt
sceneNumMaterials = {#get struct Scene->mNumMaterials#}

sceneMaterials :: ScenePtr -> IO (Ptr MaterialPtr)
sceneMaterials s = castPtr <$> {#get struct Scene->mMaterials#} s

sceneNumAnimations :: ScenePtr -> IO CUInt
sceneNumAnimations = {#get struct Scene->mNumAnimations#}

sceneAnimations :: ScenePtr -> IO (Ptr AnimationPtr)
sceneAnimations s = castPtr <$> {#get struct Scene->mAnimations#} s

sceneNumTextures :: ScenePtr -> IO CUInt
sceneNumTextures = {#get struct Scene->mNumTextures#}

sceneTextures :: ScenePtr -> IO (Ptr TexturePtr)
sceneTextures s = castPtr <$> {#get struct Scene->mTextures#} s

sceneNumLights :: ScenePtr -> IO CUInt
sceneNumLights = {#get struct Scene->mNumLights#}

sceneLights :: ScenePtr -> IO (Ptr LightPtr)
sceneLights s = castPtr <$> {#get struct Scene->mLights#} s

sceneNumCameras :: ScenePtr -> IO CUInt
sceneNumCameras = {#get struct Scene->mNumCameras#}

sceneCameras :: ScenePtr -> IO (Ptr CameraPtr)
sceneCameras s = castPtr <$> {#get struct Scene->mCameras#} s

sceneMetadata :: ScenePtr -> IO MetadataPtr
sceneMetadata s = castPtr <$> {#get struct Scene->mMetaData#} s

#include <assimp/texture.h>

data Texel = Texel
{#pointer *aiTexel as TexelPtr -> Texel#}

peekTexelBGRA :: TexelPtr -> IO (CUChar, CUChar, CUChar, CUChar)
peekTexelBGRA t = do
  let t' = castPtr t :: Ptr CUChar
  b <- peekElemOff t' 0
  g <- peekElemOff t' 1
  r <- peekElemOff t' 2
  a <- peekElemOff t' 3
  return (b, g, r, a)

data Texture = Texture
{#pointer *aiTexture as TexturePtr -> Texture#}

textureWidth :: TexturePtr -> IO CUInt
textureWidth = {#get Texture->mWidth#}

textureHeight :: TexturePtr -> IO CUInt
textureHeight = {#get Texture->mHeight#}

textureData :: TexturePtr -> IO TexelPtr
textureData = {#get Texture->pcData#}

textureFilename :: TexturePtr -> AIStringPtr
textureFilename t = t `plusPtr` {#offsetof Texture->mFilename#}

#include <assimp/light.h>

{#enum aiLightSourceType as LightSourceType
 {aiLightSource_UNDEFINED as LightSourceUndefined,
  aiLightSource_DIRECTIONAL as LightSourceDirectional,
  aiLightSource_POINT as LightSourcePoint,
  aiLightSource_SPOT as LightSourceSpot,
  aiLightSource_AMBIENT as LightSourceAmbient,
  aiLightSource_AREA as LightSourceArea}
 omit (_aiLightSource_Force32Bit)
 deriving (Eq, Ord, Show)
#}

data Light = Light
{#pointer *aiLight as LightPtr -> Light#}

#include <assimp/camera.h>

data Camera = Camera
{#pointer *aiCamera as CameraPtr -> Camera#}

#include <assimp/material.h>

{#enum aiTextureOp as TextureOp
 {aiTextureOp_Multiply as TextureOpMultiply,
  aiTextureOp_Add as TextureOpAdd,
  aiTextureOp_Subtract as TextureOpSubtract,
  aiTextureOp_Divide as TextureOpDivide,
  aiTextureOp_SmoothAdd as TextureOpSmoothAdd,
  aiTextureOp_SignedAdd as TextureOpSignedAdd}
 omit (_aiTextureOp_Force32Bit)
 deriving (Eq, Ord, Show)
#}

{#enum aiTextureMapMode as TextureMapMode
 {aiTextureMapMode_Wrap as TextureMapModeWrap,
  aiTextureMapMode_Clamp as TextureMapModeClamp,
  aiTextureMapMode_Decal as TextureMapModeDecal,
  aiTextureMapMode_Mirror as TextureMapModeMirror}
 omit (_aiTextureMapMode_Force32Bit)
 deriving (Eq, Ord, Show)
#}

{#enum aiTextureMapping as TextureMapping
 {aiTextureMapping_UV as TextureMappingUV,
  aiTextureMapping_SPHERE as TextureMappingSphere,
  aiTextureMapping_CYLINDER as TextureMappingCylinder,
  aiTextureMapping_BOX as TextureMappingBox,
  aiTextureMapping_PLANE as TextureMappingPlane,
  aiTextureMapping_OTHER as TextureMappingOther}
 omit (_aiTextureMapping_Force32Bit)
 deriving (Eq, Ord, Show)
#}

{#enum aiTextureType as TextureType
 {aiTextureType_NONE as TextureTypeNone,
  aiTextureType_DIFFUSE as TextureTypeDiffuse,
  aiTextureType_SPECULAR as TextureTypeSpecular,
  aiTextureType_AMBIENT as TextureTypeAmbient,
  aiTextureType_EMISSIVE as TextureTypeEmmisive,
  aiTextureType_HEIGHT as TextureTypeHeight,
  aiTextureType_NORMALS as TextureTypeNormals,
  aiTextureType_SHININESS as TextureTypeShininess,
  aiTextureType_OPACITY as TextureTypeOpacity,
  aiTextureType_DISPLACEMENT as TextureTypeDisplacement,
  aiTextureType_LIGHTMAP as TextureTypeLightMap,
  aiTextureType_REFLECTION as TextureTypeReflection,
  aiTextureType_UNKNOWN as TextureTypeUnknown}
 omit (_aiTextureType_Force32Bit)
 deriving (Eq, Ord, Show)
#}

{#enum aiShadingMode as ShadingMode
 {aiShadingMode_Flat as ShadingModeFlat,
  aiShadingMode_Gouraud as ShadingModeGouraud,
  aiShadingMode_Phong as ShadingModePhong,
  aiShadingMode_Blinn as ShadingModeBlinn,
  aiShadingMode_Toon as ShadingModeToon,
  aiShadingMode_OrenNayar as ShadingModeOrenNayar,
  aiShadingMode_Minnaert as ShadingModeMinnaert,
  aiShadingMode_CookTorrance as ShadingModeCookTorrance,
  aiShadingMode_NoShading as ShadingModeNoShading,
  aiShadingMode_Fresnel as ShadingModeFresnel}
 omit (_aiShadingMode_Force32Bit)
 deriving (Eq, Ord, Show)
#}

{#enum aiTextureFlags as TextureFlags
 {aiTextureFlags_Invert as TextureFlagsInvert,
  aiTextureFlags_UseAlpha as TextureFlagsUseAlpha,
  aiTextureFlags_IgnoreAlpha as TextureFlagsIgnoreAlpha}
 omit (_aiTextureFlags_Force32Bit)
 deriving (Eq, Ord, Show)
#}

{#enum aiBlendMode as BlendMode
 {aiBlendMode_Default as BlendModeDefault,
  aiBlendMode_Additive as BlendModeAdditive}
 omit (_aiBlendMode_Force32Bit)
 deriving (Eq, Ord, Show)
#}

{#pointer *aiUVTransform as UVTransform newtype#}

{#enum aiPropertyTypeInfo as PropertyTypeInfo
 {aiPTI_Float as PTIFloat,
  aiPTI_Double as PTIDouble,
  aiPTI_String as PTIString,
  aiPTI_Integer as PTIInteger,
  aiPTI_Buffer as PTIBuffer}
 omit (_aiPTI_Force32Bit)
 deriving (Eq, Ord, Show)
#}

data MaterialProperty = MaterialProperty
{#pointer *aiMaterialProperty as MaterialPropertyPtr -> MaterialProperty#}

data Material = Material
{#pointer *aiMaterial as MaterialPtr -> Material#}

materialProperties :: MaterialPtr -> IO (Ptr MaterialPropertyPtr)
materialProperties = {#get Material->mProperties#}

materialNumProperties :: MaterialPtr -> IO CUInt
materialNumProperties = {#get Material->mNumProperties#}

peekMaterialProperties :: MaterialPtr -> IO (V.Vector MaterialPropertyPtr)
peekMaterialProperties m = do
  ps <- materialProperties m
  n  <- materialNumProperties m
  ps' <- newForeignPtr_ ps
  return $ V.unsafeFromForeignPtr0 ps' (fromIntegral n)

{#fun aiGetMaterialTexture as getMaterialTexture
 { `MaterialPtr',
   `TextureType',
   `CInt',
   allocaAIString- `String' peekAIString*,
   alloca- `CInt' peek*,
   alloca- `CUInt' peek*,
   alloca- `AIReal' peek*,
   alloca- `CInt' peek*,
   alloca- `CInt' peek*,
   alloca- `CUInt' peek*} -> `Return'
#}

{#fun aiGetMaterialTextureCount as getMaterialTextureCount
 { `MaterialPtr',
   `TextureType' } -> `Int'
#}

materialTexture :: MaterialPtr -> TextureType -> IO (Maybe FilePath)
materialTexture m tt = do
  n <- getMaterialTextureCount m tt
  if n > 0
    then do
    (r, s, _, _, _, _, _, _) <- getMaterialTexture m tt 0
    if r == ReturnSuccess
      then return $ Just s
      else return Nothing
    else return Nothing

getMaterialFloat m s t i dummy = alloca $ \ptr -> do
  ret <- {#call aiGetMaterialFloatArray#} m s t i (castPtr $ ptr `asTypeOf` dummy) nullPtr
  if ret == 0 -- success
    then do
    val <- peek $ castPtr ptr
    return $ Just val
    else return Nothing

materialColorDiffuse' :: MaterialPtr -> IO (Maybe Color3D)
materialColorDiffuse' m =
  withCString "$clr.diffuse" $ \ptr -> getMaterialFloat m ptr 0 0 ptr

materialColorDiffuse m = maybe (Color3D 0 0 0) id <$> materialColorDiffuse' m

materialColorSpecular' :: MaterialPtr -> IO (Maybe Color3D)
materialColorSpecular' m =
  withCString "$clr.specular" $ \ptr -> getMaterialFloat m ptr 0 0 ptr

materialColorSpecular m = maybe (Color3D 0 0 0) id <$> materialColorSpecular' m

materialColorAmbient' :: MaterialPtr -> IO (Maybe Color3D)
materialColorAmbient' m =
  withCString "$clr.ambient" $ \ptr -> getMaterialFloat m ptr 0 0 ptr

materialColorAmbient m = maybe (Color3D 0 0 0) id <$> materialColorAmbient' m

materialShininess' :: MaterialPtr -> IO (Maybe Float)
materialShininess' m =
  withCString "$mat.shininess" $ \ptr -> getMaterialFloat m ptr 0 0 ptr

materialShininess m = maybe 0 id <$> materialShininess' m

materialShininessStrength' :: MaterialPtr -> IO (Maybe Float)
materialShininessStrength' m =
  withCString "$mat.shinpercent" $ \ptr -> getMaterialFloat m ptr 0 0 ptr

materialShininessStrength m = maybe 1.0 id <$> materialShininessStrength' m

#include <assimp/anim.h>

{#pointer *aiVectorKey as VectorKey newtype#}
{#pointer *aiQuatKey as QuatKey newtype#}
{#pointer *aiMeshKey as MeshKey newtype#}
{#pointer *aiMeshMorphKey as MeshMorphKey newtype#}

{#enum aiAnimBehaviour as AnimBehaviour
 {aiAnimBehaviour_DEFAULT as AnimBehaviorDefault,
  aiAnimBehaviour_CONSTANT as AnimBehaviourConstant,
  aiAnimBehaviour_LINEAR as AnimBehaviourLinear,
  aiAnimBehaviour_REPEAT as AnimBehaviourRepeat}
 omit (_aiAnimBehaviour_Force32Bit)
 deriving (Eq, Ord, Show)
#}

{#pointer *aiNodeAnim as NodeAnim newtype#}
{#pointer *aiMeshAnim as MeshAnim newtype#}
{#pointer *aiMeshMorphAnim as MeshMorphAnim newtype#}

data Animation = Animation
{#pointer *aiAnimation as AnimationPtr -> Animation#}

#include <assimp/metadata.h>

{#enum aiMetadataType as MetadataType
 {AI_BOOL as MetadataTypeBool,
  AI_INT32 as MetadataTypeInt32,
  AI_UINT64 as MetadataTypeUInt64,
  AI_FLOAT as MetadataTypeFloat,
  AI_DOUBLE as MetadataTypeDouble,
  AI_AISTRING as MetadataTypeAIString,
  AI_AIVECTOR3D as MetadataTypeAIVector3D}
 omit (FORCE_32BIT)
 deriving (Eq, Ord, Show)
#}

{#pointer *aiMetadataEntry as MetadataEntry newtype#}

data Metadata = Metadata
{#pointer *aiMetadata as MetadataPtr -> Metadata#}

#include <assimp/cexport.h>

{#pointer *aiExportDataBlob as ExportDataBlob newtype#}
{#pointer *aiExportFormatDesc as ExportFormatDesc newtype#}

#include <assimp/cfileio.h>

data FileIO = FileIO
{#pointer *aiFileIO as FileIOPtr -> FileIO#}
