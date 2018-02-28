module Asset.AssImp.Types where

import Foreign
import Foreign.C.Types

{#context lib = "assimp" prefix = "ai"#}

#include <assimp/types.h>

type AIReal = {#type ai_real#}

{#pointer *aiPlane as Plane newtype#}
{#pointer *aiRay as Ray newtype#}
{#pointer *aiColor3D as Color3D newtype#}
{#pointer *aiString as AIString newtype#}

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

{#pointer *aiMemoryInfo as MemoryInfo newtype#}

#include <assimp/vector2.h>

{#pointer *aiVector2D as Vector2D newtype#}

#include <assimp/vector3.h>

{#pointer *aiVector3D as Vector3D newtype#}

#include <assimp/color4.h>

{#pointer *aiColor4D as Color4D newtype#}

#include <assimp/quaternion.h>

{#pointer *aiQuaternion as Quaternion newtype#}

#include <assimp/matrix3x3.h>

{#pointer *aiMatrix3x3 as Matrix3x3 newtype#}

#include <assimp/matrix4x4.h>

{#pointer *aiMatrix4x4 as Matrix4x4 newtype#}

#include <assimp/mesh.h>

{#pointer *aiFace as Face newtype#}
{#pointer *aiVertexWeight as VertexWeight newtype#}
{#pointer *aiBone as Bone newtype#}

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
  airMorphingMethod_MORPH_RELATIVE as MorphingMethodMorphRelative}
 omit (_aiMorphingMethod_Force32Bit)
 deriving (Eq, Ord, Show)
#}

{#pointer *aiMesh as Mesh newtype#}

#include <assimp/cimport.h>

{#pointer *aiLogStream as LogStream newtype#}
{#pointer *aiPropertyStore as PropertyStore newtype#}

#include <assimp/scene.h>

{#pointer *aiNode as Node newtype#}

--nodeName :: Node -> IO AIString
--nodeName (Node n) = {#get struct Node->mName#} n

--nodeTransformation :: Node -> IO Matrix4x4
--nodeTransformation n = Matrix4x4 <$> castPtr <$> {#get struct Node->mTransformation#} n

nodeParent :: Node -> IO Node
nodeParent n = {#get struct Node->mParent#} n

{#pointer *aiScene as Scene newtype#}

sceneFlags :: Scene -> IO CUInt
sceneFlags (Scene s) = {#get struct Scene->mFlags#} s

--sceneRootNode :: Scene -> IO Node
--sceneRootNode (Scene s) = {#get struct Scene->mRootNode#} s

#include <assimp/texture.h>

{#pointer *aiTexel as Texel newtype#}
{#pointer *aiTexture as Texture newtype#}

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

{#pointer *aiLight as Light newtype#}

#include <assimp/camera.h>

{#pointer *aiCamera as Camera newtype#}

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

{#pointer *aiMaterialProperty as MaterialProperty newtype#}
{#pointer *aiMaterial as Material newtype#}

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
{#pointer *aiAnimation as Animation newtype#}

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
{#pointer *aiMetadata as Metadata newtype#}

#include <assimp/cexport.h>

{#pointer *aiExportDataBlob as ExportDataBlob newtype#}
{#pointer *aiExportFormatDesc as ExportFormatDesc newtype#}

#include <assimp/cfileio.h>

{#pointer *aiFileIO as FileIO newtype#}
