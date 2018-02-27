module Asset.AssImp.Types where

{#context lib = "assimp" prefix = "ai"#}

#include <assimp/types.h>

type AIReal = {#type ai_real#}

{#pointer *aiPlane as ^ newtype#}
{#pointer *aiRay as ^ newtype#}
{#pointer *aiColor3D as ^ newtype#}
{#pointer *aiString as AIString newtype#}

{#enum aiReturn as ^
 {aiReturn_SUCCESS as ReturnSuccess,
  aiReturn_FAILURE as ReturnFailure,
  aiReturn_OUTOFMEMORY as ReturnOutOfMemory}
 omit (_AI_ENFORCE_ENUM_SIZE)
 deriving (Eq, Ord, Show)
#}

{#enum aiOrigin as ^
 {aiOrigin_SET as OriginSet,
  aiOrigin_CUR as OriginCur,
  aiOrigin_END as OriginEnd}
 omit (_AI_ORIGIN_ENFORCE_ENUM_SIZE)
 deriving (Eq, Ord, Show)
#}

{#enum aiDefaultLogStream as ^
 {aiDefaultLogStream_FILE as DefaultLogStreamFile,
  aiDefaultLogStream_STDOUT as DefaultLogStreamStdOut,
  aiDefaultLogStream_STDERR as DefaultLogStreamStdErr,
  aiDefaultLogStream_DEBUGGER as DefaultLogStreamDebugger}
 omit (_AI_DLS_ENFORCE_ENUM_SIZE)
 deriving (Eq, Ord, Show)
#}

{#pointer *aiMemoryInfo as ^ newtype#}

#include <assimp/vector2.h>

{#pointer *aiVector2D as ^ newtype#}

#include <assimp/vector3.h>

{#pointer *aiVector3D as ^ newtype#}

#include <assimp/color4.h>

{#pointer *aiColor4D as ^ newtype#}

#include <assimp/quaternion.h>

{#pointer *aiQuaternion as ^ newtype#}

#include <assimp/matrix3x3.h>

{#pointer *aiMatrix3x3 as ^ newtype#}

#include <assimp/matrix4x4.h>

{#pointer *aiMatrix4x4 as ^ newtype#}

#include <assimp/mesh.h>

{#pointer *aiFace as ^ newtype#}
{#pointer *aiVertexWeight as ^ newtype#}
{#pointer *aiBone as ^ newtype#}

{#enum aiPrimitiveType as ^
 {aiPrimitiveType_POINT as PrimitiveTypePoint,
  aiPrimitiveType_LINE as PrimitiveTypeLine,
  aiPrimitiveType_TRIANGLE as PrimitiveTypeTriangle,
  aiPrimitiveType_POLYGON as PrimitiveTypePolygon}
 omit (_aiPrimitiveType_Force32Bit)
 deriving (Eq, Ord, Show)
#}

{#pointer *aiAnimMesh as ^ newtype#}

{#enum aiMorphingMethod as ^
 {aiMorphingMethod_VERTEX_BLEND as MorphingMethodVertexBlend,
  aiMorphingMethod_MORPH_NORMALIZED as MorphingMethodMorphNormalized,
  airMorphingMethod_MORPH_RELATIVE as MorphingMethodMorphRelative}
 omit (_aiMorphingMethod_Force32Bit)
 deriving (Eq, Ord, Show)
#}

{#pointer *aiMesh as ^ newtype#}

#include <assimp/cimport.h>

{#pointer *aiLogStream as ^ newtype#}
{#pointer *aiPropertyStore as ^ newtype#}

#include <assimp/scene.h>

{#pointer *aiNode as ^ newtype#}
{#pointer *aiScene as ^ newtype#}

#include <assimp/texture.h>

{#pointer *aiTexel as ^ newtype#}
{#pointer *aiTexture as ^ newtype#}

#include <assimp/light.h>

{#enum aiLightSourceType as ^
 {aiLightSource_UNDEFINED as LightSourceUndefined,
  aiLightSource_DIRECTIONAL as LightSourceDirectional,
  aiLightSource_POINT as LightSourcePoint,
  aiLightSource_SPOT as LightSourceSpot,
  aiLightSource_AMBIENT as LightSourceAmbient,
  aiLightSource_AREA as LightSourceArea}
 omit (_aiLightSource_Force32Bit)
 deriving (Eq, Ord, Show)
#}

{#pointer *aiLight as ^ newtype#}

#include <assimp/camera.h>

{#pointer *aiCamera as ^ newtype#}

#include <assimp/material.h>

{#enum aiTextureOp as ^
 {aiTextureOp_Multiply as TextureOpMultiply,
  aiTextureOp_Add as TextureOpAdd,
  aiTextureOp_Subtract as TextureOpSubtract,
  aiTextureOp_Divide as TextureOpDivide,
  aiTextureOp_SmoothAdd as TextureOpSmoothAdd,
  aiTextureOp_SignedAdd as TextureOpSignedAdd}
 omit (_aiTextureOp_Force32Bit)
 deriving (Eq, Ord, Show)
#}

{#enum aiTextureMapMode as ^
 {aiTextureMapMode_Wrap as TextureMapModeWrap,
  aiTextureMapMode_Clamp as TextureMapModeClamp,
  aiTextureMapMode_Decal as TextureMapModeDecal,
  aiTextureMapMode_Mirror as TextureMapModeMirror}
 omit (_aiTextureMapMode_Force32Bit)
 deriving (Eq, Ord, Show)
#}

{#enum aiTextureMapping as ^
 {aiTextureMapping_UV as TextureMappingUV,
  aiTextureMapping_SPHERE as TextureMappingSphere,
  aiTextureMapping_CYLINDER as TextureMappingCylinder,
  aiTextureMapping_BOX as TextureMappingBox,
  aiTextureMapping_PLANE as TextureMappingPlane,
  aiTextureMapping_OTHER as TextureMappingOther}
 omit (_aiTextureMapping_Force32Bit)
 deriving (Eq, Ord, Show)
#}

{#enum aiTextureType as ^
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

{#enum aiShadingMode as ^
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

{#enum aiTextureFlags as ^
 {aiTextureFlags_Invert as TextureFlagsInvert,
  aiTextureFlags_UseAlpha as TextureFlagsUseAlpha,
  aiTextureFlags_IgnoreAlpha as TextureFlagsIgnoreAlpha}
 omit (_aiTextureFlags_Force32Bit)
 deriving (Eq, Ord, Show)
#}

{#enum aiBlendMode as ^
 {aiBlendMode_Default as BlendModeDefault,
  aiBlendMode_Additive as BlendModeAdditive}
 omit (_aiBlendMode_Force32Bit)
 deriving (Eq, Ord, Show)
#}

{#pointer *aiUVTransform as ^ newtype#}

{#enum aiPropertyTypeInfo as ^
 {aiPTI_Float as PTIFloat,
  aiPTI_Double as PTIDouble,
  aiPTI_String as PTIString,
  aiPTI_Integer as PTIInteger,
  aiPTI_Buffer as PTIBuffer}
 omit (_aiPTI_Force32Bit)
 deriving (Eq, Ord, Show)
#}

{#pointer *aiMaterialProperty as ^ newtype#}
{#pointer *aiMaterial as ^ newtype#}

#include <assimp/anim.h>

{#pointer *aiVectorKey as ^ newtype#}
{#pointer *aiQuatKey as ^ newtype#}
{#pointer *aiMeshKey as ^ newtype#}
{#pointer *aiMeshMorphKey as ^ newtype#}

{#enum aiAnimBehaviour as ^
 {aiAnimBehaviour_DEFAULT as AnimBehaviorDefault,
  aiAnimBehaviour_CONSTANT as AnimBehaviourConstant,
  aiAnimBehaviour_LINEAR as AnimBehaviourLinear,
  aiAnimBehaviour_REPEAT as AnimBehaviourRepeat}
 omit (_aiAnimBehaviour_Force32Bit)
 deriving (Eq, Ord, Show)
#}

{#pointer *aiNodeAnim as ^ newtype#}
{#pointer *aiMeshAnim as ^ newtype#}
{#pointer *aiMeshMorphAnim as ^ newtype#}
{#pointer *aiAnimation as ^ newtype#}

#include <assimp/metadata.h>

{#enum aiMetadataType as ^
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

{#pointer *aiMetadataEntry as ^ newtype#}
{#pointer *aiMetadata as ^ newtype#}
