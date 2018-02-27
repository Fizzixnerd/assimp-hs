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

{#pointer *aiVector3 as ^ newtype#}

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
  aiTextureMapping_PLANE as TextureMappingPlane
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
  aiTextureType_DISPLACEMENT as TextureTypeDisplacement
  aiTextureType_LIGHTMAP as TextureTypeLightMap,
  aiTextureType_REFLECTION as TextureTypeReflection,
  aiTextureType_UNKNOWN as TextureTypeUnknown}
 omit (_aiTextureType_Force32Bit)
 deriving (Eq, Ord, Show)
#}

{#enum aiShadingMode as ^
--

-- |

--
{
    /** Flat shading. Shading is done on per-face base,
     *  diffuse only. Also known as 'faceted shading'.
     */
    aiShadingMode_Flat = 0x1,

    /** Simple Gouraud shading.
     */
    aiShadingMode_Gouraud = 0x2,

    /** Phong-Shading -
     */
    aiShadingMode_Phong = 0x3,

    /** Phong-Blinn-Shading
     */
    aiShadingMode_Blinn = 0x4,

    /** Toon-Shading per pixel
     *
     *  Also known as 'comic' shader.
     */
    aiShadingMode_Toon = 0x5,

    /** OrenNayar-Shading per pixel
     *
     *  Extension to standard Lambertian shading, taking the
     *  roughness of the material into account
     */
    aiShadingMode_OrenNayar = 0x6,

    /** Minnaert-Shading per pixel
     *
     *  Extension to standard Lambertian shading, taking the
     *  "darkness" of the material into account
     */
    aiShadingMode_Minnaert = 0x7,

    /** CookTorrance-Shading per pixel
     *
     *  Special shader for metallic surfaces.
     */
    aiShadingMode_CookTorrance = 0x8,

    /** No shading at all. Constant light influence of 1.0.
    */
    aiShadingMode_NoShading = 0x9,

     /** Fresnel shading
     */
    aiShadingMode_Fresnel = 0xa,


#ifndef SWIG
    _aiShadingMode_Force32Bit = INT_MAX
#endif
};

enum aiTextureFlags
{
    /** The texture's color values have to be inverted (componentwise 1-n)
     */
    aiTextureFlags_Invert = 0x1,

    /** Explicit request to the application to process the alpha channel
     *  of the texture.
     *
     *  Mutually exclusive with #aiTextureFlags_IgnoreAlpha. These
     *  flags are set if the library can say for sure that the alpha
     *  channel is used/is not used. If the model format does not
     *  define this, it is left to the application to decide whether
     *  the texture alpha channel - if any - is evaluated or not.
     */
    aiTextureFlags_UseAlpha = 0x2,

    /** Explicit request to the application to ignore the alpha channel
     *  of the texture.
     *
     *  Mutually exclusive with #aiTextureFlags_UseAlpha.
     */
    aiTextureFlags_IgnoreAlpha = 0x4,

#ifndef SWIG
      _aiTextureFlags_Force32Bit = INT_MAX
#endif
};

enum aiBlendMode
{
    /**
     *  Formula:
     *  @code
     *  SourceColor*SourceAlpha + DestColor*(1-SourceAlpha)
     *  @endcode
     */
    aiBlendMode_Default = 0x0,

    /** Additive blending
     *
     *  Formula:
     *  @code
     *  SourceColor*1 + DestColor*1
     *  @endcode
     */
    aiBlendMode_Additive = 0x1,

    // we don't need more for the moment, but we might need them
    // in future versions ...

#ifndef SWIG
    _aiBlendMode_Force32Bit = INT_MAX
#endif
};

{#pointer *aiUVTransform as ^ newtype#}

enum aiPropertyTypeInfo
{
    /** Array of single-precision (32 Bit) floats
     *
     *  It is possible to use aiGetMaterialInteger[Array]() (or the C++-API
     *  aiMaterial::Get()) to query properties stored in floating-point format.
     *  The material system performs the type conversion automatically.
    */
    aiPTI_Float   = 0x1,

    /** Array of double-precision (64 Bit) floats
     *
     *  It is possible to use aiGetMaterialInteger[Array]() (or the C++-API
     *  aiMaterial::Get()) to query properties stored in floating-point format.
     *  The material system performs the type conversion automatically.
    */
    aiPTI_Double   = 0x2,

    /** The material property is an aiString.
     *
     *  Arrays of strings aren't possible, aiGetMaterialString() (or the
     *  C++-API aiMaterial::Get()) *must* be used to query a string property.
    */
    aiPTI_String  = 0x3,

    /** Array of (32 Bit) integers
     *
     *  It is possible to use aiGetMaterialFloat[Array]() (or the C++-API
     *  aiMaterial::Get()) to query properties stored in integer format.
     *  The material system performs the type conversion automatically.
    */
    aiPTI_Integer = 0x4,


    /** Simple binary buffer, content undefined. Not convertible to anything.
    */
    aiPTI_Buffer  = 0x5,


     /** This value is not used. It is just there to force the
     *  compiler to map this enum to a 32 Bit integer.
     */
#ifndef SWIG
     _aiPTI_Force32Bit = INT_MAX
#endif
};

{#pointer *aiMaterialProperty as ^ newtype#}
{#pointer *aiMaterial as ^ newtype#}

#include <assimp/anim.h>

{#pointer *aiVectorKey as ^ newtype#}
{#pointer *aiQuatKey as ^ newtype#}
{#pointer *aiMeshKey as ^ newtype#}
{#pointer *aiMeshMorphKey as newtype#}

enum aiAnimBehaviour
{
    /** The value from the default node transformation is taken*/
    aiAnimBehaviour_DEFAULT  = 0x0,

    /** The nearest key value is used without interpolation */
    aiAnimBehaviour_CONSTANT = 0x1,

    /** The value of the nearest two keys is linearly
     *  extrapolated for the current time value.*/
    aiAnimBehaviour_LINEAR   = 0x2,

    /** The animation is repeated.
     *
     *  If the animation key go from n to m and the current
     *  time is t, use the value at (t-n) % (|m-n|).*/
    aiAnimBehaviour_REPEAT   = 0x3,

    /** This value is not used, it is just here to force the
     *  the compiler to map this enum to a 32 Bit integer  */
#ifndef SWIG
    _aiAnimBehaviour_Force32Bit = INT_MAX
#endif
};

{#pointer *aiNodeAnim as ^ newtype#}
{#pointer *aiMeshAnim as ^ newtype#}
{#pointer *aiMeshMorphAnim as ^ newtype#}
{#pointer *aiAnimation as ^ newtype#}

#include <assimp/metadata.h>

enum aiMetadataType {
    AI_BOOL       = 0,
    AI_INT32      = 1,
    AI_UINT64     = 2,
    AI_FLOAT      = 3,
    AI_DOUBLE     = 4,
    AI_AISTRING   = 5,
    AI_AIVECTOR3D = 6,

#ifndef SWIG
    FORCE_32BIT = INT_MAX
#endif
}

{#pointer aiMetadataEntry as ^ newtype#}
{#pointer aiMetadata as ^ newtype#}
