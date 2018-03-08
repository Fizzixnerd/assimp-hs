module Asset.AssImp.Import where

{#import Asset.AssImp.Types#}
import Foreign
import Foreign.C.Types

{#context lib = "assimp"#}

#include <assimp/cimport.h>

{#fun aiImportFile as importFile
 { `String',
   `CUInt' } -> `ScenePtr'
#}

importAndProcessFileFast :: String -> IO ScenePtr
importAndProcessFileFast s = importFile s 0x808B

importAndProcessFileGood :: String -> IO ScenePtr
importAndProcessFileGood s = importFile s 0x888B


{#fun aiImportFileEx as importFileEx
 { `String',
   `CUInt',
   `FileIOPtr' } -> `ScenePtr'
#}

{#fun aiImportFileExWithProperties as importFileExWithPropertes
 { `String',
   `CUInt',
   `FileIOPtr',
   `PropertyStorePtr' } -> `ScenePtr'
#}

{#fun aiImportFileFromMemory as importFileFromMemory
 { `String',
   `CUInt',
   `CUInt',
   `String' } -> `ScenePtr'
#}

{#fun aiImportFileFromMemoryWithProperties as importFileFromMemoryWithProperties
 { `String',
   `CUInt',
   `CUInt',
   `String',
   `PropertyStorePtr' } -> `ScenePtr'
#}

{#fun aiApplyPostProcessing as applyPostProcessing
 { `ScenePtr',
   `CUInt' } -> `ScenePtr'
#}

-- {#fun aiGetPredefinedLogStream as ^
--  { `DefaultLogStream',
--   `String' } -> `LogStreamPtr'
-- #}

{#fun aiAttachLogStream as attachLogStream
 { `LogStreamPtr' } -> `()'
#}

{#fun aiEnableVerboseLogging as enableVerboseLogging
 { `Bool' } -> `()'
#}

{#fun aiDetachLogStream as detachLogStream
 { `LogStreamPtr' } -> `Return'
#}

{#fun aiDetachAllLogStreams as detachAllLogStreams
 {} -> `()'
#}

{#fun aiReleaseImport as releaseImport
 { `ScenePtr' } -> `()'
#}

{#fun aiGetErrorString as getErrorString
 {} -> `String'
#}

{#fun aiIsExtensionSupported as isExtensionSupported
 { `String' } -> `Bool'
#}

{#fun aiGetExtensionList as getExtensionList
 { `AIStringPtr' } -> `()'
#}

-- {#fun aiGetMemoryRequirements as ^
-- { `ScenePtr',
--   alloca- `MemoryInfoPtr' peek*} -> `()'
-- #}

{#fun aiCreatePropertyStore as createPropertyStore
 {} -> `PropertyStorePtr'
#}

{#fun aiReleasePropertyStore as releasePropertyStore
 { `PropertyStorePtr' } -> `()'
#}

-- {#fun aiSetImportPropertyInteger
