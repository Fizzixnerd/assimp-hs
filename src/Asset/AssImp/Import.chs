module Asset.AssImp.Import where

{#import Asset.AssImp.Types#}
import Foreign.C.Types

{#context lib = "assimp"#}

#include <assimp/cimport.h>

{#fun aiImportFile as importFile
 { `String',
   `CUInt' } -> `Scene'
#}

{#fun aiImportFileEx as importFileEx
 { `String',
   `CUInt',
   `FileIO' } -> `Scene'
#}

{#fun aiImportFileExWithProperties as ^
 { `String',
   `CUInt',
   `FileIO',
   `PropertyStore' } -> `Scene'
#}

{#fun aiImportFileFromMemory as ^
 { `String',
   `CUInt',
   `CUInt',
   `String' } -> `Scene'
#}

{#fun aiImportFileFromMemoryWithProperties as ^
 { `String',
   `CUInt',
   `CUInt',
   `String',
   `PropertyStore' } -> `Scene'
#}

{#fun aiApplyPostProcessing as ^
 { `Scene',
   `CUInt' } -> `Scene'
#}

{#fun aiGetPredefinedLogStream as ^
 { `DefaultLogStream',
   `String' } -> `LogStream'
#}

{#fun aiAttachLogStream as ^
 { `LogStream' } -> `()'
#}

{#fun aiEnableVerboseLogging as ^
 { `Bool' } -> `()'
#}

{#fun aiDetachLogStream as ^
 { `LogStream' } -> `Return'
#}

{#fun aiDetachAllLogStreams as ^
 {} -> `()'
#}

{#fun aiReleaseImport as ^
 { `Scene' } -> `()'
#}

{#fun aiGetErrorString as ^
 {} -> `String'
#}

{#fun aiIsExtensionSupported as ^
 { `String' } -> `Bool'
#}

{#fun aiGetExtensionList as ^
 { `AIString' } -> `()'
#}

{#fun aiGetMemoryRequirements as ^
 { `Scene',
   alloca- `MemoryInfo' peek*} -> `()'
#}

{#fun aiCreatePropertyStore as ^
 {} -> `PropertyStore'
#}

{#fun aiReleasePropertyStore as ^
 { `PropertyStore' } -> `()'
#}

-- {#fun aiSetImportPropertyInteger
