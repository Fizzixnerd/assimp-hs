module Asset.AssImp.Import where

{#import Asset.AssImp.Types#}

#include <assimp/cimport.h>

{#fun aiImportFile as ^
 { `String',
   `Word' } -> `Scene'
#}

{#fun aiImportFileEx as ^
 { `String',
   `Word',
   `FileIO' } -> `Scene'
#}

{#fun aiImportFileExWithProperties as ^
 { `String',
   `Word',
   `FileIO',
   `PropertyStore' } -> `Scene'
#}

{#fun aiImportFileFromMemory as ^
 { `String',
   `Word',
   `Word',
   `String' } -> `Scene'
#}

{#fun aiImportFileFromMemoryWithProperties as ^
 { `String',
   `Word',
   `Word',
   `String',
   `PropertyStore' } -> `Scene'
#}

{#fun aiApplyPostProcessing as ^
 { `Scene',
   `Word' } -> `Scene'
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

{#fun aiDetatchAllLogStreams as ^
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
