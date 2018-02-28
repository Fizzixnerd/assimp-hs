module Lib where

import Asset.AssImp.Types
import Asset.AssImp.Import
import Control.Monad
import Foreign

someFunc :: IO ()
someFunc = do
  scene <- importFile "/home/matt/src/haskell-game/res/models/male.obj" 0
  nm <- sceneNumMeshes scene
  unless (nm == 2) $
    error "Wrong number of meshes."
  meshes <- sceneMeshes scene
  m1 <- peekElemOff meshes 0
  m2 <- peekElemOff meshes 1
  print =<< meshNumVertices m1
  print =<< meshNumVertices m2
  root <- sceneRootNode scene
  print =<< nodeNumMeshes root
  print =<< nodeNumChildren root
  children <- nodeChildren root
  c1 <- peekElemOff children 0
  c2 <- peekElemOff children 1
  -- putStrLn =<< peekAIString =<< nodeName c1
  print =<< nodeNumMeshes c1
  print =<< nodeNumChildren c1
  -- putStrLn =<< peekAIString =<< nodeName c2
  print =<< nodeNumMeshes c2
  print =<< nodeNumChildren c2
  print =<< meshNumFaces m1
  print =<< meshNumFaces m2
  
  releaseImport scene
