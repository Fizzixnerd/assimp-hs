{-# LANGUAGE ScopedTypeVariables #-}

module Lib where

import Asset.AssImp.Types
import Asset.AssImp.Import
import Control.Monad
import Foreign
import Foreign.C.Types

someFunc :: IO ()
someFunc = do
  scene <- importFile "/home/matt/src/haskell-game/res/models/simple-cube-2.obj" 0
  nm <- sceneNumMeshes scene
  unless (nm == 1) $
    error "Wrong number of meshes."
  meshes <- sceneMeshes scene
  m1 <- peekElemOff meshes 0
  print =<< meshNumVertices m1
  root <- sceneRootNode scene
  print =<< nodeNumMeshes root
  print =<< nodeNumChildren root
  children <- nodeChildren root
  c1 <- peekElemOff children 0
  -- putStrLn =<< peekAIString =<< nodeName c1
  print =<< nodeNumMeshes c1
  print =<< nodeNumChildren c1
  -- putStrLn =<< peekAIString =<< nodeName c2
  print =<< meshNumFaces m1

  bufferFaces m1
  
  releaseImport scene

