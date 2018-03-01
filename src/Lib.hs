{-# LANGUAGE ScopedTypeVariables #-}

module Lib where

import Asset.AssImp.Types
import Asset.AssImp.Import
import Control.Monad
import Foreign
import Foreign.C.Types

someFunc :: IO ()
someFunc = do
  scene <- importAndProcessFileGood "/home/matt/src/haskell-game/res/models/simple-cube-2.obj"
  print =<< sceneNumMeshes scene
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
  print =<< peekFace =<< meshFaces m1
  print =<< peekXVector3D =<< meshVertices m1
  print =<< peekYVector3D =<< meshVertices m1
  print =<< peekZVector3D =<< meshVertices m1

  print =<< (\x -> peekXVector3D $ plusPtr x 24) =<< peek =<< meshTextureCoords m1
  print =<< peekYVector3D =<< peek =<< meshTextureCoords m1
  print =<< peekZVector3D =<< peek =<< meshTextureCoords m1

  fces <- bufferFaces m1
  print =<< (peek $ fst fces)
  print $ snd fces
  
  releaseImport scene
