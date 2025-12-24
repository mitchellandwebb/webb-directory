module Webb.Directory.Internal.Action where

import Prelude

import Control.Monad.State (StateT)
import Control.Monad.State as State
import Data.Foldable (for_)
import Effect.Aff (Aff, catchError)
import Effect.Aff.Class (liftAff)
import Node.FS.Aff as FS
import Node.FS.Perms (permsReadWrite)
import Node.FS.Stats as Stat
import Webb.Directory.Data.Absolute (AbsPath, AbsolutePath)
import Webb.Directory.Data.Absolute as Abs
import Webb.Directory.Visitor (newVisitor)
import Webb.Directory.Visitor as Visitor


type State = {}

type Prog = StateT State Aff 

eval :: forall a. State -> Prog a -> Aff a
eval state prog = State.evalStateT prog state

-- Attempt to forcibly remove all entries in the directory at the given path, 
-- but only if it exists. 
removeDirChildren :: AbsolutePath -> Prog Unit
removeDirChildren path = do
  whenM (existsDir path) do
    visitor <- newVisitor path
    files <- Visitor.files visitor
    dirs <- Visitor.dirs visitor
    for_ dirs \dir -> do forceRemove dir
    for_ files \file -> do forceRemove file
          
-- Remove the item. Return Boolean indicating whether removal succeeded without
-- error.
forceRemove :: AbsolutePath -> Prog Boolean
forceRemove path = liftAff do
  catchError (do 
    let path' = Abs.unwrap path
    FS.rm' path'
      { force: true,recursive: true, maxRetries: 2, retryDelay: 100 }
    pure true 
  ) (\_ -> do 
    pure false 
  )

-- Remove the directory itself at the given path, but only if it exists.
removeDir :: AbsolutePath -> Prog Unit
removeDir path = do
  whenM (existsDir path) do
    let dirPath = Abs.unwrap path
    liftAff do
      FS.rm' dirPath 
        { force: true,recursive: true, maxRetries: 2, retryDelay: 100 }
        
-- Resets the directory to a fresh state, destroying the directory
-- entirely in the process and re-creating it.
resetDir :: AbsolutePath -> Prog Unit
resetDir path = do
  removeDir path
  liftAff do
    let dirPath = Abs.unwrap path
    FS.mkdir' dirPath { recursive: true, mode: permsReadWrite }
  
-- Does directory exist at the specified absolute path?
existsDir :: AbsPath -> Prog Boolean
existsDir path = do 
  catchError (do 
    let dirPath = Abs.unwrap path
    stats <- FS.stat dirPath # liftAff
    pure $ Stat.isDirectory stats
  ) (\_ -> 
    pure false
  )
  