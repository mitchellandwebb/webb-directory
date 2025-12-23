module Webb.Directory.Internal.Directory where

import Prelude
import Webb.State.Prelude

import Control.Monad.State (StateT)
import Data.Array as Array
import Effect.Aff (Aff)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Node.FS.Aff as File
import Node.FS.Stats as Stat
import Node.Process as Process
import Webb.Directory.Data.Absolute (AbsolutePath)
import Webb.Directory.Data.Absolute as Abs
import Webb.Directory.Data.Stack (Stack)
import Webb.Directory.Data.Stack as Stack
import Webb.Monad.Prelude (forceMaybe')


{- Define internal directory structures in terms of monads. We hide these from
  the end user. These describe the operations that can be performed on the stack,
  in concert with effects on the file system itself.
-}

type State = { stack :: ShowRef Stack }
type Path = String

type Prog = StateT State Aff

-- Initialize the stack with the current working directory
init :: Prog Unit
init = do
  this <- mread
  cwd <- Process.cwd # liftEffect
  Stack.push (Abs.new [] cwd) :> this.stack
  
-- Push the next directory onto the stack, and navigate there.
push :: String -> Prog Unit
push dir = do
  this <- mread
  let path = Abs.new [] dir
  chdir path
  Stack.push path :> this.stack

-- Pop to the previous working directory.
pop :: Prog Unit
pop = do
  this <- mread
  size <- Stack.size <: this.stack
  when (size > 1) do
    Stack.pop :> this.stack
    next <- current
    chdir $ Abs.new [] next

-- Pop to the first working directory.
popToFirst :: Prog Unit
popToFirst = do
  this <- mread
  size <- Stack.size <: this.stack
  when (size > 1) do
    Stack.popToFirst :> this.stack
    next <- current
    chdir $ Abs.new [] next
    
-- Replace the existing current directory and navigate to it.
replace :: String -> Prog Unit
replace dir = do
  this <- mread
  let path = Abs.new [] dir
  chdir path
  Stack.replace path :> this.stack
  
chdir :: AbsolutePath -> Prog Unit
chdir path = do
  let str = Abs.unwrap path
  Process.chdir str # liftEffect
  
-- What is the current directory, according to our stack?
current :: Prog String
current = do
  this <- mread
  mpath <- Stack.peek <: this.stack
  path <- forceMaybe' "Expected a stack top to exist" mpath
  pure $ Abs.unwrap path
  
currentPaths :: Prog (Array String)
currentPaths = do
  path <- current
  paths <- File.readdir path # liftAff
  pure $ paths <#> Abs.new [] >>> Abs.unwrap

-- What are the files in the current directory?
-- If we have them, we can operate on them.
files :: Prog (Array String)
files = do
  paths <- currentPaths
  Array.filterA isFile paths

-- What are the sub-directories in the current directory?
-- If we have them, we can operate on them.
dirs :: Prog (Array String)
dirs = do 
  paths <- currentPaths
  Array.filterA isDirectory paths
  
isFile :: String -> Prog Boolean
isFile path = do 
  stat <- File.stat path # liftAff
  pure $ Stat.isFile stat

isDirectory :: String -> Prog Boolean
isDirectory path = do
  stat <- File.stat path # liftAff
  pure $ Stat.isDirectory stat