module Webb.Directory.Data.Absolute where

import Prelude

import Node.Path as Path
import Webb.Stateful (localEffect)

type Path = String

newtype AbsolutePath = AP Path
derive newtype instance Eq AbsolutePath
derive newtype instance Ord AbsolutePath
derive newtype instance Show AbsolutePath

-- Resolve a path. An empty array resolves to the root directory "/"
-- A simple name without any preceding directory indicator resolves relative to
-- to the current directory; so ["hello"] becomes ["./hello"]. To get "/hello", use 
-- ["/", "hello"]
new :: Array Path -> Path -> AbsolutePath
new dir path = localEffect do 
  str <- Path.resolve dir path
  pure $ AP str
  
unwrap :: AbsolutePath -> Path
unwrap (AP str) = str

