{-#  OPTIONS_GHC -fwarn-incomplete-patterns #-}
module Ansi where
-- в этом модуле определены константы, связанные с ANSI escape sequences
-- i add elements as i need them so
data Sequence =
    SGR Int deriving ( Show )




