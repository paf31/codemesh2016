module Helpers where

import Prelude
import Control.Coroutine (consumer, Consumer, Producer, emit)
import Control.Monad.Aff (Aff)
import Control.Monad.Eff.Class (class MonadEff, liftEff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Trans.Class (lift)
import Data.Maybe (Maybe(..))
import Node.Buffer (BUFFER, readString, create)
import Node.Encoding (Encoding)
import Node.FS (FS, FileFlags(R), BufferLength)
import Node.FS.Aff (fdRead, fdOpen)
import Node.Path (FilePath)

printer
  :: ∀ a m eff
  . MonadEff (console :: CONSOLE | eff) m
  => Consumer String m a
printer = consumer \x -> do
  liftEff (log x)
  pure Nothing

readFile
  :: ∀ eff
   . Encoding
  -> FilePath
  -> BufferLength
  -> Producer
       String
       (Aff
         ( fs :: FS
         , buffer :: BUFFER
         | eff
         ))
       Unit
readFile enc path chunkSize = do
  fd <- lift (fdOpen path R Nothing)
  let loop offset = do
        buf <- lift (liftEff (create chunkSize))
        bytesRead <- lift (fdRead fd buf 0 chunkSize (Just offset))
        str <- lift (liftEff (readString enc 0 bytesRead buf))
        emit str
        unless (bytesRead == 0) (loop (offset + bytesRead))
  loop 0
