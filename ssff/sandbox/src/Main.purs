module Main where

import Prelude
import Control.Coroutine (Consumer, runProcess, consumer, ($$))
import Control.Monad.Aff (launchAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff, class MonadEff)
import Control.Monad.Eff.Console (log, CONSOLE)
import Control.Monad.Eff.Exception (EXCEPTION)
import Data.Maybe (Maybe(..))
import Helpers (readFile)
import Node.Buffer (BUFFER)
import Node.Encoding (Encoding(UTF8))
import Node.FS (FS)

main :: ∀ eff
      . Eff ( err     :: EXCEPTION
            , fs      :: FS
            , buffer  :: BUFFER
            , console :: CONSOLE
            | eff
            ) Unit
main = void <<< launchAff <<< runProcess $
  ?process
