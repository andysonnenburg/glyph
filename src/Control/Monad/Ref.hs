module Control.Monad.Ref
       ( MonadRef (..)
       , RefSupply
       , runRefSupply
       , RefSupplyT
       , runRefSupplyT
       , module X
       ) where

import Control.Monad as X
import Control.Monad.Ref.Class
import Control.Monad.Trans as X
import Control.Monad.Trans.Ref