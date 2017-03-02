module App.Wiring
  ( Wiring (..)
  , WiringR
  , expose
  , make
  )
where

import App.Effects                 ( AppEffects )
import Control.Monad.Aff.Bus       as Bus
import Control.Monad.Aff.Class     ( class MonadAff, liftAff )
import Control.Monad.Reader.Class  ( class MonadAsk, asks )
import Data.Newtype                ( class Newtype, unwrap )
import Prelude

type WiringR = { bus :: Bus.BusRW Unit }

newtype Wiring = Wiring WiringR

derive instance newtypeWiring :: Newtype Wiring _

expose :: ∀ m. MonadAsk Wiring m => m WiringR
expose = asks unwrap

make :: ∀ m. MonadAff AppEffects m => m Wiring
make = liftAff $ do
  bus <- Bus.make
  pure $ Wiring { bus }
