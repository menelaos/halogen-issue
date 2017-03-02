module Main where

import App.Effects           ( AppEffects )
import App.Monad             ( runApp )
import App.Parent            as Parent
import App.Wiring            as Wiring
import Control.Monad.Aff     ( forkAff, later' )
import Control.Monad.Aff.Bus as Bus
import Control.Monad.Eff     ( Eff )
import Halogen               as H
import Halogen.Aff           as HA
import Halogen.VDom.Driver   ( runUI )
import Prelude

main :: Eff AppEffects Unit
main = HA.runHalogenAff $ do
  body <- HA.awaitBody
  wiring@(Wiring.Wiring { bus }) <- Wiring.make
  let ui = H.hoist (runApp wiring) Parent.component
  io <- runUI ui unit body

  forkAff $ later' 10000 $ Bus.write unit bus
  pure unit
