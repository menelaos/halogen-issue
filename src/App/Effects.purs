module App.Effects
  ( AppEffects
  , AppRawEffects
  )
where

import Control.Monad.Aff.Console ( CONSOLE )
import Control.Monad.Eff.Random  ( RANDOM )
import Halogen.Aff               as HA

type AppEffects = HA.HalogenEffects AppRawEffects
type AppRawEffects = ( console :: CONSOLE, random :: RANDOM )
