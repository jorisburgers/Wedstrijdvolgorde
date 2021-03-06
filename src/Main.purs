module Main where

import Prelude

import Effect (Effect)

import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)

import Competition.UI.Body as Body

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI Body.component unit body