module Competition.UI.Overview where

import Prelude

import Data.Array (length)
import Competition.Pages (Page)
import Halogen as H
import Halogen.HTML as HH

type Slot = H.Slot Query Page

data Query a = Query a 

data Overview = Overview (Array Unit)

component :: forall q i o m. H.Component q i o m
component = H.mkComponent 
  { initialState
  , render
  , eval: H.mkEval $ H.defaultEval
  }

initialState :: forall i. i -> Overview 
initialState _ = Overview [unit]

render :: forall m. Overview -> H.ComponentHTML Unit () m
render (Overview state) = HH.text (show (length state))