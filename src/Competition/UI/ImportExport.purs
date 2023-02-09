module Competition.UI.ImportExport
  ( Action(..)
  , Slot
  , State(..)
  , component
  , handleAction
  , initialState
  , render
  , slot
  )
  where

import Prelude

import Effect.Class (class MonadEffect)
import Competition.Pages (Page (..), PageAction (..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Type.Proxy (Proxy (..))
import Simple.JSON as JSON
import Data.Maybe (Maybe (..))
import Data.Either (Either (..))
import Effect.Console (log)
import Competition.Competition (Competition, getCompetitions, importCompetition)

type Slot action = forall q. H.Slot q action Unit

data Action 
  = ImportAction
  | LoadState
  | SetInput String
  | ToOverview

type State = 
  { competitions :: Array Competition
  , inputString :: String
  }

slot :: Proxy "importExport"
slot = Proxy

component :: forall q i m. MonadEffect m => H.Component q i PageAction m
component = H.mkComponent 
  { initialState: initialState
  , render
  , eval: H.mkEval $ H.defaultEval 
      { handleAction = handleAction 
      , initialize = Just LoadState 
      }
  }

initialState :: forall i. i -> State
initialState _ = 
  { competitions: []
  , inputString: ""
  }

render :: forall m. State -> H.ComponentHTML Action () m
render state = HH.div [cls "container"] ([header, exportPage] <> importPage)
  where
    rowDiv = HH.div [cls "row"]
    header = rowDiv
      [ HH.div [cls "col-6"] [HH.h1_ [HH.text $ "Importeren / exporteren"]]
      , HH.div [cls "col-6"] [
        HH.button
          [ HE.onClick \_ -> ToOverview 
          , cls "btn btn-primary"
          ]
          [ HH.text "Overzicht" ]
        ]
      ]

    cls :: forall r i. String -> HP.IProp (class :: String | r) i
    cls name = HP.class_ (H.ClassName name)

    exportPage = rowDiv
      [ HH.label [cls "form-check-label col-2"] [HH.text "Exporteren"]
      , HH.div [cls "form-check col 10"] 
        [ HH.input 
          [ HP.value (JSON.writeJSON state.competitions)
          
          ]
        ]
      ]

    importPage = 
      [ rowDiv
        [ HH.label [cls "form-check-label col-2"] [HH.text "Importeren"]
        , HH.div [cls "form-check col 10"] 
          [ HH.input 
            [ HP.name "import-value"
            , HE.onValueInput SetInput
            ]
          ]
        ]
      , rowDiv 
        [ HH.div [cls "form-check"] 
          [ HH.button
            [ HE.onClick \_ -> ImportAction 
            , HP.class_ (H.ClassName "btn btn-primary")
            ]
            [ HH.text "Verwijder alles en importeer" ]
          ]
        ]
      ]

handleAction :: forall m. MonadEffect m => Action -> H.HalogenM State Action () PageAction m Unit
handleAction = case _ of
  ImportAction -> do
    state <- H.get
    let parsed = JSON.readJSON state.inputString
    case parsed of
      Left e -> H.liftEffect (log (show e))
      Right r -> do
        H.liftEffect (importCompetition r)
        H.raise (NavigatePage OverviewPage)
  LoadState -> do
    competitions <- H.liftEffect getCompetitions
    H.modify_ (\_ -> {competitions:competitions, inputString: ""})
  SetInput s -> do
    H.modify_ (\state -> {competitions:state.competitions, inputString: s})
  ToOverview -> do
    H.raise (NavigatePage OverviewPage)