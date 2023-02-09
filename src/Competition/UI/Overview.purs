module Competition.UI.Overview where

import Prelude

import Data.Maybe (Maybe (..))
import Effect.Class (class MonadEffect)
import Data.Array (length)
import Competition.Pages (Page (..), PageAction (..))
import Competition.Competition (Competition, CompetitionType (..), addNewCompetition, getCompetitions)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Type.Proxy (Proxy(..))

type Slot action = forall q. H.Slot q action Unit

slot :: Proxy "overview"
slot = Proxy

data Overview = Overview (Array Competition)

data Action 
  = NewCompetitionClick
  | LoadState
  | ShowCompetition Competition
  | EditCompetition Competition
  | ImportExportClick

component :: forall q i m. MonadEffect m => H.Component q i PageAction m
component = H.mkComponent 
  { initialState
  , render
  , eval: H.mkEval $ H.defaultEval 
    { handleAction = handleAction 
    , initialize = Just LoadState
    }
  }

initialState :: forall i. i -> Overview 
initialState _ = Overview []

render :: forall m. Overview -> H.ComponentHTML Action () m
render (Overview state) = HH.div
  [HP.class_ (H.ClassName "container")]
  [ header
  , listCompetitions
  ]
  where
    header = HH.div 
      [HP.class_ (H.ClassName "row")]
      [ HH.div [HP.class_ (H.ClassName "col-6")] [HH.h1_ [HH.text "Wedstrijden"]]
      , HH.div [HP.class_ (H.ClassName "col-3")] [newCompetitionButton]
      , HH.div [HP.class_ (H.ClassName "col-3")] [importExportButton]
      ]

    newCompetitionButton = 
      HH.button
        [ HE.onClick \_ -> NewCompetitionClick 
        , HP.class_ (H.ClassName "btn btn-primary")
        ]
        [ HH.text "Toevoegen" ]

    importExportButton = 
      HH.button
        [ HE.onClick \_ -> ImportExportClick 
        , HP.class_ (H.ClassName "btn btn-primary")
        ]
        [ HH.text "Import / Export" ]

    listCompetitions = HH.table
      [HP.class_ (H.ClassName "table")]
      [ HH.thead_
          [ HH.th [HP.scope HP.ScopeCol] [HH.text "Naam"]
          , HH.th [HP.scope HP.ScopeCol] [HH.text "Aantal deelnemers"]
          , HH.th [HP.scope HP.ScopeCol] [HH.text "Man/vrouw"]
          , HH.th [] []
          ]
      , HH.tbody_ (map competitionRow state)
      ]
    
    expandName "" = "Geen naam"
    expandName s = s

    competitionRow competition =
      HH.tr 
        [ ]
        [ HH.td [] [HH.text (expandName competition.competitionName)]
        , HH.td [] [HH.text (show (length (competition.competitionParticipants)))]
        , HH.td [] [HH.text $ 
            case competition.competitionType of 
              Male -> "m"
              Female -> "v"
            ]
        , HH.td 
          [ HE.onClick \_ -> ShowCompetition competition] [HH.text "Weergeven"]
        , HH.td 
          [ HE.onClick \_ -> EditCompetition competition] [HH.text "Aanpassen"]
        ]

handleAction :: forall m. MonadEffect m => Action -> H.HalogenM Overview Action () PageAction m Unit
handleAction = case _ of
  NewCompetitionClick -> do
    competition <- H.liftEffect addNewCompetition
    H.raise (CreatedNewCompetition competition)
  ImportExportClick -> do
    H.raise (NavigatePage ImportExportPage)
  LoadState -> do
    competitions <- H.liftEffect getCompetitions
    H.modify_ (\_ -> Overview competitions)
  ShowCompetition competition -> do
    H.raise (NavigatePage (ShowPage competition))
  EditCompetition competition -> do
    H.raise (NavigatePage (EditPage competition))