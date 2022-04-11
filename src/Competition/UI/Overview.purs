module Competition.UI.Overview where

import Prelude

import Data.Maybe (Maybe (..))
import Effect.Class (class MonadEffect)
import Data.Array (length)
import Competition.Pages (PageAction (..))
import Competition.Competition (Competition, addNewCompetition, getCompetitions)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

type Slot action = forall q. H.Slot q action Unit

data Query a = Query a 

data Overview = Overview (Array Competition)

data Action 
  = NewCompetitionClick
  | LoadState

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
      , HH.div [HP.class_ (H.ClassName "col-6")] [newCompetitionButton]
      ]

    newCompetitionButton = 
      HH.button
        [ HE.onClick \_ -> NewCompetitionClick ]
        [ HH.text "Toevoegen" ]

    listCompetitions = HH.table
      [HP.class_ (H.ClassName "table")]
      [ HH.thead_
          [ HH.th [HP.scope HP.ScopeCol] [HH.text "#"]
          , HH.th [HP.scope HP.ScopeCol] [HH.text "Naam"]
          , HH.th [HP.scope HP.ScopeCol] [HH.text "Aantal deelnemers"]
          ]
      , HH.tbody_ (map competitionRow state)
      ]
    
    expandName "" = "Geen naam"
    expandName s = s

    competitionRow competition =
      HH.tr 
        []
        [ HH.td [] [HH.text (show competition.competitionId)]
        , HH.td [] [HH.text (expandName competition.competitionName)]
        , HH.td [] [HH.text (show (length (competition.competitionParticipants)))]
        ]


handleAction :: forall m. MonadEffect m => Action -> H.HalogenM Overview Action () PageAction m Unit
handleAction = case _ of
  NewCompetitionClick -> do
    competition <- H.liftEffect addNewCompetition
    H.raise (CreatedNewCompetition competition)
  LoadState -> do
    competitions <- H.liftEffect getCompetitions
    H.modify_ (\_ -> Overview competitions)