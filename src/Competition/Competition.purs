module Competition.Competition
  ( Apparatus(..)
  , Competition(..)
  , Participant(..)
  , emptyCompetition
  , maleApparatuses
  , addNewCompetition
  , getCompetitions
  )
  where

import Control.Monad.Except (throwError)
import Data.Array (cons, length, head)
import Data.Either (Either (..))
import Data.List.Types (NonEmptyList (..))
import Data.Maybe (Maybe (..), fromMaybe')
import Data.NonEmpty (singleton)
import Effect (Effect)
import Effect.Console (log)
import Foreign (ForeignError (..))
import Partial.Unsafe (unsafeCrashWith)
import Prelude (($), (<$>), (>>=), bind, pure, discard, (<>), class Show, show )
import Simple.JSON as JSON
import Web.HTML (window)
import Web.HTML.Window (localStorage)
import Web.Storage.Storage (setItem, getItem)

type Competition = 
  { competitionId :: Int
  , competitionName :: String
  , competitionApparatuses :: Array Apparatus -- In order, starting with the first apparatus
  , competitionParticipants :: Array Participant
  }

-- derive instance encodeCompetition :: JSON.WriteForeign Competition

data Apparatus
  = Floor
  | PommelHorse
  | Rings
  | Vault
  | ParallelBars
  | HorizontalBar
  | UnevenBars
  | Beam

instance showApparatus :: Show Apparatus where
  show Floor = "floor"
  show PommelHorse = "pommelHorse"
  show Rings = "rings"
  show Vault = "vault"
  show ParallelBars = "parallelBars"
  show HorizontalBar = "horizontalBar"
  show UnevenBars = "unevenBars"
  show Beam = "beam"

instance writeForeignApparatus :: JSON.WriteForeign Apparatus where
  writeImpl a = JSON.writeImpl $ show a

instance readForeignApparatus :: JSON.ReadForeign Apparatus where
  readImpl x = JSON.readImpl x >>= \str -> 
    case str of
      "floor" -> pure Floor
      "pommelHorse" -> pure PommelHorse
      "rings" -> pure Rings
      "vault" -> pure Vault
      "parallelBars" -> pure ParallelBars
      "horizontalBar" -> pure HorizontalBar
      "unevenBars" -> pure UnevenBars
      "beam" -> pure Beam
      s -> throwError (NonEmptyList $ singleton $ ForeignError ("Invalid key: " <> s))

type Participant =
  { participantId :: String 
  , participantName :: String
  , participantClub :: String
  , participantPresent :: Boolean
  }

maleApparatuses :: Array Apparatus
maleApparatuses = [Floor, PommelHorse, Rings, Vault, ParallelBars, HorizontalBar]

femaleApparatuses :: Array Apparatus
femaleApparatuses = [ Vault, UnevenBars, Beam, Floor]

emptyCompetition :: Int -> Competition
emptyCompetition nextId = 
  { competitionId: nextId 
  , competitionName : "" 
  , competitionApparatuses: maleApparatuses
  , competitionParticipants: []
  }

addNewCompetition :: Effect Competition
addNewCompetition = (\x -> fromMaybe' (\_ -> unsafeCrashWith "Should not occur, head is safe due to usage of cons") $ head x) <$> modifyCompetitions (\cs -> cons (emptyCompetition (length cs)) cs)

getCompetitions :: Effect (Array Competition)
getCompetitions = modifyCompetitions (\x -> x)

modifyCompetitions :: (Array Competition -> Array Competition) -> Effect (Array Competition)
modifyCompetitions f = do
  w <- window
  s <- localStorage w
  let competitionKey = "competitions"
  existing <- getItem competitionKey s
  new <- case existing of
    Nothing -> pure (f [])
    Just str -> do
      let parsed = JSON.readJSON str
      case parsed of
        Left e -> do
          log (show e)
          pure (f [])
        Right r -> pure (f r)
  setItem competitionKey (JSON.writeJSON new) s
  pure new

