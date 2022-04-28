module Competition.Competition
  ( Apparatus(..)
  , Competition(..)
  , CompetitionId
  , CompetitionType(..)
  , addNewCompetition
  , emptyCompetition
  , femaleApparatuses
  , getCompetitions
  , maleApparatuses
  , modifyCompetition
  , rotateFirst
  , apparatusName
  , reorder
  , removeCompetition
  )
  where

import Competition.Participant (Participant, Lane (..), genId)
import Control.Monad.Except (throwError)
import Data.Array (cons, length, head, find, uncons, snoc, (:), filter)
import Data.Either (Either (..))
import Data.Eq (class Eq, (==))
import Data.Tuple (Tuple (..))
import Data.List.Types (NonEmptyList (..))
import Data.Maybe (Maybe (..), fromMaybe')
import Data.NonEmpty (singleton)
import Data.Traversable (traverse)
import Effect (Effect)
import Effect.Console (log)
import Foreign (ForeignError (..))
import Partial.Unsafe (unsafeCrashWith)
import Prelude (($), (<$>), (>>=), bind, pure, discard, (<>), class Show, show, (>), not, (/=) )
import Simple.JSON as JSON
import Web.HTML (window)
import Web.HTML.Window (localStorage)
import Web.Storage.Storage (setItem, getItem)
import Data.Int (fromString)

newtype CompetitionId = CompetitionId Int

instance writeForeignCompetitionId :: JSON.WriteForeign CompetitionId where
  writeImpl (CompetitionId a) = JSON.writeImpl $ show a

instance readForeignCompetitionId :: JSON.ReadForeign CompetitionId where
  readImpl x = JSON.readImpl x >>= \str -> 
    case fromString str of
      Just cid -> pure (CompetitionId cid)
      Nothing -> throwError (NonEmptyList $ singleton $ ForeignError ("Invalid int: " <> str))

derive instance eqCompetitionId :: Eq CompetitionId

instance showCompetitionId :: Show CompetitionId where
  show (CompetitionId x) = show x

type Competition = 
  { competitionId :: CompetitionId
  , competitionName :: String
  , competitionType :: CompetitionType
  , competitionApparatuses :: Array Apparatus -- In order, starting with the first apparatus
  , competitionParticipants :: Array Participant
  }



data CompetitionType = Male | Female

derive instance eqCompetitionType :: Eq CompetitionType

instance showCompetitionType :: Show CompetitionType where
  show Male = "male"
  show Female = "female"


instance writeForeignCompetitionType :: JSON.WriteForeign CompetitionType where
  writeImpl a = JSON.writeImpl $ show a

instance readForeignCompetitionType :: JSON.ReadForeign CompetitionType where
  readImpl x = JSON.readImpl x >>= \str -> 
    case str of
      "male" -> pure Male
      "female" -> pure Female
      s -> throwError (NonEmptyList $ singleton $ ForeignError ("Invalid key: " <> s))


data Apparatus
  = Floor
  | PommelHorse
  | Rings
  | Vault
  | ParallelBars
  | HorizontalBar
  | UnevenBars
  | Beam

derive instance eqApparatus :: Eq Apparatus

instance showApparatus :: Show Apparatus where
  show Floor = "floor"
  show PommelHorse = "pommelHorse"
  show Rings = "rings"
  show Vault = "vault"
  show ParallelBars = "parallelBars"
  show HorizontalBar = "horizontalBar"
  show UnevenBars = "unevenBars"
  show Beam = "beam"

apparatusName :: Apparatus -> String
apparatusName Floor = "Vloer"
apparatusName PommelHorse = "Voltige"
apparatusName Rings = "Ringen"
apparatusName Vault = "Sprong"
apparatusName ParallelBars = "Brug"
apparatusName HorizontalBar = "Rek"
apparatusName UnevenBars = "Brug"
apparatusName Beam = "Balk"

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

maleApparatuses :: Array Apparatus
maleApparatuses = [Floor, PommelHorse, Rings, Vault, ParallelBars, HorizontalBar]

femaleApparatuses :: Array Apparatus
femaleApparatuses = [ Vault, UnevenBars, Beam, Floor]

emptyCompetition :: CompetitionId -> Competition
emptyCompetition nextId = 
  { competitionId: nextId 
  , competitionName: "" 
  , competitionType: Male
  , competitionApparatuses: maleApparatuses
  , competitionParticipants: []
  }

addNewCompetition :: Effect Competition
addNewCompetition = do
  cid <- CompetitionId <$> genId
  competitions <- modifyCompetitions (\xs -> pure $ cons (emptyCompetition cid) xs)
  case head competitions of
    Nothing -> unsafeCrashWith "Should not occur, head is safe due to usage of cons"
    Just x -> pure x

getCompetitions :: Effect (Array Competition)
getCompetitions = modifyCompetitions (\x -> pure x)

removeCompetition :: Competition -> Effect (Array Competition)
removeCompetition competition = modifyCompetitions (\xs -> pure $ filter (\x -> x.competitionId /= competition.competitionId) xs)

modifyCompetitions :: (Array Competition -> Effect (Array Competition)) -> Effect (Array Competition)
modifyCompetitions f = do
  w <- window
  s <- localStorage w
  let competitionKey = "competitions"
  existing <- getItem competitionKey s
  new <- case existing of
    Nothing -> f []
    Just str -> do
      let parsed = JSON.readJSON str
      case parsed of
        Left e -> do
          log (show e)
          f []
        Right r -> f r
  setItem competitionKey (JSON.writeJSON new) s
  pure new

getCompetition :: CompetitionId -> Array Competition -> Competition
getCompetition cId lst = fromMaybe' (\_ -> unsafeCrashWith "Cannot modify non-existing competition") $ find (\c -> c.competitionId == cId) lst

modifyCompetition :: CompetitionId -> (Competition -> Effect Competition) -> Effect Competition
modifyCompetition cId f = getCompetition cId <$> modifyCompetitions (traverse f')
  where
    f' :: Competition -> Effect Competition
    f' c = if c.competitionId == cId then f c else pure c
        
rotate :: forall a. Array a -> Array a
rotate arr = case uncons arr of
  Nothing -> [] 
  Just x -> snoc (x.tail) (x.head)

rotateUntil :: forall a. (Array a -> Boolean) -> Array a -> Array a
rotateUntil f arr = if f arr then arr else rotateUntil f (rotate arr)

rotateFirst :: Apparatus -> Array Apparatus -> Array Apparatus
rotateFirst apparatus = rotateUntil (\arr -> head arr == Just apparatus)

reorder :: Apparatus -> Array Apparatus -> Array Participant -> Array Participant
reorder apparatus apparatusOrder participants = reorderSplit apparatus apparatusOrder (splitByLane (filter (\p -> p.participantPresent) participants)) <> filter (\p -> not p.participantPresent) participants 

splitByLane :: Array Participant -> Tuple (Array Participant) (Array Participant)
splitByLane = go (Tuple [] [])
  where
    go (Tuple l r) lst = case uncons lst of
      Nothing -> Tuple l r
      Just uc -> case uc.head.participantLane of
        Lane1 -> go (Tuple (snoc l uc.head) r) uc.tail
        Lane2 -> go (Tuple l (snoc r uc.head)) uc.tail


zipParticipants :: Tuple (Array Participant) (Array Participant) -> Array Participant
zipParticipants (Tuple l1 l2) = go startWith (Tuple l1 l2)
  where
    startWith = if length l2 > length l1 then Lane2 else Lane1
    go s (Tuple l r) = case Tuple (uncons l) (uncons r) of
      Tuple Nothing Nothing -> []
      Tuple (Just _) Nothing -> l
      Tuple Nothing (Just _) -> r 
      Tuple (Just l') (Just r') -> case s of
        Lane1 -> l'.head : go Lane2 (Tuple l'.tail r)
        Lane2 -> r'.head : go Lane1 (Tuple l r'.tail)



reorderSplit :: Apparatus -> Array Apparatus -> Tuple (Array Participant) (Array Participant) -> Array Participant
reorderSplit apparatus apparatusOrder participants 
  | head apparatusOrder == Just apparatus = zipParticipants participants
  | true = 
      let
        Tuple l r = participants
        l' = rotate l
        r' = rotate r
      in reorderSplit apparatus (rotate apparatusOrder) (Tuple l' r')
