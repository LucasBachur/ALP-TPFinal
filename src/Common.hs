module Common where
import GHC.Generics
import qualified Data.Map.Strict as Map
import qualified Data.ByteString as BS

-- Identificadores de mazos
type DeckName = String

-- Entornos
type NameEnv = [(DeckName, DeckExp)]

-- Comandos
data Comm = Def DeckName DeckExp
          | Show DeckExp
          | IsDeck DeckExp
          | Export DeckExp DeckHero
          | GetCardData CardDef

-- Expresiones de mazos
data DeckExp = Union DeckExp DeckExp
            | Diff DeckExp DeckExp
            | InCommon DeckExp DeckExp
            | Filter [Filter] DeckExp
            | CardList CardList
            | Deck DeckName
            | Import String
            deriving (Show,Eq)

-- Expresiones de heroe
data DeckHero = Class String | HeroName String

-- Definiciones de tipos usados dentro de las expresiones de mazo
type CardList = [CardUnit]

-- Expresiones de carta
data CardUnit = One CardDef | Two CardDef deriving (Show,Eq)

data CardDef = Id Int | Name String deriving (Show,Eq)

data Filter = Field Field | FieldN FieldN Int deriving (Show,Eq)

data Field = Minion | Spell | Weapon | Hero | Location deriving (Show,Eq)

data FieldN = Attack | Health | Cost deriving (Show,Eq)

-- Tipo para campos con multiples campos divididos por idioma
type MultiLangText = Map.Map String String

-- Tipo para cartas de Hearthstone
-- Estan incluidos todos los campos que pueden aparecer en el JSON
data HearthstoneCard = Card { 
  armor :: Maybe Int,
  artist :: Maybe String,
  attack :: Maybe Int,
  battlegroundsPremiumDbfId :: Maybe Int,
  cardClass :: String,
  classes :: Maybe [String],
  collectible :: Bool,
  cost :: Maybe Int,
  countAsCopyOfDbfId :: Maybe Int,
  dbfId :: Int,
  durability :: Maybe Int,
  elite :: Maybe Bool,
  faction :: Maybe String,
  flavor :: Maybe MultiLangText,
  hasDiamondSkin :: Maybe Bool,
  health :: Maybe Int,
  heroPowerDbfId :: Maybe Int,
  hideStats :: Maybe Bool,
  howToEarn :: Maybe MultiLangText,
  howToEarnGolden :: Maybe MultiLangText,
  id :: String,
  isMiniSet :: Maybe Bool,
  mechanics :: Maybe [String],
  multiClassGroup :: Maybe String,
  name :: MultiLangText,
  overload :: Maybe Int,
  questReward :: Maybe String,
  race :: Maybe String,
  races :: Maybe [String],
  rarity :: String,
  referencedTags :: Maybe [String],
  set :: Maybe String,
  spellDamage :: Maybe Int,
  spellSchool :: Maybe String,
  targetingArrowText :: Maybe MultiLangText,
  techLevel :: Maybe Int,
  text :: Maybe MultiLangText,
  cardType :: String
} deriving (Show)

-- Tipo para guardar los datos de todas las cartas disponibles (leidas del JSON)
data CardData = CardData {
  idMap :: Map.Map Int HearthstoneCard,
  nameMap :: Map.Map String HearthstoneCard
} deriving Show

-- Card Data nulo
initCD :: CardData
initCD = CardData Map.empty Map.empty

-- Tipo para mazos de Hearthstone
-- Se mapea el id de cada carta presente el mazo con una tupla que representa
-- la cantidad de copias de dicha carta en el mazo y los datos de la carta.
type HearthstoneDeck = Map.Map Int (Int, HearthstoneCard)

-- Tipo para errores
data Error = VariableNotFound DeckName | VarIntUnexpectedEnd String | VarIntExceedsMaxSize String | SectionMissing String
  deriving (Eq, Show)