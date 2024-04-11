{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}
module ReadJSON where

import Data.Aeson
import Common
import qualified Data.ByteString.Lazy as B
import qualified Data.Text as T
import qualified Data.Map.Strict as Map

instance FromJSON HearthstoneCard where
    parseJSON = withObject "HearthstoneCard" $ \v -> Card
        <$> v.:? "armor"
        <*> v.:? "artist"
        <*> v.:? "attack"
        <*> v.:? "battlegroundsPremiumDbfId"
        <*> v.: "cardClass"
        <*> v.:? "classes"
        <*> v.: "collectible"
        <*> v.:? "cost"
        <*> v.:? "countAsCopyOfDbfId"
        <*> v.: "dbfId"
        <*> v.:? "durability"
        <*> v.:? "elite"
        <*> v.:? "faction"
        <*> v.:? "flavor"
        <*> v.:? "hasDiamondSkin"
        <*> v.:? "health"
        <*> v.:? "heroPowerDbfId"
        <*> v.:? "hideStats"
        <*> v.:? "howToEarn"
        <*> v.:? "howToEarnGolden"
        <*> v.: "id"
        <*> v.:? "isMiniSet"
        <*> v.:? "mechanics"
        <*> v.:? "multiClassGroup"
        <*> v.: "name"
        <*> v.:? "overload"
        <*> v.:? "questReward"
        <*> v.:? "race"
        <*> v.:? "races"
        <*> v.: "rarity"
        <*> v.:? "referencedTags"
        <*> v.:? "set"
        <*> v.:? "spellDamage"
        <*> v.:? "spellSchool"
        <*> v.:? "targetingArrowText"
        <*> v.:? "techLevel"
        <*> v.:? "text"
        <*> v.: "type"


-- readJSONFile: lee el archivo Json y devuelve la informacion en la estructura correspondiente
readJSONFile :: FilePath -> IO (CardData)
readJSONFile filePath = do
  fileContents <- B.readFile filePath -- lectura del archivo
  case eitherDecode fileContents of   -- si la lectura fue exitosa, copio la informacion en los dos Maps de mi estructura
    Right dataList -> return (CardData (Map.fromList [(dbfId x, x) | x <- dataList]) (Map.fromList [((name x) Map.! "enUS", x) | x <- dataList ]))
    Left err       -> return initCD   -- si no, devuelvo la estructura vacia