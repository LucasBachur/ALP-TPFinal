module PrettyPrinter
  {-( printTerm
  ,     -- pretty printer para terminos
    printType     -- pretty printer para tipos
  )-}
where

import           Common
import qualified Text.PrettyPrint.HughesPJ as T
import qualified Data.Map.Strict as M
import           Prelude                 hiding ( (<>) )

printDeck :: HearthstoneDeck -> T.Doc
printDeck d = printDeck' (getDeckForPrint d)

getDeckForPrint :: HearthstoneDeck -> [String]
getDeckForPrint d = (map (\(n,x) -> (show n) ++ "x " ++ 
                                    "\"" ++ ((name x) M.! "enUS") ++ "\"") -- Obtengo el nombre en ingles de las cartas
                                    (M.elems d))                           -- y les pongo comillas

printDeck' :: [String] -> T.Doc
printDeck' [] = T.text ""
printDeck' [x] = T.text x
printDeck' (x:xs) = T.text x
                    T.<> T.text "\n"
                    T.<> printDeck' xs

printCode :: String -> T.Doc
printCode = T.text

printIsDeck :: Bool -> T.Doc
printIsDeck True  = T.text "Deck valido"
printIsDeck False = T.text "Deck invalido"

printCard :: HearthstoneCard -> T.Doc
printCard c = T.text (maybe "" (\x->"Attack: "++(show x)++"\n") (attack c))
              T.<> T.text (maybe ("Class: "++(cardClass c)++"\n") (\x->"Classes: "++(unwords x)++"\n") (classes c))
              T.<> T.text (maybe "" (\x->"Cost: "++(show x)++"\n") (cost c))
              T.<> T.text ("Id: "++(show (dbfId c))++"\n")
              T.<> T.text (maybe "" (\x->"Durability: "++(show x)++"\n") (durability c))
              T.<> T.text (maybe "" (\x->"Health: "++(show x)++"\n") (health c))
              T.<> T.text ("Name: "++"\""++((name c) M.! "enUS")++"\"\n")
              T.<> T.text (maybe "" (\x->"Races: "++(unwords x)++"\n") (races c))
              T.<> T.text ("Rarity: "++(rarity c)++"\n")
              T.<> T.text ("Card Type: "++(cardType c))

printCardInvalid :: T.Doc
printCardInvalid = T.text "No se pudo encontrar la carta"

printError :: Error -> T.Doc
printError (VariableNotFound d) = T.text $ "No se pudo encontrar el mazo \""++d++"\""
printError (VarIntUnexpectedEnd s) = T.text $ "Error al leer el codigo: fin inesperado de variable. \""++s++"\""
printError (VarIntExceedsMaxSize s) = T.text $ "Error al leer el codigo: maximo excedido. \""++s++"\""
printError (SectionMissing s) = T.text $ "Error al leer el codigo: faltan secciones. \""++s++"\""
