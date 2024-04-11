module Comms where

import           Common
import           Monads
import           PrettyPrinter
import           Data.Maybe
import qualified Data.Map.Strict as M
import           Text.PrettyPrint.HughesPJ
import           DeckString
import           Control.Monad                  ( liftM
                                                , ap
                                                )

-- Entorno nulo
initEnv :: NameEnv
initEnv = []

emptyDeck :: DeckExp
emptyDeck = CardList []

-- Mónada estado, con manejo de errores
newtype StateError a =
  StateError { runStateError :: NameEnv -> Either Error (a, NameEnv) }

-- Instanciamientos de StateError
instance Functor StateError where
  fmap = liftM
instance Applicative StateError where
  pure  = return
  (<*>) = ap
instance Monad StateError where
  return x = StateError (\e -> Right (x, e))
  m >>= f = StateError (\e -> case runStateError m e of
                                   Left e -> Left e
                                   Right (v, e') -> runStateError (f v) e')
instance MonadState StateError where
  lookfor d = StateError (\e -> case (lookfor' d e) of Left e -> Left e
                                                       Right d -> Right (d,e))
   where lookfor' d []            = Left (VariableNotFound d)
         lookfor' d ((d1,de):r)| d == d1   = Right de
                               | otherwise = lookfor' d r
  update n d = StateError (\e -> Right ((), update' n d e))
   where update' n d [] = [(n,d)]
         update' n d ((n1,d1):r) | n == n1 = (n,d):r
                                 | otherwise = (n1,d1):(update' n d r)
instance MonadError StateError where
  throw e = StateError (\_ -> Left e)

-- def: dado un nombre y una expresion de mazo, define un mazo dentro del entorno.
def :: DeckName -> DeckExp -> NameEnv -> Either Error NameEnv
def name exp env = case runStateError (update name exp') env of
                    Left err -> Left err
                    Right (_, env') -> Right env'
                   where exp' = defineDecks exp env

-- searchDeck: busca un mazo dentro del entorno. Si no lo encuentra devuelve un mazo vacio.
searchDeck :: NameEnv -> DeckName -> DeckExp
searchDeck env deckName = case runStateError (lookfor deckName) env of
                            Left _ -> emptyDeck
                            Right (d,_) -> d

-- defineDecks: toma una expresion de mazo y un entorno, y devuelve la expresion con las referencias a mazos
--              dentro del entorno reemplazadas por sus respectivas expresiones.
defineDecks :: DeckExp -> NameEnv -> DeckExp
defineDecks (Union d1 d2) env    = Union (defineDecks d1 env) (defineDecks d2 env)
defineDecks (Diff d1 d2) env     = Diff (defineDecks d1 env) (defineDecks d2 env)
defineDecks (InCommon d1 d2) env = InCommon (defineDecks d1 env) (defineDecks d2 env)
defineDecks (Filter f d) env     = Filter f (defineDecks d env)
defineDecks (CardList cl) env    = CardList cl
defineDecks (Import s) env       = Import s
defineDecks (Deck d) env         = searchDeck env d

-- eitherDeck: dada una expresion de mazo y una funcion, la trata de convertir a un mazo literal. Si no hubo error,
--             le aplica la funcion al mazo obtenido.
eitherDeck :: CardData -> NameEnv -> DeckExp -> (HearthstoneDeck -> Doc) -> Doc
eitherDeck cd env d f = case convert cd env d of
                          Left err -> printError err
                          Right deck -> f deck

-- eval: evalua una expresion de deck.
eval :: CardData -> NameEnv -> DeckExp -> Doc
eval cd env d = eitherDeck cd env d printDeck

-- searchCD: toma una expresion de carta y (si la encuentra) devuelve una tupla que representa:
--           la cantidad de copias de la carta en el mazo y la carta en si.
searchCD :: CardData -> CardUnit -> Maybe (Int,HearthstoneCard)
searchCD cd (One c) = maybe Nothing (\y -> Just (1,y)) (searchCD' cd c)
searchCD cd (Two c) = maybe Nothing (\y -> Just (2,y)) (searchCD' cd c)

searchCD' :: CardData -> CardDef -> Maybe HearthstoneCard
searchCD' (CardData idList _) (Id i) = M.lookup i idList
searchCD' (CardData _ nameList) (Name n) = M.lookup n nameList

-- searchID: toma un id de carta y (si lo encuentra) devuelve la carta en si.
searchID :: CardData -> Int -> Maybe HearthstoneCard
searchID (CardData idList _) id = M.lookup id idList

-- convert: convierte una expresion de mazo a un mazo literal.
convert :: CardData -> NameEnv -> DeckExp -> Either Error HearthstoneDeck
convert cd env (Union d1 d2)    = do d1' <- (convert cd env d1)
                                     d2' <- (convert cd env d2)
                                     return $ M.unionWith (\(n1,c1) (n2,c2)->(n1+n2,c1)) d1' d2'
convert cd env (Diff d1 d2)     = do d1' <- (convert cd env d1)
                                     d2' <- (convert cd env d2)
                                     return $ M.differenceWith (\(n1,c1) (n2,c2)->if n2>=n1 then Nothing else Just (n1-n2,c1)) 
                                                               d1' d2'
convert cd env (InCommon d1 d2) = do d1' <- (convert cd env d1)
                                     d2' <- (convert cd env d2)
                                     return $ M.intersectionWith (\(n1,c1) (n2,c2)->(min n1 n2,c1)) d1' d2'
convert cd env (Filter f d)     = do d' <- (convert cd env d)
                                     return $ M.filter (getFilter f) d'
convert cd env (CardList cl)    = do return $ M.fromList (foldr (\x l-> maybe l (\(n,c)->(dbfId c,(n,c)):l) (searchCD cd x)) [] cl)
convert cd env (Import s)       = do decodedString <- fromDeckstring s
                                     return $ M.fromList (foldr (\(n,i) l -> maybe l (\c -> (i,(n,c)):l) (searchID cd i)) [] decodedString)
convert cd env (Deck d)         = do d' <- convert cd env (searchDeck env d)
                                     return $ d'

-- getFilter: dada una lista de filtros devuelve la funcion que los aplica.
getFilter :: [Filter] -> ((Int, HearthstoneCard) -> Bool)
getFilter [] = (\(m,c) -> True)
getFilter ((Field f):xs) = (\(m,c)->getFieldFilter f (m,c) && ((getFilter xs) (m,c)))
getFilter ((FieldN f n):xs) = (\(m,c)->getFieldNFilter f n (m,c) && ((getFilter xs) (m,c)))

getFieldFilter :: Field -> (Int, HearthstoneCard) -> Bool
getFieldFilter Minion (m,c)   = (cardType c) == "MINION"
getFieldFilter Spell  (m,c)   = (cardType c) == "SPELL"
getFieldFilter Weapon (m,c)   = (cardType c) == "WEAPON"
getFieldFilter Hero (m,c)     = (cardType c) == "HERO"
getFieldFilter Location (m,c) = (cardType c) == "LOCATION"

getFieldNFilter :: FieldN -> Int -> (Int, HearthstoneCard) -> Bool
getFieldNFilter Attack n (m,c) = maybe False (n ==) (attack c)
getFieldNFilter Health n (m,c) = maybe False (n ==) (health c)
getFieldNFilter Cost n (m,c)   = maybe False (n ==) (cost c)

-- isDeck: devuelve si un mazo es valido para usar dentro del juego o no.
isDeck :: CardData -> NameEnv -> DeckExp -> Doc
isDeck cd env d = eitherDeck cd env d (printIsDeck . isDeck')

isDeck' :: HearthstoneDeck -> Bool
isDeck' d = (correctSizeAndCopies d)

-- correctSizeAndCopies: toma un mazo y devuelve True si tiene el tamaño y la cantidad de copias correcta
--                       por carta, o False en caso contrario.
correctSizeAndCopies :: HearthstoneDeck -> Bool
correctSizeAndCopies d = correctSize && correctCopies where  
                          (deckSize,invCopies) = M.foldr (\(n,_) (x,y)-> if (n>2) then (n+x,y+1) else (n+x,y)) (0,0) d
                          correctSize = if elem idRenathal (M.keys d) then deckSize == 40 else deckSize == 30
                          correctCopies = invCopies == 0

idRenathal :: Int
idRenathal = 79767

-- export:: exporta el mazo codificandolo.
export :: CardData -> NameEnv -> DeckExp -> DeckHero -> Doc
export cd env d h = eitherDeck cd env d (\deck -> printCode (toDeckstring (getIdList deck) (getIdHero cd h)))

getIdList :: HearthstoneDeck -> [(Int,Int)]
getIdList d = map (\(i,(c,_)) -> (c,i)) (M.toList d)

-- getIdHero: toma una expresion de heroe y busca el id correspodiente.
getIdHero :: CardData -> DeckHero -> Int
getIdHero cd (Class c) = case c of
                            "Mage" -> 637 -- Jaina Proudmoore
                            "Priest" -> 813 -- Anduin Wrynn
                            "Warlock" -> 893 -- Gul'dan
                            "Shaman" -> 1066 -- Thrall
                            "Druid" -> 274 -- Malfurion Stormrage
                            "Rogue" -> 930 -- Valeera Sanguinar
                            "Paladin" -> 671 -- Uther Lightbringer
                            "Warrior" -> 7 -- Garrosh Hellscream
                            "Hunter" -> 31 -- Rexxar
                            "DemonHunter" -> 56550 -- Illidan Stormrage
                            "DH" -> 56550
                            "DeathKnight" -> 78065 -- The Lich King
                            "DK" -> 78065
getIdHero (CardData _ nameList) (HeroName n) = dbfId (nameList M.! n)

-- showCardData: muestra los datos de una carta.
showCardData :: CardData -> CardDef -> Doc
showCardData cd cdef = case searchCD' cd cdef of
                          Nothing   -> printCardInvalid
                          Just card -> printCard card