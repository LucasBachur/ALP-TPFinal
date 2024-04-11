module DeckString where

import Common
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Char8 as BC8
import Data.Word
import Data.Bits
import Data.Word
import Data.List

-- Funciones de decode

-- readVarint: lee un VarInt de una lista de bytes, y devuelve el entero y el resto de la lista (con manejo de errores)
readVarint :: [Word8] -> Either Error (Int, [Word8])
readVarint = readVarint' 0 0
    
readVarint' :: Int -> Int -> [Word8] -> Either Error (Int, [Word8])
readVarint' shift result [] = Left (VarIntUnexpectedEnd "") -- si el VarInt esta codificado correctamente, nunca deberia llegar aca
readVarint' shift result (x:xs)
  | shift >= 63 = Left (VarIntExceedsMaxSize "") -- maximo de tamaño estipulado para VarInt
  | otherwise =
      if i .&. 0x80 /= 0 -- si el primer bit del byte es 1, sigo leyendo guardandome el resultado
            then readVarint' shift' newResult xs
            else Right (newResult, xs) -- si no, devuelvo lo que fui leyendo
      where i = fromIntegral x
            newResult = result .|. (shiftL (i .&. 0x7F) shift)
            shift' = shift + 7

-- fromDeckstring: lee un deckstring (deck codificado en base64) y devuelve una lista con
--                 cada carta y la cantidad presente en el mazo (con manejo de errores)
fromDeckstring :: String -> Either Error [(Int,Int)]
fromDeckstring ds = case mySplitAt 3 (BS.unpack (B64.decodeLenient (BC8.pack ds))) of
                      Left (VarIntUnexpectedEnd _) -> Left (VarIntUnexpectedEnd ds) -- Agrego el string a los posibles errores
                      Left (VarIntExceedsMaxSize _) -> Left (VarIntExceedsMaxSize ds)
                      Left err -> Left err
                      Right (_, sections) -> case parseSections sections of
                                                  Left (VarIntUnexpectedEnd _) -> Left (VarIntUnexpectedEnd ds)
                                                  Left (VarIntExceedsMaxSize _) -> Left (VarIntExceedsMaxSize ds)
                                                  Left (SectionMissing _) -> Left (SectionMissing ds)
                                                  Left err -> Left err
                                                  Right res -> Right res

-- parseSections: toma una lista de bytes y devuelve una lista cada carta y la cantidad presente en el mazo 
--                (con manejo de errores)
parseSections :: [Word8] -> Either Error [(Int,Int)]
parseSections bS = do sections <- parseSections' 4 bS 
                      case sections of
                        (s0:(s1:(s2:(s3:rest)))) -> Right ((map (\x->(1,x)) s1)++(map (\x->(2,x)) s2)++(getTuples s3))
                        _ -> Left (SectionMissing "") -- caso en que no se encuentra la cantidad suficiente de secciones
                      
parseSections' :: Int -> [Word8] -> Either Error [[Int]]
parseSections' 0 _      = Right []
parseSections' n []     = do  sections <- parseSections' (n-1) []
                              return $ ([] : sections)
parseSections' n bS     = do  (x,xs) <- readVarint bS
                              let lastSection = if n==1 then 2 else 1 -- en la ultima seccion, tengo que leer pares
                              (section, rest) <- mySplitAt (x * lastSection) xs
                              sections <- parseSections' (n-1) rest
                              return $ (section:sections)

-- mySpliAt: Toma un entero y una lista de bytes y devuelve: una lista con la cantidad de enteros
--           indicados por el primer argumento (leidos como VarInt); y la lista restante. 
mySplitAt :: Int -> [Word8] -> Either Error ([Int], [Word8])
mySplitAt 0 bS = Right ([], bS)
mySplitAt n bS = do (x,xs) <- readVarint bS
                    (a,b) <- mySplitAt (n-1) xs
                    return $ (x:a,b)

getTuples :: [Int] -> [(Int, Int)]
getTuples []       = []
getTuples [x]      = []
getTuples (x:y:xs) = (y,x):(getTuples xs)

-- Functiones de encode

-- encodeVarInt: toma un entero y devuelve una lista de bytes que contiene el entero codificado como VarInt
encodeVarInt :: Int -> [Word8]
encodeVarInt n
  | n < 0x80 = [fromIntegral n]  -- cuando el entero ocupa menos de 8 bits, el resultado es el mismo
  | otherwise = (fromIntegral ((n .&. 0x7f) .|. 0x80)) : (encodeVarInt (shiftR n 7)) -- si no, pongo el primer bit en 1
                                                                                     -- y sigo codificando
encodeVarIntList :: [Int] -> BS.ByteString
encodeVarIntList l = BS.pack (concat (map encodeVarInt l))

-- toDeckString: toma una lista de pares de enteros -que representan el id de cada carta y 
--               la cantidad de copias en el mazo- y un entero -representanto el id del heroe-,
--               y devuelve el mazo codificado
toDeckstring :: [(Int,Int)] -> Int -> String
toDeckstring d h = let (oneCopy,rest) = partition (\(x,_)->x==1) d
                       (twoCopies,nCopies) = partition (\(x,_)->x==2) rest
                       oneCopy' = map (\(_,y)->y) oneCopy
                       twoCopies' = map (\(_,y)->y) twoCopies
                       nCopies' = foldr (\(x,y) l->x:(y:l)) [] nCopies
                       deckBS = toDeckstring' ([0,1,1,1]++[h]++[length oneCopy']++oneCopy'++
                                                [length twoCopies']++twoCopies'++
                                                [length nCopies]++nCopies')
                    in BC8.unpack deckBS

toDeckstring' :: [Int] -> BS.ByteString
toDeckstring' = B64.encode . encodeVarIntList
