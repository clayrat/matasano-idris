module Base64

import Data.Bits
import Bytes

octetsToSextets: List (Bits 8) -> List (Bits 6)
octetsToSextets Nil = Nil
octetsToSextets (b1 :: b2 :: b3 :: bs) = head6 b1 :: append (tail2 b1) (head4 b2) :: append (tail4 b2) (head2 b3) :: tail6 b3 :: octetsToSextets bs
octetsToSextets (b1 :: b2 :: Nil) = head6 b1 :: append (tail2 b1) (head4 b2) :: zeroShift (tail4 b2) :: Nil
octetsToSextets (b1 :: Nil) = head6 b1 :: zeroShift (tail2 b1) :: Nil

base64lookup : List Char
base64lookup = unpack "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"

natToB64 : Nat -> Maybe Char
natToB64 n = index' n base64lookup  

encodeSextets : List (Bits 6) -> Maybe String
encodeSextets l = do chars <- sequence $ map (natToB64 . cast . bitsToInt) l
                     return (pack chars)

encodeBytes : List (Bits 8) -> Maybe String 
encodeBytes l = do coded <- encodeSextets (octetsToSextets l)
                   return (coded ++ pack (replicate ((3 - ((length l) `mod` 3)) `mod` 3) '='))