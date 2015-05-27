module Hex

import Data.Bits
import Seq

hexLookup : List Char
hexLookup = unpack "0123456789ABCDEF"

hexCharToNat : Char -> Maybe Nat
hexCharToNat c = elemIndex (toUpper c) hexLookup

natToHexChar : Nat -> Maybe Char
natToHexChar n = index' n hexLookup

byteToHexNats : (Bits 8) -> (Nat, Nat)
byteToHexNats b = let word = bitsToInt b in ((cast word) `div` 16, (cast word) `mod` 16)

hexNatsToByte : (Nat, Nat) -> (Bits 8)
hexNatsToByte (hi, lo) = intToBits (cast (hi*16+lo))

hexStringToBytes : String -> Maybe (List (Bits 8))
hexStringToBytes s = do il <- sequence (map hexCharToNat (unpack s))
                        cl <- chunk2 il
                        return (map hexNatsToByte cl)
                        
bytesToHexString : List (Bits 8) -> Maybe String
bytesToHexString lb = let nats = unchunk2 $ map byteToHexNats lb in
                      do chars <- sequence (map natToHexChar nats)
                         return (pack chars)            


