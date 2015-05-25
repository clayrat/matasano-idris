module Hex

import Data.Bits
import Seq

hexLookup : List Char
hexLookup = unpack "0123456789ABCDEF"

hexCharToNat : Char -> Maybe Nat
hexCharToNat c = elemIndex (toUpper c) hexLookup

hexNatsToByte : (Nat, Nat) -> (Bits 8)
hexNatsToByte (hi, lo) = intToBits (cast (hi*16+lo))

hexStringToBytes : String -> Maybe (List (Bits 8))
hexStringToBytes s = do il <- sequence (map hexCharToNat (unpack s))
                        cl <- chunk2 il
                        return (map hexNatsToByte cl)

