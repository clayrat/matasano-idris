module Main

import Hex
import Base64
import Seq
import Data.Bits

-- task 1.1
hexToBase64 : String -> Maybe String
hexToBase64 input = do bytes <- hexStringToBytes input
                       encoded <- encodeBytes bytes
                       return encoded

-- task 1.2
xorTwo : String -> String -> Maybe String
xorTwo x y = do xb <- hexStringToBytes x
                yb <- hexStringToBytes y
                str <- bytesToHexString (zipWithI xb yb xor)
                return str 

main : IO ()
main = putStrLn $ show $ xorTwo "1c0111001f010100061a024b53535009181c" "686974207468652062756c6c277320657965"

