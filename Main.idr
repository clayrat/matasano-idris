module Main

import Hex
import Base64
import Data.Bits

-- task 1.1
hexToBase64 : String -> Maybe String
hexToBase64 input = do bytes <- hexStringToBytes input
                       encoded <- encodeBytes bytes
                       return encoded
                                 
main : IO () 
main = putStrLn $ show $ hexToBase64 "49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6e6f7573206d757368726f6f6d" 

