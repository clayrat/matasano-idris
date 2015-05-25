module Main

import Data.Bits
import Hex
import Base64

main : IO () 
main = putStrLn $ show $ do bytes <- hexStringToBytes "49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6e6f7573206d757368726f6f6d"
                            encoded <- encodeBytes bytes
                            return encoded