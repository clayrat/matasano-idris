module Bytes

import Data.Bits

head2: Bits 8 -> Bits 2
head2 bits = truncate {m=6} (shiftRightLogical bits (intToBits 6))

head4: Bits 8 -> Bits 4
head4 bits = truncate {m=4} (shiftRightLogical bits (intToBits 4))

head6: Bits 8 -> Bits 6
head6 bits = truncate {m=2} (shiftRightLogical bits (intToBits 2))

tail2: Bits 8 -> Bits 2
tail2 = truncate {m=6} 

tail4: Bits 8 -> Bits 4
tail4 = truncate {m=4} 

tail6: Bits 8 -> Bits 6
tail6 = truncate {m=2} 

zeroShift : Bits n -> Bits (n+m)
zeroShift {m=m} bits = shiftLeft (zeroExtend bits) (intToBits (cast m))

append : Bits m -> Bits n -> Bits (m + n)
append {m=m} {n=n} a b = zeroShift a `or` replace (sym (plusCommutative m n)) (zeroExtend b)
