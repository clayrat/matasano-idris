module Bytes

import Data.Bits

take: (n: Nat) -> Bits (n+m) -> Bits n
take {m=m} n bits = truncate (replace (sym (plusCommutative m n)) (shiftRightLogical bits (intToBits (cast m))))

drop: (n: Nat) -> Bits (n+m) -> Bits m
drop {m=m} _ = truncate

zeroShift : Bits n -> Bits (n+m)
zeroShift {m=m} bits = shiftLeft (zeroExtend bits) (intToBits (cast m))

append : Bits m -> Bits n -> Bits (m + n)
append {m=m} {n=n} a b = zeroShift a `or` replace (sym (plusCommutative m n)) (zeroExtend b)
