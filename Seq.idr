module Seq

chunk2 : List a -> Maybe (List (a,a))
chunk2 l = if ((length l) `mod` 2) == 1 then Nothing else Just (chunk2H l) where
  chunk2H Nil = Nil
  chunk2H (x1 :: x2 :: xs) = (x1, x2) :: chunk2H xs
