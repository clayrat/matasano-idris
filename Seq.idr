module Seq

chunk2 : List a -> Maybe (List (a,a))
chunk2 l = if ((length l) `mod` 2) == 1 then Nothing else Just (chunk2H l) where
  chunk2H Nil = Nil
  chunk2H (x1 :: x2 :: xs) = (x1, x2) :: chunk2H xs
  
unchunk2 : List (a,a) -> List a
unchunk2 Nil = Nil
unchunk2 ((x1, x2) :: xs ) = x1 :: x2 :: unchunk2 xs

zipWithI : List a -> List b -> (a -> b -> c) -> List c
zipWithI Nil _ _ = Nil
zipWithI _ Nil _ = Nil
zipWithI (a :: as) (b :: bs) f = f a b :: zipWithI as bs f 
