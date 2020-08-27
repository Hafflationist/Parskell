module Test where
--
--import Data.List
--
--add x y = x+y
--mul x y = x*y
--sub x y = x-y
--
----"Main"
--arith :: [Char] -> Int2+2

--arith x = a_rec 0 0 (add 0) x
--
--a_rec :: Int -> Int -> (Int -> Int) -> [Char] -> Int
--a_rec ans buf func [] = func ans buf
----a_rec ans buf func [+,...] = a_rec (func ans buf) 0 add (tail str)
----a_rec ans buf func [*,...] = a_rec (func ans buf) 0 mul (tail str)
----a_rec ans buf func [-,...] = a_rec (func ans buf) 0 sub (tail str)
----a_rec ans buf fucn str = a_rec ans ((buf * 10) + (head str)) func (tail str)