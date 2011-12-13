module Main where

import Data.List
import Number.Fibonacci(fibs)

main :: IO ()
main = print $ snd $ head $ dropWhile (\(x,y) -> (not . bothNinePandigit "123456789") x) (zip fibs [1..])

{--
  | test last 9 first make great difference
--}
    
bothNinePandigit digits n =  isLastNinePandigit digits n && isFirstNinePandigit digits n

isLastNinePandigit  digits n = digits == sort (lastDigits 9 n)
isFirstNinePandigit digits n = digits == sort (firstDigits 9 n)

firstDigits k n = take k (show n)
lastDigits  k n = show (n `mod` 10^k)

{-- FROM haskell-beginer mail
Aha, this is sneaky!

Having a bunch of function calls should not make
a difference if you are compiling with -O2 (you are compiling with
-O2, aren't you)?  Nonetheless, even compiling with -O2 I was also
getting the results you mention -- the wiki version was pretty fast
(about 24s) whereas your version took more than 15 minutes.

So I ran your version with profiling to help figure out what was going
on.  I compiled with

 ghc --make -O2 -prof -auto-all -rtsopts PE104.hs

and then ran with

 ./PE104 +RTS -p -RTS

This causes a file "PE104.prof" to be written which has a bunch of
data on execution time and allocation broken down by function. The
results showed that 95% of the program's run time was being spent in
'firstDigits'.

And then it hit me -- the difference is due to the fact that your
version and the wiki version test the first digits and the last digits
in a different order!

'show' on integers is (relatively) very slow.  Your version first
tests the first 9 digits of the number -- note that computing the
first digits of a number requires computing all the digits, even the
ones that don't get shown.  Only if the first 9 digits are "123456789"
does your version go on to test the last nine digits (since (&&) is
lazy).  The wiki version, on the other hand, first tests the last 9
digits (much faster) and only if those are "123456789" does it bother
doing the (expensive) test for the first 9 digits.  Since only 112 out
of the first 329000 or so Fibonacci numbers end in the digits 1..9,
this makes a huge difference.

-Brent
--}
