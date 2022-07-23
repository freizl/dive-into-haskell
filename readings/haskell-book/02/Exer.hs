-- | chapter exercise

module Exer where

a1 = (^) 10 $ 1 + 1
a2 = 10 ^ (1 + 1)

b1 = 2 ^ 2 * 4 ^ 5 + 1
b2 = (2^2) * (4^5) + 1

z = 7
y = z + 8 -- 15
x = y ^ 2 -- 225
waxOn = x * 5 -- 1125

triple x = x * 3

t1 = triple waxOn -- 1125 * 3
t2 = triple waxOn2
  where waxOn2 = x * 5

waxOff x = triple x
waxOff2 x = triple (x*2 - 1)
