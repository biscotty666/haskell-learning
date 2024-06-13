{-
1. Start with the infinite sequence 2, 3, 4, ...
2. Mark first number as prime
3. Delete all multiples of p
4. Return to the second step
-}

primes = sieve [2..]

sieve (p:xs) = p : sieve [x | x <- xs, mod x p /= 0]

-- Twin primes are prime numbers which differ by 2
twin (x,y) = y == x + 2
twins = filter twin (zip primes (tail primes))
