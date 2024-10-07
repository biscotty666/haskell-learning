module Main (main) where

import Lib

main :: IO ()
main = do
    print "Enter a number to test for primality:"
    n <- read <$> getLine
    let result = isPrime n
    print (displayResult result)

-- ❯ stack exec headaches-exe
-- "Enter a number to test for primality:"
-- 6
-- "It's composite"
--
-- get-programming-with-Haskell/Unit-7/headaches on  main [?] via λ <custom snapshot> via ❄  impure (nix-shell-env) took 5s
-- ❯ stack exec headaches-exe
-- "Enter a number to test for primality:"
-- 5
-- "It's prime"
--
-- get-programming-with-Haskell/Unit-7/headaches on  main [?] via λ <custom snapshot> via ❄  impure (nix-shell-env) took 7s
-- ❯ stack exec headaches-exe
-- "Enter a number to test for primality:"
-- 213
-- "Value exceed max bound"
--
-- get-programming-with-Haskell/Unit-7/headaches on  main [?] via λ <custom snapshot> via ❄  impure (nix-shell-env) took 7s
-- ❯ stack exec headaches-exe
-- "Enter a number to test for primality:"
-- 0
-- "Value is not valid candidate for prime checking
