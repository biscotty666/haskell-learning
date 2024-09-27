type Events = [String]
type Probs = [Double]

data PTable = PTable Events Probs

createPTable :: Events -> Probs -> PTable
createPTable events probs = PTable events normalizedProbs
    where totalProbs = sum probs
          normalizedProbs = map (\x -> x/totalProbs) probs

showPair :: String -> Double -> String
showPair event prob = mconcat [event, "|", show prob, "\n"]

instance Show PTable where
    show (PTable events probs) = mconcat pairs
        where pairs = zipWith showPair events probs

cartCombine :: (a -> b -> c) -> [a] -> [b] -> [c]
cartCombine func l1 l2 = zipWith func newL1 cycledL2
    where nToAdd = length l2
          repeatedL1 = map (take nToAdd . repeat) l1
          newL1 = mconcat repeatedL1
          cycledL2 = cycle l2

combineEvents :: Events -> Events -> Events
combineEvents e1 e2 = cartCombine combiner e1 e2
    where combiner = (\x y -> mconcat [x,"-",y])

combineProbs :: Probs -> Probs -> Probs
combineProbs p1 p2 = cartCombine (*) p1 p2

instance Semigroup PTable where
    (<>) ptable1 (PTable [] []) = ptable1
    (<>) (PTable [] []) ptable2 = ptable2
    (<>) (PTable e1 p1) (PTable e2 p2)  = createPTable newEvents newProbs
        where newEvents = combineEvents e1 e2
              newProbs = combineProbs p1 p2

instance Monoid PTable where
    mempty = PTable [] []
    mappend = (<>)

coin :: PTable
coin = createPTable ["heads","tails"] [0.5,0.5]

spinner :: PTable
spinner = createPTable ["red","blue","green"] [0.1,0.2,0.7]

-- ghci> coin <> spinner
-- heads-red|5.0e-2
-- heads-blue|0.1
-- heads-green|0.35
-- tails-red|5.0e-2
-- tails-blue|0.1
-- tails-green|0.35
--
-- ghci> mconcat [spinner,spinner]
-- red-red|1.0000000000000002e-2
-- red-blue|2.0000000000000004e-2
-- red-green|6.999999999999999e-2
-- blue-red|2.0000000000000004e-2
-- blue-blue|4.000000000000001e-2
-- blue-green|0.13999999999999999
-- green-red|6.999999999999999e-2
-- green-blue|0.13999999999999999
-- green-green|0.48999999999999994
