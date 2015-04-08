module Main where

import Genes;
import Util;
import Debug.Trace;

fit::Fitness
fit chr = sum $ map fromIntegral chr

fitGenerator::Double->(Double->Double)->Double->Fitness
fitGenerator num f target =  (\chromosome ->
    let x = binToDouble num chromosome
        delta = target - f x :: Double
    in 1.0 / delta ** 2.0)

fit' = fitGenerator 3 (\x -> x * sin x) 3

opts::EvOptions
opts= EvOptions{populationSize = 42,
                individLength = 10,
                maxGeneration = 1000,
                fitness = fit',
                targetFitness = 10^10,
                mutationChance = 0.3,
                elitePart = 0.1}

main::IO()
main = do
   (n,result, bestInd) <- initEvol opts
   print $ "Generation:      " ++ show n
   print $ "Best Individual: " ++ show bestInd
   print $ "Individual val:  " ++ show (binToDouble 3 bestInd)
   print $ "Error equals:    " ++ show (1.0 / (fit' bestInd))
   print $ "Whole population:" ++ show result