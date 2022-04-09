-- FLP proj 1 - BKG-2-CNF
-- Author: Dávid Špavor (xspavo00)
-- File: RemoverOfSimpleRules.hs
-- Year: 2022
module RemoverOfSimpleRules where 
import CustomDatatypes
    ( Gramatics(..), NaSets, Neterminals, Rules )
import Data.List (nub,union)

-- create N_A set for each neterminal in parsed gramatics
createNaSets :: [Neterminals] -> [Rules] -> [NaSets]
createNaSets neter rls = foldl f [] neter
    where
        f acc x = (x, nxSet)  : acc
            where
                nxSet = createNaSetRecursive x neter simpleRules [x]
                    where 
                        simpleRules = [rule | rule <- rls , fst rule `elem` neter && length (snd rule ) == 1 && head (snd rule) `elem` neter]


-- for neterminal create N_A set based on algorithm 4.5 from TIN
createNaSetRecursive :: Neterminals -> [Neterminals] ->[Rules] -> [String]-> [String]
createNaSetRecursive neterminal parsedNeterminals simpleRules prev = let
    accumulator = foldl f prev simpleRules
        where f acc x 
                | leftSide `elem` acc  && rightSide `notElem` acc = rightSide : acc 
                | otherwise = acc
                    where
                        rightSide = head (snd x)
                        leftSide = fst x
    in if accumulator == prev then accumulator else createNaSetRecursive neterminal parsedNeterminals simpleRules accumulator `union` prev


--remove simple rules from parsed gramatics and return new bkg gramatics
removeSimpleRules :: Gramatics -> [NaSets] -> Gramatics
removeSimpleRules bkg naSets = Gramatics neters ter startTer (nub newRules)
    where
        neters = neterminals bkg
        ter = terminals bkg
        startTer = startingTerminal bkg
        newRules = foldl f [] naSets
            where
                f acc x =  getRulesBasedOnNaSet x neters (rules bkg) ++ acc
                    
-- acquire only complex rules, which depends on elements in N_A set
getRulesBasedOnNaSet :: NaSets -> [Neterminals] -> [Rules] -> [Rules]
getRulesBasedOnNaSet naSet neters rls = foldl f [] (snd naSet)
    where
        f acc x = getOnlyComplexRules x rls neters (fst naSet) ++ acc

-- get complex rules for concrete neterminal A from N_A set
getOnlyComplexRules :: Neterminals -> [Rules] -> [Neterminals] -> Neterminals -> [Rules]
getOnlyComplexRules neter rls neters firstNeter = foldl f [] rls
    where f acc rl
            | fst rl == neter && isComplexRule rl neters = (firstNeter, snd rl): acc
            | otherwise = acc

-- help function which determine, whether rule is complex or not
isComplexRule :: Rules -> [Neterminals]  -> Bool
isComplexRule rl neter
    | length (snd rl) == 1 &&  head rightSide `elem` neter = False
    | otherwise = True 
        where 
            rightSide = snd rl