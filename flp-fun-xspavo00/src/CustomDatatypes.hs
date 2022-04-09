-- FLP proj 1 - BKG-2-CNF
-- Author: Dávid Špavor (xspavo00)
-- File: CustomDatatypes.hs
-- Year: 2022
module CustomDatatypes where 


type Neterminals = String 
type Terminals =  String 
type StartNeterminal = String 
type Rules = (Neterminals, [String])
type NaSets = (Neterminals, [String])

data Gramatics = Gramatics { neterminals :: [Neterminals]  
                         , terminals :: [Terminals]  
                         , startingTerminal :: StartNeterminal 
                         , rules :: [Rules]   
                         } deriving (Show)   
                         
data Arguments = Arguments{ isPrintBKG :: Bool
                        , isPrintRules :: Bool 
                        , isPrintCNF :: Bool
                        , filePath :: String       
}deriving (Show)

data SymbolType = IsTerminal | IsNeterminal deriving (Show,Eq)