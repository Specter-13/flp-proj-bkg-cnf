module CustomDatatypes where 


type Neterminals = String 
type Terminals =  String 
type StartNeterminal = String 
type Rules = (Neterminals, [String])
type NaSets = (Neterminals, [String])

data SymbolType = IsTerminal | IsNeterminal deriving (Show,Eq)

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