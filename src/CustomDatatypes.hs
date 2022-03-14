module CustomDatatypes where 


type Neterminal = String 
type Terminals =  String 
type StartNeterminal = String 
type Rules = (Neterminal, String)

data SymbolType = IsTerminal | IsNeterminal deriving (Show,Eq)

data Gramatics = Gramatics { neterminals :: [Neterminal]  
                         , terminals :: [Terminals]  
                         , startingTerminal :: StartNeterminal 
                         , rules :: [(Neterminal,String)]   
                         } deriving (Show)   
                         
data Arguments = Arguments{ isPrintBKG :: Bool
                        , isPrintRules :: Bool 
                        , isPrintCNF :: Bool
                        , filePath :: String       
}deriving (Show)