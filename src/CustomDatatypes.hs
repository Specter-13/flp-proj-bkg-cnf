module CustomDatatypes where 


data Gramatics = Gramatics { neterminals :: [String]  
                         , terminals :: [String]  
                         , startingTerminal :: String 
                         , rules :: (String,String)    
                         } deriving (Show)   
data Arguments = Arguments{ isPrintBKG :: Bool
                        , isPrintRules :: Bool 
                        , isPrintCNF :: Bool
                        , filePath :: String       
}deriving (Show)