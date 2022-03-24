module Parser where 
import CustomDatatypes
import Data.Maybe

-- parsing gramatics with correct syntax into Gramatics datatype
parseGramatics :: [String] -> Gramatics
parseGramatics [] = error "Wrong input format!"
parseGramatics [_] = error "Wrong input format!"
parseGramatics [_,_] = error "Wrong input format!"
parseGramatics [_,_,_] = error "Wrong input format!"
parseGramatics (x:y:z:xs) = let
    parsedNeterminals = parseSymbols x IsNeterminal
    parsedTerminals = parseSymbols y IsTerminal
    parsedRules = parseRules xs parsedNeterminals parsedTerminals
    startNeterminal
        | isNothing (lookup z parsedRules) = error "Missing rule, which starting with start terminal!"
        | z `elem` parsedNeterminals = z
        | otherwise = error "Starting symbol isn't neterminal!"
    in Gramatics parsedNeterminals parsedTerminals startNeterminal parsedRules

-- parse array of Rules into array of tuples [(Neterminals,string)] in Gramatics datatype
parseRules :: [String] -> [Neterminals] -> [Terminals] -> [Rules]
parseRules [] _ _ = error "No rules!"
parseRules xs parsedNeterminals parsedTerminals  = map f xs
    where f rule = parseRule rule parsedNeterminals parsedTerminals

-- check syntax of one rule 
parseRule :: String -> [Neterminals] -> [Terminals] -> Rules
parseRule [] _ _ = error "Wrong rule format!"
parseRule [_] _ _ = error "Wrong rule format!"
parseRule [_,_] _ _ = error "Wrong rule format!"
parseRule [_,_,_] _ _ = error "Wrong rule format!"
parseRule (x:y:z:xs) parsedNeterminals parsedTerminals
    | [x] `elem` parsedNeterminals && y  == '-' && z == '>' && evaluateRightSide xs parsedNeterminals parsedTerminals = ([x],splitRightSideOfRule xs)
    | otherwise = error "Error in rule format!"

-- split right side of rule into list of strings
splitRightSideOfRule :: String -> [String]
splitRightSideOfRule [] = []
splitRightSideOfRule (x:xs) = [x] : splitRightSideOfRule xs

-- check syntax of right side of rule
evaluateRightSide :: String -> [Neterminals] -> [Terminals] -> Bool
evaluateRightSide [] _ _ = False
evaluateRightSide [x] parsedNeterminals parsedTerminals = [x] `elem` parsedNeterminals || [x] `elem` parsedTerminals
evaluateRightSide (x:xs) parsedNeterminals parsedTerminals 
    | [x] `elem` parsedNeterminals || [x] `elem` parsedTerminals = evaluateRightSide xs parsedNeterminals parsedTerminals 
    | otherwise = False 

-- parse terminals and neterminals - parsing first and second line
parseSymbols :: String -> SymbolType -> [String]
parseSymbols [] symbolType = let message = "No symbols of type " ++ show symbolType in error message
parseSymbols xs symbolType
    | head xs == ',' || last xs == ',' = let message = "Wrong format of type " ++ show symbolType in error message
    | otherwise = foldr f [] xs
        where
            f x acc
                | [x] `elem` acc = let message = "Reapeating symbols in type of " ++ show symbolType in error message
                | x `elem` ['A'..'Z'] && symbolType == IsNeterminal || (x `elem` ['a'..'z'] && symbolType == IsTerminal) = [x] : acc
                | x == ',' = acc
                | otherwise  =  let message = "Unexpected symbols in type of " ++ show symbolType in error message

