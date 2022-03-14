import CustomDatatypes
import System.Environment
import System.IO





-- Main program
main :: IO ()
main = do
 args <- getArgs
 let arguments = parseCommands args
 print arguments
 print (isPrintBKG arguments)
 content <- readInput $ filePath arguments
 runProgramByArg arguments content


--read input, whether from file or stdin
readInput :: FilePath -> IO String
readInput fileName = do
    if fileName == "" then getContents else readFile fileName

--
runProgramByArg :: Arguments -> String -> IO ()
runProgramByArg a input
    | isPrintBKG a = print (parseGramatics $ lines input)
    | isPrintRules a = print (parseGramatics $ lines input)
    | isPrintCNF a = print (parseGramatics $ lines input)
    | otherwise = print "cau"

parseGramatics :: [String] -> Gramatics 
parseGramatics [] = error "Wrong input format!"
parseGramatics [_] = error "Wrong input format!"
parseGramatics [_,_] = error "Wrong input format!"
parseGramatics [_,_,_] = error "Wrong input format!"
parseGramatics (x:y:z:xs) = let
    parsedNeterminals = parseSymbols x IsNeterminal
    parsedTerminals = parseSymbols y IsTerminal
    startNeterminal
        | z `elem` parsedNeterminals = z
        | otherwise = error "Starting symbol isn't neterminal!"
    parsedRules = parseRules xs parsedNeterminals parsedTerminals
    in Gramatics parsedNeterminals parsedTerminals startNeterminal parsedRules


parseRules :: [String] -> [String] -> [String] -> [(Neterminal,String)]
parseRules [] _ _ = error "No rules!"
parseRules xs parsedNeterminals parsedTerminals  = map f xs
    where f rule = parseRule rule parsedNeterminals parsedTerminals


parseRule :: String -> [String] -> [String] -> (Neterminal,String)
parseRule [] _ _ = error "Wrong rule format!"
parseRule [_] _ _ = error "Wrong rule format!"
parseRule [_,_] _ _ = error "Wrong rule format!"
parseRule [_,_,_] _ _ = error "Wrong rule format!"
parseRule (x:y:z:xs) parsedNeterminals parsedTerminals 
    | [x] `elem` parsedNeterminals && y  == '-' && z == '>' = ([x],xs)
    | otherwise = error "Error in rule format!"

--  let commands = parseCommands args


--  rs <- sequence [getLine, getLine, getLine, getContentgetLines ]  
--  print (splitByComma (head rs))  

--parse program arguments
parseCommands :: [String] -> Arguments
parseCommands []  = error "No arguments"
parseCommands [x]
    | x == "-i" = Arguments True False False ""
    | x == "-1" = Arguments False True False ""
    | x == "-2" = Arguments False False True ""
    | otherwise = Arguments False False False x
parseCommands [x,y]
    | x == "-i" = Arguments True False False y
    | x == "-1" = Arguments False True False y
    | x == "-2" = Arguments False False True y
    | otherwise = error "Unknown argument"
parseCommands (_:_:_) = error "Too many arguments"




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


-- splitByComma :: String -> [String]
-- splitByComma [] = []
-- splitByComma (x:xs)
--  | x == ',' = z: splitByComma xs
--  | otherwise = [x] : z 
--  where z = []

-- createGramatics :: [String] -> Gramatics 
-- createGramatics xs = foldl f [] xs
--  where f = 


-- main = getArgs >>= parse >>= putStr . tac

-- tac  = unlines . reverse . lines

-- parse ["-h"] = usage  >> exit
-- parse ["-i"] = printBKG >> exit
-- parse ["-1"] = printBKGsimpleFree >> exit
-- parse ["-2"] = printCNF >> exit
-- parse []     = getContents
-- parse fs     = concat `fmap` mapM readFile fs

-- usage   = putStrLn "Usage: tac [-vh] [file ..]"
-- printBKG = putStrLn "Haskell tac 0.1"
-- printBKGsimpleFree = putStrLn "Simple free"
-- printCNF = putStrLn "Simple free CNF"
-- exit    = exitWith ExitSuccess
-- die     = exitWith (ExitFailure 1)


