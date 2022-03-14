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
 print content
 runProgramByArg arguments


--read input, whether from file or stdin
readInput :: FilePath -> IO String
readInput fileName = do
    if fileName == "" then getContents else readFile fileName

--
runProgramByArg :: Arguments  -> IO ()
runProgramByArg a
    | isPrintBKG a = parseGramatics
    | isPrintRules a = parseGramatics
    | isPrintCNF a = parseGramatics
    | otherwise = print "cau"

parseGramatics :: IO ()
parseGramatics = print "ahoj"

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






splitByComma :: String -> [String]
splitByComma [] = []
splitByComma (x:xs)
 | x == ',' = splitByComma xs
 | otherwise = [x] : splitByComma xs

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


