import System.Environment
import Data.List (intercalate)
import CustomDatatypes
import Parser
import RemoverOfSimpleRules
import ConverterToCnf


-- MAP DOUBLE RULES TO CORRECT AND FIX a' rule creation!

-- Main program
main :: IO ()
main = do
 args <- getArgs
 let arguments = parseCommands args
 --print arguments
 --print (isPrintBKG arguments)
 content <- readInput $ filePath arguments
 runProgramByArg arguments content



--read input, whether from file or stdin
readInput :: FilePath -> IO String
readInput fileName =
    if fileName == "" then getContents else readFile fileName

--run program based on input arguments
runProgramByArg :: Arguments -> String -> IO ()
runProgramByArg a input
    | isPrintBKG a = printBKG parsedBKG
    | isPrintRules a = printBKG withoutSimpleRulesBKG
    | isPrintCNF a = printBKG cnfBKG -- fix!
    | otherwise = print "cau"
        where
            parsedBKG = parseGramatics $ lines input
            naSets = createNaSets (neterminals parsedBKG) (rules parsedBKG)
            withoutSimpleRulesBKG = removeSimpleRules parsedBKG naSets
            cnfBKG = convertToCnf parsedBKG

--print internal representation of BKG after syntax check
printBKG :: Gramatics -> IO ()
printBKG bkg = do
    putStrLn (unwords' (neterminals bkg))
    putStrLn (unwords' (terminals bkg))
    putStrLn $ startingTerminal bkg
    let rulesBasicFormat = map f (rules bkg)
            where
                f rule = fst rule ++ "->" ++ concat (snd rule)
    putStrLn (intercalate "\n" rulesBasicFormat)

unwords' :: [String] -> String
unwords' = intercalate ","

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




