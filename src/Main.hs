import System.Environment
import Data.List (intercalate)
import CustomDatatypes
import Parser
import RemoverOfSimpleRules
import Control.Arrow (ArrowChoice(right))

-- Main program
main :: IO ()
main = do
 args <- getArgs
 let arguments = parseCommands args
 --print arguments
 --print (isPrintBKG arguments)
 content <- readInput $ filePath arguments
 runProgramByArg arguments content

convertToCnf :: Gramatics -> Gramatics
convertToCnf bkg  = Gramatics neters ters startTer newRules
    where
        neters = neterminals bkg
        ters = terminals bkg
        startTer = startingTerminal bkg
        oldRules = rules bkg
        --newRules = complexCnf
        newRules = getTerminalRules oldRules ters ++ getDoubleNonTerminalRules oldRules neters ++ complexCnf
            where complexCnf = foldl f [] (getComplexRules oldRules)
                    where
                        f acc x =  (leftSideSimple,rigthSideComplex) : createCnfRules leftSideSimple rigthSideComplex neters ters ++ acc
                            where
                                rigthSideComplex = createComplexNeterminal (snd x) ters
                                leftSideSimple = fst x


-- A->A<BC> = A
getPrefixOfRightSide :: String  -> [Terminals] -> String
getPrefixOfRightSide [] _ = error "Wrong input"
getPrefixOfRightSide [_] _ = error "Wrong input"
getPrefixOfRightSide [_,_] _ = error "Wrong input"
getPrefixOfRightSide (x:y:xs) terms
    | [x] `elem` terms && y == '\'' = x : [y]
    | otherwise = [x]

-- A->A<BC> = <BC> 
getSufixOfRightSide :: String -> [Terminals] -> String
getSufixOfRightSide [] _ = error "Wrong input"
getSufixOfRightSide [_] _ = error "Wrong input"
getSufixOfRightSide (x:xs) terms
    | [x] `elem` terms = tail xs
    | otherwise = xs


-- <BCD> = B pr <bCD> = b'
getFirstRightSufix :: String -> [Terminals] -> String
getFirstRightSufix [] _ = error "Wrong input"
getFirstRightSufix [_] _ = error "Wrong input"
getFirstRightSufix [_,_] _ = error "Wrong input"
getFirstRightSufix (_:y:xs) ters
    | [y] `elem` ters = y : "'"
    | otherwise = [y]

-- get only rules, which lenght is more than 2
getComplexRules :: [Rules] -> [Rules]
getComplexRules = filter (\x -> length (snd x) > 2)

-- ABC -> A<BC>
createComplexNeterminal :: String -> [Terminals] -> String
createComplexNeterminal [] _ = error "Empty list"
createComplexNeterminal [_] _ = error "Not enough symbols"
createComplexNeterminal (x:xs) terms
    | [x] `elem` terms = [x] ++ "'<" ++ xs ++ ">"
    | otherwise  = [x] ++ "<" ++ xs ++ ">"

-- S -> A<BCD> createCnfRules "S" "A<BCD>" ["A","B","C","D","S"] ["a"]
createCnfRules :: String -> String -> [Neterminals] -> [Terminals] -> [Rules]
createCnfRules left right neters ters 
    | length rightSufix == 4 = [(rightSufix,lastNeterminal)] -- end recursion when rigtsufix is <CD>
    | otherwise = (rightSufix,newComplex) : createCnfRules rightSufix newComplex neters ters 
    -- | otherwise =   (rightSufix,newComplex): (firstRightSufix,[head firstRightSufix]) : createCnfRules rightSufix newComplex neters ters
        where
            rightPrefix = getPrefixOfRightSide right ters -- A
            rightSufix = getSufixOfRightSide right ters -- <BCD>
            firstRightSufix = getFirstRightSufix rightSufix ters -- B or b'
            newNeterminal = '<' : tail (tail rightSufix) -- <CD>
            newComplex = firstRightSufix ++ newNeterminal -- B<CD>
            lastNeterminal = init (tail rightSufix) -- CD


-- get only rules in format A->b where b is terminal
getTerminalRules :: [Rules] -> [Terminals] -> [Rules]
getTerminalRules rls ters = filter isTerminalRule rls
    where
        isTerminalRule rl = snd rl `elem` ters

-- get only rules in format A->BC where B,C are neterminals
getDoubleNonTerminalRules :: [Rules] -> [Neterminals] -> [Rules]
getDoubleNonTerminalRules rls neters = filter isDoubleNonTerminalRule rls
    where
        isDoubleNonTerminalRule rl = length rightSide == 2 && [head rightSide] `elem` neters && [last rightSide] `elem` neters
            where rightSide = snd rl

--read input, whether from file or stdin
readInput :: FilePath -> IO String
readInput fileName =
    if fileName == "" then getContents else readFile fileName

--run program based on input arguments
runProgramByArg :: Arguments -> String -> IO ()
runProgramByArg a input
    | isPrintBKG a = printBKG parsedBKG
    | isPrintRules a = printBKG withoutSimpleRulesBKG
    | isPrintCNF a = printBKG cnfBKG
    | otherwise = print "cau"
        where
            parsedBKG = parseGramatics $ lines input
            naSets = createNaSets (neterminals parsedBKG) (rules parsedBKG)
            withoutSimpleRulesBKG = removeSimpleRules parsedBKG naSets
            cnfBKG = convertToCnf withoutSimpleRulesBKG

--print internal representation of BKG after syntax check
printBKG :: Gramatics -> IO ()
printBKG bkg = do
    putStrLn (unwords' (neterminals bkg))
    putStrLn (unwords' (terminals bkg))
    putStrLn $ startingTerminal bkg
    let rulesBasicFormat = map f (rules bkg)
            where
                f rule = fst rule ++ "->" ++ snd rule
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


