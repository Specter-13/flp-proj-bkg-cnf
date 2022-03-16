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
        newRules = complexCnf
        -- newRules = getTerminalRules oldRules ters ++ getDoubleNonTerminalRules oldRules neters ++ complexCnf
            where complexCnf = foldl f [] (getComplexRules oldRules)
                    where
                        f acc x =  createCnfRules (fst x) rigthSideComplex neters ters ++ acc
                            where
                                rigthSideComplex = createComplexNeterminal (snd x) 




getComplexRules :: [Rules] -> [Rules]
getComplexRules = filter (\x -> length (snd x) > 2)

-- ABC -> A<BC>
createComplexNeterminal :: String -> String
createComplexNeterminal [] = error "Empty list"
createComplexNeterminal [_] = error "Not enough symbols"
createComplexNeterminal (x:xs) = [x] ++ "<" ++ xs ++ ">"

-- S -> A<BCD> createCnfRules "S" "<ABCD>" ["A","B","C","D","S"] ["a"]
createCnfRules :: String -> String -> [Neterminals] -> [Terminals] -> [Rules]
createCnfRules _ [] _ _ = error "Nothing"
createCnfRules _ [x] _ _ = error "Nothing"
createCnfRules _ [x,y] _ _ = error "Nothing"
createCnfRules _ [x,y,w] _ _ = error "Nothing"
createCnfRules _ [x,y,w,z] _ _ = error "Nothing"
createCnfRules _ [x,y,w,z,d] _ _ = [(leftSide,rightSide)]
    where
        leftSide = [y, z, w, d]
        rightSide = [z, w]
createCnfRules left (x:y:z:xs) neters ters = (oldComplex,newComplex) : createCnfRules oldComplex newComplex neters ters
    where
        oldComplex = [y]++[z]++xs
        newNeterminal = [y] ++ xs -- <CD>
        firstRightSymbol = z -- B
        newComplex = [firstRightSymbol] ++ newNeterminal --B<CD> 

    -- | length newRight == 4 =  newRule: [] -- <CD>
    -- | otherwise = newRule : createCnfRules left newRight neters ters
    --     where
    --     firstRightSymbol = [head right] -- A
    --     complexNeterminal = getComplexNeterminalFrom right -- <BCD>
    --     newRight = "<" ++ tail (tail complexNeterminal) -- <CD>
    --     newRule = (firstRightSymbol, newRight)
    --     accumulator = newRule : createCnfRules left newRight neters ters
-- createCnfRules (x:xs) neters ters
--     | x `elem` ters = (x ++ "'", "<" ++ xs ">") : createCnfRules neters ters xs
--     | x `elem` neters = (x , "<" ++ xs ">") : createCnfRules neters ters xs

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
    | isPrintCNF a = print cnfBKG
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


