import System.Environment
import Data.List (intercalate,nub)
import CustomDatatypes
import Parser


-- Main program
main :: IO ()
main = do
 args <- getArgs
 let arguments = parseCommands args
 --print arguments
 --print (isPrintBKG arguments)
 content <- readInput $ filePath arguments
 runProgramByArg arguments content

removeSimpleRules :: Gramatics -> [NaSets] -> Gramatics
removeSimpleRules bkg naSets = Gramatics neters ter startTer (nub newRules)
    where
        neters = neterminals bkg
        ter = terminals bkg
        startTer = startingTerminal bkg
        newRules = foldl f [] naSets
            where
                f acc x =  getRulesBasedOnNaSet x neters (rules bkg) ++ acc
                    


--getRulesBasedOnNaSet ("E",["E","T","F"]) ["E","T"] [("E","EaT"), ("E","T"),("T","TbF"), ("T","F"),("F","cFc"),("F","i")] 

getRulesBasedOnNaSet :: NaSets -> [Neterminals] ->[Rules] -> [Rules]
getRulesBasedOnNaSet naSet neters rls = foldl f [] (snd naSet)
    where
        f acc x = getOnlyComplexRules x rls neters (fst naSet) ++ acc

getOnlyComplexRules :: Neterminals -> [Rules] -> [Neterminals]-> Neterminals ->[Rules]
getOnlyComplexRules neter rls neters firstNeter = foldl f [] rls
    where f acc rl
            | fst rl == neter && isComplexRule rl neters = (firstNeter,snd rl): acc
            | otherwise = acc


isComplexRule :: Rules -> [Neterminals]  -> Bool
isComplexRule rl neter
    | snd rl `notElem` neter = True
    | otherwise = False


createNaSets :: [Neterminals] -> [Rules] -> [NaSets]
createNaSets neter rls = foldl f [] neter
    where
        f acc x = (x, nxSet)  : acc
            where
                nxSet = createNaSetForNeterminal x neter rls

-- create N_A set for neterminal A
createNaSetForNeterminal :: Neterminals -> [Neterminals] ->[Rules] -> [String]
createNaSetForNeterminal neterminal parsedNeterminals = foldl f [neterminal]
    where
        f acc x
            | rightSide `elem` parsedNeterminals &&
              rightSide `notElem` acc &&
              leftSide `elem` acc = rightSide : acc
            | otherwise = acc
                where
                    rightSide = snd x
                    leftSide = fst x

--read input, whether from file or stdin
readInput :: FilePath -> IO String
readInput fileName =
    if fileName == "" then getContents else readFile fileName

--run program based on input arguments
runProgramByArg :: Arguments -> String -> IO ()
runProgramByArg a input
    | isPrintBKG a = printBKG (parseGramatics $ lines input)
    | isPrintRules a = let
        parsedBKG = parseGramatics $ lines input
        naSets = createNaSets (neterminals parsedBKG) (rules parsedBKG)
        withoutSimpleRulesBKG = removeSimpleRules parsedBKG naSets
        in printBKG withoutSimpleRulesBKG
    | isPrintCNF a = print (parseGramatics $ lines input)
    | otherwise = print "cau"

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


