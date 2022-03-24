import System.Environment
import Data.List (intercalate,nub)
import CustomDatatypes
import Parser
import RemoverOfSimpleRules
import Control.Arrow (ArrowChoice(right))


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

-- get only rules in format A->b where b is terminal
getTerminalRules :: [Rules] -> [Terminals] -> [Rules]
getTerminalRules rls ters = filter isTerminalRule rls
    where
        isTerminalRule rl = length rightSide == 1 && head rightSide `elem` ters
            where
                rightSide = snd rl

-- get only rules in format A->BC where B,C are neterminals
getDoubleNonTerminalRules :: [Rules] -> [Neterminals] -> [Rules]
getDoubleNonTerminalRules rls neters = filter isDoubleNonTerminalRule rls
    where
        isDoubleNonTerminalRule rl = length rightSide == 2 && head rightSide `elem` neters && last rightSide `elem` neters
            where rightSide = snd rl

-- get rules, which length of right side is bigger than 2 -> A->ABC, 
getTreblePlusRules :: [Rules] -> [Rules]
getTreblePlusRules = filter isTreble
    where
        isTreble rl = length rightSide > 2
            where
                rightSide = snd rl

filterApostropheRules :: [Neterminals] -> [Terminals] -> [Neterminals]
filterApostropheRules neters ters = filter (\x -> [head x] `elem` ters) neters

createApostropheRules :: [Neterminals] -> [Rules]
createApostropheRules = foldl f []
    where f acc x = (x,[[head x]]):acc

convertToCnf :: Gramatics -> Gramatics
convertToCnf bkg  = Gramatics neters ters startTer newRules
    where
        neters = neterminals bkg ++ snd cnfRulesTuple
        ters = terminals bkg
        startTer = startingTerminal bkg
        oldRules = rules bkg
        terminalRules = getTerminalRules oldRules ters
        doubleNonTerminalRules = getDoubleNonTerminalRules oldRules neters
        cnfRulesTuple =  createCnfRules oldRules neters ters
        newRules = terminalRules ++ doubleNonTerminalRules ++ fst cnfRulesTuple ++ apostropheRules
        apostropheRules = createApostropheRules (filterApostropheRules neters ters) 



-- create cnfRules and return them with new created neterminals
createCnfRules :: [Rules]-> [Neterminals] -> [Terminals] -> ([Rules],[Neterminals])
createCnfRules rls neters ters = foldl f ([],[]) treblePlusRules
            where
                treblePlusRules = getTreblePlusRules rls
                f acc rl = (newFirstFormatRule: fst acc ++ newRules, snd acc ++ newNeterminals)
                    where
                        newFirstFormatRule =  createFirstNewFormat rl ters
                        newNeterminalsTuple = createNewNeterminals newFirstFormatRule neters ters
                        newNeterminals = snd newNeterminalsTuple
                        newRules = fst newNeterminalsTuple

-- S -> A<BC> ===  <BC> -> BC
createNewNeterminals :: Rules -> [Neterminals] -> [Terminals] -> ([Rules],[Neterminals])
createNewNeterminals rl neters ters
    | length suffix == 4 = ([(suffix,suffixArrayRaw)],newNeters) -- end recursion when rightsuffix is <CD>
    | otherwise = ((suffix,newNeterminal) : fst newNeterminalsTuple, newNeters ++ snd newNeterminalsTuple )
        where
            (x,[prefix,suffix]) = rl-- S -> ABC === S -> A<BC>
            leftSide = fst rl -- original left side
            rightSide = snd rl -- original right side
            suffixArrayRaw
                | firstSymbol `elem` ters && secondSymbol `elem` ters = [firstSymbol ++ "\'", secondSymbol ++ "\'"]
                | firstSymbol `elem` ters = [firstSymbol ++ "\'", secondSymbol]
                | secondSymbol `elem` ters = [firstSymbol, secondSymbol ++ "\'"]
                | otherwise = [firstSymbol,secondSymbol]
                    where
                        firstSymbol = head newNeterminal
                        secondSymbol = [last (init suffix)] -- created 2 last neterminals for end of recursion
            newNeterminal = extractNewNeterminal suffix ters
            newRule = (suffix,newNeterminal)
            newNeters
                | firstSymbol `elem` ters && secondSymbol `elem` ters = [suffix, firstSymbol ++ "\'", secondSymbol ++ "\'"]
                | firstSymbol `elem` ters = [suffix, firstSymbol ++ "\'"]
                | secondSymbol `elem` ters = [suffix, secondSymbol ++ "\'"]
                | otherwise = [suffix]
                where
                    firstSymbol = head newNeterminal
                    secondSymbol = [last (init suffix)]
            newNeterminalsTuple = createNewNeterminals newRule newNeters ters



-- <ABC> = A<BC> or <aBC> = a'<BC>
extractNewNeterminal :: String -> [Terminals] -> [String]
extractNewNeterminal [] _= error "Empty input of rule"
extractNewNeterminal [_] _= error "Empty input of rule"
extractNewNeterminal (_:x:xs) ters
    | [x] `elem` ters = [x : "\'", "<"++xs]
    | otherwise = [[x], "<"++xs]

-- S -> ABC === S -> A<BC> or S -> aBC === S -> a'<BC>
createFirstNewFormat :: Rules -> [Terminals] -> Rules
createFirstNewFormat (_,[]) _ = error "Wrong format of rule in firstNewFormat"
createFirstNewFormat (left,y:ys) ters = newFormat
    where
        newFormat
            | y `elem` ters = (left,[y++"\'","<" ++ concat ys ++ ">"])
            | otherwise  = (left,[y,"<" ++ concat ys ++ ">"])



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




