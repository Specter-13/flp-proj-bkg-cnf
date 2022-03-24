import System.Environment
import Data.List (intercalate,nub)
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
        newRules = terminalRules ++ doubleNonTerminalRules ++ fst cnfRulesTuple



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
    | otherwise = ((suffix,newNeterminal) : fst newNeterminalsTuple, newNeters ++ snd newNeterminalsTuple)
        where
            (x,[prefix,suffix]) = rl-- S -> ABC === S -> A<BC>
            leftSide = fst rl -- original left side
            rightSide = snd rl -- original right side
            suffixArrayRaw = [ head newNeterminal, [last (init suffix)] ] -- created 2 last neterminals for end of recursion
            newNeterminal = extractNewNeterminal suffix ters
            newRule = (suffix,newNeterminal)
            newNeters = [suffix]
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



-- -- S -> A<BCD> createCnfRules "S" "A<BCD>" ["A","B","C","D","S"] ["a"]
-- createCnfRules :: String -> String -> [Neterminals] -> [Terminals] -> [Rules]
-- createCnfRules left right neters ters
--     | length rightSufix == 4 = [(rightSufix,lastNeterminal)] -- end recursion when rigthsufix is <CD>
--     | otherwise = (rightSufix,newComplex) : createCnfRules rightSufix newComplex neters ters
--         where
--             rightPrefix = getPrefixOfRightSide right ters -- A
--             rightSufix = getSufixOfRightSide right ters -- <BCD>
--             firstRightSufix = getFirstRightSufix rightSufix ters -- B or b'
--             newNeterminal = '<' : tail (tail rightSufix) -- <CD>
--             newComplex = firstRightSufix ++ newNeterminal -- B<CD>
--             lastNeterminal
--                 | length rawNeterminal == 2 && terHead `elem` ters = terHead ++ "'" ++ terLast
--                 | length rawNeterminal == 2 && terLast `elem` ters = terHead ++ terLast ++ "'"
--                 | otherwise = rawNeterminal -- CD
--                     where
--                         rawNeterminal = init (tail rightSufix)
--                         terHead = [head rawNeterminal]
--                         terLast = [last rawNeterminal]

-- convertToCnf :: Gramatics -> Gramatics
-- convertToCnf bkg  = Gramatics neters ters startTer newRules
--     where
--         neters = nub (neterminals bkg ++ getNewComplexNeterminals newRules ++ getNewSimpleNeterminals newRules ters)
--         ters = terminals bkg
--         startTer = startingTerminal bkg
--         oldRules = rules bkg
--         newRules = getTerminalRules oldRules ters  --complexCnf ++ getDoubleNonTerminalRules oldRules neters
--         --newRules = getTerminalRules oldRules ters ++ getDoubleNonTerminalRules oldRules neters ++ complexCnf
--             -- where 
--             --     complexCnf = foldl f [] (getComplexRules oldRules)
--             --         where
--             --             f acc x =  (leftSideSimple,rigthSideComplex) : createCnfRules leftSideSimple rigthSideComplex neters ters ++ acc
--             --                 where
--             --                     rigthSideComplex = createComplexNeterminal (snd x) ters
--             --                     leftSideSimple = fst x




-- filterMixedRules :: [Rules] -> [Terminals] -> [Rules]
-- filterMixedRules rls ters = filter f rls
--     where f rule = length rightSide == 2 && ([head rightSide] `elem` ters || [last rightSide] `elem` ters)
--             where rightSide = snd rule


-- getNewComplexNeterminals :: [Rules] -> [Neterminals]
-- getNewComplexNeterminals rls = map f (filterNewCreatedComplexRules rls)
--     where f rule = fst rule 

-- filterNewCreatedComplexRules :: [Rules] -> [Rules]
-- filterNewCreatedComplexRules rls = filter f rls
--     where f rule = head leftSide == '<' && last leftSide == '>' 
--             where leftSide = fst rule



-- getNewSimpleNeterminals :: [Rules] -> [Terminals] -> [Neterminals]
-- getNewSimpleNeterminals rls ters = map f (filterNewCreatedSimpleRules rls)
--     where f rule = getPrefixOfRightSide rightSide ters
--             where rightSide = snd rule

-- filterNewCreatedSimpleRules :: [Rules] -> [Rules]
-- filterNewCreatedSimpleRules rls = filter f rls
--     where f rule = head (tail rightSide) == '\'' 
--             where rightSide = snd rule

-- -- getNeterminalsFromMixedRules :: [Rules] -> [Neterminals]
-- -- getNeterminalsFromMixedRules rls = 


-- -- A->A<BC> = A A->a'<BC> = a'
-- getPrefixOfRightSide :: String  -> [Terminals] -> String
-- getPrefixOfRightSide [] _ = error "Wrong input"
-- getPrefixOfRightSide [_] _ = error "Wrong input"
-- getPrefixOfRightSide [_,_] _ = error "Wrong input"
-- getPrefixOfRightSide (x:y:xs) terms
--     | [x] `elem` terms && y == '\'' = x : [y]
--     | otherwise = [x]

-- -- A->A<BC> = <BC> 
-- getSufixOfRightSide :: String -> [Terminals] -> String
-- getSufixOfRightSide [] _ = error "Wrong input"
-- getSufixOfRightSide [_] _ = error "Wrong input"
-- getSufixOfRightSide (x:xs) terms
--     | [x] `elem` terms = tail xs
--     | otherwise = xs


-- -- <BCD> = B pr <bCD> = b'
-- getFirstRightSufix :: String -> [Terminals] -> String
-- getFirstRightSufix [] _ = error "Wrong input"
-- getFirstRightSufix [_] _ = error "Wrong input"
-- getFirstRightSufix [_,_] _ = error "Wrong input"
-- getFirstRightSufix (_:y:xs) ters
--     | [y] `elem` ters = y : "'"
--     | otherwise = [y]

-- -- get only rules, which lenght is more than 2
-- getComplexRules :: [Rules] -> [Rules]
-- getComplexRules = filter (\x -> length (snd x) > 2)

-- -- ABC -> A<BC>
-- createComplexNeterminal :: String -> [Terminals] -> String
-- createComplexNeterminal [] _ = error "Empty list"
-- createComplexNeterminal [_] _ = error "Not enough symbols"
-- createComplexNeterminal (x:xs) terms
--     | [x] `elem` terms = [x] ++ "'<" ++ xs ++ ">"
--     | otherwise  = [x] ++ "<" ++ xs ++ ">"








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




