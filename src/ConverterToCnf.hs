module ConverterToCnf where 
import CustomDatatypes
import Data.List (nub)
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
        neters = neterminals bkg ++ nub newNeterminals
        ters = terminals bkg
        startTer = startingTerminal bkg
        oldRules = rules bkg
        terminalRules = getTerminalRules oldRules ters
        doubleNonTerminalRules = getDoubleNonTerminalRules oldRules neters
        cnfRulesTuple =  createCnfRules oldRules neters ters
        newRules = terminalRules ++ doubleNonTerminalRules ++ newComplexRules ++ newDoubleMixedRules ++ apostropheRules
        apostropheRules = createApostropheRules (filterApostropheRules neters ters) 
        newNeterminals = snd cnfRulesTuple ++ getNewNeterminalsDoubleMixedRules newDoubleMixedRules
        newComplexRules = fst cnfRulesTuple
        doubleMixedRules = getDoubleMixedRules oldRules ters
        newDoubleMixedRules = mapDoubleMixedRules doubleMixedRules ters




-- create cnfRules and return them with new created neterminals
createCnfRules :: [Rules]-> [Neterminals] -> [Terminals] -> ([Rules],[Neterminals])
createCnfRules rls neters ters = foldl f ([],[]) treblePlusRules
            where
                treblePlusRules = getTreblePlusRules rls
                f acc rl = (newFirstFormatRule: fst acc ++ newRules, snd acc ++ newNeterminals)
                    where
                        newFirstFormatRule =  createFirstNewFormat rl ters
                        newNeterminalsTuple = createNewNeterminals newFirstFormatRule neters ters
                        newNeterminals = snd newNeterminalsTuple ++ getNewNeterminalsDoubleMixedRules [newFirstFormatRule]
                        newRules = fst newNeterminalsTuple

-- S -> A<BC> ===  <BC> -> BC
createNewNeterminals :: Rules -> [Neterminals] -> [Terminals] -> ([Rules],[Neterminals])
createNewNeterminals rl _ ters
    | length suffix == 4 = (lastRule, lastNewNeters ++ newNeters) -- end recursion when rightsuffix is <CD>
    | otherwise = ((suffix,newNeterminal) : fst newNeterminalsTuple, newNeters ++ snd newNeterminalsTuple )
        where
            (_,[_,suffix]) = rl-- S -> ABC === S -> A<BC>
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
            lastRule = [(suffix,suffixArrayRaw)]
            lastNewNeters = getNewNeterminalsDoubleMixedRules [(suffix,suffixArrayRaw)]



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


getDoubleMixedRules :: [Rules] -> [Terminals] -> [Rules]
getDoubleMixedRules rls ters = filter isDoubleMixedRule rls
    where
        isDoubleMixedRule rl = length rightSide == 2 && (head rightSide `elem` ters || last rightSide `elem` ters)
            where rightSide = snd rl

mapDoubleMixedRules :: [Rules] -> [Terminals] -> [Rules]
mapDoubleMixedRules [] _ = []
mapDoubleMixedRules rls ters = map f rls
    where
        f rl
            | first `elem` ters && second `notElem` ters  = (leftSide, [newFirst, second])
            | second `elem` ters && first `notElem` ters  = (leftSide, [first, newSecond])
            | otherwise = (leftSide,[newFirst, newSecond])
                where
                    leftSide = fst rl
                    rightSide = snd rl
                    first = head rightSide
                    second = last rightSide
                    newFirst = first ++ "\'"
                    newSecond = second  ++ "\'"

getNewNeterminalsDoubleMixedRules :: [Rules] -> [String]
getNewNeterminalsDoubleMixedRules = foldl f []
    where f acc x 
            | '\'' `elem` first && '\'' `notElem` second = first : acc
            | '\'' `elem` second && '\'' `notElem` first = second : acc
            | '\'' `elem` first && '\'' `elem` second = first : second : acc
            | otherwise = acc
                where
                    rightSide = snd x
                    first = head rightSide
                    second = last rightSide