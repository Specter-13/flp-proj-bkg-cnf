# FLP project 1 - BKG-2-CNF
**Author:** Dávid Špavor (xspavo00)

This functional Haskell program converts context-free gramatics to Chomsky normal form.  

## Build and Run

Make sure you have ghci environment installed. Use `make` command to build project. File `flp21-fun` is created. 

You can run program with following arguments:

`/.flp21-fun mode [input-file]`

where `mode` can be:
- `-i` - print internal representation of gramatics after syntax check
- `-1` - print gramatics without simple rules.
- `-2` - print gramatics in Chomsky normal form.

If input file is omitted, program will read input from stdin.
## Implementation

Source files are located in `src` folder. Project is implemented in following 5 files:

- `Main.hs` - contains implementation of IO functions, parsing arguments and main function for running whole program. 
- `CustomDatatypes.hs` - contains implementation of custom datatypes, mainly datatype for internal representation of gramatics.

- `Parser.hs` - contains implementation of syntax parser for input gramatics. 
- `RemoverOfSimpleRules.hs` - contains implementation of first algorithm, which removes simple rules from input gramatics.  
- `ConverterToCnf` - contains implementation of second algorithm, which converts gramatics to Chomsky normal form (cnf).

### Basic description of implementation

 - **Internal representation of gramatics** - gramatics is stored in following data structure:

```
data Gramatics = Gramatics { 
      neterminals :: [Neterminals]  
    , terminals :: [Terminals]  
    , startingTerminal :: StartNeterminal 
    , rules :: [Rules]   
    } deriving (Show)   
``` 

where `Neterminals`, `Terminals` and `StartNeterminal` are type of `String` and `Rules` have format of tuple `(Neterminals, [String])`

- **Arguments** - Program supports only one arguments of type `mode` at the time. So no multiple arguments. 

- **Parsing** - No custom external library (like Parsec) was use to implement parser. Parser is implemented with basic fundamental functions, pattern matching etc. Parsing is working line by line. First and foremost correct syntax of neterminals and terminals (first and second line) is evaluated. After that is checked, whether start symbol  (third line) is present and is contained in list of neterminals. Last but not least correct syntax of rules is checked. Also is evaluated, whether are used only defined symbols. This is done by `parseGramatics` function.

- **Removing of simple rules** - This is implementation of algorithm for removing simple rules. It contains recusive function to create *Na sets* for each neterminal. Based on *Na sets* are obtained only complex rules of gramatics. After succesful remove of simple rules, new gramatics is loaded into internal representation ready to be printed out. This is done by `removeSimpleRules` function.

- **Converter to Cnf** - This is implementation of algorithm for converting to cnf. New internal representation of gramatics is done by `convertToCnf` function. There are mulitple 'help' functions, for symbols parsing from rules, created new sets of terminals and neterminals etc.

## Testing
Program was tested for wrong inputs consistently during implementation. Also is implemented `tester.sh` script located in `scripts` folder , which use *diff* utillity to compare output files with reference files. Script is really simple and easy to use. Just run `make testing` command. 