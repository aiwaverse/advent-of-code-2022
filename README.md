# Advent of Code 2022 Solutions

These are my solutions for the [Advent of Code 2022](https://adventofcode.com/2022) written in Haskell, not aiming for the most optimal solution, just exploring the language.

## Running solutions
This project only contains a library, each day is contained in a single file within the `src` folder. To run in the way I ran this, just run `stack repl` on the project folder, then use the command `:set args [file path for input]` with the input you want to use, each day has it's .txt file on the inputs folder. After that, just run the function `processInputDay[Day Number]`, so, for example, to run day 1 on my machine, already on the REPL:
```
:set args /home/phi/Documents/codes/haskell/advent-of-code/inputs/DayOne.txt
processInputDay1
```
The results are always in the same format, a pair of strings, where the first one is the result of the first part of the puzzle, and the second is the result of the second part.
