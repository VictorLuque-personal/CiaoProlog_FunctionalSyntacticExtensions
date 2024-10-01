## Translating Advent of Code 2022 done in OCaml into Ciao Prolog using functional syntax

### Context

The aim of this folder is to translate several solutions for the AoC 2023 done in OCaml into Ciao Prolog using functional syntax. Thus, you will find chosen examples that can be interesting to translate from the AoC 2023 written in OCaml. Every folder has the code of the original author written in OCaml and antoher file containing the code translated in Ciao Prolog. Moreover, there is the original statement for the specific problem of the AoC 2023 example translated.

The link to the AoC 2022:

[Advent Of Code 2022](https://adventofcode.com/2022)

The link to the author of the original examples that have been translated:

[DrearyLisper solutions for AoC 2022 in OCaml](https://github.com/DrearyLisper/aoc-2022/tree/master)

### Translation

Ciao Prolog is a multiparadigm language based on Prolog. We can write things in a functional-style manner with it. So, the aim of this folder is to show how something written in OCaml can be written in Ciao Prolog but there have been some decisions made to do so.

The translation aims to show how from a functional language like OCaml can be idiomatically translated into Ciao Prolog. However, there are many differences bettwen both languages and it makes no sense to translate everything exactly how it was written originally for OCaml.

You must take into account that things to run the code, the standard IO and general code that is not the algorithm to solve the problem of the statement, will have changes. Some of them, for readability, have been written in logic-programming like the IO and the main function to run the code. Others, have been slightly changed (although written in a functional-style programming) in order to make it more understandable and also because of the differences between both languages. For example, due to the separation between code for the IO from the code that is specifically written to solve the problem the input for the algorithm to solve the problem has been changed (e.g. previously processed to avoid doing it during the algorithm).