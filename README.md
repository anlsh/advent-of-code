Advent of Code 2020 in Common Lisp
==================================

Solutions read aoc inputs from the `inputs/`. The idea is that one can load the
system and then do `(advent:day1a)`, `(advent:day2a)`, etc. but unfortunately
you generally have to be `cd`'d into `src/`for things to work,

Also, at least up to day 12, running the programs with different inputs involves
changing the hardcoded paths and reloading the functions, though this is more of
a development annoyance than anything. I'll have to try and remedy this going
forward
