# AOCaml 2023

We'll see how far I get this year. These are my solutions to the [Advent of Code 2023](https://adventofcode.com/2023) in OCaml.

My goal this year is to try and maintain a good project structure with interface files and unit tests where applicable.

## Development

I run `dune runtest -w` while developing the library part of the problem to continually run my unit tests. And then when I'm ready to wire things up, I run `dune exec dayX -w`.

## Running code for a specific day

e.g. If you want to run the code for day 7, you can run `dune exec day07`.
