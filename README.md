# Haskell Advent of Code 2022

Advent of Code 2022 solutions in Haskell.

## Running

There are several modes availabe with help text - just use `stack run` to
see them.

To run all solutions you can run `stack run run all`.

## Benchmarks

To run all the benchmarks just run `stack bench`.
This will give the full criterion HTML output at `bench.html`.

To benchmark individual tests you can either use `stack bench` with criterion
options passed through as `--benchmark-options`, or to just see the
CLI output you can use `stack run bench [day] [part]`.

## Acknowledgement

Most of the framework code here was very heavily based on https://github.com/mstksg/advent-of-code-dev.

I started from a blank slate and re-implemented it (mostly in a pretty similar fashion) to help get my head around it.
I've ignored anything I didn't understand (for now), so this has less functionality, but enough for me to be happy using it this year (2022).

## To-do

- Add functionality to pass test-specific parameters to solution functions.
