# compMPG

This is a compositional solver for MPGs.

## Requirement 
- stack https://docs.haskellstack.org/en/stable/

## How to build
1 `stack build`

## Inputs 
Every input of the solver should be in the folder `generalized_contexts`. 
The data of benchmarks is in `data/`, and please move the input file to the folder `generalized_contexts` when you run this solver. For example, 

`cp data/benchmarkC-5-20/input1.txt generalized_contexts/`

## How to run 
`stack run solveBench filenumber`

For example, the following command solve the file `input1.txt`

`stack run solveBench 1`


