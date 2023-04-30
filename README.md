# compMPG

This is a compositional solver for MPGs.

## How to build
1 `cd Implementation`

2 `stack build`

## Inputs 
Every input of the solver should be in the folder `generalized_contexts`. 
The data of benchmarks in `data/`, and please move the input file to the folder `generalized_contexts` when you run this solver. For example, 

`mv data/benchmarkC-5-20/input1.txt generalized_contexts/`

## How to run 
`stack run solveBench filename`

For example, 

`stack run solveBench input1.txt`



## Requirement 
- stack https://docs.haskellstack.org/en/stable/


