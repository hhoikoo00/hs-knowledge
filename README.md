# hs_knowledge

A library written in Haskell that determines whether a logical argument is valid through either Model Checking or The DPLL Algorithm.

## Explanation

`Formula.hs` contains type definitions for a propositional formula and some helper functions related to the formula.

`DPLL.hs` contains a library that implements the Davis–Putnam–Logemann–Loveland (DPLL) algorithm, which determines whether a formula is satisfiable, by converting a general formula into a Conjunctive Normal Form (CNF) and then repeatedly propagating singleton clauses.

`Logic.hs` contains two functions that determines whether `KB ⊨ α` holds: 
- `modelCheck` uses a brute force approach by generating all possible combinations of truth values for each variables and checking if KB holds then α also holds
- `checkWithUnSAT` uses the `DPLL` module by checking whether `KB & !α` is unsatisfiable i.e. there is no case where KB holds but α does not.

Files with an `ex_` prefix are example modules that each contains an example knowledge and query search with a complementary main function.

## Credit

The implementation of the DPLL algorithm is based on a [past Haskell exam paper](http://wp.doc.ic.ac.uk/ajf/sat/) written by Dr Tony Field and offered to the Computing students at Imperial College London. Although the implementation is entirely mine, the design is based on the skeleton provided by the exam.
