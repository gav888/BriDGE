# Causal Discovery Analysis

Performs causal discovery to learn the causal structure from data using
various algorithms with prior knowledge integration.

## Usage

``` r
bridge_discover(
  data,
  treatment,
  mediators,
  outcome,
  method = "mmhc",
  discretize = TRUE,
  n_bins = 5
)
```

## Arguments

- data:

  A data frame containing the variables

- treatment:

  Character string specifying the treatment variable name

- mediators:

  Character vector specifying the mediator variable names

- outcome:

  Character string specifying the outcome variable name

- method:

  Character string specifying the discovery method ("mmhc", "hc", "pc")

- discretize:

  Logical indicating whether to discretize continuous variables

- n_bins:

  Integer specifying number of bins for discretization (default: 5)

## Value

A list containing discovered DAG and comparison with researcher's DAG
