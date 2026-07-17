# Causal Mediation Analysis

Performs causal mediation analysis using GAMs with bootstrapping to
estimate direct and indirect effects through multiple mediators.

## Usage

``` r
bridge_mediate(
  data,
  treatment,
  mediators,
  outcome,
  n_bootstraps = 500,
  nonlinear = TRUE,
  parallel = TRUE,
  k_basis = 10,
  handle_convergence = "warn",
  gam_maxit = 200,
  gam_epsilon = 1e-07,
  seed = 123
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

- n_bootstraps:

  Integer specifying the number of bootstrap iterations

- nonlinear:

  Logical indicating whether to use nonlinear GAM models

- parallel:

  Logical indicating whether to use parallel processing

- k_basis:

  Integer specifying the number of basis functions for GAM (default: 10)

- handle_convergence:

  Character string specifying how to handle convergence issues ("warn",
  "simplify", "error")

- gam_maxit:

  Integer specifying maximum iterations for GAM fitting (default: 200)

- gam_epsilon:

  Numeric specifying convergence tolerance for GAM fitting (default:
  1e-7)

- seed:

  Integer base seed for reproducible bootstrap resampling (default: 123)

## Value

A list containing mediation analysis results

## Details

The treatment variable must be a factor (or coercible to one) with
exactly two levels. The first factor level is treated as control and the
second as treated (e.g. `0`/`1` or `"control"`/`"treatment"`).
