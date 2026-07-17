# Sensitivity Analysis

Performs sensitivity analysis by adding small perturbations to the data
and re-running the mediation analysis.

## Usage

``` r
bridge_sensitivity(
  data,
  treatment,
  mediators,
  outcome,
  n_bootstraps = 100,
  nonlinear = TRUE,
  parallel = TRUE,
  perturbation_sd = 0.1,
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

- perturbation_sd:

  Numeric specifying the standard deviation of perturbations

- handle_convergence:

  Character string specifying how to handle convergence issues

- gam_maxit:

  Integer specifying maximum iterations for GAM fitting

- gam_epsilon:

  Numeric specifying convergence tolerance for GAM fitting

- seed:

  Integer base seed for reproducible perturbation and bootstrap
  resampling (default: 123)

## Value

A list containing sensitivity analysis results
