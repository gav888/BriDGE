# Complete Causal Analysis Pipeline

Main wrapper function that performs a complete causal analysis pipeline
including causal discovery, mediation analysis, sensitivity analysis,
and visualization.

## Usage

``` r
bridge_analyze(
  data,
  treatment,
  mediators,
  outcome,
  n_bootstraps = 500,
  discovery_method = "mmhc",
  nonlinear = TRUE,
  plot = TRUE,
  parallel = TRUE,
  sensitivity = TRUE,
  handle_convergence = "warn",
  gam_maxit = 200,
  gam_epsilon = 1e-07,
  seed = 123
)
```

## Arguments

- data:

  A data frame containing the variables for analysis

- treatment:

  Character string specifying the treatment variable name

- mediators:

  Character vector specifying the mediator variable names

- outcome:

  Character string specifying the outcome variable name

- n_bootstraps:

  Integer specifying the number of bootstrap iterations (default: 500)

- discovery_method:

  Character string specifying the causal discovery method ("mmhc", "hc",
  "pc")

- nonlinear:

  Logical indicating whether to use nonlinear GAM models (default: TRUE)

- plot:

  Logical indicating whether to generate plots (default: TRUE)

- parallel:

  Logical indicating whether to use parallel processing (default: TRUE)

- sensitivity:

  Logical indicating whether to perform sensitivity analysis (default:
  TRUE)

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

A list containing all analysis results and plots

## Examples

``` r
# Generate example data
data <- bridge_generate_data(n = 200) # Smaller n for faster example

# Perform complete analysis
# Reducing bootstraps and disabling parallel for faster example
results <- bridge_analyze(
  data = data,
  treatment = "treatment",
  mediators = c("mediator_1", "mediator_2"),
  outcome = "outcome",
  n_bootstraps = 10,
  parallel = FALSE
)
#> Starting BriDGE Causal Analysis Pipeline...
#> Step 1: Performing causal discovery...
#> Step 2: Performing mediation analysis...
#> Step 3: Performing comparative analysis...
#> Step 4: Performing sensitivity analysis...
#> Step 5: Generating visualizations...
#> Step 6: Generating summary...
#> BriDGE Analysis Complete!

# View summary
print(results$summary)
#> ==== BriDGE CAUSAL ANALYSIS SUMMARY ====
#> 
#> 1. CAUSAL DISCOVERY:
#>    Method: mmhc
#>    DAG Agreement: 60%
#> 
#> 2. MEDIATION ANALYSIS:
#>    direct_effect: 2.3065 (95% CI: 2.021 to 2.5031)
#>    total_effect: 5.3636 (95% CI: 4.9063 to 5.8059)
#>    nie_mediator_1: 1.7379 (95% CI: 1.412 to 2.2203)
#>    nie_mediator_2: 1.3192 (95% CI: 1.091 to 1.7404)
#> 
#> 3. COMPARATIVE ANALYSIS:
#>    mediator_1 - Difference: 0.9168
#>    mediator_2 - Difference: 0.926
#>    outcome - Difference: 5.4391
#> 
#> 4. SENSITIVITY ANALYSIS:
#>    Perturbation SD: 0.1
#>    Sensitivity analysis performed.
#> 
#> ==== END SUMMARY ====
```
