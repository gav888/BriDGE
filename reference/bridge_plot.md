# Generate Visualization Plots

Creates various plots for the causal analysis results.

## Usage

``` r
bridge_plot(
  discovery_results = NULL,
  mediation_results = NULL,
  data = NULL,
  treatment = NULL,
  mediators = NULL,
  outcome = NULL
)
```

## Arguments

- discovery_results:

  Results from bridge_discover()

- mediation_results:

  Results from bridge_mediate()

- data:

  Original data frame

- treatment:

  Character string specifying the treatment variable name

- mediators:

  Character vector specifying the mediator variable names

- outcome:

  Character string specifying the outcome variable name

## Value

A list containing various ggplot objects
