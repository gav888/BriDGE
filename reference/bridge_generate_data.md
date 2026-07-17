# Generate Example Data for Testing

Generates synthetic RCT data with nonlinear relationships for testing
the package.

## Usage

``` r
bridge_generate_data(n = 1000, nonlinear_strength = 0.5, seed = 42)
```

## Arguments

- n:

  Integer specifying the number of observations

- nonlinear_strength:

  Numeric specifying the strength of nonlinear relationships

- seed:

  Integer for reproducibility

## Value

A data frame with treatment, mediators, and outcome variables
