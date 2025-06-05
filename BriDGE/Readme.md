---
editor_options: 
  markdown: 
    wrap: 72
---

# BriDGE: Bayesian Research in Directed Graph Estimation

<!-- badges: start -->

[![R-CMD-check](https://github.com/username/BriDGE/workflows/R-CMD-check/badge.svg)](https://github.com/username/BriDGE/actions)
[![CRAN
status](https://www.r-pkg.org/badges/version/BriDGE)](https://CRAN.R-project.org/package=BriDGE)

<!-- badges: end -->

BriDGE is a comprehensive R package for causal analysis of randomized
controlled trial (RCT) data. It provides tools for causal discovery,
mediation analysis with nonlinear relationships, bootstrapping,
sensitivity analysis, and visualization.

## Features

-   **Causal Discovery**: Learn causal structures from data using
    various algorithms (MMHC, HC, PC)
-   **Mediation Analysis**: Estimate direct and indirect effects through
    multiple mediators using GAMs
-   **Nonlinear Relationships**: Handle complex nonlinear relationships
    between variables
-   **Bootstrapping**: Robust uncertainty quantification through
    bootstrap resampling
-   **Sensitivity Analysis**: Test robustness of results to data
    perturbations
-   **Visualization**: Rich plotting capabilities for results
    interpretation
-   **Parallel Processing**: Efficient computation using parallel
    processing

## Installation

``` r
# Install from CRAN (when available)
install.packages("BriDGE")

# Install development version from GitHub
# install.packages("devtools")
devtools::install_github("username/BriDGE")
```

## Quick Start

``` r
library(BriDGE)

# Generate example data
data <- bridge_generate_data(n = 1000)

# Perform complete causal analysis
results <- bridge_analyze(
  data = data,
  treatment = "treatment",
  mediators = c("mediator_1", "mediator_2"),
  outcome = "outcome",
  n_bootstraps = 500
)

# View summary
print(results)

# Create plots
plot(results)
```

## Main Functions

### `bridge_analyze()`

The main wrapper function that performs a complete causal analysis
pipeline:

``` r
results <- bridge_analyze(
  data = your_data,
  treatment = "treatment_variable",
  mediators = c("mediator1", "mediator2"),
  outcome = "outcome_variable",
  n_bootstraps = 500,
  discovery_method = "mmhc",
  nonlinear = TRUE,
  parallel = TRUE,
  sensitivity = TRUE
)
```

### `bridge_discover()`

Performs causal discovery to learn causal structure:

``` r
discovery_results <- bridge_discover(
  data = your_data,
  treatment = "treatment",
  mediators = c("mediator1", "mediator2"),
  outcome = "outcome",
  method = "mmhc"
)
```

### `bridge_mediate()`

Performs causal mediation analysis:

``` r
mediation_results <- bridge_mediate(
  data = your_data,
  treatment = "treatment",
  mediators = c("mediator1", "mediator2"),
  outcome = "outcome",
  n_bootstraps = 500,
  nonlinear = TRUE
)
```

### `bridge_sensitivity()`

Performs sensitivity analysis:

``` r
sensitivity_results <- bridge_sensitivity(
  data = your_data,
  treatment = "treatment",
  mediators = c("mediator1", "mediator2"),
  outcome = "outcome",
  perturbation_sd = 0.1
)
```

## Example Analysis

Here's a complete example using the BriDGE package:

``` r
library(BriDGE)
library(ggplot2)

# Step 1: Generate or load your data
# For this example, we'll generate synthetic RCT data
set.seed(123)
data <- bridge_generate_data(
  n = 2000,
  nonlinear_strength = 0.5
)

# Step 2: Explore the data
head(data)
summary(data)

# Step 3: Perform complete causal analysis
results <- bridge_analyze(
  data = data,
  treatment = "treatment",
  mediators = c("mediator_1", "mediator_2"),
  outcome = "outcome",
  n_bootstraps = 1000,
  discovery_method = "mmhc",
  nonlinear = TRUE,
  parallel = TRUE,
  sensitivity = TRUE
)

# Step 4: View results
print(results)

# Step 5: Examine specific components
# Causal discovery results
results$discovery$comparison

# Mediation effects with confidence intervals
results$mediation$summaries

# Comparative analysis
results$comparison

# Step 6: Create visualizations
plot(results)

# Step 7: Access individual plots
if (!is.null(results$plots$bootstrap_distributions)) {
  print(results$plots$bootstrap_distributions)
}

if (!is.null(results$plots$effect_sizes)) {
  print(results$plots$effect_sizes)
}
```

## Working with Your Own Data

To use BriDGE with your own RCT data:

``` r
# Assume your data has the following structure:
# - treatment: binary factor (0/1 or "control"/"treatment")
# - mediator variables: continuous or discrete
# - outcome: continuous

# Load your data
your_data <- read.csv("your_rct_data.csv")

# Ensure treatment is a factor
your_data$treatment <- as.factor(your_data$treatment)

# Run analysis
results <- bridge_analyze(
  data = your_data,
  treatment = "treatment",
  mediators = c("mediator1", "mediator2", "mediator3"),
  outcome = "outcome",
  n_bootstraps = 500
)
```

## Advanced Usage

### Custom Causal Discovery

``` r
# Use different discovery methods
discovery_hc <- bridge_discover(
  data = data,
  treatment = "treatment",
  mediators = c("mediator_1", "mediator_2"),
  outcome = "outcome",
  method = "hc"
)

discovery_pc <- bridge_discover(
  data = data,
  treatment = "treatment",
  mediators = c("mediator_1", "mediator_2"),
  outcome = "outcome",
  method = "pc"
)
```

### Linear vs Nonlinear Models

``` r
# Compare linear and nonlinear approaches
linear_results <- bridge_mediate(
  data = data,
  treatment = "treatment",
  mediators = c("mediator_1", "mediator_2"),
  outcome = "outcome",
  nonlinear = FALSE
)

nonlinear_results <- bridge_mediate(
  data = data,
  treatment = "treatment",
  mediators = c("mediator_1", "mediator_2"),
  outcome = "outcome",
  nonlinear = TRUE
)
```

### Sensitivity Analysis with Different Perturbations

``` r
# Test sensitivity with different perturbation levels
sensitivity_low <- bridge_sensitivity(
  data = data,
  treatment = "treatment",
  mediators = c("mediator_1", "mediator_2"),
  outcome = "outcome",
  perturbation_sd = 0.05
)

sensitivity_high <- bridge_sensitivity(
  data = data,
  treatment = "treatment",
  mediators = c("mediator_1", "mediator_2"),
  outcome = "outcome",
  perturbation_sd = 0.2
)
```

## Interpreting Results

### Causal Discovery

-   **DAG Agreement**: Percentage of agreement between discovered and
    assumed causal structures
-   **Edge Comparison**: Detailed comparison of causal relationships

### Mediation Analysis

-   **Direct Effect**: Effect of treatment on outcome not mediated
    through specified mediators
-   **Indirect Effects**: Effects of treatment on outcome through each
    mediator
-   **Total Effect**: Sum of direct and all indirect effects
-   **Confidence Intervals**: 95% CIs from bootstrap resampling

### Sensitivity Analysis

-   Robustness of results to small data perturbations
-   Helps assess reliability of findings

## Data Requirements

BriDGE works best with: - **Sample Size**: At least 200 observations
(larger samples recommended) - **Treatment Variable**: Binary factor
variable - **Mediators**: Continuous or discrete variables -
**Outcome**: Continuous variable - **Missing Data**: Complete cases
(handle missing data before analysis)

## Performance Considerations

-   Use `parallel = TRUE` for faster computation
-   Reduce `n_bootstraps` for quicker results during exploration
-   For very large datasets, consider sampling for initial analysis

## Troubleshooting

### Common Issues

1.  **Factor Variables**: Ensure treatment is coded as a factor

``` r
data$treatment <- as.factor(data$treatment)
```

2.  **Missing Data**: Remove or impute missing values before analysis

``` r
data <- na.omit(data)
```

3.  **Memory Issues**: Reduce bootstrap iterations or use sampling

``` r
results <- bridge_analyze(data, ..., n_bootstraps = 100)
```

4.  **Convergence Issues**: Try different discovery methods or check
    data quality

## Citation

If you use BriDGE in your research, please cite:

```         
Your Name (2024). BriDGE: Bayesian Research in Directed Graph Estimation. 
R package version 1.0.0. https://github.com/username/BriDGE
```

## Contributing

Contributions are welcome! Please feel free to submit a Pull Request.

## License

This project is licensed under the MIT License - see the LICENSE file
for details.

## Support

For questions and support: - Open an issue on GitHub - Email:
[your.email\@domain.com](mailto:your.email@domain.com){.email}

------------------------------------------------------------------------

## Package Structure

```         
BriDGE/
├── DESCRIPTION
├── NAMESPACE
├── R/
│   ├── bridge_analyze.R
│   ├── bridge_discover.R
│   ├── bridge_mediate.R
│   ├── bridge_sensitivity.R
│   ├── bridge_compare.R
│   ├── bridge_plot.R
│   ├── bridge_summary.R
│   ├── bridge_generate_data.R
│   └── utils.R
├── man/
│   ├── bridge_analyze.Rd
│   ├── bridge_discover.Rd
│   └── ... (other .Rd files)
├── tests/
│   └── testthat/
│       ├── test-bridge_analyze.R
│       ├── test-bridge_discover.R
│       └── ... (other test files)
├── vignettes/
│   └── BriDGE_tutorial.Rmd
├── data/
│   └── example_rct_data.rda
├── README.md
└── LICENSE
```
