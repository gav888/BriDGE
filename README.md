# BriDGE: Behavioural Research by Integrating DAGs and GAMs in Experiments

<!-- badges: start -->
[![R-CMD-check](https://github.com/gav888/BriDGE/workflows/R-CMD-check/badge.svg)](https://github.com/gav888/BriDGE/actions)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![CRAN status](https://www.r-pkg.org/badges/version/BriDGE)](https://CRAN.R-project.org/package=BriDGE)
<!-- badges: end -->

## Overview

BriDGE is a comprehensive R package for causal analysis of randomized controlled trial (RCT) data. It seamlessly integrates causal discovery through Directed Acyclic Graphs (DAGs) with flexible mediation analysis using Generalized Additive Models (GAMs), providing researchers with powerful tools to understand complex causal relationships in behavioral experiments.

### Key Features

- 🔍 **Causal Discovery**: Multiple algorithms (MMHC, HC, PC-stable) to learn causal structures from data
- 📊 **Flexible Mediation Analysis**: Handles nonlinear relationships using GAMs with automatic smoothing
- 🔄 **Bootstrap Inference**: Robust uncertainty quantification through parallel bootstrapping
- 🎯 **Sensitivity Analysis**: Assess robustness of findings to data perturbations
- 📈 **Comprehensive Visualization**: Publication-ready plots for all analysis components
- ⚡ **Parallel Processing**: Efficient computation for large-scale analyses
- 🛡️ **Robust Error Handling**: Graceful handling of convergence issues and edge cases

## Installation

### Development Version from GitHub

```r
# install.packages("devtools")
devtools::install_github("yourusername/BriDGE")
```

### CRAN Version (when available)

```r
install.packages("BriDGE")
```

## Quick Start

```r
library(BriDGE)

# Generate example RCT data with nonlinear relationships
data <- bridge_generate_data(n = 1000, nonlinear_strength = 0.5)

# Run complete causal analysis pipeline
results <- bridge_analyze(
  data = data,
  treatment = "treatment",
  mediators = c("mediator_1", "mediator_2"),
  outcome = "outcome",
  n_bootstraps = 500,
  parallel = TRUE
)

# View results
print(results$summary)
plot(results)
```

## Detailed Usage

### 1. Causal Discovery

Discover causal relationships from your data using various algorithms:

```r
discovery_results <- bridge_discover(
  data = data,
  treatment = "treatment",
  mediators = c("mediator_1", "mediator_2"),
  outcome = "outcome",
  method = "mmhc"  # Options: "mmhc", "hc", "pc"
)

# Compare discovered DAG with theoretical expectations
plot(discovery_results$researcher_dag)
plot(discovery_results$discovered_igraph)
```

### 2. Mediation Analysis

Perform mediation analysis with support for nonlinear relationships:

```r
mediation_results <- bridge_mediate(
  data = data,
  treatment = "treatment",
  mediators = c("mediator_1", "mediator_2"),
  outcome = "outcome",
  n_bootstraps = 1000,
  nonlinear = TRUE,  # Use GAMs for flexible modeling
  handle_convergence = "simplify"  # Options: "warn", "simplify", "error"
)

# Extract specific effects
direct_effect <- mediation_results$summaries$direct_effect
indirect_effect_m1 <- mediation_results$summaries$nie_mediator_1
```

### 3. Sensitivity Analysis

Assess the robustness of your findings:

```r
sensitivity_results <- bridge_sensitivity(
  data = data,
  treatment = "treatment",
  mediators = c("mediator_1", "mediator_2"),
  outcome = "outcome",
  perturbation_sd = 0.1
)
```

### 4. Visualization

Create publication-ready visualizations:

```r
# Generate all plots
plots <- bridge_plot(
  discovery_results = discovery_results,
  mediation_results = mediation_results,
  data = data,
  treatment = "treatment",
  mediators = c("mediator_1", "mediator_2"),
  outcome = "outcome"
)

# Access specific plots
plots$effect_sizes
plots$bootstrap_distributions
```

## Main Functions

| Function | Description |
|----------|-------------|
| `bridge_analyze()` | Complete analysis pipeline wrapper |
| `bridge_discover()` | Causal discovery from data |
| `bridge_mediate()` | Mediation analysis with bootstrapping |
| `bridge_sensitivity()` | Sensitivity analysis |
| `bridge_compare()` | Group comparison statistics |
| `bridge_plot()` | Generate visualizations |
| `bridge_summary()` | Create analysis summary |
| `bridge_generate_data()` | Generate synthetic RCT data |

## Advanced Options

### Handling Convergence Issues

```r
# Strict convergence requirements
results <- bridge_analyze(
  data = data,
  treatment = "treatment",
  mediators = mediators,
  outcome = "outcome",
  handle_convergence = "error",
  gam_maxit = 500,
  gam_epsilon = 1e-8
)

# Automatic model simplification
results <- bridge_analyze(
  data = data,
  treatment = "treatment",
  mediators = mediators,
  outcome = "outcome",
  handle_convergence = "simplify"
)
```

### Parallel Processing

```r
# Utilize multiple cores for faster computation
results <- bridge_analyze(
  data = data,
  treatment = "treatment",
  mediators = mediators,
  outcome = "outcome",
  parallel = TRUE,  # Auto-detects available cores
  n_bootstraps = 2000
)
```

## Citation

If you use BriDGE in your research, please cite:

```bibtex
@software{veltri2025bridge,
  author = {Veltri, Giuseppe A.},
  title = {BriDGE: Behavioural Research by Integrating DAGs and GAMs in Experiments},
  year = {2025},
  url = {https://github.com/yourusername/BriDGE},
  version = {1.0.0}
}
```

## Contributing

We welcome contributions! Please see our [Contributing Guidelines](CONTRIBUTING.md) for details.

### Development

```r
# Install development dependencies
devtools::install_deps(dependencies = TRUE)

# Run tests
devtools::test()

# Check package
devtools::check()
```

## Requirements

- R (>= 4.0.0)
- Dependencies: `ggplot2`, `dplyr`, `bnlearn`, `mgcv`, `igraph`, `boot`, `parallel`, `Hmisc`

## License

This project is licensed under the MIT License - see the [LICENSE](LICENSE.md) file for details.

Copyright (c) 2025 Giuseppe A. Veltri

## Acknowledgments

- Built on top of excellent R packages including `bnlearn` for causal discovery and `mgcv` for GAM fitting
- Inspired by modern causal inference methodologies in behavioral research

## Support

- 📧 Email: [ga.veltri@gmail.com](mailto:ga.veltri@gmail.com)
- 🐛 Issues: [GitHub Issues](https://github.com/yourusername/BriDGE/issues)
- 📖 Documentation: [Package Website](https://yourusername.github.io/BriDGE/) (Under development)

## News

### Version 1.0.0 (2025-10-06)

- Initial CRAN release
- Core functionality for causal discovery and mediation analysis
- Comprehensive bootstrapping and sensitivity analysis
- Full visualization suite
- Robust error handling and convergence management
