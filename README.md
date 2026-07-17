# BriDGE: Behavioural research by integrating DAGs and GAMs in Experiments

<!-- badges: start -->
[![R-CMD-check](https://github.com/gav888/BriDGE/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/gav888/BriDGE/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

BriDGE is an R package for mechanistic causal analysis of randomized controlled
trial (RCT) data. It implements the BriDGE protocol described in the companion
paper:

> Veltri, G. A., & Banerjee, S. (in press). **BriDGE the gap – improving
> Behavioural research by integrating DAGs and GAMs in Experiments.**
> *Behavior Research Methods.*

While RCTs provide reliable evidence for intervention efficacy, they are seldom
designed to reveal the causal pathways that drive observed outcomes. BriDGE
bridges the gap from "what works" to "why and how it works" by combining
Directed Acyclic Graphs (DAGs), causal discovery algorithms, and Generalized
Additive Models (GAMs).

## Features

- **Causal Discovery**: Learn causal structures from data using structure
  learning algorithms (MMHC, HC, PC-stable via `bnlearn`), with experimental
  design knowledge enforced through whitelists (treatment → mediators) and
  blacklists (no edges into the randomized treatment)
- **Mediation Analysis**: Estimate direct and indirect effects through multiple
  mediators using GAMs (`mgcv`), capturing nonlinear relationships
- **Bootstrapping**: Uncertainty quantification through bootstrap resampling,
  with reproducible seeding
- **Sensitivity Analysis**: Test robustness of results to data perturbations
- **Visualization**: Plots for bootstrap distributions and effect sizes
- **Parallel Processing**: Efficient computation across cores

## Installation

```r
# Install development version from GitHub
# install.packages("devtools")
devtools::install_github("gav888/BriDGE")
```

## Quick Start

```r
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
summary(results)

# Create plots
plot(results)
```

## Main Functions

| Function | Purpose |
|---|---|
| `bridge_analyze()` | Complete pipeline: discovery → mediation → comparison → sensitivity → plots → summary |
| `bridge_discover()` | Constrained causal discovery (`method = "mmhc"`, `"hc"`, or `"pc"`) and comparison with the researcher's assumed DAG |
| `bridge_mediate()` | GAM-based mediation analysis with bootstrap confidence intervals |
| `bridge_sensitivity()` | Robustness check under Gaussian perturbation of mediators and outcome |
| `bridge_compare()` | Treatment vs control group means for mediators and outcome |
| `bridge_plot()` | Bootstrap-distribution and effect-size plots |
| `bridge_summary()` | Formatted text summary of all results |
| `bridge_generate_data()` | Synthetic RCT data with nonlinear mediation structure, for examples and testing |

## Working with Your Own Data

```r
your_data <- read.csv("your_rct_data.csv")

# Treatment must be a two-level variable; the FIRST factor level is taken as
# control and the SECOND as treated. Both of these work:
your_data$treatment <- factor(your_data$treatment)                          # 0/1
your_data$treatment <- factor(your_data$group,
                              levels = c("control", "treatment"))           # labels

results <- bridge_analyze(
  data = your_data,
  treatment = "treatment",
  mediators = c("mediator1", "mediator2", "mediator3"),
  outcome = "outcome",
  n_bootstraps = 500,
  seed = 42   # reproducible bootstrap
)
```

### Data requirements

- **Sample size**: at least 200 observations recommended (more for stable
  discovery and mediation of subtle effects)
- **Treatment**: binary factor (exactly two levels)
- **Mediators**: continuous or discrete
- **Outcome**: continuous
- **Missing data**: complete cases required — remove or impute first
  (`na.omit(data)`)

## Interpreting Results

- **DAG agreement**: overlap between the discovered structure and your assumed
  DAG; disagreements flag pathways worth re-examining rather than definitive
  causal claims
- **Direct effect**: treatment effect on the outcome not flowing through the
  specified mediators
- **Indirect effects (`nie_*`)**: treatment effect transmitted through each
  mediator
- **Total effect**: overall treatment effect with all mediators responding
- **Confidence intervals**: 95% bootstrap intervals
- **Sensitivity analysis**: stability of the estimates under small data
  perturbations

## Reproducing the Paper

The simulation scripts that generated the results in the companion paper
(synthetic scenarios with 2, 3, and 5 mediators, and the JOBS II semi-synthetic
benchmark) ship with the package:

```r
list.files(system.file("simulations", package = "BriDGE"))
```

See `inst/simulations/README.md` for details. The scripts are self-contained
and long-running with default settings.

## Performance Considerations

- Use `parallel = TRUE` (default) for faster computation
- Reduce `n_bootstraps` during exploration; increase (≥ 1000) for final results
- `handle_convergence = "simplify"` falls back to simpler models when GAM
  fitting struggles on small bootstrap samples

## Citation

```r
citation("BriDGE")
```

> Veltri, G. A., & Banerjee, S. (in press). BriDGE the gap – improving
> Behavioural research by integrating DAGs and GAMs in Experiments.
> *Behavior Research Methods.*

## Contributing

Contributions are welcome — please open an issue or submit a Pull Request at
<https://github.com/gav888/BriDGE>.

## License

MIT © Giuseppe A. Veltri. See `LICENSE.md` for details.

## Support

- Open an issue: <https://github.com/gav888/BriDGE/issues>
- Email: <gaveltri@nus.edu.sg>
