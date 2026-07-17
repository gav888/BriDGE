# BriDGE 1.1.0

## Documentation & infrastructure

- New "Getting started with BriDGE" vignette walking through the full
  workflow: data requirements, causal discovery and how to read the DAG
  comparison, GAM-based mediation and effect interpretation, sensitivity
  analysis, and practical guidance (`vignette("BriDGE")`).
- New package website built with pkgdown: <https://gav888.github.io/BriDGE/>.
- Added `CITATION.cff` so GitHub shows a "Cite this repository" button.
- Added `CONTRIBUTING.md` and issue templates (bug report, feature request).
- README now shows the researcher-vs-discovered DAG comparison figure.
- Added test-coverage and pkgdown CI workflows.

# BriDGE 1.0.0

First public release, accompanying the accepted paper:
Veltri, G. A., & Banerjee, S. (in press). *BriDGE the gap – improving
Behavioural research by integrating DAGs and GAMs in Experiments.*
Behavior Research Methods.

## Breaking changes

- The package title and description now use the paper's expansion of the
  acronym: **B**ehavioural **r**esearch by **i**ntegrating **D**AGs and
  **G**AMs in **E**xperiments.
- `bridge_mediate()` and `bridge_compare()` now require the treatment variable
  to have exactly two levels and raise an informative error otherwise.

## Bug fixes

- `bridge_mediate()` and `bridge_compare()` no longer hardcode the treatment
  levels `"0"`/`"1"`. Any two-level coding (e.g. `"control"`/`"treatment"`)
  now works; the first factor level is taken as control, the second as treated.

## New features

- New `seed` argument in `bridge_analyze()`, `bridge_mediate()`, and
  `bridge_sensitivity()` for reproducible bootstrap resampling and
  perturbation (previously a fixed internal seed was used).
- New `summary()` method for `bridge_analysis` objects.
- The companion simulation scripts that reproduce the paper's results are now
  shipped under `inst/simulations/` (see
  `system.file("simulations", package = "BriDGE")`).
- Added citation information (`citation("BriDGE")`).

## Packaging

- Package moved to the repository root, so
  `devtools::install_github("gav888/BriDGE")` works directly.
- Unit tests moved into `tests/testthat/` so they run with `R CMD check`;
  added regression tests for treatment coding, seeding, and the new
  `summary()` method.
- License formalized as MIT + file LICENSE; repository URL corrected.
- Added GitHub Actions R-CMD-check workflow.

# BriDGE 0.1.0

Initial development version.
