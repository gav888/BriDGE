# Companion simulation scripts

These scripts reproduce the results reported in the companion paper:

> Veltri, G. A., & Banerjee, S. (in press). *BriDGE the gap – improving
> Behavioural research by integrating DAGs and GAMs in Experiments.*
> Behavior Research Methods.

Both scripts are self-contained (they do not require the BriDGE package to be
installed) and write their outputs to `tables/`, `figures/`, and `dag_outputs/`
subdirectories of the working directory.

## `simulation_synthetic.R`

The fully synthetic simulation study (paper Sections 4–6 and Technical Annex).
Includes:

- Mediator scenarios: p = 2 (parallel), p = 3 (sequential chain), p = 5 (mixed
  true/null mediators)
- Constrained causal discovery (MMHC with treatment whitelists/blacklists) with
  bootstrap arc-strength stability
- Interventional mediation with GAMs and BCa bootstrap confidence intervals
  (B = 2000 default)
- Comparison with traditional mediation (Imai-style via the `mediation` package,
  and Sobel tests)
- Discretization, measurement-error, and unmeasured-confounding sensitivity
  analyses
- DAG contradiction diagnostics (multi-algorithm triangulation)
- Benchmarking grid (runtime by N, number of mediators, and bootstrap size) and
  power-analysis grid

Note: this is a long-running script (hours with default settings). A fast mode
for debugging is available via the controls near the top of the script.

## `simulation_jobs2_semisynthetic.R`

The semi-synthetic benchmark built on the JOBS II covariate scaffold
(`mediation::jobs`), with an interaction-enabled ground-truth data-generating
process and an additive-only estimation sensitivity check.
