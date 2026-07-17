# BriDGE — Development Handoff

**Last updated:** 2026-07-17 (post-v1.0.0 release)

> **Status update (v1.0.0, 2026-07-17):** the public repo was brought up
> to release state. Done: package moved to repo root
> (`install_github("gav888/BriDGE")` now works); treatment-level
> hardcoding fixed (any two-level coding works, with validation +
> regression tests); `seed` argument added to
> `bridge_analyze/ bridge_mediate/bridge_sensitivity`;
> [`summary()`](https://rdrr.io/r/base/summary.html) method added;
> [`igraph::graph()`](https://r.igraph.org/reference/graph.html)
> deprecation fixed; DESCRIPTION retitled to the paper’s acronym,
> v1.0.0, `MIT + file LICENSE`, correct URL; README rewritten (no
> placeholders, paper citation, treatment-coding docs); tests moved to
> `tests/testthat/`; companion scripts shipped in `inst/simulations/`;
> `inst/CITATION` + `NEWS.md` added; GitHub Actions R-CMD-check workflow
> added. `R CMD check`: 0 errors / 0 warnings / 0 notes locally (R
> 4.6.1). Pushed to `gav888/BriDGE` main (merge commit `aa025b5`, tag +
> GitHub Release `v1.0.0`) after merging GitHub-web-UI commits (curated
> README with logo — kept its style, fixed its `yourusername`
> placeholders; legacy `r.yml` workflow removed in favor of
> `R-CMD-check.yaml`). HANDOFF.md and this file remain local-only
> (untracked) because they reference local machine paths. Sections below
> describe the pre-release state; still open: §3 paper-parity porting
> (BCa CIs, arc-strength, sensitivity grids as package functions),
> covariate support, real DAG plots, vignette, CRAN. **Repo:**
> `git@github.com:gav888/BriDGE.git` (single commit
> `0d61738 "Initial commit: BriDGE R package"` on `main`) **Companion
> paper:** *“BriDGE the gap – improving Behavioural research by
> integrating DAGs and GAMs in Experiments”*, accepted at Behavior
> Research Methods. Production sources:
> `/Users/giuseppe/Downloads/BriDGE_BRM_production/` (main manuscript,
> Technical Annex, figures, auto-generated tables).

------------------------------------------------------------------------

## 1. What BriDGE is

An R package implementing the BriDGE protocol for mechanistic analysis
of RCT data:

1.  **Causal discovery** on RCT data with experimental constraints —
    treatment→mediator edges whitelisted, edges into treatment
    blacklisted — via `bnlearn` (MMHC default; HC and PC-stable also
    supported). Continuous variables are discretized
    ([`Hmisc::cut2`](https://rdrr.io/pkg/Hmisc/man/cut2.html), default 5
    bins) for mutual-information tests.
2.  **Mediation analysis** with GAMs (`mgcv`) to capture nonlinear
    mediator–outcome relationships; estimates natural direct effect,
    per-mediator indirect effects, and total effect via counterfactual
    prediction, with bootstrap CIs.
3.  **Sensitivity analysis** by Gaussian perturbation of
    mediators/outcome, re-running mediation.
4.  **Comparison & visualization**: researcher-DAG vs discovered-DAG
    edge comparison, group-mean comparison, bootstrap-distribution and
    effect-size plots (ggplot2).

## 2. Repository layout

    BriDGE R package/            ← git root
    ├── HANDOFF.md               ← this file
    ├── Bridge icon.png          ← untracked
    └── BriDGE/                  ← the actual R package
        ├── DESCRIPTION          ← v0.1.0, MIT
        ├── NAMESPACE            ← roxygen-generated
        ├── R/BriDGE.R           ← ALL package code in one 974-line file
        ├── man/*.Rd             ← roxygen-generated docs (9 files)
        ├── bridge_tests.r       ← testthat tests, sitting at package root (NOT run by R CMD check)
        ├── bridge_examples.r    ← usage walkthrough script
        ├── Readme.md, LICENSE.md
        └── .RData, .Rhistory, .Rproj.user/   ← RStudio artifacts

**Exported functions** (all in `R/BriDGE.R`): `bridge_analyze` (pipeline
wrapper), `bridge_discover`, `bridge_mediate`, `bridge_sensitivity`,
`bridge_compare`, `bridge_plot`, `bridge_summary`,
`bridge_generate_data`; S3 methods `print`/`plot` for `bridge_analysis`,
`print` for `bridge_summary`. Internal helpers: `validate_data`,
`compare_dags`.

## 3. Companion research code (FOUND — not in this repo)

The paper’s simulations live in the research working directory:
`~/Library/CloudStorage/Dropbox-Personal/My Desk/Working directory/Research Projects/Behavioural Science/BPP mech R/`

Latest versions (both self-contained, do **not** call the BriDGE
package):

- **`Simulation revised6.R`** (~65 KB, Feb 2026) — the fully synthetic
  study that produced the paper’s tables/figures. Implements everything
  the paper promises: p ∈ {2, 3 chain, 5 mixed} mediator scenarios, BCa
  bootstrap CIs (B = 2000 default), bootstrap arc-strength stability,
  benchmarking grid (N × p × B), power-analysis grid, discretization /
  measurement-error / unmeasured-confounding sensitivity, DAG
  contradiction diagnostics (multi-algorithm triangulation), and
  traditional mediation comparison (Imai + Sobel via `mediation`). Its
  outputs (`tables/`, `figures/`, `dag_outputs/`) are what was copied
  into the BRM production folder. Earlier iterations
  (`Simulation revised.R`–`revised5.R`, `Simulation 1 Easy/Complex.R`,
  `BPP mech R*.R`) chart the history; treat revised6 as canonical.
- **`BriDGE_JOBS2_SemiSynthetic_v4.R`** (~40 KB, Feb 2026) — the JOBS II
  semi-synthetic benchmark (scaffold from
  [`mediation::jobs`](https://rdrr.io/pkg/mediation/man/jobs.html),
  interaction-enabled ground truth, additive-only sensitivity check).
  Outputs under `bridge_semisynth_jobs2_outputs/`.
- The folder also holds the latest manuscript sources
  (`Bridge_revised5.tex`, `Annex.tex`, `Response_to_Reviewers.tex`) that
  fed the production package.

**Important divergence:** the revised6 DGP deliberately includes an
M1→M2 edge (coeff 0.4, α = 0.8, n = 2000, researcher DAG assumes
parallel mediators) to create an informative theory–data discrepancy,
and its whitelist also forces treatment→outcome while the blacklist
forbids outcome→mediator edges. The package’s
[`bridge_generate_data()`](https://gav888.github.io/BriDGE/reference/bridge_generate_data.md)
and
[`bridge_discover()`](https://gav888.github.io/BriDGE/reference/bridge_discover.md)
predate all of this. The package should be brought up to parity with
revised6, not the other way round.

### Paper claims vs package reality

| Paper claims | Package reality |
|----|----|
| BCa bootstrap CIs as default, B ≥ 1000 recommended | Percentile CIs only; `boot` package is imported but **never used**; default B = 500 |
| Bootstrap arc-strength (stability) summaries for discovered DAG; low-stability edges flagged as “ambiguous hypotheses” | Not implemented ([`bnlearn::boot.strength`](https://rdrr.io/pkg/bnlearn/man/arc.strength.html) would do it) |
| Discovery sensitivity grid over bins g ∈ {3,4,5,6} and α ∈ {0.01,0.05,0.10}, reporting SHD and directed-edge overlap | Not implemented; only single-run discovery with fixed `n_bins` |
| Extended scenarios: p ∈ {3,5} mediators, sequential-chain and mixed (true+null) structures | [`bridge_generate_data()`](https://gav888.github.io/BriDGE/reference/bridge_generate_data.md) only generates the fixed 2-mediator scenario |
| Mediator measurement-error stress test; mediator–outcome confounding sensitivity | [`bridge_sensitivity()`](https://gav888.github.io/BriDGE/reference/bridge_sensitivity.md) only does simple Gaussian perturbation |
| Benchmarking templates (runtime by N, p, bootstrap size); power-planning heatmaps | Not in package (outputs exist as paper tables/figures) |
| Package name given as **`bridgeR`** in the implementation note | Package is named **`BriDGE`** — reconcile before readers look for it |

All of the left-hand column exists in `Simulation revised6.R` /
`BriDGE_JOBS2_SemiSynthetic_v4.R` as standalone script code — the work
is porting it into package functions, not reinventing it.

## 4. Known code issues in `R/BriDGE.R`

- **Hardcoded treatment levels `"0"`/`"1"`** in `bridge_mediate`
  (counterfactual prediction, `R/BriDGE.R:474-519`) and `bridge_compare`
  (`:674-675`). The README claims `"control"/"treatment"` coding works —
  it silently doesn’t (predictions/subsets would be empty or wrong).
  Either generalize to `levels(treatment)[1:2]` or validate and error
  clearly.
- **Deterministic bootstrap seeds**: `set.seed(123 + i)` per iteration
  (`:539`, `:549`) means every user gets identical bootstrap draws and
  there’s no user-facing seed argument. Prefer
  [`parallel::clusterSetRNGStream`](https://rdrr.io/r/parallel/RngStream.html)
  / a `seed` parameter.
- **`handle_convergence = "warn"` refits the identical model** inside
  the warning handler (`:399-406`, `:450-456`) — doubles cost and still
  swallows the warning message. Should re-signal the warning and reuse
  the fit.
- **GAM for mediator models is pointless as specified**:
  `gam(mediator ~ treatment)` with a factor treatment contains no smooth
  term (`:378-385`), so it’s an lm at GAM cost. Either document why or
  use lm; smooths only matter once baseline covariates are supported.
- **No covariate support anywhere** — mediation formulas are built only
  from treatment + mediators. The paper’s JOBS II semi-synthetic
  benchmark uses a covariate scaffold, so this is needed for real use.
- **`print.bridge_analysis` tells users to call `summary(object)`**
  (`:942`) but no `summary.bridge_analysis` method exists.
- **DAG plotting is a text placeholder** (`bridge_plot`, `:730`) telling
  users to plot manually.
- **[`utils::combn`](https://rdrr.io/r/utils/combn.html),
  `graphics::hist/par`,
  [`igraph::plot.igraph`](https://r.igraph.org/reference/plot.igraph.html)
  imported but unused** (leftovers); same for
  [`boot::boot`](https://rdrr.io/pkg/boot/man/boot.html).
- Naming/branding inconsistency: DESCRIPTION title says “**Bayesian**
  Research in Directed Graph Estimation” (nothing in the package is
  Bayesian); the paper and the source-file header use “**B**ehavioural
  **r**esearch by **i**ntegrating **D**AGs and **G**AMs in
  **E**xperiments”. Pick one (the paper’s) everywhere.

## 5. Packaging / CRAN-readiness gaps

- **Tests**: move `bridge_tests.r` to `tests/testthat/` with a
  `tests/testthat.R` runner; currently R CMD check runs zero tests. Add
  tests for the treatment-coding bug above once fixed.
- **README placeholders**: badges point to `username/BriDGE`; install
  instructions say `devtools::install_github("username/BriDGE")`;
  citation says “Your Name (2024) … version 1.0.0”; support email is
  `your.email@domain.com`. DESCRIPTION URL says `gaveltri/BriDGE` but
  the actual remote is `gav888/BriDGE`. README also documents a
  multi-file `R/` layout, `tests/`, `vignettes/`, `data/` that don’t
  exist.
- **License**: DESCRIPTION `License: MIT` is not CRAN-valid — needs
  `MIT + file LICENSE` plus the two-line `LICENSE` file (a `LICENSE.md`
  exists but that alone fails check).
- **No vignette** despite `VignetteBuilder: knitr` in DESCRIPTION.
- **Repo hygiene**: `.RData`, `.Rhistory`, `.Rproj.user/` sit inside the
  package dir but are correctly untracked (verified via `git ls-files`);
  add `.Rbuildignore` entries for `bridge_tests.r`/`bridge_examples.r`
  if they stay at root; untracked `.DS_Store` files should be
  gitignored.
- **No CI**: README badge references a GitHub Actions R-CMD-check
  workflow that doesn’t exist; add
  `usethis::use_github_action("check-standard")`.
- File-size note: `R/BriDGE.R` at ~1000 lines should be split into
  per-function files (`bridge_discover.R`, `bridge_mediate.R`, …) as the
  README already advertises.

## 6. Suggested roadmap

1.  **v0.1.1 — correctness & hygiene**: fix treatment-level hardcoding;
    add `seed` argument and proper parallel RNG; add
    `summary.bridge_analysis`; move tests into `tests/testthat/`; fix
    README/DESCRIPTION placeholders, license, and repo URL; split the
    monolith; set up CI.
2.  **v0.2.0 — paper parity**: port from `Simulation revised6.R` — BCa
    CIs via `boot`, arc-strength stability with an “ambiguous edge”
    report, discovery sensitivity grid (bins × α, SHD + edge overlap),
    generalized data generation (p mediators, chain/mixed, M1→M2 edge),
    measurement-error and confounding sensitivity modes,
    traditional-mediation comparison; copy revised6 + JOBS2 v4 into the
    repo under `inst/simulations/` so the paper’s results are
    reproducible from the public repo.
3.  **v0.3.0 — usability**: baseline-covariate support in discovery and
    mediation; real DAG comparison plots (ggraph or igraph
    side-by-side); vignette reproducing the paper’s p=2 example;
    power/runtime planning helpers (the paper’s power heatmap and
    benchmark templates).
4.  **CRAN submission** after R CMD check –as-cran is clean and the
    paper’s “anonymised link for review” is replaced with the public
    repo.

## 7. Useful references

- Paper build: `latexmk -pdf Bridge_main.tex` from the production
  folder; tables in `tables/*.tex` are auto-generated by
  `Simulation revised6.R` (synthetic study) and
  `BriDGE_JOBS2_SemiSynthetic_v4.R` (JOBS II benchmark) in the
  `BPP mech R` folder listed in §3.
- Key packages: `bnlearn` (discovery), `mgcv` (GAMs),
  [`Hmisc::cut2`](https://rdrr.io/pkg/Hmisc/man/cut2.html)
  (discretization), `boot` (to be actually used for BCa).
- Ground-truth simulation DGP (paper §4.1 = `bridge_generate_data`): T ~
  Bern(0.5); M1 = T + ε + α·sin(ν); M2 = 0.8T + ε + α·exp(ν/2); Y =
  1.5T + 1.2M1 + 0.9M2 + 0.5M1² + 0.3M1M2 + 0.5T·M1 + 0.7T·M2 + ε, with
  α = 0.5, N = 1000, seed 42.
- ⚠️ Check consistency: `Simulation revised6.R` defaults differ from
  paper §4.1 text — `generate_data_p2(n = 2000, alpha = 0.8)` and a
  deliberate M1→M2 edge (0.4). Verify which parameterization produced
  the published tables before porting, and reconcile paper text /
  package generator / script if needed.
