#!/usr/bin/env Rscript
# ============================================================
# BriDGE — Semi-synthetic demonstration using JOBS II scaffold
#          (mediation::jobs) with interaction-enabled ground truth
#          + additive-only estimation sensitivity check
# ============================================================
# Key design choice (for reviewer-facing semi-synthetic benchmark)
#   * Ground truth: interaction-enabled DGP (mirrors manuscript Eq.(4)-style
#     non-additivity via quadratic + mediator–mediator interaction + T×M terms).
#   * Estimation:
#       - Primary: interaction-capable GAM
#       - Sensitivity: additive-only GAM (no T×M or M1×M2 surface)
#
# Outputs are written under OUT_ROOT (see Section 1):
#   tables/
#     dag_comparison_jobs2_focus.tex
#     truth_vs_estimates_jobs2.tex
#     mediation_effects_jobs2_primary_interaction.tex
#     mediation_effects_jobs2_sensitivity_additive.tex
#     mediation_effects_jobs2_models_with_ci.tex
#   figures/
#     fig_dag_true_vs_discovered_jobs2.png
#     fig_gam_partial_effects_jobs2_interaction.png
#     fig_gam_partial_effects_jobs2_additive.png
#     fig_bootstrap_hist_jobs2_interaction.png
#     fig_bootstrap_hist_jobs2_additive.png
#   dag_outputs/
#     true_dag_jobs2.png
#     discovered_dag_jobs2.png
#   jobs2_semisynthetic_data.csv
#   sessionInfo_jobs2_semisynth.txt
#
# Notes
#   * This script is self-contained (does not require bridgeR).
#   * For a revision package, you can ship this as an additional script/vignette,
#     while keeping the original fully synthetic simulation script unchanged.
# ============================================================

# ------------------------------------------------------------
# 0) Packages
# ------------------------------------------------------------
pkgs <- c("mediation", "bnlearn", "mgcv", "boot", "igraph", "png")
for (p in pkgs) {
  if (!requireNamespace(p, quietly = TRUE)) {
    install.packages(p, repos = "https://cloud.r-project.org")
  }
}
suppressPackageStartupMessages({
  library(mediation)
  library(bnlearn)
  library(mgcv)
  library(boot)
  library(igraph)
  library(png)
})

USE_RGRAPHVIZ <- requireNamespace("Rgraphviz", quietly = TRUE)

# ------------------------------------------------------------
# 1) Robust output root (OUT_ROOT) and folders
# ------------------------------------------------------------
get_script_dir <- function() {
  # 1) Rscript --file=...
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", args, value = TRUE)
  if (length(file_arg) == 1L) {
    return(dirname(normalizePath(sub("^--file=", "", file_arg))))
  }
  # 2) source() with a known file: best-effort via sys.frames
  of <- try(sys.frames()[[1]]$ofile, silent = TRUE)
  if (!inherits(of, "try-error") && !is.null(of) && nzchar(of)) {
    return(dirname(normalizePath(of)))
  }
  # 3) RStudio active document (optional)
  if (requireNamespace("rstudioapi", quietly = TRUE)) {
    ctx <- try(rstudioapi::getActiveDocumentContext(), silent = TRUE)
    if (!inherits(ctx, "try-error") && nzchar(ctx$path)) {
      return(dirname(normalizePath(ctx$path)))
    }
  }
  # 4) fallback
  normalizePath(getwd())
}

OUT_ROOT <- Sys.getenv("BRIDGE_OUTDIR", unset = "")
if (!nzchar(OUT_ROOT)) {
  OUT_ROOT <- file.path(get_script_dir(), "bridge_semisynth_jobs2_outputs")
}
OUT_ROOT <- normalizePath(OUT_ROOT, winslash = "/", mustWork = FALSE)

DIR_TABLES  <- file.path(OUT_ROOT, "tables")
DIR_FIGURES <- file.path(OUT_ROOT, "figures")
DIR_DAGS    <- file.path(OUT_ROOT, "dag_outputs")

for (d in c(DIR_TABLES, DIR_FIGURES, DIR_DAGS)) {
  if (!dir.exists(d)) dir.create(d, recursive = TRUE, showWarnings = FALSE)
  if (!dir.exists(d)) stop("Cannot create output directory: ", d)
}

message("BriDGE semi-synthetic outputs will be written under:\n  ", OUT_ROOT,
        "\n  Tables:   ", DIR_TABLES,
        "\n  Figures:  ", DIR_FIGURES,
        "\n  DAG PNGs: ", DIR_DAGS)

# ------------------------------------------------------------
# 2) Configuration
# ------------------------------------------------------------
SEED_MAIN    <- 42
ALPHA_NONLIN <- 0.5

# Ground-truth DGP type
#   - "interaction": includes M1^2, M1*M2, T*M1, T*M2 in outcome equation
#   - "additive": drops the non-additive terms (M1*M2 and T×M terms); keeps M1^2
#                by default to retain nonlinearity but remain additive
GROUND_TRUTH <- Sys.getenv("BRIDGE_GROUND_TRUTH", unset = "interaction")

# Estimation models to fit (always both by default)
EST_MODELS <- c("interaction", "additive")

# Monte Carlo integration sizes
MC_DRAWS_POINT <- 200    # point-estimate g-computation draws
MC_DRAWS_TRUTH <- 2000   # "true" effects under known DGP (semi-synthetic benchmark)
MC_DRAWS_BOOT  <- 100    # inner MC draws within each bootstrap replicate

# Bootstrap
RUN_BOOTSTRAP <- TRUE
BOOT_R        <- 1000    # BCa intervals typically need >= 1000

# Discovery
RUN_DISCOVERY      <- TRUE
DISC_BINS          <- 5
DISC_ALPHA         <- 0.05
DISC_SCORE         <- "bde"   # "bde" or "bic"
DISC_ISS           <- 10
DISC_STRENGTH_R    <- 300
DISC_AVG_THR       <- 0.85

# Quick test mode (optional)
FAST_MODE <- Sys.getenv("BRIDGE_FAST", unset = "0") == "1"
if (FAST_MODE) {
  message("FAST_MODE enabled (BRIDGE_FAST=1): using reduced settings.")
  MC_DRAWS_POINT <- 80
  MC_DRAWS_TRUTH <- 400
  MC_DRAWS_BOOT  <- 40
  BOOT_R         <- 200
  DISC_STRENGTH_R <- 80
}


# ------------------------------------------------------------
# 2.1) Runtime overrides (no script edits required)
# ------------------------------------------------------------
# These options are useful for interactive testing versus full production runs.
# Example (fast sanity check):
#   Sys.setenv(BRIDGE_FAST="1")            # reduces BOOT_R and Monte-Carlo sizes
# Example (custom bootstrap settings):
#   Sys.setenv(BRIDGE_BOOT_R="300")
#   Sys.setenv(BRIDGE_MC_BOOT="40")
# Example (skip bootstrap temporarily):
#   Sys.setenv(BRIDGE_RUN_BOOTSTRAP="0")

BOOT_R        <- suppressWarnings(as.integer(Sys.getenv("BRIDGE_BOOT_R", unset = as.character(BOOT_R))))
MC_DRAWS_BOOT <- suppressWarnings(as.integer(Sys.getenv("BRIDGE_MC_BOOT", unset = as.character(MC_DRAWS_BOOT))))
MC_DRAWS_POINT <- suppressWarnings(as.integer(Sys.getenv("BRIDGE_MC_POINT", unset = as.character(MC_DRAWS_POINT))))
MC_DRAWS_TRUTH <- suppressWarnings(as.integer(Sys.getenv("BRIDGE_MC_TRUTH", unset = as.character(MC_DRAWS_TRUTH))))

if (is.na(BOOT_R) || BOOT_R < 1L) BOOT_R <- 200
if (is.na(MC_DRAWS_BOOT) || MC_DRAWS_BOOT < 1L) MC_DRAWS_BOOT <- 40
if (is.na(MC_DRAWS_POINT) || MC_DRAWS_POINT < 1L) MC_DRAWS_POINT <- 80
if (is.na(MC_DRAWS_TRUTH) || MC_DRAWS_TRUTH < 1L) MC_DRAWS_TRUTH <- 400

RUN_BOOTSTRAP <- Sys.getenv("BRIDGE_RUN_BOOTSTRAP", unset = if (RUN_BOOTSTRAP) "1" else "0") == "1"
RUN_DISCOVERY <- Sys.getenv("BRIDGE_RUN_DISCOVERY", unset = if (RUN_DISCOVERY) "1" else "0") == "1"

BOOT_PROGRESS_EVERY <- suppressWarnings(as.integer(Sys.getenv("BRIDGE_BOOT_PROGRESS_EVERY", unset = "25")))
if (is.na(BOOT_PROGRESS_EVERY) || BOOT_PROGRESS_EVERY < 1L) BOOT_PROGRESS_EVERY <- 25

BOOT_SAVE_RDS  <- Sys.getenv("BRIDGE_BOOT_SAVE_RDS", unset = "1") == "1"
BOOT_PARALLEL  <- Sys.getenv("BRIDGE_BOOT_PARALLEL", unset = "0") == "1"
BOOT_NCORES    <- suppressWarnings(as.integer(Sys.getenv("BRIDGE_NCORES", unset = "0")))
if (is.na(BOOT_NCORES) || BOOT_NCORES <= 0L) {
  BOOT_NCORES <- max(1L, parallel::detectCores(logical = TRUE) - 1L)
}

message("Effective settings:",
        "\n  Ground truth: ", GROUND_TRUTH,
        "\n  RUN_DISCOVERY: ", RUN_DISCOVERY,
        "\n  RUN_BOOTSTRAP: ", RUN_BOOTSTRAP,
        "\n  BOOT_R: ", BOOT_R,
        "\n  MC_DRAWS_POINT: ", MC_DRAWS_POINT,
        "\n  MC_DRAWS_BOOT: ", MC_DRAWS_BOOT,
        "\n  MC_DRAWS_TRUTH: ", MC_DRAWS_TRUTH,
        if (BOOT_PARALLEL) paste0("\n  BOOT_PARALLEL: TRUE (cores=", BOOT_NCORES, ")") else "\n  BOOT_PARALLEL: FALSE"
)


# ------------------------------------------------------------
# 3) Small utilities
# ------------------------------------------------------------
fmt_num <- function(x, digits = 2) {
  ifelse(is.na(x), "", formatC(x, format = "f", digits = digits))
}

write_latex_table_booktabs <- function(df, file, caption = NULL, label = NULL, digits = 2) {
  stopifnot(is.data.frame(df))
  path <- file.path(DIR_TABLES, file)

  df2 <- df
  for (j in seq_len(ncol(df2))) {
    if (is.numeric(df2[[j]])) df2[[j]] <- fmt_num(df2[[j]], digits = digits)
  }

  con <- file(path, open = "wt")
  on.exit(close(con), add = TRUE)

  cat("% Auto-generated by BriDGE semi-synthetic JOBS II script\n", file = con)
  cat("\\begin{table}[!htbp]\n\\centering\n", file = con)
  if (!is.null(caption)) cat("\\caption{", caption, "}\n", sep = "", file = con)
  if (!is.null(label))   cat("\\label{", label, "}\n", sep = "", file = con)

  align <- paste(rep("l", ncol(df2)), collapse = "")
  cat("\\begin{tabular}{", align, "}\n\\toprule\n", sep = "", file = con)
  cat(paste(colnames(df2), collapse = " & "), " \\\\\n\\midrule\n", sep = "", file = con)

  for (i in seq_len(nrow(df2))) {
    row <- df2[i, , drop = TRUE]
    cat(paste(row, collapse = " & "), " \\\\\n", sep = "", file = con)
  }

  cat("\\bottomrule\n\\end{tabular}\n\\end{table}\n", file = con)
  invisible(path)
}
# ------------------------------------------------------------
# 3.1) Discretization helper (robust to binary / low-unique covariates)
#      bnlearn MI tests require discrete variables as factors.
#      bnlearn::discretize(method="quantile", breaks=g) can fail when a variable
#      has too few distinct values or strong mass points (duplicate quantiles),
#      producing zero-length intervals. We therefore:
#        * treat variables with <= g unique values as factors (no discretization),
#        * otherwise discretize via tie-robust quantile cuts (unique breakpoints),
#        * drop constant variables (1 unique value) from the discovery data.
# ------------------------------------------------------------
safe_factor_for_bnlearn <- function(v, g, varname = "") {
  # Returns:
  #   - a factor/ordered factor suitable for bnlearn, or
  #   - NULL to indicate the variable should be dropped (constant / non-informative).
  if (is.factor(v)) {
    v2 <- droplevels(v)
    if (nlevels(v2) < 2L) return(NULL)
    return(v2)
  }
  if (is.character(v) || is.logical(v)) {
    v2 <- factor(v)
    if (nlevels(v2) < 2L) return(NULL)
    return(v2)
  }
  if (inherits(v, c("Date", "POSIXct", "POSIXt"))) {
    v2 <- factor(v)
    if (nlevels(v2) < 2L) return(NULL)
    return(v2)
  }
  if (!is.numeric(v) && !is.integer(v)) {
    v2 <- factor(v)
    if (nlevels(v2) < 2L) return(NULL)
    return(v2)
  }

  vv <- v[!is.na(v)]
  u  <- unique(vv)
  if (length(u) < 2L) return(NULL)

  # If already low-cardinality (e.g., binary dummies), do NOT discretize.
  if (length(u) <= g) {
    v2 <- factor(v)
    if (nlevels(v2) < 2L) return(NULL)
    return(v2)
  }

  # Tie-robust quantile discretization: use unique breakpoints.
  probs <- seq(0, 1, length.out = g + 1)
  br <- unique(stats::quantile(v, probs = probs, na.rm = TRUE, type = 8))

  # If quantiles collapse due to mass points, fall back to factor (still discrete).
  if (length(br) <= 2L) {
    v2 <- factor(v)
    if (nlevels(v2) < 2L) return(NULL)
    return(v2)
  }

  out <- cut(v, breaks = br, include.lowest = TRUE, ordered_result = TRUE)

  # Defensive fallback: if cut failed unexpectedly, treat as factor.
  if (all(is.na(out)) && !all(is.na(v))) {
    v2 <- factor(v)
    if (nlevels(v2) < 2L) return(NULL)
    return(v2)
  }

  out <- droplevels(out)
  if (nlevels(out) < 2L) return(NULL)
  return(out)
}

safe_discretize_df_for_bnlearn <- function(df, g) {
  stopifnot(is.data.frame(df))
  out <- list()
  dropped <- character(0)

  for (nm in names(df)) {
    v <- safe_factor_for_bnlearn(df[[nm]], g = g, varname = nm)
    if (is.null(v)) {
      dropped <- c(dropped, nm)
    } else {
      out[[nm]] <- v
    }
  }

  out_df <- as.data.frame(out, stringsAsFactors = TRUE)
  attr(out_df, "dropped") <- dropped
  out_df
}


bn_to_igraph <- function(bn) {
  a <- bnlearn::arcs(bn)
  if (is.null(a) || nrow(a) == 0L) {
    g0 <- igraph::make_empty_graph(n = length(bnlearn::nodes(bn)), directed = TRUE)
    g0 <- igraph::set_vertex_attr(g0, "name", value = bnlearn::nodes(bn))
    return(g0)
  }
  igraph::graph_from_data_frame(a, directed = TRUE, vertices = bnlearn::nodes(bn))
}

tree_layout <- function(g) {
  if (igraph::gorder(g) <= 1) return(matrix(c(0, 0), ncol = 2))
  r <- which(igraph::V(g)$name == "treatment")
  if (length(r) == 0L) r <- 1L
  igraph::layout_as_tree(g, root = r, circular = FALSE)
}

save_dag_png <- function(bn, file, title, fill = "lightgray") {
  out_file <- file.path(DIR_DAGS, file)
  ok <- FALSE

  if (USE_RGRAPHVIZ) {
    try({
      png(out_file, width = 1200, height = 900, res = 150)
      bnlearn::graphviz.plot(
        bn, layout = "dot", main = title,
        shape = "ellipse",
        highlight = list(nodes = "treatment", fill = fill),
        render = TRUE
      )
      dev.off()
      ok <- TRUE
    }, silent = TRUE)
  }

  if (!ok) {
    g <- bn_to_igraph(bn)
    png(out_file, width = 1200, height = 900, res = 150)
    plot(
      g, main = title,
      vertex.size = 26, vertex.color = fill, vertex.label.cex = 0.9,
      edge.arrow.size = 0.6,
      layout = tree_layout(g)
    )
    dev.off()
  }
  invisible(normalizePath(out_file))
}

combine_two_pngs <- function(file_left, file_right, out_file) {
  out_file <- file.path(DIR_FIGURES, out_file)
  img1 <- png::readPNG(file_left)
  img2 <- png::readPNG(file_right)
  png(out_file, width = 1800, height = 900, res = 150)
  par(mfrow = c(1, 2), mar = c(0.5, 0.5, 2, 0.5))
  plot(0:1, 0:1, type = "n", axes = FALSE, xlab = "", ylab = "")
  rasterImage(img1, 0, 0, 1, 1)
  plot(0:1, 0:1, type = "n", axes = FALSE, xlab = "", ylab = "")
  rasterImage(img2, 0, 0, 1, 1)
  dev.off()
  invisible(normalizePath(out_file))
}

fit_gam_safely <- function(formula, data) {
  # Fit GAM with warning capture + a conservative fallback.
  w <- character(0)
  fit1 <- withCallingHandlers(
    tryCatch(
      mgcv::gam(
        formula, data = data,
        family = gaussian(), method = "REML",
        select = TRUE,
        control = mgcv::gam.control(maxit = 300, trace = FALSE)
      ),
      error = function(e) e
    ),
    warning = function(wrn) {
      w <<- c(w, conditionMessage(wrn))
      invokeRestart("muffleWarning")
    }
  )

  if (inherits(fit1, "error")) {
    fit2 <- withCallingHandlers(
      tryCatch(
        mgcv::gam(
          formula, data = data,
          family = gaussian(), method = "REML",
          select = FALSE,
          control = mgcv::gam.control(maxit = 500, trace = FALSE)
        ),
        error = function(e) e
      ),
      warning = function(wrn) {
        w <<- c(w, conditionMessage(wrn))
        invokeRestart("muffleWarning")
      }
    )
    if (inherits(fit2, "error")) {
      stop("GAM failed under both primary and fallback settings. Last error: ", fit2$message)
    }
    attr(fit2, "warnings") <- unique(w)
    return(fit2)
  }

  attr(fit1, "warnings") <- unique(w)
  fit1
}

# ------------------------------------------------------------
# 4) Load JOBS II scaffold + build baseline covariate matrix
# ------------------------------------------------------------
message("\n--- LOAD: mediation::jobs (JOBS II scaffold) ---")
data("jobs", package = "mediation")  # loads 'jobs'
jobs_raw <- jobs

if (!("treat" %in% names(jobs_raw))) {
  stop("Expected a 'treat' column in mediation::jobs but did not find it.")
}

# Treatment as numeric 0/1 (preserve RCT assignment)
treat_raw <- jobs_raw$treat
treat_num <- if (is.factor(treat_raw)) as.integer(treat_raw) - 1L else as.integer(treat_raw)
treat_num <- ifelse(treat_num > 0, 1L, 0L)

# Candidate baseline covariates (by common JOBS II naming; robust fallback below)
cand_x <- c("econ_hard", "depress1", "age", "educ", "income", "sex", "marital", "race", "nonwhite")
x_vars <- intersect(cand_x, names(jobs_raw))

# Fallback: choose up to 6 numeric non-treatment columns
if (length(x_vars) == 0L) {
  message("No candidate baseline covariates found by name; using numeric columns fallback.")
  num_cols <- names(jobs_raw)[vapply(jobs_raw, is.numeric, logical(1))]
  num_cols <- setdiff(num_cols, c("treat", "treatment"))
  x_vars <- head(num_cols, 6)
}

message("Using baseline covariates (raw columns): ", paste(x_vars, collapse = ", "))

# Model matrix (handles factors if present); cap dimensionality for discovery stability
X_raw <- jobs_raw[, x_vars, drop = FALSE]
X_mm  <- model.matrix(~ . - 1, data = X_raw)
colnames(X_mm) <- make.names(colnames(X_mm))

# Optional cap to avoid very high-dimensional X in bnlearn discovery
MAX_X_COLS <- 12L
if (ncol(X_mm) > MAX_X_COLS) {
  message("Model-matrix baseline covariates expanded to ", ncol(X_mm),
          " columns; capping to first ", MAX_X_COLS, " for stability.")
  X_mm <- X_mm[, seq_len(MAX_X_COLS), drop = FALSE]
}

# Standardize for stable coefficient scaling in DGP
X_std <- scale(X_mm)

n <- nrow(jobs_raw)
p <- ncol(X_std)

# ------------------------------------------------------------
# 5) Semi-synthetic DGP: mediators and outcome
# ------------------------------------------------------------
# Coefficients:
#   - Treatment / mediator / outcome magnitudes mirror the main manuscript simulation.
#   - X effects create realistic dependence and mediator–outcome confounding structure.
set.seed(SEED_MAIN + 100)
beta_X_M1 <- rnorm(p, mean = 0, sd = 0.20)
beta_X_M2 <- rnorm(p, mean = 0, sd = 0.20)
beta_X_Y  <- rnorm(p, mean = 0, sd = 0.20)
w1 <- rnorm(p, mean = 0, sd = 0.50)
w2 <- rnorm(p, mean = 0, sd = 0.50)

# Structural coefficients (match the simulation section)
beta_T_M1 <- 1.0
beta_T_M2 <- 0.8
beta_T_Y  <- 1.5
beta_M1Y  <- 1.2
beta_M2Y  <- 0.9
gamma_M1_2 <- 0.5
gamma_M1M2 <- 0.3
gamma_TM1  <- 0.5
gamma_TM2  <- 0.7

# If additive ground truth requested: remove non-additive terms but keep M1^2 (nonlinearity)
if (tolower(GROUND_TRUTH) == "additive") {
  gamma_M1M2 <- 0.0
  gamma_TM1  <- 0.0
  gamma_TM2  <- 0.0
}

DGP <- list(
  alpha = ALPHA_NONLIN,
  beta_T_M1 = beta_T_M1,
  beta_T_M2 = beta_T_M2,
  beta_T_Y  = beta_T_Y,
  beta_M1Y  = beta_M1Y,
  beta_M2Y  = beta_M2Y,
  gamma_M1_2 = gamma_M1_2,
  gamma_M1M2 = gamma_M1M2,
  gamma_TM1  = gamma_TM1,
  gamma_TM2  = gamma_TM2,
  beta_X_M1 = beta_X_M1,
  beta_X_M2 = beta_X_M2,
  beta_X_Y  = beta_X_Y,
  w1 = w1,
  w2 = w2
)

dgp_draw_mediators <- function(treat_vec, X_std, DGP, seed = NULL) {
  if (!is.null(seed)) set.seed(seed)
  n <- length(treat_vec)
  eps1 <- rnorm(n)
  eps2 <- rnorm(n)
  eta1 <- rnorm(n)
  eta2 <- rnorm(n)

  z1 <- as.numeric(X_std %*% DGP$w1) + eta1
  z2 <- as.numeric(X_std %*% DGP$w2) + eta2

  m1 <- DGP$beta_T_M1 * treat_vec + as.numeric(X_std %*% DGP$beta_X_M1) + eps1 +
    DGP$alpha * sin(z1)
  m2 <- DGP$beta_T_M2 * treat_vec + as.numeric(X_std %*% DGP$beta_X_M2) + eps2 +
    DGP$alpha * exp(z2 / 2)

  list(m1 = m1, m2 = m2, eps1 = eps1, eps2 = eps2, eta1 = eta1, eta2 = eta2)
}

dgp_compute_outcome <- function(treat_vec, m1, m2, X_std, DGP, eps3 = NULL, seed = NULL) {
  if (!is.null(seed)) set.seed(seed)
  n <- length(treat_vec)
  if (is.null(eps3)) eps3 <- rnorm(n)

  y <- DGP$beta_T_Y * treat_vec +
    DGP$beta_M1Y * m1 + DGP$beta_M2Y * m2 +
    DGP$gamma_M1_2 * (m1^2) + DGP$gamma_M1M2 * m1 * m2 +
    DGP$gamma_TM1 * treat_vec * m1 + DGP$gamma_TM2 * treat_vec * m2 +
    as.numeric(X_std %*% DGP$beta_X_Y) +
    eps3

  y
}

# Generate a single observed semi-synthetic dataset (one draw of the SCM)
set.seed(SEED_MAIN)
m_draw <- dgp_draw_mediators(treat_num, X_std, DGP)
eps3  <- rnorm(n)
y_obs <- dgp_compute_outcome(treat_num, m_draw$m1, m_draw$m2, X_std, DGP, eps3 = eps3)

# Build analysis dataset (BriDGE-friendly names)
X_df <- as.data.frame(X_mm)
names(X_df) <- paste0("x_", make.names(names(X_df)))

dat <- data.frame(
  treatment  = treat_num,
  mediator_1 = m_draw$m1,
  mediator_2 = m_draw$m2,
  outcome    = y_obs,
  X_df,
  check.names = FALSE
)
dat <- dat[complete.cases(dat), , drop = FALSE]

vars_all <- names(dat)
vars_x   <- grep("^x_", vars_all, value = TRUE)
vars_m   <- grep("^mediator_", vars_all, value = TRUE)

message("Semi-synthetic dataset size: N = ", nrow(dat),
        " (treatment proportion = ", round(mean(dat$treatment), 3), ")",
        "\nGround truth DGP: ", GROUND_TRUTH)

# Save dataset + session info for reproducibility
write.csv(dat, file = file.path(OUT_ROOT, "jobs2_semisynthetic_data.csv"), row.names = FALSE)
capture.output(sessionInfo(), file = file.path(OUT_ROOT, "sessionInfo_jobs2_semisynth.txt"))

# ------------------------------------------------------------
# 6) True DAG + constrained discovery (MMHC)
# ------------------------------------------------------------
if (RUN_DISCOVERY) {
  message("\n--- RUN: causal discovery (MMHC; discretized) ---")

  true_bn <- bnlearn::empty.graph(nodes = vars_all)

  # Baseline covariates -> mediators and outcome (semi-synthetic design)
  for (xv in vars_x) {
    true_bn <- bnlearn::set.arc(true_bn, from = xv, to = "mediator_1")
    true_bn <- bnlearn::set.arc(true_bn, from = xv, to = "mediator_2")
    true_bn <- bnlearn::set.arc(true_bn, from = xv, to = "outcome")
  }
  # Treatment -> mediators and outcome
  true_bn <- bnlearn::set.arc(true_bn, from = "treatment", to = "mediator_1")
  true_bn <- bnlearn::set.arc(true_bn, from = "treatment", to = "mediator_2")
  true_bn <- bnlearn::set.arc(true_bn, from = "treatment", to = "outcome")
  # Mediators -> outcome
  true_bn <- bnlearn::set.arc(true_bn, from = "mediator_1", to = "outcome")
  true_bn <- bnlearn::set.arc(true_bn, from = "mediator_2", to = "outcome")

  # Discretize/encode variables for bnlearn discrete MI tests (robust to low-unique covariates)
  dd_raw <- as.data.frame(dat)
  dd_raw$treatment <- factor(dd_raw$treatment, levels = c(0, 1))

  cont_vars <- setdiff(names(dd_raw), "treatment")
  dd_disc <- safe_discretize_df_for_bnlearn(dd_raw[cont_vars], g = DISC_BINS)
  dropped <- attr(dd_disc, "dropped")
  if (length(dropped) > 0L) {
    message("Note: dropping ", length(dropped), " constant/non-informative variable(s) from discovery: ",
            paste(dropped, collapse = ", "))
  }
  dd <- cbind(treatment = dd_raw$treatment, dd_disc)

  # Update variable sets for discovery to match dd (after dropping constants)
  vars_all_disc <- names(dd)
  vars_x_disc   <- grep("^x_", vars_all_disc, value = TRUE)
  vars_m_disc   <- grep("^mediator_", vars_all_disc, value = TRUE)

  # Constraints:
  #  - Randomization: no inbound edges to treatment; require treatment -> mediators.
  #  - Baseline X: treat baseline covariates as exogenous (no inbound edges).
  wl <- data.frame(from = "treatment", to = vars_m_disc, stringsAsFactors = FALSE)

  bl1 <- data.frame(from = setdiff(vars_all_disc, "treatment"), to = "treatment", stringsAsFactors = FALSE)
  bl2 <- expand.grid(from = vars_all_disc, to = vars_x_disc, stringsAsFactors = FALSE)
  bl  <- unique(rbind(bl1, bl2))
  bl  <- bl[bl$from != bl$to, , drop = FALSE]

  maximize.args <- if (DISC_SCORE == "bde") list(score = "bde", iss = DISC_ISS) else list(score = "bic")

  fit_bn <- bnlearn::mmhc(
    dd,
    whitelist = wl, blacklist = bl,
    restrict.args = list(test = "mi", alpha = DISC_ALPHA),
    maximize.args = maximize.args
  )

  strn <- bnlearn::boot.strength(
    dd, R = DISC_STRENGTH_R,
    algorithm = "mmhc",
    algorithm.args = list(
      whitelist = wl, blacklist = bl,
      restrict.args = list(test = "mi", alpha = DISC_ALPHA),
      maximize.args = maximize.args
    )
  )
  avg_bn <- bnlearn::averaged.network(strn, threshold = DISC_AVG_THR)

  # Save DAGs as PNGs + combined panel
  f_true <- save_dag_png(true_bn, "true_dag_jobs2.png", "True DAG (semi-synthetic; JOBS II scaffold)")
  f_disc <- save_dag_png(avg_bn,  "discovered_dag_jobs2.png",
                         sprintf("Discovered DAG (MMHC; bins=%d, alpha=%.2f, thr=%.2f)", DISC_BINS, DISC_ALPHA, DISC_AVG_THR))
  combine_two_pngs(f_true, f_disc, "fig_dag_true_vs_discovered_jobs2.png")

  # DAG comparison table (focused on {treatment, mediator_1, mediator_2, outcome})
  focus_nodes <- c("treatment", "mediator_1", "mediator_2", "outcome")

  arcs_focus <- function(bn) {
    a <- bnlearn::arcs(bn)
    if (is.null(a) || nrow(a) == 0L) return(matrix(character(0), ncol = 2))
    a <- as.matrix(a)
    a[a[,1] %in% focus_nodes & a[,2] %in% focus_nodes, , drop = FALSE]
  }

  A_true <- arcs_focus(true_bn)
  A_disc <- arcs_focus(avg_bn)

  edge_key <- function(a) if (nrow(a) == 0L) character(0) else paste(a[,1], a[,2], sep = "->")
  E_true <- edge_key(A_true)
  E_disc <- edge_key(A_disc)
  E_all  <- sort(unique(c(E_true, E_disc)))

  cmp <- data.frame(
    From = sub("->.*$", "", E_all),
    To   = sub("^.*->", "", E_all),
    True_DAG   = E_all %in% E_true,
    Discovered = E_all %in% E_disc,
    stringsAsFactors = FALSE
  )
  write_latex_table_booktabs(
    cmp,
    file = "dag_comparison_jobs2_focus.tex",
    caption = "Directed edge comparison (focus set: treatment, mediators, outcome) for the semi-synthetic JOBS II scaffold.",
    label   = "tab:dag_jobs2_focus",
    digits  = 0
  )
}

# ------------------------------------------------------------
# 7) Mediator models + outcome models (primary + sensitivity)
# ------------------------------------------------------------
message("\n--- FIT: mediator models (linear) + outcome GAMs (primary + sensitivity) ---")

# Mediator models (parametric; used to simulate joint mediator draws under t=0/1)
m1_fit <- lm(
  as.formula(paste("mediator_1 ~ treatment +", paste(vars_x, collapse = " + "))),
  data = dat
)
m2_fit <- lm(
  as.formula(paste("mediator_2 ~ treatment + mediator_1 +", paste(vars_x, collapse = " + "))),
  data = dat
)

# Outcome model formulas
gam_formula_additive <- as.formula(paste(
  "outcome ~ treatment",
  "+ s(mediator_1, bs='tp', k=10)",
  "+ s(mediator_2, bs='tp', k=10)",
  if (length(vars_x) > 0) paste("+", paste(vars_x, collapse = " + ")) else "",
  sep = " "
))

gam_formula_interaction <- as.formula(paste(
  "outcome ~ treatment",
  "+ s(mediator_1, bs='tp', k=10)",
  "+ s(mediator_2, bs='tp', k=10)",
  "+ ti(mediator_1, mediator_2, bs='tp', k=c(10,10))",
  "+ s(mediator_1, bs='tp', k=10, by=treatment)",
  "+ s(mediator_2, bs='tp', k=10, by=treatment)",
  if (length(vars_x) > 0) paste("+", paste(vars_x, collapse = " + ")) else "",
  sep = " "
))

# Fit GAMs
y_fit_int <- fit_gam_safely(gam_formula_interaction, dat)
y_fit_add <- fit_gam_safely(gam_formula_additive, dat)

# Save GAM partial effect plots for both models (base plotting)
png(file.path(DIR_FIGURES, "fig_gam_partial_effects_jobs2_interaction.png"), width = 1400, height = 900, res = 160)
par(mfrow = c(2, 3), mar = c(4, 4, 2, 1))
plot(y_fit_int, pages = 1, shade = TRUE, residuals = FALSE)
dev.off()

png(file.path(DIR_FIGURES, "fig_gam_partial_effects_jobs2_additive.png"), width = 1400, height = 900, res = 160)
par(mfrow = c(2, 2), mar = c(4, 4, 2, 1))
plot(y_fit_add, pages = 1, shade = TRUE, residuals = FALSE)
dev.off()

# ------------------------------------------------------------
# 8) Interventional effects (g-computation) for each estimated model
# ------------------------------------------------------------
simulate_mediators_given_TX <- function(dat, t_val, m1_fit, m2_fit, covars) {
  nd1 <- dat
  nd1$treatment <- t_val
  mu1 <- as.numeric(predict(m1_fit, newdata = nd1))
  e1  <- sample(residuals(m1_fit), nrow(dat), replace = TRUE)
  m1  <- mu1 + e1

  nd2 <- dat
  nd2$treatment  <- t_val
  nd2$mediator_1 <- m1
  mu2 <- as.numeric(predict(m2_fit, newdata = nd2))
  e2  <- sample(residuals(m2_fit), nrow(dat), replace = TRUE)
  m2  <- mu2 + e2

  data.frame(mediator_1 = m1, mediator_2 = m2)
}

interventional_effects_2med_cov <- function(dat, y_fit, m1_fit, m2_fit, covars,
                                            mc_draws = 200, seed_mc = 2025,
                                            compute_marginal_iies = TRUE) {
  set.seed(seed_mc)
  E00 <- E10 <- E01 <- E11 <- numeric(mc_draws)
  IIE1 <- IIE2 <- if (compute_marginal_iies) numeric(mc_draws) else NULL

  base <- if (length(covars) > 0) dat[, covars, drop = FALSE] else data.frame(row_id = seq_len(nrow(dat)))

  for (b in seq_len(mc_draws)) {
    m0 <- simulate_mediators_given_TX(dat, 0, m1_fit, m2_fit, covars)
    m1 <- simulate_mediators_given_TX(dat, 1, m1_fit, m2_fit, covars)

    y00 <- predict(y_fit, newdata = cbind(base, treatment = 0, mediator_1 = m0$mediator_1, mediator_2 = m0$mediator_2))
    y10 <- predict(y_fit, newdata = cbind(base, treatment = 1, mediator_1 = m0$mediator_1, mediator_2 = m0$mediator_2))
    y01 <- predict(y_fit, newdata = cbind(base, treatment = 0, mediator_1 = m1$mediator_1, mediator_2 = m1$mediator_2))
    y11 <- predict(y_fit, newdata = cbind(base, treatment = 1, mediator_1 = m1$mediator_1, mediator_2 = m1$mediator_2))

    E00[b] <- mean(y00)
    E10[b] <- mean(y10)
    E01[b] <- mean(y01)
    E11[b] <- mean(y11)

    if (compute_marginal_iies) {
      y0_base  <- y00
      y0_m1hyb <- predict(y_fit, newdata = cbind(base, treatment = 0, mediator_1 = m1$mediator_1, mediator_2 = m0$mediator_2))
      y0_m2hyb <- predict(y_fit, newdata = cbind(base, treatment = 0, mediator_1 = m0$mediator_1, mediator_2 = m1$mediator_2))
      IIE1[b] <- mean(y0_m1hyb - y0_base)
      IIE2[b] <- mean(y0_m2hyb - y0_base)
    }
  }

  ide  <- mean(E10 - E00)
  jiie <- mean(E01 - E00)
  te   <- mean(E11 - E00)

  out <- c(IDE = ide, JIIE = jiie, TE = te)
  if (compute_marginal_iies) {
    out <- c(out, IIE_M1 = mean(IIE1), IIE_M2 = mean(IIE2))
  }
  out
}

message("\n--- ESTIMATE: interventional effects (primary interaction GAM + additive sensitivity) ---")
seed_common <- 2025
eff_hat_int <- interventional_effects_2med_cov(
  dat = dat, y_fit = y_fit_int, m1_fit = m1_fit, m2_fit = m2_fit,
  covars = vars_x, mc_draws = MC_DRAWS_POINT, seed_mc = seed_common, compute_marginal_iies = TRUE
)
eff_hat_add <- interventional_effects_2med_cov(
  dat = dat, y_fit = y_fit_add, m1_fit = m1_fit, m2_fit = m2_fit,
  covars = vars_x, mc_draws = MC_DRAWS_POINT, seed_mc = seed_common, compute_marginal_iies = TRUE
)

# ------------------------------------------------------------
# 9) "True" effects under the known DGP (semi-synthetic benchmark)
# ------------------------------------------------------------
true_interventional_effects <- function(X_std, DGP, mc_draws = 2000, seed = 777) {
  set.seed(seed)
  n <- nrow(X_std)

  E00 <- E10 <- E01 <- E11 <- numeric(mc_draws)
  IIE1 <- IIE2 <- numeric(mc_draws)

  # Use shared exogenous draws within each Monte Carlo iteration for coherence
  for (b in seq_len(mc_draws)) {
    eps1 <- rnorm(n); eps2 <- rnorm(n); eps3 <- rnorm(n)
    eta1 <- rnorm(n); eta2 <- rnorm(n)

    z1 <- as.numeric(X_std %*% DGP$w1) + eta1
    z2 <- as.numeric(X_std %*% DGP$w2) + eta2

    m1_0 <- DGP$beta_T_M1 * 0 + as.numeric(X_std %*% DGP$beta_X_M1) + eps1 + DGP$alpha * sin(z1)
    m1_1 <- DGP$beta_T_M1 * 1 + as.numeric(X_std %*% DGP$beta_X_M1) + eps1 + DGP$alpha * sin(z1)

    m2_0 <- DGP$beta_T_M2 * 0 + as.numeric(X_std %*% DGP$beta_X_M2) + eps2 + DGP$alpha * exp(z2 / 2)
    m2_1 <- DGP$beta_T_M2 * 1 + as.numeric(X_std %*% DGP$beta_X_M2) + eps2 + DGP$alpha * exp(z2 / 2)

    y00 <- dgp_compute_outcome(rep(0, n), m1_0, m2_0, X_std, DGP, eps3 = eps3)
    y10 <- dgp_compute_outcome(rep(1, n), m1_0, m2_0, X_std, DGP, eps3 = eps3)
    y01 <- dgp_compute_outcome(rep(0, n), m1_1, m2_1, X_std, DGP, eps3 = eps3)
    y11 <- dgp_compute_outcome(rep(1, n), m1_1, m2_1, X_std, DGP, eps3 = eps3)

    y0_m1hyb <- dgp_compute_outcome(rep(0, n), m1_1, m2_0, X_std, DGP, eps3 = eps3)
    y0_m2hyb <- dgp_compute_outcome(rep(0, n), m1_0, m2_1, X_std, DGP, eps3 = eps3)

    E00[b] <- mean(y00)
    E10[b] <- mean(y10)
    E01[b] <- mean(y01)
    E11[b] <- mean(y11)

    IIE1[b] <- mean(y0_m1hyb - y00)
    IIE2[b] <- mean(y0_m2hyb - y00)
  }

  ide  <- mean(E10 - E00)
  jiie <- mean(E01 - E00)
  te   <- mean(E11 - E00)

  c(IDE = ide, JIIE = jiie, TE = te, IIE_M1 = mean(IIE1), IIE_M2 = mean(IIE2))
}

message("\n--- TRUTH: Monte Carlo ground truth under the semi-synthetic DGP ---")
eff_true <- true_interventional_effects(X_std = X_std, DGP = DGP, mc_draws = MC_DRAWS_TRUTH, seed = 777)

# ------------------------------------------------------------
# 10) Write truth vs estimates table (no bootstrap yet)
# ------------------------------------------------------------
effects <- names(eff_true)
tab_truth <- data.frame(
  Effect = effects,
  Truth  = as.numeric(eff_true[effects]),
  Est_InteractionGAM = as.numeric(eff_hat_int[effects]),
  Bias_InteractionGAM = as.numeric(eff_hat_int[effects] - eff_true[effects]),
  Est_AdditiveGAM = as.numeric(eff_hat_add[effects]),
  Bias_AdditiveGAM = as.numeric(eff_hat_add[effects] - eff_true[effects]),
  stringsAsFactors = FALSE
)

write_latex_table_booktabs(
  tab_truth,
  file = "truth_vs_estimates_jobs2.tex",
  caption = "Semi-synthetic JOBS II benchmark: known ground truth effects under the structural model versus BriDGE estimates under (i) an interaction-capable GAM (primary) and (ii) an additive-only GAM (sensitivity).",
  label   = "tab:truth_vs_est_jobs2",
  digits  = 3
)

# Also write simple model-specific tables (point estimates only)
tab_primary <- data.frame(
  Effect = effects,
  Estimate = as.numeric(eff_hat_int[effects]),
  stringsAsFactors = FALSE
)
write_latex_table_booktabs(
  tab_primary,
  file = "mediation_effects_jobs2_primary_interaction.tex",
  caption = "Semi-synthetic JOBS II: point estimates using the interaction-capable outcome GAM (primary specification).",
  label   = "tab:jobs2_primary_int",
  digits  = 3
)

tab_sens <- data.frame(
  Effect = effects,
  Estimate = as.numeric(eff_hat_add[effects]),
  stringsAsFactors = FALSE
)
write_latex_table_booktabs(
  tab_sens,
  file = "mediation_effects_jobs2_sensitivity_additive.tex",
  caption = "Semi-synthetic JOBS II: point estimates using the additive-only outcome GAM (sensitivity specification).",
  label   = "tab:jobs2_sens_add",
  digits  = 3
)

# ------------------------------------------------------------
# 11) Bootstrap + BCa intervals for both models (primary + sensitivity)
# ------------------------------------------------------------
ci_summary <- NULL
if (RUN_BOOTSTRAP) {
  message("\n--- BOOTSTRAP: BCa intervals (primary interaction GAM + additive sensitivity) ---")
  message("Bootstrap replicates (BOOT_R): ", BOOT_R, " ; inner MC draws: ", MC_DRAWS_BOOT)
  # Progress counter for serial runs (boot() evaluates the statistic once for t0, then R times).
  boot_counter <- 0L


  boot_stat <- function(data, indices) {
    boot_counter <<- boot_counter + 1L
    if (!BOOT_PARALLEL && boot_counter > 1L && ((boot_counter - 1L) %% BOOT_PROGRESS_EVERY) == 0L) {
      message(sprintf("  bootstrap replicate %d / %d", boot_counter - 1L, BOOT_R))
    }

    d <- data[indices, , drop = FALSE]

    # Refit mediator models
    m1b <- lm(
      as.formula(paste("mediator_1 ~ treatment +", paste(vars_x, collapse = " + "))),
      data = d
    )
    m2b <- lm(
      as.formula(paste("mediator_2 ~ treatment + mediator_1 +", paste(vars_x, collapse = " + "))),
      data = d
    )

    # Refit outcome GAMs
    yb_int <- fit_gam_safely(gam_formula_interaction, d)
    yb_add <- fit_gam_safely(gam_formula_additive, d)

    # Use identical MC seed across the two model calls within this bootstrap replicate
    seed_rep <- 1000 + as.integer(sum(indices) %% 1000000)

    eff_int <- interventional_effects_2med_cov(
      dat = d, y_fit = yb_int, m1_fit = m1b, m2_fit = m2b,
      covars = vars_x, mc_draws = MC_DRAWS_BOOT, seed_mc = seed_rep, compute_marginal_iies = TRUE
    )
    eff_add <- interventional_effects_2med_cov(
      dat = d, y_fit = yb_add, m1_fit = m1b, m2_fit = m2b,
      covars = vars_x, mc_draws = MC_DRAWS_BOOT, seed_mc = seed_rep, compute_marginal_iies = TRUE
    )

    out <- c(eff_int, eff_add)
    names(out) <- c(paste0(names(eff_int), "_int"), paste0(names(eff_add), "_add"))
    as.numeric(out)
  }

  set.seed(1234)

  if (BOOT_PARALLEL) {
    message("Running bootstrap in parallel (snow). Workers: ", BOOT_NCORES,
            ". Note: per-replicate progress messages are disabled in parallel mode.")
    cl <- try(parallel::makeCluster(BOOT_NCORES), silent = TRUE)
    if (inherits(cl, "try-error")) {
      message("Could not create a parallel cluster; falling back to serial bootstrap.")
      BOOT_PARALLEL <- FALSE
    } else {
      on.exit(parallel::stopCluster(cl), add = TRUE)
      parallel::clusterEvalQ(cl, {library(mgcv); library(boot)})

      # Export minimal objects/functions referenced inside boot_stat.
      parallel::clusterExport(
        cl,
        varlist = c("vars_x", "gam_formula_interaction", "gam_formula_additive",
                    "fit_gam_safely", "interventional_effects_2med_cov",
                    "simulate_mediators_given_TX", "MC_DRAWS_BOOT"),
        envir = environment()
      )

      bobj <- boot::boot(
        data = dat,
        statistic = boot_stat,
        R = BOOT_R,
        parallel = "snow",
        cl = cl
      )
    }
  }

  if (!BOOT_PARALLEL) {
    bobj <- boot::boot(
      data = dat,
      statistic = boot_stat,
      R = BOOT_R
    )
  }

  if (BOOT_SAVE_RDS) {
    rds_path <- file.path(OUT_ROOT, sprintf("boot_object_jobs2_B%d.rds", BOOT_R))
    saveRDS(bobj, rds_path)
    message("Saved bootstrap object to: ", rds_path)
  }

  # Name columns
  eff_names <- effects
  colnames(bobj$t) <- c(paste0(eff_names, "_int"), paste0(eff_names, "_add"))

  # Helper to compute BCa CI
  get_bca <- function(bobj, idx) {
    ci <- try(boot::boot.ci(bobj, type = "bca", index = idx), silent = TRUE)
    if (inherits(ci, "try-error") || is.null(ci$bca)) return(c(NA_real_, NA_real_))
    c(ci$bca[4], ci$bca[5])
  }

  # Collect intervals
  ci_int <- t(vapply(seq_along(eff_names), function(j) get_bca(bobj, j), numeric(2)))
  ci_add <- t(vapply(seq_along(eff_names), function(j) get_bca(bobj, length(eff_names) + j), numeric(2)))

  colnames(ci_int) <- c("CI_low", "CI_high")
  colnames(ci_add) <- c("CI_low", "CI_high")

  # Build combined CI table (truth + two model estimates with CIs)
  tab_ci <- data.frame(
    Effect = eff_names,
    Truth = as.numeric(eff_true[eff_names]),
    Est_Int = as.numeric(eff_hat_int[eff_names]),
    CI_Int = paste0("[", fmt_num(ci_int[,1], 3), ", ", fmt_num(ci_int[,2], 3), "]"),
    Est_Add = as.numeric(eff_hat_add[eff_names]),
    CI_Add = paste0("[", fmt_num(ci_add[,1], 3), ", ", fmt_num(ci_add[,2], 3), "]"),
    stringsAsFactors = FALSE
  )

  write_latex_table_booktabs(
    tab_ci,
    file = "mediation_effects_jobs2_models_with_ci.tex",
    caption = "Semi-synthetic JOBS II: ground truth and BriDGE estimates with 95\\% BCa bootstrap intervals under the primary interaction-capable GAM and the additive-only sensitivity GAM.",
    label   = "tab:jobs2_models_ci",
    digits  = 3
  )

  # Bootstrap histograms for each model (5 effects -> 2×3 panel)
  png(file.path(DIR_FIGURES, "fig_bootstrap_hist_jobs2_interaction.png"), width = 1600, height = 900, res = 160)
  par(mfrow = c(2, 3), mar = c(4, 4, 2, 1))
  for (j in seq_along(eff_names)) {
    hist(bobj$t[, j], main = paste0(eff_names[j], " (interaction)"),
         xlab = "bootstrap estimate", col = "gray80", border = "white")
    abline(v = eff_hat_int[eff_names[j]], lwd = 2)
    abline(v = eff_true[eff_names[j]], lwd = 2, lty = 2)
    legend("topright", legend = c("Estimate", "Truth"), lty = c(1,2), bty = "n", cex = 0.8)
  }
  dev.off()

  png(file.path(DIR_FIGURES, "fig_bootstrap_hist_jobs2_additive.png"), width = 1600, height = 900, res = 160)
  par(mfrow = c(2, 3), mar = c(4, 4, 2, 1))
  for (j in seq_along(eff_names)) {
    jj <- length(eff_names) + j
    hist(bobj$t[, jj], main = paste0(eff_names[j], " (additive)"),
         xlab = "bootstrap estimate", col = "gray80", border = "white")
    abline(v = eff_hat_add[eff_names[j]], lwd = 2)
    abline(v = eff_true[eff_names[j]], lwd = 2, lty = 2)
    legend("topright", legend = c("Estimate", "Truth"), lty = c(1,2), bty = "n", cex = 0.8)
  }
  dev.off()
}

# ------------------------------------------------------------
# 12) Minimal run summary
# ------------------------------------------------------------
message("\n--- DONE ---")
files_created <- c(
  list.files(DIR_TABLES,  full.names = TRUE),
  list.files(DIR_FIGURES, full.names = TRUE),
  list.files(DIR_DAGS,    full.names = TRUE),
  file.path(OUT_ROOT, "jobs2_semisynthetic_data.csv"),
  file.path(OUT_ROOT, "sessionInfo_jobs2_semisynth.txt")
)
files_created <- files_created[file.exists(files_created)]
message("Files created (", length(files_created), "):")
for (f in files_created) message("  - ", f)
