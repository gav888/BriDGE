# ============================================================
# BriDGE (v6) — Cooldown/Throttled Edition
# DAG + GAM + Interventional Mediation
# -----------------------------------------------------------
# Extends revised4 with all simulation-addressable reviewer concerns:
#   * Multiple mediator scenarios: p=2, p=3 (chain), p=5 (mixed)
#   * Traditional mediation comparison (Imai + Sobel)
#   * Benchmarking grid (N x p x B scaling)
#   * Power analysis grid
#   * Discretization sensitivity (promoted from annex)
#   * Measurement error sensitivity
#   * Unmeasured confounding sensitivity
#   * DAG contradiction diagnostics (multi-algorithm triangulation)
#   * Bootstrap replicate comparison (B=500/1000/2000)
#   * BCa bootstrap CIs (upgraded from percentile)
#   * Bootstrap R default increased to 2000
# ============================================================

# ==========================================================================
# PHASE 0: Packages, Platform Detection, Parallel Cluster
# ==========================================================================

req_pkgs <- c("bnlearn", "mgcv", "ggplot2", "dplyr", "igraph", "boot",
              "parallel", "mediation", "gridExtra")
to_install <- setdiff(req_pkgs, rownames(installed.packages()))
if (length(to_install)) install.packages(to_install, dependencies = TRUE)
invisible(lapply(req_pkgs, library, character.only = TRUE))

USE_RGRAPHVIZ <- requireNamespace("Rgraphviz", quietly = TRUE)
TABLES_DIR  <- "tables";  if (!dir.exists(TABLES_DIR)) dir.create(TABLES_DIR, recursive = TRUE)
FIGURES_DIR <- "figures"; if (!dir.exists(FIGURES_DIR)) dir.create(FIGURES_DIR, recursive = TRUE)
DAG_PNG_DIR <- "dag_outputs"; if (!dir.exists(DAG_PNG_DIR)) dir.create(DAG_PNG_DIR, recursive = TRUE)

# Clean output directories — remove stale files from prior script versions.
# revised6 uses scenario-suffixed names (_p2, _p3_chain, _p5_mixed).
.clean_stale <- function() {
  stale_figs <- c("discovered_dag.png", "researcher_dag.png",
                  "dagcomparison.png", "bootstrap_histograms.png", "gams.png")
  stale_tabs <- c("dag_comparison.tex", "mediation_effects.tex",
                  "traditional_comparison.tex", "gam_sensitivity.tex",
                  "scaling_mediators.tex", "bootstrap_arc_strength.rds",
                  "discovered_averaged_network.rds")
  for (f in stale_figs) {
    fp <- file.path(FIGURES_DIR, f); if (file.exists(fp)) unlink(fp)
    fp <- file.path(DAG_PNG_DIR, f); if (file.exists(fp)) unlink(fp)
  }
  for (f in stale_tabs) {
    fp <- file.path(TABLES_DIR, f); if (file.exists(fp)) unlink(fp)
  }
}
.clean_stale()

# ---- Apple Silicon / parallel backend ----
IS_MACOS   <- Sys.info()["sysname"] == "Darwin"
IS_ARM64   <- Sys.info()["machine"] == "arm64"
IS_APPLE_SILICON <- IS_MACOS && IS_ARM64
IS_LINUX   <- Sys.info()["sysname"] == "Linux"

TOTAL_CORES <- parallel::detectCores(logical = TRUE)
N_WORKERS   <- max(2L, TOTAL_CORES - 2L)
env_cores   <- Sys.getenv("BRIDGE_NCORES", unset = "")
if (nzchar(env_cores)) {
  uc <- suppressWarnings(as.integer(env_cores))
  if (!is.na(uc) && uc >= 1L) N_WORKERS <- uc
}
if (IS_APPLE_SILICON) Sys.setenv(VECLIB_MAXIMUM_THREADS = "1")
Sys.setenv(OPENBLAS_NUM_THREADS = "1", MKL_NUM_THREADS = "1", OMP_NUM_THREADS = "1")

PARALLEL_BACKEND <- if (IS_LINUX) "fork" else "psock"

.make_cluster <- function(n_workers, type = PARALLEL_BACKEND) {
  if (type == "fork") {
    cl <- parallel::makeForkCluster(n_workers)
  } else {
    cl <- parallel::makeCluster(n_workers, type = "PSOCK")
    parallel::clusterEvalQ(cl, {
      suppressPackageStartupMessages({
        library(bnlearn); library(mgcv); library(dplyr); library(boot); library(mediation)
      })
      Sys.setenv(VECLIB_MAXIMUM_THREADS = "1", OPENBLAS_NUM_THREADS = "1",
                 MKL_NUM_THREADS = "1", OMP_NUM_THREADS = "1")
      options(mc.cores = 1L)
    })
  }
  cl
}

CL <- .make_cluster(N_WORKERS)
on.exit(try(parallel::stopCluster(CL), silent = TRUE), add = TRUE)

cl_export <- function(cl, var_names, envir = parent.frame()) {
  parallel::clusterExport(cl, var_names, envir = envir)
  invisible(NULL)
}

timer_start   <- function() proc.time()
timer_elapsed  <- function(t0) { dt <- proc.time() - t0; sprintf("%.1fs", dt[3]) }
timer_seconds  <- function(t0) { dt <- proc.time() - t0; as.numeric(dt[3]) }

cat("============================================================\n")
cat("BriDGE v6 — Cooldown/Throttled Edition\n")
cat("============================================================\n")
cat(sprintf("Platform        : %s %s\n", Sys.info()["sysname"], Sys.info()["machine"]))
cat(sprintf("Apple Silicon   : %s\n", ifelse(IS_APPLE_SILICON, "YES", "No")))
cat(sprintf("Parallel backend: %s | Workers: %d / %d cores\n",
            toupper(PARALLEL_BACKEND), N_WORKERS, TOTAL_CORES))
cat("============================================================\n\n")

# ---- Controls ----
SCENARIO    <- "all"        # "all", "p2_only", "benchmark", "power"
SEED_MAIN   <- 20240115
BOOT_R      <- 2000         # reviewer concern G: increased from 250
MC_DRAWS    <- 200          # reviewer concern G: increased from 50
LEVELS01    <- c("0", "1")

# Fast mode for debugging: reduce bootstrap/MC drastically
FAST_MODE <- as.logical(Sys.getenv("BRIDGE_FAST", unset = "FALSE"))
if (FAST_MODE) { BOOT_R <- 50; MC_DRAWS <- 10; cat("[FAST_MODE] BOOT_R=50, MC_DRAWS=10\n\n") }

# ---- Cooldown / throttling controls ----
COOLDOWN_SEC   <- suppressWarnings(as.numeric(Sys.getenv("BRIDGE_COOLDOWN_SEC", unset = "5")))
COOLDOWN_EVERY <- suppressWarnings(as.integer(Sys.getenv("BRIDGE_COOLDOWN_EVERY", unset = "200")))
COOLDOWN_ENABLED <- is.finite(COOLDOWN_SEC) && !is.na(COOLDOWN_EVERY) &&
  COOLDOWN_SEC > 0 && COOLDOWN_EVERY > 0

cooldown_pause <- function(reason = "cooldown") {
  if (COOLDOWN_ENABLED) {
    cat(sprintf("[Cooldown] %s: sleeping %ds\n", reason, COOLDOWN_SEC))
    Sys.sleep(COOLDOWN_SEC)
  }
}

if (COOLDOWN_ENABLED) {
  cat(sprintf("[Cooldown] Enabled: every %d boot reps, %ds sleep\n\n", COOLDOWN_EVERY, COOLDOWN_SEC))
} else {
  cat("[Cooldown] Disabled (set BRIDGE_COOLDOWN_SEC and BRIDGE_COOLDOWN_EVERY to enable)\n\n")
}

# ==========================================================================
# PHASE 1: DGP Definitions + Generalised Utility Functions
# ==========================================================================

# ---- 1a) Generalised white/blacklist builders ----
# Whitelist: force treatment -> each mediator and treatment -> outcome
#   (RCT design knowledge: treatment precedes all downstream variables).
.make_whitelist_gen <- function(mediator_names) {
  data.frame(from = rep("treatment", length(mediator_names) + 1L),
             to   = c(mediator_names, "outcome"),
             stringsAsFactors = FALSE)
}
# Blacklist: forbid any reverse edges into treatment (exogenous by design)
#   AND forbid outcome -> mediator edges (temporal ordering: mediators precede outcome).
.make_blacklist_gen <- function(mediator_names) {
  bl <- data.frame(from = c(mediator_names, "outcome"),
                   to   = rep("treatment", length(mediator_names) + 1L),
                   stringsAsFactors = FALSE)
  # Also forbid outcome -> mediator_j (temporal ordering)
  bl_om <- data.frame(from = rep("outcome", length(mediator_names)),
                      to   = mediator_names,
                      stringsAsFactors = FALSE)
  rbind(bl, bl_om)
}

.score_args <- function(score = c("bde", "bic"), iss = 10) {
  score <- match.arg(score)
  if (score == "bde") list(score = "bde", iss = iss) else list(score = "bic")
}

has_arc <- function(bn, from, to) {
  a <- bnlearn::arcs(bn)
  any(a[, 1] == from & a[, 2] == to)
}

# ---- 1b) DGP: p=2 mediators (baseline, from revised4) ----
# DGP DELIBERATELY includes a mediator-mediator edge (M1 -> M2, coeff 0.4)
# that the researcher DAG omits.  This creates an informative discrepancy
# between the researcher's parallel-mediator hypothesis and the discovered
# structure, which is the whole point of the discovery step.
generate_data_p2 <- function(n = 2000, alpha = 0.8, seed = SEED_MAIN) {
  set.seed(seed)
  treatment  <- rbinom(n, 1, 0.5)
  mediator_1 <- 1.0 * treatment + rnorm(n) + alpha * sin(rnorm(n))
  mediator_2 <- 0.8 * treatment + 0.4 * mediator_1 + rnorm(n) + alpha * exp(rnorm(n) / 2)
  outcome    <- 1.5 * treatment +
    1.2 * mediator_1 + 0.9 * mediator_2 +
    0.5 * (mediator_1^2) + 0.3 * mediator_1 * mediator_2 +
    0.5 * treatment * mediator_1 + 0.7 * treatment * mediator_2 +
    rnorm(n)
  d <- data.frame(treatment = factor(treatment, levels = c(0, 1)),
                  mediator_1 = mediator_1, mediator_2 = mediator_2,
                  outcome = outcome)
  # Researcher DAG: assumes parallel mediators (no M1->M2 edge)
  dag_str <- "[treatment][mediator_1|treatment][mediator_2|treatment][outcome|treatment:mediator_1:mediator_2]"
  list(data = d, mediator_names = c("mediator_1", "mediator_2"),
       researcher_dag = bnlearn::model2network(dag_str),
       label = "p=2 parallel")
}

# ---- 1c) DGP: p=3 sequential chain ----
generate_data_p3_chain <- function(n = 2000, alpha = 0.8, seed = SEED_MAIN + 100) {
  set.seed(seed)
  treatment  <- rbinom(n, 1, 0.5)
  mediator_1 <- 0.9 * treatment + rnorm(n) + alpha * sin(rnorm(n))
  mediator_2 <- 0.4 * treatment + 0.6 * mediator_1 + rnorm(n)
  mediator_3 <- 0.3 * treatment + 0.5 * mediator_2 + rnorm(n)
  outcome    <- 1.0 * treatment +
    0.8 * mediator_1 + 0.6 * mediator_2 + 0.5 * mediator_3 +
    0.3 * mediator_1 * mediator_2 +
    0.4 * treatment * mediator_1 +
    rnorm(n)
  d <- data.frame(treatment = factor(treatment, levels = c(0, 1)),
                  mediator_1 = mediator_1, mediator_2 = mediator_2,
                  mediator_3 = mediator_3, outcome = outcome)
  dag_str <- paste0("[treatment][mediator_1|treatment][mediator_2|treatment:mediator_1]",
                    "[mediator_3|treatment:mediator_2][outcome|treatment:mediator_1:mediator_2:mediator_3]")
  list(data = d, mediator_names = c("mediator_1", "mediator_2", "mediator_3"),
       researcher_dag = bnlearn::model2network(dag_str),
       label = "p=3 sequential chain")
}

# ---- 1d) DGP: p=5 mixed (true + null mediators) ----
generate_data_p5_mixed <- function(n = 2000, alpha = 0.8, seed = SEED_MAIN + 200) {
  set.seed(seed)
  treatment  <- rbinom(n, 1, 0.5)
  mediator_1 <- 1.0 * treatment + rnorm(n) + alpha * sin(rnorm(n))   # strong
  mediator_2 <- 0.6 * treatment + rnorm(n)                           # moderate
  mediator_3 <- 0.3 * treatment + rnorm(n)                           # weak
  mediator_4 <- rnorm(n)                                              # null
  mediator_5 <- rnorm(n)                                              # null
  outcome    <- 1.5 * treatment +
    1.2 * mediator_1 + 0.7 * mediator_2 + 0.3 * mediator_3 +
    0.0 * mediator_4 + 0.0 * mediator_5 +
    0.3 * mediator_1 * mediator_2 +
    0.4 * treatment * mediator_1 +
    rnorm(n)
  d <- data.frame(treatment = factor(treatment, levels = c(0, 1)),
                  mediator_1 = mediator_1, mediator_2 = mediator_2,
                  mediator_3 = mediator_3, mediator_4 = mediator_4,
                  mediator_5 = mediator_5, outcome = outcome)
  dag_str <- paste0("[treatment]",
                    "[mediator_1|treatment][mediator_2|treatment][mediator_3|treatment]",
                    "[mediator_4|treatment][mediator_5|treatment]",
                    "[outcome|treatment:mediator_1:mediator_2:mediator_3:mediator_4:mediator_5]")
  list(data = d, mediator_names = paste0("mediator_", 1:5),
       researcher_dag = bnlearn::model2network(dag_str),
       label = "p=5 mixed (3 true + 2 null)")
}

# ---- 1e) Generalised GAM formula builder ----
build_gam_formula <- function(mediator_names) {
  # Main smooth per mediator + by-treatment smooth
  main_terms <- paste0("s(", mediator_names, ', bs = "tp")', collapse = " + ")
  by_terms   <- paste0("s(", mediator_names, ', bs = "tp", by = treatment)', collapse = " + ")
  # Pairwise tensor interactions (skip if >10 pairs for tractability)
  ti_terms <- ""
  if (length(mediator_names) >= 2 && length(mediator_names) <= 5) {
    pairs <- combn(mediator_names, 2, simplify = FALSE)
    if (length(pairs) <= 10) {
      ti_terms <- paste0("ti(", sapply(pairs, paste, collapse = ", "), ', bs = "tp")',
                         collapse = " + ")
    }
  }
  fml_str <- paste("outcome ~ treatment", main_terms, by_terms, sep = " + ")
  if (nzchar(ti_terms)) fml_str <- paste(fml_str, ti_terms, sep = " + ")
  as.formula(fml_str)
}

fit_outcome_gam_gen <- function(data, mediator_names, use_bam = FALSE) {
  fml <- build_gam_formula(mediator_names)
  if (use_bam) {
    mgcv::bam(fml, data = data, family = gaussian(), method = "fREML",
              discrete = TRUE, select = TRUE)
  } else {
    mgcv::gam(fml, data = data, family = gaussian(), method = "REML", select = TRUE)
  }
}

# ---- 1f) Generalised discovery (parallel bootstrap strength) ----
run_discovery_gen <- function(dat, mediator_names,
                              bins = 5, alpha = 0.05,
                              score = "bde", iss = 10,
                              strength_R = 200, avg_threshold = 0.85,
                              seed = 1, use_parallel = TRUE) {
  set.seed(seed)
  dd <- as.data.frame(dat)
  cont_vars <- c(mediator_names, "outcome")
  dd[cont_vars] <- bnlearn::discretize(dd[cont_vars], method = "quantile", breaks = bins)

  WL <- .make_whitelist_gen(mediator_names)
  BL <- .make_blacklist_gen(mediator_names)

  fit <- bnlearn::mmhc(dd, whitelist = WL, blacklist = BL,
                        restrict.args = list(test = "mi", alpha = alpha),
                        maximize.args = .score_args(score, iss))

  if (use_parallel && N_WORKERS > 1L) {
    n_chunks <- min(N_WORKERS, strength_R)
    chunks <- split(seq_len(strength_R), cut(seq_len(strength_R), n_chunks, labels = FALSE))
    cl_export(CL, c("dd", "WL", "BL", "alpha", "score", "iss", "seed", "chunks"),
              envir = environment())
    chunk_results <- parallel::parLapply(CL, seq_along(chunks), function(i) {
      chunk_R <- length(chunks[[i]])
      set.seed(seed + i * 1000L)
      bnlearn::boot.strength(dd, R = chunk_R, algorithm = "mmhc",
                             algorithm.args = list(whitelist = WL, blacklist = BL,
                                                   restrict.args = list(test = "mi", alpha = alpha),
                                                   maximize.args = if (score == "bde") list(score = "bde", iss = iss) else list(score = "bic")))
    })
    combined <- chunk_results[[1]]
    chunk_sizes <- sapply(chunks, length)
    for (col in c("strength", "direction")) {
      ws <- combined[[col]] * chunk_sizes[1]
      for (j in 2:length(chunk_results)) ws <- ws + chunk_results[[j]][[col]] * chunk_sizes[j]
      combined[[col]] <- ws / sum(chunk_sizes)
    }
    attr(combined, "nodes") <- attr(chunk_results[[1]], "nodes")
    strn <- combined
  } else {
    strn <- bnlearn::boot.strength(dd, R = strength_R, algorithm = "mmhc",
                                   algorithm.args = list(whitelist = WL, blacklist = BL,
                                                         restrict.args = list(test = "mi", alpha = alpha),
                                                         maximize.args = .score_args(score, iss)))
  }
  avg <- bnlearn::averaged.network(strn, threshold = avg_threshold)
  list(fit = fit, strength = strn, averaged = avg,
       bins = bins, alpha = alpha, score = score)
}

# ---- 1g) Generalised interventional mediation effects ----
interventional_effects_gen <- function(data, mediator_names,
                                       B_mc = MC_DRAWS, use_bam = TRUE) {
  y_fit <- fit_outcome_gam_gen(data, mediator_names, use_bam = use_bam)
  n <- nrow(data)
  p <- length(mediator_names)
  pool0 <- data[data$treatment == "0", mediator_names, drop = FALSE]
  pool1 <- data[data$treatment == "1", mediator_names, drop = FALSE]
  if (nrow(pool0) == 0) pool0 <- data[, mediator_names, drop = FALSE]
  if (nrow(pool1) == 0) pool1 <- data[, mediator_names, drop = FALSE]

  samp <- function(pool, n) pool[sample.int(nrow(pool), n, replace = TRUE), , drop = FALSE]

  make_newdata <- function(treat_val, med_df) {
    nd <- data.frame(treatment = factor(rep(treat_val, n), levels = LEVELS01))
    for (m in mediator_names) nd[[m]] <- med_df[[m]]
    nd
  }

  est <- replicate(B_mc, {
    M0 <- samp(pool0, n); M1 <- samp(pool1, n)
    y00_m0 <- predict(y_fit, newdata = make_newdata("0", M0))
    y10_m0 <- predict(y_fit, newdata = make_newdata("1", M0))
    y01_m1 <- predict(y_fit, newdata = make_newdata("0", M1))
    y11_m1 <- predict(y_fit, newdata = make_newdata("1", M1))

    res <- c(ide = mean(y10_m0 - y00_m0),
             jiie = mean(y01_m1 - y00_m0),
             te = mean(y11_m1 - y00_m0))

    # Individual IIE per mediator: shift mediator k to treated, rest at control
    for (k in seq_along(mediator_names)) {
      M_mix <- M0
      M_mix[[mediator_names[k]]] <- M1[[mediator_names[k]]]
      y_mix <- predict(y_fit, newdata = make_newdata("0", M_mix))
      res[paste0("iie_", mediator_names[k])] <- mean(y_mix - y00_m0)
    }
    res
  })

  rowMeans(est)
}

# ---- 1h) Generalised bootstrap for mediation effects ----
boot_mediation_gen <- function(data, mediator_names,
                               B_boot = BOOT_R, B_mc = MC_DRAWS) {
  n <- nrow(data)
  set.seed(1234)
  boot_idx <- lapply(seq_len(B_boot), function(i) sample.int(n, n, replace = TRUE))

  cl_export(CL, c("data", "boot_idx", "mediator_names", "B_mc",
                   "fit_outcome_gam_gen", "build_gam_formula",
                   "interventional_effects_gen", "LEVELS01", "MC_DRAWS"),
            envir = environment())

  results <- vector("list", length = B_boot)
  batch_size <- if (COOLDOWN_ENABLED) min(COOLDOWN_EVERY, B_boot) else B_boot
  batch_starts <- seq(1L, B_boot, by = batch_size)
  n_batches <- length(batch_starts)

  for (b in seq_along(batch_starts)) {
    i_start <- batch_starts[b]
    i_end <- min(i_start + batch_size - 1L, B_boot)
    idx <- i_start:i_end

    batch_res <- parallel::parLapply(CL, idx, function(i) {
      tryCatch({
        d <- data[boot_idx[[i]], ]
        interventional_effects_gen(d, mediator_names, B_mc = B_mc, use_bam = TRUE)
      }, error = function(e) {
        out <- rep(NA_real_, 3L + length(mediator_names))
        names(out) <- c("ide", "jiie", "te", paste0("iie_", mediator_names))
        out
      })
    })

    results[idx] <- batch_res
    if (b < n_batches) cooldown_pause(sprintf("bootstrap batch %d/%d", b, n_batches))
  }

  boot_mat <- do.call(rbind, results)
  n_failed <- sum(apply(boot_mat, 1, function(x) any(is.na(x))))
  if (n_failed > 0) warning(sprintf("%d/%d bootstrap replicates returned NA.", n_failed, B_boot))

  list(matrix = boot_mat, n_failed = n_failed, B = B_boot)
}

# ---- 1i) Skeleton metrics ----
skeleton_metrics <- function(bn_est, bn_true) {
  A_est  <- bnlearn::amat(bn_est);  S_est  <- A_est + t(A_est); S_est[S_est > 0] <- 1; diag(S_est) <- 0
  A_true <- bnlearn::amat(bn_true); S_true <- A_true + t(A_true); S_true[S_true > 0] <- 1; diag(S_true) <- 0
  U <- upper.tri(S_true)
  TP <- sum(S_est[U] == 1 & S_true[U] == 1)
  FP <- sum(S_est[U] == 1 & S_true[U] == 0)
  FN <- sum(S_est[U] == 0 & S_true[U] == 1)
  prec <- ifelse((TP + FP) > 0, TP / (TP + FP), NA)
  rec  <- ifelse((TP + FN) > 0, TP / (TP + FN), NA)
  f1   <- ifelse(is.na(prec) | is.na(rec) | (prec + rec) == 0, NA, 2 * prec * rec / (prec + rec))
  list(precision = prec, recall = rec, f1 = f1, TP = TP, FP = FP, FN = FN)
}

# ---- 1j) BCa bootstrap confidence intervals ----
# Computes bias-corrected and accelerated (BCa) CIs from bootstrap matrix.
# Uses an efficient approach:
#   z0 (bias correction): computed from proportion of boot < point_est (free)
#   a  (acceleration):    estimated via jackknife-after-bootstrap on the
#                          *already-fitted* GAM (only prediction, no re-fitting),
#                          using delete-d groups for speed.
# boot_mat: B x K matrix, point_est: named vector, data: original data
bca_ci <- function(boot_mat, point_est, data, mediator_names,
                   alpha = 0.05) {
  K <- ncol(boot_mat)
  eff_names <- colnames(boot_mat)
  ci_lo <- ci_hi <- numeric(K)
  names(ci_lo) <- names(ci_hi) <- eff_names

  z_alpha  <- qnorm(alpha / 2)
  z_1alpha <- qnorm(1 - alpha / 2)
  n <- nrow(data)

  # --- Fast acceleration via delete-group jackknife on predictions ---
  # Fit GAM once on full data, then compute leave-group-out influence
  y_fit <- fit_outcome_gam_gen(data, mediator_names, use_bam = TRUE)
  pool0 <- data[data$treatment == "0", mediator_names, drop = FALSE]
  pool1 <- data[data$treatment == "1", mediator_names, drop = FALSE]
  if (nrow(pool0) == 0) pool0 <- data[, mediator_names, drop = FALSE]
  if (nrow(pool1) == 0) pool1 <- data[, mediator_names, drop = FALSE]

  n_groups <- min(50L, n)  # 50 jackknife groups (fast)
  group_ids <- cut(seq_len(n), breaks = n_groups, labels = FALSE)
  set.seed(99)

  # For each group, compute influence on mediation effects via leave-group-out
  jack_mat <- matrix(NA_real_, nrow = n_groups, ncol = K)
  colnames(jack_mat) <- eff_names
  for (g in seq_len(n_groups)) {
    idx_out <- which(group_ids == g)
    d_g <- data[-idx_out, ]
    tryCatch({
      pool0_g <- d_g[d_g$treatment == "0", mediator_names, drop = FALSE]
      pool1_g <- d_g[d_g$treatment == "1", mediator_names, drop = FALSE]
      if (nrow(pool0_g) == 0) pool0_g <- d_g[, mediator_names, drop = FALSE]
      if (nrow(pool1_g) == 0) pool1_g <- d_g[, mediator_names, drop = FALSE]
      n_g <- nrow(d_g)
      samp <- function(pool, sz) pool[sample.int(nrow(pool), sz, replace = TRUE), , drop = FALSE]
      make_nd <- function(tv, mdf) {
        nd <- data.frame(treatment = factor(rep(tv, n_g), levels = LEVELS01))
        for (m in mediator_names) nd[[m]] <- mdf[[m]]
        nd
      }
      # Single MC draw (fast influence estimate)
      M0 <- samp(pool0_g, n_g); M1 <- samp(pool1_g, n_g)
      y00 <- predict(y_fit, make_nd("0", M0))
      y10 <- predict(y_fit, make_nd("1", M0))
      y01 <- predict(y_fit, make_nd("0", M1))
      y11 <- predict(y_fit, make_nd("1", M1))
      jack_mat[g, "ide"]  <- mean(y10 - y00)
      jack_mat[g, "jiie"] <- mean(y01 - y00)
      jack_mat[g, "te"]   <- mean(y11 - y00)
      for (mk in seq_along(mediator_names)) {
        M_mix <- M0
        M_mix[[mediator_names[mk]]] <- M1[[mediator_names[mk]]]
        y_mix <- predict(y_fit, make_nd("0", M_mix))
        jack_mat[g, paste0("iie_", mediator_names[mk])] <- mean(y_mix - y00)
      }
    }, error = function(e) NULL)
  }

  for (k in seq_len(K)) {
    boot_k <- boot_mat[, k]
    boot_k <- boot_k[!is.na(boot_k)]
    B_ok <- length(boot_k)
    if (B_ok < 10) { ci_lo[k] <- NA; ci_hi[k] <- NA; next }

    theta0 <- point_est[eff_names[k]]

    # z0: bias correction (from bootstrap)
    prop_below <- mean(boot_k < theta0)
    prop_below <- max(1 / (2 * B_ok), min(1 - 1 / (2 * B_ok), prop_below))
    z0 <- qnorm(prop_below)

    # a: acceleration (from jackknife)
    jk <- jack_mat[, k]
    jk <- jk[!is.na(jk)]
    if (length(jk) >= 5) {
      jk_mean <- mean(jk)
      jk_diff <- jk_mean - jk
      denom <- sum(jk_diff^2)
      a_hat <- if (denom > 0) sum(jk_diff^3) / (6 * denom^1.5) else 0
      if (is.na(a_hat) || is.infinite(a_hat)) a_hat <- 0
    } else {
      a_hat <- 0  # fall back to BC (no acceleration)
    }

    # BCa adjusted quantiles
    alpha1 <- pnorm(z0 + (z0 + z_alpha)  / (1 - a_hat * (z0 + z_alpha)))
    alpha2 <- pnorm(z0 + (z0 + z_1alpha) / (1 - a_hat * (z0 + z_1alpha)))
    alpha1 <- max(0.5 / B_ok, min(1 - 0.5 / B_ok, alpha1))
    alpha2 <- max(0.5 / B_ok, min(1 - 0.5 / B_ok, alpha2))

    ci_lo[k] <- quantile(boot_k, alpha1)
    ci_hi[k] <- quantile(boot_k, alpha2)
  }
  data.frame(Effect = eff_names, CI_lo_bca = ci_lo, CI_hi_bca = ci_hi,
             row.names = NULL)
}

# ---- 1k) LaTeX table writer helper ----
write_latex_table <- function(df, file, caption = "", label = "") {
  # Emit only the tabular environment (no table wrapper, caption, or label).
  # The manuscript provides its own \begin{table}, \caption, and \label.
  .esc <- function(s) gsub("_", "\\\\_", s)   # escape underscores for LaTeX
  path <- file.path(TABLES_DIR, file)
  lines <- sprintf("\\begin{tabular}{%s}", paste(rep("l", ncol(df)), collapse = ""))
  lines <- c(lines, "\\hline",
             paste(.esc(names(df)), collapse = " & "), "\\\\", "\\hline")
  for (i in seq_len(nrow(df))) {
    vals <- sapply(df[i, ], function(x) if (is.numeric(x)) sprintf("%.3f", x) else .esc(as.character(x)))
    lines <- c(lines, paste(vals, collapse = " & "), "\\\\")
  }
  lines <- c(lines, "\\hline", "\\end{tabular}")
  writeLines(lines, path)
  cat(sprintf("  Saved: %s\n", path))
}

# ---- 1k) DAG saving helper ----
bn_to_igraph <- function(bn) {
  igraph::graph_from_adjacency_matrix(bnlearn::amat(bn), mode = "directed")
}

# Abbreviated node labels for cleaner DAG display
.dag_short_labels <- function(node_names) {
  sapply(node_names, function(x) {
    if (x == "treatment") return("T")
    if (x == "outcome")   return("Y")
    if (grepl("^mediator_", x)) return(paste0("M", sub("mediator_", "", x)))
    x
  }, USE.NAMES = FALSE)
}

# Custom layered layout: detects chain vs parallel mediator structures
.dag_manual_layout <- function(g) {
  vnames <- igraph::V(g)$name
  n_v <- length(vnames)
  layer <- rep(1L, n_v)
  names(layer) <- vnames

  med_names <- setdiff(vnames, c("treatment", "outcome"))
  adj <- as.matrix(igraph::as_adjacency_matrix(g, sparse = FALSE))

  # Check for inter-mediator edges (chain structure)
  has_chain <- FALSE
  for (m1 in med_names) for (m2 in med_names) {
    if (m1 != m2 && adj[m1, m2] == 1) { has_chain <- TRUE; break }
    if (has_chain) break
  }

  if (has_chain && length(med_names) > 1) {
    # Use distance from treatment to assign depth layers
    dists <- igraph::distances(g, v = "treatment", mode = "out")
    med_depths <- dists[1, med_names]
    med_depths[!is.finite(med_depths)] <- 1
    unique_d <- sort(unique(med_depths))
    layer["treatment"] <- 0
    for (i in seq_along(unique_d)) {
      for (m in med_names[med_depths == unique_d[i]]) layer[m] <- i
    }
    layer["outcome"] <- length(unique_d) + 1
  } else {
    layer["treatment"] <- 0
    layer[med_names] <- 1
    layer["outcome"] <- 2
  }

  max_layer <- max(layer)
  coords <- matrix(0, nrow = n_v, ncol = 2)
  for (lyr in 0:max_layer) {
    nodes_in <- which(layer == lyr)
    n_in <- length(nodes_in)
    coords[nodes_in, 1] <- if (n_in == 1) 0 else seq(-1, 1, length.out = n_in)
    coords[nodes_in, 2] <- -lyr / max(max_layer, 1)
  }
  coords
}

save_dag_png <- function(bn, file, title, fill = "lightgray") {
  file <- file.path(DAG_PNG_DIR, file)
  g <- bn_to_igraph(bn)
  n_v <- length(igraph::V(g))

  # Dynamic sizing for more nodes
  w <- max(1200, 200 * n_v)
  vsize     <- ifelse(n_v <= 4, 28, ifelse(n_v <= 6, 22, 18))
  lab_cex   <- ifelse(n_v <= 4, 1.0, ifelse(n_v <= 6, 0.85, 0.75))
  arrow_sz  <- ifelse(n_v <= 4, 0.6, 0.45)
  short_lab <- .dag_short_labels(igraph::V(g)$name)
  lay <- .dag_manual_layout(g)

  png(file, width = w, height = 900, res = 150)
  par(mar = c(3, 1, 3, 1))
  plot(g, main = title,
       vertex.size       = vsize,
       vertex.color      = fill,
       vertex.label      = short_lab,
       vertex.label.cex  = lab_cex,
       vertex.label.color = "black",
       vertex.frame.color = "gray40",
       edge.arrow.size   = arrow_sz,
       edge.color        = "gray40",
       edge.width        = 1.5,
       layout            = lay)
  # Legend mapping abbreviations to full names
  med_names <- setdiff(igraph::V(g)$name, c("treatment", "outcome"))
  if (length(med_names) > 0) {
    leg <- c("T = treatment",
             paste0("M", sub("mediator_", "", med_names), " = ", med_names),
             "Y = outcome")
    legend("bottomright", legend = leg, cex = 0.55, bty = "n", text.col = "gray30")
  }
  dev.off()
}

# ==========================================================================
# PHASE 2: Main Analysis Loop (per scenario)
# ==========================================================================
cat("\n########## PHASE 2: Main Analysis Loop ##########\n")
t0_phase2 <- timer_start()

run_scenario <- function(dgp, scenario_tag) {
  cat(sprintf("\n===== Scenario: %s =====\n", dgp$label))
  t0 <- timer_start()

  d <- dgp$data
  meds <- dgp$mediator_names
  rdag <- dgp$researcher_dag

  # 2a) Discovery
  cat("  [Discovery] ...")
  disc <- run_discovery_gen(d, meds, bins = 5, alpha = 0.05, score = "bde",
                            iss = 10, strength_R = 200, avg_threshold = 0.85,
                            seed = 1, use_parallel = TRUE)
  shd <- bnlearn::shd(disc$averaged, rdag)
  skm <- skeleton_metrics(disc$averaged, rdag)
  cat(sprintf(" SHD=%d, F1=%.3f\n", shd, ifelse(is.na(skm$f1), 0, skm$f1)))

  # DAG comparison table
  edge_df <- function(bn) as.data.frame(bnlearn::arcs(bn), stringsAsFactors = FALSE)
  e_r <- edge_df(rdag); e_d <- edge_df(disc$averaged)
  colnames(e_r) <- colnames(e_d) <- c("from", "to")
  cmp <- merge(e_r, e_d, by = c("from", "to"), all = TRUE)
  cmp$Researcher <- paste0(cmp$from, "->", cmp$to) %in% paste0(e_r$from, "->", e_r$to)
  cmp$Discovered <- paste0(cmp$from, "->", cmp$to) %in% paste0(e_d$from, "->", e_d$to)

  write_latex_table(cmp, sprintf("dag_comparison_%s.tex", scenario_tag),
                    sprintf("DAG Comparison (%s)", dgp$label),
                    sprintf("dag_comparison_%s", scenario_tag))

  # Save DAGs
  save_dag_png(rdag, sprintf("researcher_dag_%s.png", scenario_tag),
               sprintf("Researcher DAG (%s)", dgp$label), "lightblue")
  save_dag_png(disc$averaged, sprintf("discovered_dag_%s.png", scenario_tag),
               sprintf("Discovered DAG (%s)", dgp$label), "lightgreen")

  # 2b) GAM
  cat("  [GAM] ...")
  gam_fit <- fit_outcome_gam_gen(d, meds, use_bam = FALSE)
  cat(" done\n")

  # 2c) Point estimates
  cat("  [Mediation] ...")
  ie_point <- interventional_effects_gen(d, meds, B_mc = MC_DRAWS, use_bam = TRUE)
  cat(sprintf(" IDE=%.3f, JIIE=%.3f, TE=%.3f\n", ie_point["ide"], ie_point["jiie"], ie_point["te"]))

  # 2d) Bootstrap CIs
  cat(sprintf("  [Bootstrap] R=%d ...", BOOT_R))
  bres <- boot_mediation_gen(d, meds, B_boot = BOOT_R, B_mc = MC_DRAWS)
  cat(sprintf(" done (%d failed)\n", bres$n_failed))

  # Compute CIs: percentile + BCa (reviewer concern G)
  ci_table <- data.frame(Effect = names(ie_point), Estimate = as.numeric(ie_point))
  ci_lo_pct <- apply(bres$matrix, 2, quantile, probs = 0.025, na.rm = TRUE)
  ci_hi_pct <- apply(bres$matrix, 2, quantile, probs = 0.975, na.rm = TRUE)
  ci_table$CI_lo_pct <- ci_lo_pct; ci_table$CI_hi_pct <- ci_hi_pct

  # BCa CIs
  cat("  [BCa CIs] ...")
  bca <- tryCatch(
    bca_ci(bres$matrix, ie_point, d, meds),
    error = function(e) {
      cat(sprintf(" BCa failed (%s), using percentile\n", e$message))
      data.frame(Effect = names(ie_point),
                 CI_lo_bca = ci_lo_pct, CI_hi_bca = ci_hi_pct)
    }
  )
  ci_table$CI_lo_bca <- bca$CI_lo_bca
  ci_table$CI_hi_bca <- bca$CI_hi_bca
  # Use BCa as the primary CI (fallback to percentile if BCa is NA)
  ci_table$CI_lo <- ifelse(is.na(ci_table$CI_lo_bca), ci_table$CI_lo_pct, ci_table$CI_lo_bca)
  ci_table$CI_hi <- ifelse(is.na(ci_table$CI_hi_bca), ci_table$CI_hi_pct, ci_table$CI_hi_bca)
  cat(" done\n")

  print(ci_table[, c("Effect", "Estimate", "CI_lo", "CI_hi")])
  write_latex_table(ci_table[, c("Effect", "Estimate", "CI_lo_pct", "CI_hi_pct", "CI_lo_bca", "CI_hi_bca")],
                    sprintf("mediation_effects_%s.tex", scenario_tag),
                    sprintf("Interventional Mediation Effects with BCa CIs (%s)", dgp$label),
                    sprintf("mediation_effects_%s", scenario_tag))

  cat(sprintf("  [Scenario %s] %s\n", scenario_tag, timer_elapsed(t0)))
  cooldown_pause(sprintf("after scenario %s", scenario_tag))

  list(ie_point = ie_point, boot = bres, disc = disc, gam = gam_fit,
       ci_table = ci_table, dgp = dgp, data = d)
}

# Generate data and run scenarios
scenarios <- list()
if (SCENARIO %in% c("all", "p2_only")) {
  dgp2 <- generate_data_p2()
  scenarios[["p2"]] <- run_scenario(dgp2, "p2")
}
if (SCENARIO %in% c("all")) {
  dgp3 <- generate_data_p3_chain()
  scenarios[["p3"]] <- run_scenario(dgp3, "p3_chain")

  dgp5 <- generate_data_p5_mixed()
  scenarios[["p5"]] <- run_scenario(dgp5, "p5_mixed")
}

cat(sprintf("\n[Phase 2 total] %s\n", timer_elapsed(t0_phase2)))

# ==========================================================================
# PHASE 3: Traditional Mediation Comparison (Imai + Sobel)
# ==========================================================================
if (SCENARIO %in% c("all", "p2_only")) {
  cat("\n########## PHASE 3: Traditional Mediation Comparison ##########\n")
  t0_phase3 <- timer_start()

  run_traditional_comparison <- function(scen, scenario_tag) {
    d <- scen$data
    meds <- scen$dgp$mediator_names
    d_num <- d; d_num$treatment <- as.numeric(as.character(d$treatment))

    bridge_effects <- scen$ie_point
    bridge_ci <- scen$ci_table

    # ---- Imai causal mediation (per mediator) ----
    # NOTE: mediation::mediate() internally uses eval(model$call, parent.frame()),
    # which requires the formula and data objects to be findable from the global env.
    # We temporarily assign them to .GlobalEnv for mediate() frame-walking to work.
    imai_results <- list()
    .imai_out_fml  <- as.formula(paste("outcome ~", paste(c("treatment", meds), collapse = " + ")))
    .imai_d_num    <- d_num
    assign(".imai_out_fml", .imai_out_fml, envir = .GlobalEnv)
    assign(".imai_d_num",   .imai_d_num,   envir = .GlobalEnv)
    for (m in meds) {
      .imai_med_fml <- as.formula(paste(m, "~ treatment"))
      assign(".imai_med_fml", .imai_med_fml, envir = .GlobalEnv)
      med_fit <- lm(.imai_med_fml, data = .imai_d_num)
      out_fit <- lm(.imai_out_fml, data = .imai_d_num)
      med_res <- mediation::mediate(med_fit, out_fit,
                                    treat = "treatment", mediator = m,
                                    boot = TRUE, sims = min(500, BOOT_R))
      imai_results[[m]] <- list(ACME = med_res$d0, ADE = med_res$z0,
                                TE = med_res$tau.coef,
                                ACME_ci = c(med_res$d0.ci[1], med_res$d0.ci[2]))
    }
    rm(.imai_out_fml, .imai_d_num, .imai_med_fml, envir = .GlobalEnv)

    # ---- Product-of-coefficients (Sobel) ----
    out_fml <- as.formula(paste("outcome ~", paste(c("treatment", meds), collapse = " + ")))
    out_fit <- lm(out_fml, data = d_num)
    sobel_results <- list()
    for (m in meds) {
      med_fit <- lm(as.formula(paste(m, "~ treatment")), data = d_num)
      a <- coef(med_fit)["treatment"]
      b <- coef(out_fit)[m]
      se_a <- summary(med_fit)$coefficients["treatment", "Std. Error"]
      se_b <- summary(out_fit)$coefficients[m, "Std. Error"]
      sobel_se <- sqrt(a^2 * se_b^2 + b^2 * se_a^2)
      sobel_results[[m]] <- list(indirect = a * b, se = sobel_se,
                                 z = (a * b) / sobel_se,
                                 p = 2 * pnorm(-abs((a * b) / sobel_se)))
    }

    # ---- Build comparison table ----
    rows <- list()
    # BriDGE
    rows[[length(rows) + 1]] <- data.frame(Method = "BriDGE", Effect = "IDE (direct)",
                                            Estimate = bridge_effects["ide"],
                                            CI_lo = bridge_ci$CI_lo[bridge_ci$Effect == "ide"],
                                            CI_hi = bridge_ci$CI_hi[bridge_ci$Effect == "ide"],
                                            stringsAsFactors = FALSE)
    rows[[length(rows) + 1]] <- data.frame(Method = "BriDGE", Effect = "JIIE (joint indirect)",
                                            Estimate = bridge_effects["jiie"],
                                            CI_lo = bridge_ci$CI_lo[bridge_ci$Effect == "jiie"],
                                            CI_hi = bridge_ci$CI_hi[bridge_ci$Effect == "jiie"],
                                            stringsAsFactors = FALSE)
    rows[[length(rows) + 1]] <- data.frame(Method = "BriDGE", Effect = "TE (total)",
                                            Estimate = bridge_effects["te"],
                                            CI_lo = bridge_ci$CI_lo[bridge_ci$Effect == "te"],
                                            CI_hi = bridge_ci$CI_hi[bridge_ci$Effect == "te"],
                                            stringsAsFactors = FALSE)
    for (m in meds) {
      iie_name <- paste0("iie_", m)
      rows[[length(rows) + 1]] <- data.frame(Method = "BriDGE",
                                              Effect = sprintf("IIE (%s)", m),
                                              Estimate = bridge_effects[iie_name],
                                              CI_lo = bridge_ci$CI_lo[bridge_ci$Effect == iie_name],
                                              CI_hi = bridge_ci$CI_hi[bridge_ci$Effect == iie_name],
                                              stringsAsFactors = FALSE)
    }
    # Imai
    for (m in meds) {
      ir <- imai_results[[m]]
      rows[[length(rows) + 1]] <- data.frame(Method = "Imai",
                                              Effect = sprintf("ACME (%s)", m),
                                              Estimate = ir$ACME,
                                              CI_lo = ir$ACME_ci[1],
                                              CI_hi = ir$ACME_ci[2],
                                              stringsAsFactors = FALSE)
    }
    rows[[length(rows) + 1]] <- data.frame(Method = "Imai", Effect = "ADE (direct)",
                                            Estimate = imai_results[[1]]$ADE,
                                            CI_lo = NA, CI_hi = NA,
                                            stringsAsFactors = FALSE)
    # Sobel
    for (m in meds) {
      sr <- sobel_results[[m]]
      rows[[length(rows) + 1]] <- data.frame(Method = "Sobel",
                                              Effect = sprintf("Indirect (%s)", m),
                                              Estimate = sr$indirect,
                                              CI_lo = sr$indirect - 1.96 * sr$se,
                                              CI_hi = sr$indirect + 1.96 * sr$se,
                                              stringsAsFactors = FALSE)
    }

    comp_df <- do.call(rbind, rows)
    rownames(comp_df) <- NULL
    print(comp_df)

    write_latex_table(comp_df, sprintf("traditional_comparison_%s.tex", scenario_tag),
                      sprintf("BriDGE vs Traditional Mediation (%s)", scen$dgp$label),
                      sprintf("traditional_comparison_%s", scenario_tag))
  }

  for (tag in names(scenarios)) {
    run_traditional_comparison(scenarios[[tag]], tag)
    cooldown_pause(sprintf("after traditional comparison %s", tag))
  }
  cat(sprintf("[Phase 3] %s\n", timer_elapsed(t0_phase3)))
}

# ==========================================================================
# PHASE 4: Benchmarking Grid (N x p x B)
# ==========================================================================
if (SCENARIO %in% c("all", "benchmark")) {
  cat("\n########## PHASE 4: Benchmarking Grid ##########\n")
  t0_phase4 <- timer_start()

  bench_grid <- rbind(
    expand.grid(N = c(200, 500, 1000, 2000), p = 2, B = 1000, stringsAsFactors = FALSE),
    expand.grid(N = c(500, 1000, 2000),      p = 3, B = 1000, stringsAsFactors = FALSE),
    expand.grid(N = c(500, 1000, 2000),      p = 5, B = 1000, stringsAsFactors = FALSE),
    expand.grid(N = 1000, p = 2, B = c(500, 2000), stringsAsFactors = FALSE)
  )
  bench_grid <- unique(bench_grid)

  cat(sprintf("  Running %d benchmark configs...\n", nrow(bench_grid)))

  gen_fn <- function(p_val) {
    if (p_val == 2) generate_data_p2
    else if (p_val == 3) generate_data_p3_chain
    else generate_data_p5_mixed
  }
  med_names_fn <- function(p_val) paste0("mediator_", seq_len(p_val))

  bench_results <- list()
  for (i in seq_len(nrow(bench_grid))) {
    g <- bench_grid[i, ]
    cat(sprintf("  [%d/%d] N=%d p=%d B=%d ... ", i, nrow(bench_grid), g$N, g$p, g$B))

    dgp_fn <- gen_fn(g$p)
    dgp <- dgp_fn(n = g$N, seed = SEED_MAIN + i * 10)
    meds <- dgp$mediator_names

    # Time discovery
    t1 <- timer_start()
    disc <- run_discovery_gen(dgp$data, meds, bins = 5, strength_R = 100,
                              avg_threshold = 0.85, seed = i, use_parallel = TRUE)
    t_disc <- timer_seconds(t1)

    # Time GAM
    t1 <- timer_start()
    gam_fit <- fit_outcome_gam_gen(dgp$data, meds, use_bam = TRUE)
    t_gam <- timer_seconds(t1)

    # Time point estimate
    t1 <- timer_start()
    ie <- interventional_effects_gen(dgp$data, meds, B_mc = min(50, MC_DRAWS), use_bam = TRUE)
    t_point <- timer_seconds(t1)

    # Time bootstrap (reduced for benchmark)
    B_bench <- min(g$B, ifelse(FAST_MODE, 20, 100))
    t1 <- timer_start()
    br <- boot_mediation_gen(dgp$data, meds, B_boot = B_bench, B_mc = min(20, MC_DRAWS))
    t_boot_per <- timer_seconds(t1) / B_bench
    t_boot_est <- t_boot_per * g$B

    bench_results[[i]] <- data.frame(
      N = g$N, p = g$p, B = g$B,
      t_discovery = round(t_disc, 1),
      t_gam = round(t_gam, 1),
      t_point = round(t_point, 1),
      t_boot_est = round(t_boot_est, 1),
      t_total_est = round(t_disc + t_gam + t_point + t_boot_est, 1)
    )
    cat(sprintf("total_est=%.0fs\n", t_disc + t_gam + t_point + t_boot_est))
    cooldown_pause(sprintf("after benchmark %d/%d", i, nrow(bench_grid)))
  }

  bench_df <- do.call(rbind, bench_results)
  print(bench_df)
  write_latex_table(bench_df, "benchmark_runtime.tex",
                    "Runtime Benchmarking (seconds) by N, p, and B",
                    "benchmark_runtime")

  # Benchmark figure
  tryCatch({
    p_bench <- ggplot2::ggplot(bench_df, ggplot2::aes(x = N, y = t_total_est, color = factor(p))) +
      ggplot2::geom_line(linewidth = 1) + ggplot2::geom_point(size = 3) +
      ggplot2::labs(title = "BriDGE Runtime Scaling", x = "Sample Size (N)",
                    y = "Estimated Total Runtime (s)", color = "# Mediators (p)") +
      ggplot2::theme_minimal()
    ggplot2::ggsave(file.path(FIGURES_DIR, "benchmark_scaling.png"), p_bench,
                    width = 8, height = 5, dpi = 150)
    cat("  Saved: figures/benchmark_scaling.png\n")
  }, error = function(e) cat(sprintf("  [Warning] Could not save benchmark figure: %s\n", e$message)))

  cat(sprintf("[Phase 4] %s\n", timer_elapsed(t0_phase4)))
}

# ==========================================================================
# PHASE 5: Power Analysis Grid
# ==========================================================================
if (SCENARIO %in% c("all", "power")) {
  cat("\n########## PHASE 5: Power Analysis Grid ##########\n")
  t0_phase5 <- timer_start()

  power_N    <- c(100, 200, 500, 1000, 2000)
  power_mult <- c(0.05, 0.1, 0.2, 0.3, 0.5, 1.0)  # effect size multiplier (wider range for gradient)
  n_sim      <- ifelse(FAST_MODE, 5, 50)
  B_power    <- ifelse(FAST_MODE, 50, 500)

  # In FAST_MODE, shrink grid to save time
  if (FAST_MODE) {
    power_N    <- c(200, 500, 1000)
    power_mult <- c(0.1, 0.3, 1.0)
  }

  # Scaled DGP: effect_mult scales BOTH T→M and M→Y paths so that the
  # indirect effect attenuates quadratically, producing a visible power gradient
  generate_data_p2_scaled <- function(n, alpha = 0.8, effect_mult = 1.0, seed = 1) {
    set.seed(seed)
    treatment  <- rbinom(n, 1, 0.5)
    mediator_1 <- (1.0 * effect_mult) * treatment + rnorm(n) + alpha * sin(rnorm(n))
    mediator_2 <- (0.8 * effect_mult) * treatment + rnorm(n) + alpha * exp(rnorm(n) / 2)
    outcome    <- 1.5 * treatment +
      (1.2 * effect_mult) * mediator_1 + (0.9 * effect_mult) * mediator_2 +
      (0.5 * effect_mult) * (mediator_1^2) + (0.3 * effect_mult) * mediator_1 * mediator_2 +
      (0.5 * effect_mult) * treatment * mediator_1 + (0.7 * effect_mult) * treatment * mediator_2 +
      rnorm(n)
    data.frame(treatment = factor(treatment, levels = c(0, 1)),
               mediator_1 = mediator_1, mediator_2 = mediator_2,
               outcome = outcome)
  }

  power_grid <- expand.grid(N = power_N, effect_mult = power_mult, stringsAsFactors = FALSE)
  meds_p2 <- c("mediator_1", "mediator_2")

  cat(sprintf("  Power grid: %d cells x %d sims each\n", nrow(power_grid), n_sim))

  # Note: power sims run sequentially (each calls bootstrap internally via CL)
  # No need to export CL/cl_export to workers — they are only used on the master

  power_results <- list()
  for (gi in seq_len(nrow(power_grid))) {
    g <- power_grid[gi, ]
    cat(sprintf("  [%d/%d] N=%d effect_mult=%.1f ... ", gi, nrow(power_grid), g$N, g$effect_mult))

    # Run n_sim replications sequentially (each sim uses bootstrap internally via cluster)
    sig_count <- 0
    for (s in seq_len(n_sim)) {
      d <- generate_data_p2_scaled(n = g$N, effect_mult = g$effect_mult,
                                   seed = SEED_MAIN + gi * 1000 + s)
      # Quick bootstrap for power (reduced B)
      ie <- interventional_effects_gen(d, meds_p2, B_mc = min(30, MC_DRAWS), use_bam = TRUE)
      br <- boot_mediation_gen(d, meds_p2, B_boot = B_power, B_mc = min(20, MC_DRAWS))
      jiie_ci <- quantile(br$matrix[, "jiie"], c(0.025, 0.975), na.rm = TRUE)
      if (jiie_ci[1] > 0 || jiie_ci[2] < 0) sig_count <- sig_count + 1  # CI excludes 0
    }
    power_val <- sig_count / n_sim
    cat(sprintf("power=%.2f\n", power_val))
    power_results[[gi]] <- data.frame(N = g$N, effect_mult = g$effect_mult, power = power_val)
    cooldown_pause(sprintf("after power grid %d/%d", gi, nrow(power_grid)))
  }

  power_df <- do.call(rbind, power_results)
  print(power_df)
  write_latex_table(power_df, "power_grid.tex",
                    "Statistical Power for JIIE (proportion of CIs excluding 0)",
                    "power_grid")

  # Heatmap — adaptive text colour for readability
  tryCatch({
    power_df$txt_col <- ifelse(power_df$power > 0.5, "white", "black")
    p_pow <- ggplot2::ggplot(power_df, ggplot2::aes(x = factor(N), y = factor(effect_mult), fill = power)) +
      ggplot2::geom_tile(colour = "grey80", linewidth = 0.4) +
      ggplot2::geom_text(ggplot2::aes(label = sprintf("%.2f", power), colour = txt_col), size = 4) +
      ggplot2::scale_colour_identity() +
      ggplot2::scale_fill_gradient(low = "firebrick", high = "forestgreen", limits = c(0, 1)) +
      ggplot2::labs(title = "Power to Detect JIIE", x = "Sample Size (N)",
                    y = "Effect Size Multiplier", fill = "Power") +
      ggplot2::theme_minimal(base_size = 12)
    ggplot2::ggsave(file.path(FIGURES_DIR, "power_heatmap.png"), p_pow,
                    width = 8, height = 6, dpi = 150)
    cat("  Saved: figures/power_heatmap.png\n")
  }, error = function(e) cat(sprintf("  [Warning] Could not save power figure: %s\n", e$message)))

  cat(sprintf("[Phase 5] %s\n", timer_elapsed(t0_phase5)))
}

# ==========================================================================
# PHASE 6: Discretization Sensitivity (expanded)
# ==========================================================================
if (SCENARIO %in% c("all", "p2_only")) {
  cat("\n########## PHASE 6: Discretization Sensitivity ##########\n")
  t0_phase6 <- timer_start()

  d <- scenarios[["p2"]]$data
  meds <- scenarios[["p2"]]$dgp$mediator_names
  rdag <- scenarios[["p2"]]$dgp$researcher_dag
  bins_grid <- 3:7

  disc_sens_results <- list()
  for (g_bins in bins_grid) {
    cat(sprintf("  bins=%d ... ", g_bins))
    disc <- run_discovery_gen(d, meds, bins = g_bins, alpha = 0.05, score = "bde",
                              iss = 10, strength_R = 150, avg_threshold = 0.85,
                              seed = 1, use_parallel = TRUE)
    shd <- bnlearn::shd(disc$averaged, rdag)
    skm <- skeleton_metrics(disc$averaged, rdag)

    # Also re-run mediation with this discovery
    ie <- interventional_effects_gen(d, meds, B_mc = min(50, MC_DRAWS), use_bam = TRUE)

    disc_sens_results[[length(disc_sens_results) + 1]] <- data.frame(
      bins = g_bins, SHD = shd,
      precision = ifelse(is.na(skm$precision), 0, skm$precision),
      recall = ifelse(is.na(skm$recall), 0, skm$recall),
      f1 = ifelse(is.na(skm$f1), 0, skm$f1),
      n_arcs = nrow(bnlearn::arcs(disc$averaged)),
      IDE = ie["ide"], JIIE = ie["jiie"], TE = ie["te"]
    )
    cat(sprintf("SHD=%d, F1=%.3f\n", shd, ifelse(is.na(skm$f1), 0, skm$f1)))
    cooldown_pause(sprintf("after discretization bins=%d", g_bins))
  }

  disc_sens_df <- do.call(rbind, disc_sens_results)
  rownames(disc_sens_df) <- NULL
  print(disc_sens_df)
  write_latex_table(disc_sens_df, "discretization_sensitivity.tex",
                    "Discretization Sensitivity: Discovery and Mediation Effects by Number of Bins",
                    "discretization_sensitivity")

  # Figure: two-panel (A: discovery quality, B: effect stability)
  tryCatch({
    # Panel A: Discovery quality metrics (SHD and F1) — these actually vary
    disc_quality <- data.frame(
      bins   = rep(disc_sens_df$bins, 2),
      Metric = rep(c("SHD", "F1"), each = nrow(disc_sens_df)),
      Value  = c(disc_sens_df$SHD, disc_sens_df$f1)
    )
    p_quality <- ggplot2::ggplot(disc_quality,
                    ggplot2::aes(x = bins, y = Value, color = Metric)) +
      ggplot2::geom_line(linewidth = 1) + ggplot2::geom_point(size = 3) +
      ggplot2::scale_color_manual(values = c("SHD" = "darkorange", "F1" = "steelblue")) +
      ggplot2::scale_x_continuous(breaks = bins_grid) +
      ggplot2::labs(title = "A) DAG Discovery Quality vs Discretization",
                    x = "Number of Bins (g)", y = "Metric Value") +
      ggplot2::theme_minimal(base_size = 11)

    # Panel B: Mediation effect stability
    disc_effects <- data.frame(
      bins   = rep(disc_sens_df$bins, 3),
      Effect = rep(c("IDE", "JIIE", "TE"), each = nrow(disc_sens_df)),
      Value  = c(disc_sens_df$IDE, disc_sens_df$JIIE, disc_sens_df$TE)
    )
    p_effects <- ggplot2::ggplot(disc_effects,
                    ggplot2::aes(x = bins, y = Value, color = Effect)) +
      ggplot2::geom_line(linewidth = 1) + ggplot2::geom_point(size = 3) +
      ggplot2::scale_color_manual(values = c("IDE" = "firebrick", "JIIE" = "royalblue", "TE" = "gray40")) +
      ggplot2::scale_x_continuous(breaks = bins_grid) +
      ggplot2::labs(title = "B) Mediation Effect Stability",
                    x = "Number of Bins (g)", y = "Effect Estimate") +
      ggplot2::theme_minimal(base_size = 11)

    p_combined <- gridExtra::arrangeGrob(p_quality, p_effects, ncol = 1)
    ggplot2::ggsave(file.path(FIGURES_DIR, "discretization_robustness.png"),
                    p_combined, width = 8, height = 8, dpi = 150)
    cat("  Saved: figures/discretization_robustness.png\n")
  }, error = function(e) cat(sprintf("  [Warning] %s\n", e$message)))

  cat(sprintf("[Phase 6] %s\n", timer_elapsed(t0_phase6)))
}

# ==========================================================================
# PHASE 7: Measurement Error Sensitivity
# ==========================================================================
if (SCENARIO %in% c("all", "p2_only")) {
  cat("\n########## PHASE 7: Measurement Error Sensitivity ##########\n")
  t0_phase7 <- timer_start()

  d_orig <- scenarios[["p2"]]$data
  meds <- scenarios[["p2"]]$dgp$mediator_names
  rdag <- scenarios[["p2"]]$dgp$researcher_dag
  me_sds <- c(0, 0.25, 0.5, 1.0)

  me_results <- list()
  for (me_sd in me_sds) {
    cat(sprintf("  ME_SD=%.2f ... ", me_sd))
    set.seed(42)
    d_noisy <- d_orig
    for (m in meds) d_noisy[[m]] <- d_noisy[[m]] + rnorm(nrow(d_noisy), 0, me_sd)

    disc <- run_discovery_gen(d_noisy, meds, bins = 5, strength_R = 150,
                              avg_threshold = 0.85, seed = 1, use_parallel = TRUE)
    shd <- bnlearn::shd(disc$averaged, rdag)
    ie <- interventional_effects_gen(d_noisy, meds, B_mc = min(50, MC_DRAWS), use_bam = TRUE)

    me_results[[length(me_results) + 1]] <- data.frame(
      ME_SD = me_sd, SHD = shd,
      IDE = ie["ide"], JIIE = ie["jiie"], TE = ie["te"]
    )
    cat(sprintf("SHD=%d, JIIE=%.3f\n", shd, ie["jiie"]))
    cooldown_pause(sprintf("after measurement error SD=%.2f", me_sd))
  }

  me_df <- do.call(rbind, me_results)
  rownames(me_df) <- NULL
  print(me_df)
  write_latex_table(me_df, "measurement_error_sensitivity.tex",
                    "Measurement Error Sensitivity: Effects of Additive Noise on Mediators",
                    "measurement_error_sensitivity")

  cat(sprintf("[Phase 7] %s\n", timer_elapsed(t0_phase7)))
}

# ==========================================================================
# PHASE 8: Unmeasured Confounding Sensitivity
# ==========================================================================
if (SCENARIO %in% c("all", "p2_only")) {
  cat("\n########## PHASE 8: Unmeasured Confounding Sensitivity ##########\n")
  t0_phase8 <- timer_start()

  gamma_us <- c(0, 0.2, 0.5, 1.0)
  meds <- c("mediator_1", "mediator_2")

  conf_results <- list()
  for (gu in gamma_us) {
    cat(sprintf("  gamma_u=%.2f ... ", gu))
    set.seed(SEED_MAIN + 500)
    n <- 2000
    treatment <- rbinom(n, 1, 0.5)
    U <- rnorm(n)  # unmeasured confounder
    mediator_1 <- 1.0 * treatment + gu * U + rnorm(n) + 0.8 * sin(rnorm(n))
    mediator_2 <- 0.8 * treatment + rnorm(n) + 0.8 * exp(rnorm(n) / 2)
    outcome <- 1.5 * treatment +
      1.2 * mediator_1 + 0.9 * mediator_2 +
      0.5 * (mediator_1^2) + 0.3 * mediator_1 * mediator_2 +
      0.5 * treatment * mediator_1 + 0.7 * treatment * mediator_2 +
      gu * U + rnorm(n)  # U also affects outcome
    d_conf <- data.frame(treatment = factor(treatment, levels = c(0, 1)),
                         mediator_1 = mediator_1, mediator_2 = mediator_2,
                         outcome = outcome)

    ie <- interventional_effects_gen(d_conf, meds, B_mc = min(50, MC_DRAWS), use_bam = TRUE)

    conf_results[[length(conf_results) + 1]] <- data.frame(
      gamma_u = gu, IDE = ie["ide"], JIIE = ie["jiie"], TE = ie["te"]
    )
    cat(sprintf("IDE=%.3f, JIIE=%.3f, TE=%.3f\n", ie["ide"], ie["jiie"], ie["te"]))
    cooldown_pause(sprintf("after confounding gamma_u=%.2f", gu))
  }

  conf_df <- do.call(rbind, conf_results)
  rownames(conf_df) <- NULL
  print(conf_df)
  write_latex_table(conf_df, "confounding_sensitivity.tex",
                    "Unmeasured Confounding Sensitivity: Bias Under M-Y Confounding",
                    "confounding_sensitivity")

  cat(sprintf("[Phase 8] %s\n", timer_elapsed(t0_phase8)))
}

# ==========================================================================
# PHASE 9: DAG Contradiction Diagnostics (multi-algorithm triangulation)
# ==========================================================================
if (SCENARIO %in% c("all", "p2_only")) {
  cat("\n########## PHASE 9: DAG Contradiction Diagnostics ##########\n")
  t0_phase9 <- timer_start()

  d <- scenarios[["p2"]]$data
  meds <- scenarios[["p2"]]$dgp$mediator_names
  rdag <- scenarios[["p2"]]$dgp$researcher_dag

  # 9a) Multi-algorithm comparison: MMHC, HC, TABU
  cat("  [Multi-algorithm triangulation]\n")
  dd <- as.data.frame(d)
  dd[c(meds, "outcome")] <- bnlearn::discretize(dd[c(meds, "outcome")],
                                                 method = "quantile", breaks = 5)
  WL <- .make_whitelist_gen(meds); BL <- .make_blacklist_gen(meds)

  algos <- list(
    MMHC = function() bnlearn::mmhc(dd, whitelist = WL, blacklist = BL,
                                    restrict.args = list(test = "mi", alpha = 0.05),
                                    maximize.args = list(score = "bde", iss = 10)),
    HC   = function() bnlearn::hc(dd, whitelist = WL, blacklist = BL,
                                  score = "bde", iss = 10),
    TABU = function() bnlearn::tabu(dd, whitelist = WL, blacklist = BL,
                                    score = "bde", iss = 10)
  )

  algo_results <- list()
  for (aname in names(algos)) {
    fit <- tryCatch(algos[[aname]](), error = function(e) NULL)
    if (!is.null(fit)) {
      shd <- bnlearn::shd(fit, rdag)
      arcs_str <- apply(bnlearn::arcs(fit), 1, paste, collapse = "->")
      algo_results[[aname]] <- data.frame(Algorithm = aname, SHD = shd,
                                          n_arcs = nrow(bnlearn::arcs(fit)),
                                          arcs = paste(arcs_str, collapse = "; "),
                                          stringsAsFactors = FALSE)
      cat(sprintf("    %s: SHD=%d, arcs=%s\n", aname, shd, paste(arcs_str, collapse = ", ")))
    }
  }
  algo_df <- do.call(rbind, algo_results)
  rownames(algo_df) <- NULL

  # 9b) Bootstrap arc strength for all edges (both directions)
  cat("  [Bootstrap arc strength]\n")
  strn <- scenarios[["p2"]]$disc$strength
  strn_df <- as.data.frame(strn, stringsAsFactors = FALSE)
  disputed <- subset(strn_df, (from %in% meds | to %in% meds) & (from == "outcome" | to == "outcome"))
  cat("  Disputed/key edges (mediator <-> outcome):\n")
  print(disputed[order(disputed$strength, decreasing = TRUE), ])

  # 9c) With vs without theory constraints
  cat("  [With vs without constraints]\n")
  fit_unconstrained <- bnlearn::mmhc(dd, blacklist = BL,
                                     restrict.args = list(test = "mi", alpha = 0.05),
                                     maximize.args = list(score = "bde", iss = 10))
  fit_constrained <- bnlearn::mmhc(dd, whitelist = WL, blacklist = BL,
                                   restrict.args = list(test = "mi", alpha = 0.05),
                                   maximize.args = list(score = "bde", iss = 10))

  cat(sprintf("    Unconstrained: SHD=%d, arcs=%d\n",
              bnlearn::shd(fit_unconstrained, rdag), nrow(bnlearn::arcs(fit_unconstrained))))
  cat(sprintf("    Constrained:   SHD=%d, arcs=%d\n",
              bnlearn::shd(fit_constrained, rdag), nrow(bnlearn::arcs(fit_constrained))))

  # 9d) Mediation results comparison: constrained vs unconstrained DAG
  # Derive mediator sets from each DAG (nodes on T->...->Y paths, excluding T and Y)
  get_dag_mediators <- function(bn, all_meds) {
    arcs_df <- bnlearn::arcs(bn)
    # A variable is a mediator if: treatment -> m (direct or indirect) AND m -> outcome
    dag_meds <- character(0)
    for (m in all_meds) {
      t_to_m <- any(arcs_df[, 1] == "treatment" & arcs_df[, 2] == m)
      m_to_y <- any(arcs_df[, 1] == m & arcs_df[, 2] == "outcome")
      if (t_to_m && m_to_y) dag_meds <- c(dag_meds, m)
    }
    dag_meds
  }

  meds_constrained   <- get_dag_mediators(fit_constrained, meds)
  meds_unconstrained <- get_dag_mediators(fit_unconstrained, meds)
  cat(sprintf("    Constrained mediators:   %s\n", paste(meds_constrained, collapse = ", ")))
  cat(sprintf("    Unconstrained mediators: %s\n", paste(meds_unconstrained, collapse = ", ")))

  ie_constrained <- interventional_effects_gen(d, meds_constrained,
                                               B_mc = min(50, MC_DRAWS), use_bam = TRUE)
  # Only compute unconstrained if the mediator set differs
  if (identical(sort(meds_constrained), sort(meds_unconstrained))) {
    ie_unconstrained <- ie_constrained
    cat("    Same mediator set — effects identical.\n")
  } else {
    ie_unconstrained <- interventional_effects_gen(d, meds_unconstrained,
                                                   B_mc = min(50, MC_DRAWS), use_bam = TRUE)
  }

  diag_rows <- list(
    data.frame(Diagnostic = "Algorithm consensus (SHDs)",
               Value = paste(unique(algo_df$SHD), collapse = "/"), stringsAsFactors = FALSE),
    data.frame(Diagnostic = "Unconstrained SHD",
               Value = as.character(bnlearn::shd(fit_unconstrained, rdag)), stringsAsFactors = FALSE),
    data.frame(Diagnostic = "Constrained SHD",
               Value = as.character(bnlearn::shd(fit_constrained, rdag)), stringsAsFactors = FALSE),
    data.frame(Diagnostic = "Constrained mediators",
               Value = paste(meds_constrained, collapse = ", "), stringsAsFactors = FALSE),
    data.frame(Diagnostic = "Unconstrained mediators",
               Value = paste(meds_unconstrained, collapse = ", "), stringsAsFactors = FALSE),
    data.frame(Diagnostic = "IDE (constrained)",
               Value = sprintf("%.3f", ie_constrained["ide"]), stringsAsFactors = FALSE),
    data.frame(Diagnostic = "IDE (unconstrained)",
               Value = sprintf("%.3f", ie_unconstrained["ide"]), stringsAsFactors = FALSE),
    data.frame(Diagnostic = "JIIE (constrained)",
               Value = sprintf("%.3f", ie_constrained["jiie"]), stringsAsFactors = FALSE),
    data.frame(Diagnostic = "JIIE (unconstrained)",
               Value = sprintf("%.3f", ie_unconstrained["jiie"]), stringsAsFactors = FALSE)
  )
  diag_df <- do.call(rbind, diag_rows)
  rownames(diag_df) <- NULL
  print(diag_df)
  write_latex_table(diag_df, "dag_contradiction_diagnostics.tex",
                    "DAG Contradiction Diagnostics: Multi-Algorithm Triangulation and Constraint Sensitivity",
                    "dag_contradiction_diagnostics")

  cat(sprintf("[Phase 9] %s\n", timer_elapsed(t0_phase9)))
}

# ==========================================================================
# PHASE 10: Bootstrap Replicate Comparison (B=500/1000/2000)
# ==========================================================================
if (SCENARIO %in% c("all", "p2_only")) {
  cat("\n########## PHASE 10: Bootstrap Replicate Comparison ##########\n")
  t0_phase10 <- timer_start()

  d <- scenarios[["p2"]]$data
  meds <- scenarios[["p2"]]$dgp$mediator_names
  B_set <- if (FAST_MODE) c(20, 50) else c(500, 1000, 2000)

  boot_comp <- list()
  for (B_val in B_set) {
    cat(sprintf("  B=%d ... ", B_val))
    t1 <- timer_start()
    br <- boot_mediation_gen(d, meds, B_boot = B_val, B_mc = min(30, MC_DRAWS))
    t_elapsed <- timer_seconds(t1)

    ci_jiie <- quantile(br$matrix[, "jiie"], c(0.025, 0.975), na.rm = TRUE)
    ci_ide  <- quantile(br$matrix[, "ide"],  c(0.025, 0.975), na.rm = TRUE)

    boot_comp[[length(boot_comp) + 1]] <- data.frame(
      B = B_val,
      IDE_width = ci_ide[2] - ci_ide[1],
      JIIE_width = ci_jiie[2] - ci_jiie[1],
      JIIE_lo = ci_jiie[1], JIIE_hi = ci_jiie[2],
      Runtime_s = round(t_elapsed, 1),
      n_failed = br$n_failed
    )
    cat(sprintf("JIIE CI=[%.3f, %.3f], width=%.3f, %.0fs\n",
                ci_jiie[1], ci_jiie[2], ci_jiie[2] - ci_jiie[1], t_elapsed))
    cooldown_pause(sprintf("after bootstrap comparison B=%d", B_val))
  }

  boot_comp_df <- do.call(rbind, boot_comp)
  rownames(boot_comp_df) <- NULL
  print(boot_comp_df)
  write_latex_table(boot_comp_df, "bootstrap_comparison.tex",
                    "Bootstrap Replicate Comparison: CI Width and Runtime by B",
                    "bootstrap_comparison")

  cat(sprintf("[Phase 10] %s\n", timer_elapsed(t0_phase10)))
}

# ==========================================================================
# PHASE 11: Performance Summary
# ==========================================================================
cat("\n============================================================\n")
cat("PERFORMANCE SUMMARY — BriDGE v6\n")
cat("============================================================\n")
cat(sprintf("Workers: %d / %d cores | Backend: %s\n", N_WORKERS, TOTAL_CORES, toupper(PARALLEL_BACKEND)))
if (IS_APPLE_SILICON) cat("Apple Silicon: Accelerate BLAS (1 thread/worker)\n")
if (COOLDOWN_ENABLED) {
  cat(sprintf("Cooldowns: every %d boot reps, %ds sleep\n", COOLDOWN_EVERY, COOLDOWN_SEC))
} else {
  cat("Cooldowns: disabled\n")
}
cat(sprintf("Bootstrap R: %d | MC draws: %d\n", BOOT_R, MC_DRAWS))
cat("------------------------------------------------------------\n")
if (exists("t0_phase2"))  cat(sprintf("Phase 2 (Main analysis)   : %s\n", timer_elapsed(t0_phase2)))
if (exists("t0_phase3"))  cat(sprintf("Phase 3 (Trad. comparison): %s\n", timer_elapsed(t0_phase3)))
if (exists("t0_phase4"))  cat(sprintf("Phase 4 (Benchmarking)    : %s\n", timer_elapsed(t0_phase4)))
if (exists("t0_phase5"))  cat(sprintf("Phase 5 (Power analysis)  : %s\n", timer_elapsed(t0_phase5)))
if (exists("t0_phase6"))  cat(sprintf("Phase 6 (Discretization)  : %s\n", timer_elapsed(t0_phase6)))
if (exists("t0_phase7"))  cat(sprintf("Phase 7 (Meas. error)     : %s\n", timer_elapsed(t0_phase7)))
if (exists("t0_phase8"))  cat(sprintf("Phase 8 (Confounding)     : %s\n", timer_elapsed(t0_phase8)))
if (exists("t0_phase9"))  cat(sprintf("Phase 9 (DAG diagnostics) : %s\n", timer_elapsed(t0_phase9)))
if (exists("t0_phase10")) cat(sprintf("Phase 10 (Boot. compare)  : %s\n", timer_elapsed(t0_phase10)))
cat("============================================================\n")

# Generated tables
cat("\nGenerated tables:\n")
for (f in list.files(TABLES_DIR, pattern = "\\.tex$")) cat(sprintf("  %s/%s\n", TABLES_DIR, f))
cat("\nGenerated figures:\n")
for (f in list.files(FIGURES_DIR, pattern = "\\.png$")) cat(sprintf("  %s/%s\n", FIGURES_DIR, f))
for (f in list.files(DAG_PNG_DIR, pattern = "\\.png$")) cat(sprintf("  %s/%s\n", DAG_PNG_DIR, f))

# Cleanup
try(parallel::stopCluster(CL), silent = TRUE)
cat("\nCluster stopped. Done.\n")
# ========================= END OF SCRIPT ==================================
