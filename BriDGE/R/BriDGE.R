# BriDGE R Package - Behavioural research
#by integrating DAGs and GAMs in Experiments
# Main Package Functions

#' BriDGE:Behavioural research by integrating DAGs and GAMs in Experiments
#'
#' @description A comprehensive package for causal analysis of randomized controlled trial data.
#' @details It features causal discovery, mediation analysis with nonlinear relationships using GAMs,
#' bootstrapping, sensitivity analysis, and visualization tools.
#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @import ggplot2
#' @import dplyr
#' @importFrom bnlearn mmhc amat hc pc.stable
#' @importFrom mgcv gam gam.check gam.control
#' @importFrom igraph graph graph_from_adjacency_matrix plot.igraph as_edgelist
#' @importFrom boot boot
#' @importFrom parallel detectCores makeCluster stopCluster parLapply clusterExport clusterEvalQ
#' @importFrom Hmisc cut2
#' @importFrom stats as.formula lm predict quantile rbinom rnorm na.omit complete.cases
#' @importFrom graphics par hist plot
#' @importFrom utils combn
## usethis namespace: end
NULL

#' @keywords internal
utils::globalVariables(c("Value", "Effect", "Mean", "CI_Lower", "CI_Upper"))

# Helper function to validate data (internal)
validate_data <- function(data, variables) {
  # Check if data is a data frame
  if (!is.data.frame(data)) {
    stop("Data must be a data frame")
  }

  # Check for missing columns
  missing_vars <- variables[!variables %in% names(data)]
  if (length(missing_vars) > 0) {
    stop(paste("Missing variables in data:", paste(missing_vars, collapse = ", ")))
  }

  # Check for NA values
  na_check <- sapply(data[variables], function(x) any(is.na(x)))
  if (any(na_check)) {
    vars_with_na <- names(na_check)[na_check]
    stop(paste("Missing values (NA) found in variables:",
               paste(vars_with_na, collapse = ", "),
               "\nPlease remove or impute missing values before analysis."))
  }

  # Check for sufficient observations
  if (nrow(data) < 10) {
    stop("Insufficient data: at least 10 observations required")
  }

  return(TRUE)
}

#' Complete Causal Analysis Pipeline
#'
#' Main wrapper function that performs a complete causal analysis pipeline including
#' causal discovery, mediation analysis, sensitivity analysis, and visualization.
#'
#' @param data A data frame containing the variables for analysis
#' @param treatment Character string specifying the treatment variable name
#' @param mediators Character vector specifying the mediator variable names
#' @param outcome Character string specifying the outcome variable name
#' @param n_bootstraps Integer specifying the number of bootstrap iterations (default: 500)
#' @param discovery_method Character string specifying the causal discovery method ("mmhc", "hc", "pc")
#' @param nonlinear Logical indicating whether to use nonlinear GAM models (default: TRUE)
#' @param plot Logical indicating whether to generate plots (default: TRUE)
#' @param parallel Logical indicating whether to use parallel processing (default: TRUE)
#' @param sensitivity Logical indicating whether to perform sensitivity analysis (default: TRUE)
#' @param handle_convergence Character string specifying how to handle convergence issues ("warn", "simplify", "error")
#' @param gam_maxit Integer specifying maximum iterations for GAM fitting (default: 200)
#' @param gam_epsilon Numeric specifying convergence tolerance for GAM fitting (default: 1e-7)
#'
#' @return A list containing all analysis results and plots
#' @export
#'
#' @examples
#' # Generate example data
#' data <- bridge_generate_data(n = 200) # Smaller n for faster example
#'
#' # Perform complete analysis
#' # Reducing bootstraps and disabling parallel for faster example
#' results <- bridge_analyze(
#'   data = data,
#'   treatment = "treatment",
#'   mediators = c("mediator_1", "mediator_2"),
#'   outcome = "outcome",
#'   n_bootstraps = 10,
#'   parallel = FALSE
#' )
#'
#' # View summary
#' print(results$summary)
bridge_analyze <- function(data,
                           treatment,
                           mediators,
                           outcome,
                           n_bootstraps = 500,
                           discovery_method = "mmhc",
                           nonlinear = TRUE,
                           plot = TRUE,
                           parallel = TRUE,
                           sensitivity = TRUE,
                           handle_convergence = "warn",
                           gam_maxit = 200,
                           gam_epsilon = 1e-7) {

  # Input validation
  required_vars <- c(treatment, mediators, outcome)
  validate_data(data, required_vars)

  # Validate handle_convergence parameter
  if (!handle_convergence %in% c("warn", "simplify", "error")) {
    stop("handle_convergence must be one of: 'warn', 'simplify', 'error'")
  }

  cat("Starting BriDGE Causal Analysis Pipeline...\n")

  # 1. Causal Discovery
  cat("Step 1: Performing causal discovery...\n")
  discovery_results <- bridge_discover(
    data = data,
    treatment = treatment,
    mediators = mediators,
    outcome = outcome,
    method = discovery_method
  )

  # 2. Mediation Analysis
  cat("Step 2: Performing mediation analysis...\n")
  mediation_results <- bridge_mediate(
    data = data,
    treatment = treatment,
    mediators = mediators,
    outcome = outcome,
    n_bootstraps = n_bootstraps,
    nonlinear = nonlinear,
    parallel = parallel,
    handle_convergence = handle_convergence,
    gam_maxit = gam_maxit,
    gam_epsilon = gam_epsilon
  )

  # 3. Comparative Analysis
  cat("Step 3: Performing comparative analysis...\n")
  comparison_results <- bridge_compare(
    data = data,
    treatment = treatment,
    mediators = mediators,
    outcome = outcome
  )

  # 4. Sensitivity Analysis
  sensitivity_results <- NULL
  if (sensitivity) {
    cat("Step 4: Performing sensitivity analysis...\n")
    sensitivity_results <- bridge_sensitivity(
      data = data,
      treatment = treatment,
      mediators = mediators,
      outcome = outcome,
      n_bootstraps = min(n_bootstraps, 100), # Reduced for sensitivity
      nonlinear = nonlinear,
      parallel = parallel,
      handle_convergence = handle_convergence,
      gam_maxit = gam_maxit,
      gam_epsilon = gam_epsilon
    )
  }

  # 5. Generate Plots
  plots <- NULL
  if (plot) {
    cat("Step 5: Generating visualizations...\n")
    plots <- bridge_plot(
      discovery_results = discovery_results,
      mediation_results = mediation_results,
      data = data,
      treatment = treatment,
      mediators = mediators,
      outcome = outcome
    )
  }

  # 6. Generate Summary
  cat("Step 6: Generating summary...\n")
  summary_results <- bridge_summary(
    discovery_results = discovery_results,
    mediation_results = mediation_results,
    comparison_results = comparison_results,
    sensitivity_results = sensitivity_results
  )

  # Compile results
  results <- list(
    discovery = discovery_results,
    mediation = mediation_results,
    comparison = comparison_results,
    sensitivity = sensitivity_results,
    plots = plots,
    summary = summary_results,
    call = match.call()
  )

  class(results) <- "bridge_analysis"

  cat("BriDGE Analysis Complete!\n")
  return(results)
}

#' Causal Discovery Analysis
#'
#' Performs causal discovery to learn the causal structure from data using
#' various algorithms with prior knowledge integration.
#'
#' @param data A data frame containing the variables
#' @param treatment Character string specifying the treatment variable name
#' @param mediators Character vector specifying the mediator variable names
#' @param outcome Character string specifying the outcome variable name
#' @param method Character string specifying the discovery method ("mmhc", "hc", "pc")
#' @param discretize Logical indicating whether to discretize continuous variables
#' @param n_bins Integer specifying number of bins for discretization (default: 5)
#'
#' @return A list containing discovered DAG and comparison with researcher's DAG
#' @export
bridge_discover <- function(data,
                            treatment,
                            mediators,
                            outcome,
                            method = "mmhc",
                            discretize = TRUE,
                            n_bins = 5) {

  # Validate data
  variables <- c(treatment, mediators, outcome)
  validate_data(data, variables)

  # Prepare data
  analysis_data <- data[, variables, drop = FALSE]

  # Ensure treatment is factor
  analysis_data[[treatment]] <- as.factor(analysis_data[[treatment]])

  # Discretize if requested
  if (discretize) {
    for (var in c(mediators, outcome)) {
      if (is.numeric(analysis_data[[var]])) {
        # Hmisc::cut2 is imported via NAMESPACE
        analysis_data[[var]] <- cut2(analysis_data[[var]], g = n_bins)
      }
    }
  }

  # Define blacklist (no edges into treatment)
  other_vars <- c(mediators, outcome)
  blacklist <- data.frame(
    from = rep(other_vars, each = 1),
    to = rep(treatment, length(other_vars)),
    stringsAsFactors = FALSE
  )

  # Define whitelist (treatment to mediators)
  whitelist <- data.frame(
    from = rep(treatment, length(mediators)),
    to = mediators,
    stringsAsFactors = FALSE
  )

  # Perform causal discovery
  if (method == "mmhc") {
    # bnlearn::mmhc is imported via NAMESPACE
    discovered_dag <- bnlearn::mmhc(
      analysis_data,
      whitelist = whitelist,
      blacklist = blacklist,
      restrict.args = list(test = "mi")
    )
  } else if (method == "hc") {
    # bnlearn::hc is imported via NAMESPACE
    discovered_dag <- bnlearn::hc(
      analysis_data,
      whitelist = whitelist,
      blacklist = blacklist
    )
  } else if (method == "pc") {
    # bnlearn::pc.stable is imported via NAMESPACE
    discovered_dag <- bnlearn::pc.stable(
      analysis_data,
      whitelist = whitelist,
      blacklist = blacklist
    )
  } else {
    stop("Unsupported discovery method. Use 'mmhc', 'hc', or 'pc'.")
  }

  # Create researcher's assumed DAG for comparison
  researcher_edges <- c()
  for (mediator in mediators) {
    researcher_edges <- c(researcher_edges, treatment, mediator)
    researcher_edges <- c(researcher_edges, mediator, outcome)
  }
  researcher_edges <- c(researcher_edges, treatment, outcome)

  # igraph::graph is imported via NAMESPACE
  researcher_dag <- igraph::graph(edges = researcher_edges, directed = TRUE)

  # Convert discovered DAG to igraph format
  # bnlearn::amat is imported via NAMESPACE
  adj_matrix <- bnlearn::amat(discovered_dag)
  # igraph::graph_from_adjacency_matrix is imported via NAMESPACE
  discovered_igraph <- igraph::graph_from_adjacency_matrix(adj_matrix, mode = "directed")

  # Compare DAGs
  dag_comparison <- compare_dags(researcher_dag, discovered_igraph)

  return(list(
    discovered_dag = discovered_dag,
    discovered_igraph = discovered_igraph,
    researcher_dag = researcher_dag,
    comparison = dag_comparison,
    method = method
  ))
}

#' Causal Mediation Analysis
#'
#' Performs causal mediation analysis using GAMs with bootstrapping to estimate
#' direct and indirect effects through multiple mediators.
#'
#' @param data A data frame containing the variables
#' @param treatment Character string specifying the treatment variable name
#' @param mediators Character vector specifying the mediator variable names
#' @param outcome Character string specifying the outcome variable name
#' @param n_bootstraps Integer specifying the number of bootstrap iterations
#' @param nonlinear Logical indicating whether to use nonlinear GAM models
#' @param parallel Logical indicating whether to use parallel processing
#' @param k_basis Integer specifying the number of basis functions for GAM (default: 10)
#' @param handle_convergence Character string specifying how to handle convergence issues ("warn", "simplify", "error")
#' @param gam_maxit Integer specifying maximum iterations for GAM fitting (default: 200)
#' @param gam_epsilon Numeric specifying convergence tolerance for GAM fitting (default: 1e-7)
#'
#' @return A list containing mediation analysis results
#' @export
bridge_mediate <- function(data,
                           treatment,
                           mediators,
                           outcome,
                           n_bootstraps = 500,
                           nonlinear = TRUE,
                           parallel = TRUE,
                           k_basis = 10,
                           handle_convergence = "warn",
                           gam_maxit = 200,
                           gam_epsilon = 1e-7) {

  # Validate data
  variables <- c(treatment, mediators, outcome)
  validate_data(data, variables)

  # Prepare data
  analysis_data <- data[, variables, drop = FALSE]
  analysis_data[[treatment]] <- as.factor(analysis_data[[treatment]])

  # Define bootstrap function
  bootstrap_mediation <- function(data_boot, treat_boot, meds_boot, out_boot, nlin_boot, k_boot, handle_conv, gam_mx, gam_eps) {
    n_sample <- nrow(data_boot)

    # Fit mediator models
    mediator_models <- list()
    for (mediator in meds_boot) {
      if (nlin_boot) {
        formula_str <- paste(mediator, "~", treat_boot)

        mediator_models[[mediator]] <- tryCatch({
          mgcv::gam(
            stats::as.formula(formula_str),
            data = data_boot,
            method = "REML",
            control = mgcv::gam.control(maxit = gam_mx, epsilon = gam_eps)
          )
        }, warning = function(w) {
          if (handle_conv == "error") {
            stop(paste("GAM convergence warning in mediator model:", w$message))
          } else if (handle_conv == "simplify") {
            # Try with increased iterations and relaxed tolerance
            mgcv::gam(
              stats::as.formula(formula_str),
              data = data_boot,
              method = "REML",
              control = mgcv::gam.control(maxit = gam_mx * 2, epsilon = gam_eps * 10)
            )
          } else {
            # handle_conv == "warn" - return original fit with warning
            mgcv::gam(
              stats::as.formula(formula_str),
              data = data_boot,
              method = "REML",
              control = mgcv::gam.control(maxit = gam_mx, epsilon = gam_eps)
            )
          }
        }, error = function(e) {
          if (handle_conv == "simplify") {
            # Fall back to linear model if GAM fails
            stats::lm(stats::as.formula(formula_str), data = data_boot)
          } else {
            stop(paste("GAM fitting error in mediator model:", e$message))
          }
        })
      } else {
        formula_str <- paste(mediator, "~", treat_boot)
        mediator_models[[mediator]] <- stats::lm(stats::as.formula(formula_str), data = data_boot)
      }
    }

    # Fit outcome model
    if (nlin_boot) {
      # Adjust k based on sample size to avoid overfitting
      k_adjusted <- min(k_boot, floor(n_sample / 4))
      smooth_terms <- paste0("s(", meds_boot, ", bs = 'tp', k = ", k_adjusted, ")", collapse = " + ")
      formula_str <- paste(out_boot, "~", treat_boot, "+", smooth_terms)

      outcome_model <- tryCatch({
        mgcv::gam(
          stats::as.formula(formula_str),
          data = data_boot,
          method = "REML",
          control = mgcv::gam.control(maxit = gam_mx, epsilon = gam_eps)
        )
      }, warning = function(w) {
        if (handle_conv == "error") {
          stop(paste("GAM convergence warning in outcome model:", w$message))
        } else if (handle_conv == "simplify") {
          # Try simpler model with fewer basis functions
          k_simple <- max(3, floor(k_adjusted / 2))
          smooth_terms_simple <- paste0("s(", meds_boot, ", bs = 'tp', k = ", k_simple, ")", collapse = " + ")
          formula_str_simple <- paste(out_boot, "~", treat_boot, "+", smooth_terms_simple)
          mgcv::gam(
            stats::as.formula(formula_str_simple),
            data = data_boot,
            method = "REML",
            control = mgcv::gam.control(maxit = gam_mx * 2, epsilon = gam_eps * 10)
          )
        } else {
          # handle_conv == "warn" - return original fit
          mgcv::gam(
            stats::as.formula(formula_str),
            data = data_boot,
            method = "REML",
            control = mgcv::gam.control(maxit = gam_mx, epsilon = gam_eps)
          )
        }
      }, error = function(e) {
        if (handle_conv == "simplify") {
          # Fall back to linear model if GAM fails
          formula_str_linear <- paste(out_boot, "~", treat_boot, "+", paste(meds_boot, collapse = " + "))
          stats::lm(stats::as.formula(formula_str_linear), data = data_boot)
        } else {
          stop(paste("GAM fitting error in outcome model:", e$message))
        }
      })
    } else {
      formula_str <- paste(out_boot, "~", treat_boot, "+", paste(meds_boot, collapse = " + "))
      outcome_model <- stats::lm(stats::as.formula(formula_str), data = data_boot)
    }

    # Compute effects
    mediator_preds <- list()
    for (treat_val in c("0", "1")) {
      mediator_preds[[treat_val]] <- list()
      for (mediator in meds_boot) {
        pred_data <- data.frame(treatment_col = factor(rep(treat_val, n_sample)))
        names(pred_data)[1] <- treat_boot
        mediator_preds[[treat_val]][[mediator]] <- stats::predict(mediator_models[[mediator]], newdata = pred_data)
      }
    }

    pred_data_t1 <- data.frame(treatment_col = factor(rep("1", n_sample)))
    names(pred_data_t1)[1] <- treat_boot
    pred_data_t0 <- data.frame(treatment_col = factor(rep("0", n_sample)))
    names(pred_data_t0)[1] <- treat_boot

    for (mediator in meds_boot) {
      pred_data_t1[[mediator]] <- mediator_preds[["0"]][[mediator]]
      pred_data_t0[[mediator]] <- mediator_preds[["0"]][[mediator]]
    }

    outcome_t1_m0 <- stats::predict(outcome_model, newdata = pred_data_t1)
    outcome_t0_m0 <- stats::predict(outcome_model, newdata = pred_data_t0)
    nde <- mean(outcome_t1_m0 - outcome_t0_m0, na.rm = TRUE)

    indirect_effects <- list()
    for (i in seq_along(meds_boot)) {
      mediator_focus <- meds_boot[i]
      pred_data_nie <- data.frame(treatment_col = factor(rep("0", n_sample)))
      names(pred_data_nie)[1] <- treat_boot
      for (j in seq_along(meds_boot)) {
        if (j == i) {
          pred_data_nie[[meds_boot[j]]] <- mediator_preds[["1"]][[meds_boot[j]]]
        } else {
          pred_data_nie[[meds_boot[j]]] <- mediator_preds[["0"]][[meds_boot[j]]]
        }
      }
      outcome_nie <- stats::predict(outcome_model, newdata = pred_data_nie)
      indirect_effects[[paste0("nie_", mediator_focus)]] <- mean(outcome_nie - outcome_t0_m0, na.rm = TRUE)
    }

    pred_data_total_t1 <- data.frame(treatment_col = factor(rep("1", n_sample)))
    names(pred_data_total_t1)[1] <- treat_boot
    for (mediator in meds_boot) {
      pred_data_total_t1[[mediator]] <- mediator_preds[["1"]][[mediator]]
    }
    outcome_total_t1 <- stats::predict(outcome_model, newdata = pred_data_total_t1)
    total_effect <- mean(outcome_total_t1 - outcome_t0_m0, na.rm = TRUE)

    c(list(direct_effect = nde, total_effect = total_effect), indirect_effects)
  }

  # Perform bootstrap analysis
  if (parallel) {
    n_cores <- max(1, parallel::detectCores() - 1)
    cl <- parallel::makeCluster(n_cores)
    on.exit(parallel::stopCluster(cl))

    parallel::clusterExport(cl, varlist = c("analysis_data", "treatment", "mediators", "outcome",
                                            "nonlinear", "k_basis", "bootstrap_mediation",
                                            "handle_convergence", "gam_maxit", "gam_epsilon"),
                            envir = environment())
    parallel::clusterEvalQ(cl, {
      library(mgcv)
    })

    boot_results_list <- parallel::parLapply(cl, 1:n_bootstraps, function(i) {
      set.seed(123 + i)
      sample_indices <- sample(1:nrow(analysis_data), replace = TRUE)
      sample_data <- analysis_data[sample_indices, ]
      bootstrap_mediation(sample_data, treatment, mediators, outcome, nonlinear, k_basis,
                          handle_convergence, gam_maxit, gam_epsilon)
    })

  } else {
    boot_results_list <- list()
    for (i in 1:n_bootstraps) {
      set.seed(123 + i)
      sample_indices <- sample(1:nrow(analysis_data), replace = TRUE)
      sample_data <- analysis_data[sample_indices, ]
      boot_results_list[[i]] <- bootstrap_mediation(sample_data, treatment, mediators, outcome,
                                                    nonlinear, k_basis, handle_convergence,
                                                    gam_maxit, gam_epsilon)
    }
  }

  # Extract and summarize results
  effect_names <- names(boot_results_list[[1]])
  bootstrap_results <- list()

  for (effect_name in effect_names) {
    bootstrap_results[[effect_name]] <- sapply(boot_results_list, function(x) x[[effect_name]])
  }

  # Compute summaries
  effect_summaries <- list()
  for (effect_name in effect_names) {
    effects <- bootstrap_results[[effect_name]]
    effect_summaries[[effect_name]] <- list(
      mean = mean(effects, na.rm = TRUE),
      ci_lower = stats::quantile(effects, 0.025, na.rm = TRUE),
      ci_upper = stats::quantile(effects, 0.975, na.rm = TRUE),
      effects = effects
    )
  }

  return(list(
    summaries = effect_summaries,
    bootstrap_results = bootstrap_results,
    n_bootstraps = n_bootstraps,
    method = ifelse(nonlinear, "GAM", "Linear")
  ))
}

#' Sensitivity Analysis
#'
#' Performs sensitivity analysis by adding small perturbations to the data
#' and re-running the mediation analysis.
#'
#' @param data A data frame containing the variables
#' @param treatment Character string specifying the treatment variable name
#' @param mediators Character vector specifying the mediator variable names
#' @param outcome Character string specifying the outcome variable name
#' @param n_bootstraps Integer specifying the number of bootstrap iterations
#' @param nonlinear Logical indicating whether to use nonlinear GAM models
#' @param parallel Logical indicating whether to use parallel processing
#' @param perturbation_sd Numeric specifying the standard deviation of perturbations
#' @param handle_convergence Character string specifying how to handle convergence issues
#' @param gam_maxit Integer specifying maximum iterations for GAM fitting
#' @param gam_epsilon Numeric specifying convergence tolerance for GAM fitting
#'
#' @return A list containing sensitivity analysis results
#' @export
bridge_sensitivity <- function(data,
                               treatment,
                               mediators,
                               outcome,
                               n_bootstraps = 100,
                               nonlinear = TRUE,
                               parallel = TRUE,
                               perturbation_sd = 0.1,
                               handle_convergence = "warn",
                               gam_maxit = 200,
                               gam_epsilon = 1e-7) {

  # Validate data
  variables <- c(treatment, mediators, outcome)
  validate_data(data, variables)

  # Create perturbed data
  perturbed_data <- data
  for (mediator in mediators) {
    if (is.numeric(perturbed_data[[mediator]])) {
      perturbed_data[[mediator]] <- perturbed_data[[mediator]] +
        stats::rnorm(nrow(perturbed_data), 0, perturbation_sd)
    }
  }

  if (is.numeric(perturbed_data[[outcome]])) {
    perturbed_data[[outcome]] <- perturbed_data[[outcome]] +
      stats::rnorm(nrow(perturbed_data), 0, perturbation_sd)
  }

  # Run mediation analysis on perturbed data
  sensitivity_results <- bridge_mediate(
    data = perturbed_data,
    treatment = treatment,
    mediators = mediators,
    outcome = outcome,
    n_bootstraps = n_bootstraps,
    nonlinear = nonlinear,
    parallel = parallel,
    handle_convergence = handle_convergence,
    gam_maxit = gam_maxit,
    gam_epsilon = gam_epsilon
  )

  return(list(
    perturbed_results = sensitivity_results,
    perturbation_sd = perturbation_sd
  ))
}

#' Comparative Analysis
#'
#' Performs comparative analysis between treatment and control groups.
#'
#' @param data A data frame containing the variables
#' @param treatment Character string specifying the treatment variable name
#' @param mediators Character vector specifying the mediator variable names
#' @param outcome Character string specifying the outcome variable name
#'
#' @return A data frame containing comparative statistics
#' @export
bridge_compare <- function(data, treatment, mediators, outcome) {
  # Validate data
  variables <- c(treatment, mediators, outcome)
  validate_data(data, variables)

  variables_to_compare <- c(mediators, outcome)

  # Split data by treatment
  control_data <- data[data[[treatment]] == "0" | data[[treatment]] == 0, variables_to_compare, drop = FALSE]
  treatment_data <- data[data[[treatment]] == "1" | data[[treatment]] == 1, variables_to_compare, drop = FALSE]

  # Check if groups have sufficient data
  if (nrow(control_data) < 2 || nrow(treatment_data) < 2) {
    stop("Insufficient data in treatment or control groups for comparison")
  }

  # Compute means with na.rm = TRUE
  control_means <- sapply(control_data, function(x) {
    if (is.numeric(x)) mean(x, na.rm = TRUE) else NA
  })

  treatment_means <- sapply(treatment_data, function(x) {
    if (is.numeric(x)) mean(x, na.rm = TRUE) else NA
  })

  # Create comparison data frame
  comparison_df <- data.frame(
    Variable = names(control_means),
    Control_Mean = as.numeric(control_means),
    Treatment_Mean = as.numeric(treatment_means),
    Difference = as.numeric(treatment_means - control_means),
    stringsAsFactors = FALSE
  )

  # Remove rows with NA (non-numeric variables)
  comparison_df <- comparison_df[!is.na(comparison_df$Control_Mean), ]

  return(comparison_df)
}

#' Generate Visualization Plots
#'
#' Creates various plots for the causal analysis results.
#'
#' @param discovery_results Results from bridge_discover()
#' @param mediation_results Results from bridge_mediate()
#' @param data Original data frame
#' @param treatment Character string specifying the treatment variable name
#' @param mediators Character vector specifying the mediator variable names
#' @param outcome Character string specifying the outcome variable name
#'
#' @return A list containing various ggplot objects
#' @export
bridge_plot <- function(discovery_results = NULL,
                        mediation_results = NULL,
                        data = NULL,
                        treatment = NULL,
                        mediators = NULL,
                        outcome = NULL) {

  plots <- list()

  # DAG Comparison Plot
  if (!is.null(discovery_results)) {
    plots$dag_comparison_text <- "DAG plots: Use plot() on discovery_results$researcher_dag and discovery_results$discovered_igraph separately."
  }

  # Bootstrap Distribution Plots
  if (!is.null(mediation_results)) {
    bootstrap_data_list <- list()

    for (effect_name_plot in names(mediation_results$bootstrap_results)) {
      bootstrap_data_list[[effect_name_plot]] <- data.frame(
        Effect_col = effect_name_plot,
        Value_col = mediation_results$bootstrap_results[[effect_name_plot]]
      )
    }

    if (length(bootstrap_data_list) > 0) {
      combined_data <- do.call(rbind, bootstrap_data_list)
      plots$bootstrap_distributions <- ggplot2::ggplot(combined_data, ggplot2::aes(x = .data$Value_col, fill = .data$Effect_col)) +
        ggplot2::geom_histogram(alpha = 0.7, bins = 30, show.legend = FALSE) +
        ggplot2::facet_wrap(~ .data$Effect_col, scales = "free") +
        ggplot2::theme_minimal() +
        ggplot2::labs(title = "Bootstrap Distributions of Effects",
                      x = "Effect Size", y = "Frequency")
    } else {
      plots$bootstrap_distributions <- "No bootstrap data to plot."
    }
  }

  # Effect Size Plot
  if (!is.null(mediation_results)) {
    effect_data_df <- data.frame(
      Effect_col = names(mediation_results$summaries),
      Mean_val = sapply(mediation_results$summaries, function(x) x$mean),
      CI_Lower_val = sapply(mediation_results$summaries, function(x) x$ci_lower),
      CI_Upper_val = sapply(mediation_results$summaries, function(x) x$ci_upper)
    )

    if (nrow(effect_data_df) > 0) {
      plots$effect_sizes <- ggplot2::ggplot(effect_data_df, ggplot2::aes(x = .data$Effect_col, y = .data$Mean_val)) +
        ggplot2::geom_point(size = 3) +
        ggplot2::geom_errorbar(ggplot2::aes(ymin = .data$CI_Lower_val, ymax = .data$CI_Upper_val), width = 0.2) +
        ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
        ggplot2::theme_minimal() +
        ggplot2::labs(title = "Effect Sizes with 95% Confidence Intervals",
                      x = "Effect Type", y = "Effect Size") +
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
    } else {
      plots$effect_sizes <- "No effect summary data to plot."
    }

  }

  return(plots)
}

#' Generate Analysis Summary
#'
#' Creates a comprehensive summary of all analysis results.
#'
#' @param discovery_results Results from causal discovery
#' @param mediation_results Results from mediation analysis
#' @param comparison_results Results from comparative analysis
#' @param sensitivity_results Results from sensitivity analysis
#'
#' @return A formatted summary object
#' @export
bridge_summary <- function(discovery_results = NULL,
                           mediation_results = NULL,
                           comparison_results = NULL,
                           sensitivity_results = NULL) {

  summary_text <- "==== BriDGE CAUSAL ANALYSIS SUMMARY ====\n\n"

  # Discovery Summary
  if (!is.null(discovery_results)) {
    if (!is.null(discovery_results$comparison) && nrow(discovery_results$comparison) > 0) {
      total_edges <- nrow(discovery_results$comparison)
      matching_edges <- sum(discovery_results$comparison$Researcher & discovery_results$comparison$Discovered, na.rm = TRUE)
      # Ensure total_edges is not zero to prevent NaN
      if (total_edges > 0) {
        agreement <- (matching_edges / total_edges) * 100
      } else {
        agreement <- 0 # Or NA, depending on desired behavior for no edges
      }
      summary_text <- paste0(summary_text,
                             "1. CAUSAL DISCOVERY:\n",
                             "   Method: ", discovery_results$method, "\n",
                             "   DAG Agreement: ", round(agreement, 2), "%\n\n")
    } else {
      summary_text <- paste0(summary_text,
                             "1. CAUSAL DISCOVERY:\n",
                             "   Method: ", discovery_results$method, "\n",
                             "   Comparison data not available or empty.\n\n")
    }
  }

  # Mediation Summary
  if (!is.null(mediation_results) && !is.null(mediation_results$summaries)) {
    summary_text <- paste0(summary_text, "2. MEDIATION ANALYSIS:\n")

    for (effect_name in names(mediation_results$summaries)) {
      effect_summary <- mediation_results$summaries[[effect_name]]
      summary_text <- paste0(summary_text,
                             "   ", effect_name, ": ",
                             round(effect_summary$mean, 4),
                             " (95% CI: ", round(effect_summary$ci_lower, 4),
                             " to ", round(effect_summary$ci_upper, 4), ")\n")
    }
    summary_text <- paste0(summary_text, "\n")
  }

  # Comparison Summary
  if (!is.null(comparison_results) && nrow(comparison_results) > 0) {
    summary_text <- paste0(summary_text, "3. COMPARATIVE ANALYSIS:\n")
    for (i in 1:nrow(comparison_results)) {
      row <- comparison_results[i, ]
      summary_text <- paste0(summary_text,
                             "   ", row$Variable, " - Difference: ",
                             round(row$Difference, 4), "\n")
    }
    summary_text <- paste0(summary_text, "\n")
  }

  # Sensitivity Summary
  if (!is.null(sensitivity_results)) {
    summary_text <- paste0(summary_text, "4. SENSITIVITY ANALYSIS:\n")
    summary_text <- paste0(summary_text, "   Perturbation SD: ",
                           sensitivity_results$perturbation_sd, "\n")
    summary_text <- paste0(summary_text, "   Sensitivity analysis performed.\n\n")
  }

  summary_text <- paste0(summary_text, "==== END SUMMARY ====")

  structure(summary_text, class = "bridge_summary")
}

#' Generate Example Data for Testing
#'
#' Generates synthetic RCT data with nonlinear relationships for testing the package.
#'
#' @param n Integer specifying the number of observations
#' @param nonlinear_strength Numeric specifying the strength of nonlinear relationships
#' @param seed Integer for reproducibility
#'
#' @return A data frame with treatment, mediators, and outcome variables
#' @export
bridge_generate_data <- function(n = 1000, nonlinear_strength = 0.5, seed = 42) {
  set.seed(seed) # Use directly, it's a base R function

  treatment <- as.factor(stats::rbinom(n, 1, 0.5)) # stats::rbinom is imported

  mediator_1 <- 1.0 * as.numeric(as.character(treatment)) +
    stats::rnorm(n) + nonlinear_strength * sin(stats::rnorm(n)) # stats::rnorm is imported

  mediator_2 <- 0.8 * as.numeric(as.character(treatment)) +
    stats::rnorm(n) + nonlinear_strength * exp(stats::rnorm(n) / 2) # stats::rnorm is imported

  outcome <- 1.5 * as.numeric(as.character(treatment)) +
    1.2 * mediator_1 +
    0.9 * mediator_2 +
    0.5 * (mediator_1^2) +
    0.3 * mediator_1 * mediator_2 +
    0.5 * as.numeric(as.character(treatment)) * mediator_1 +
    0.7 * as.numeric(as.character(treatment)) * mediator_2 +
    stats::rnorm(n) # stats::rnorm is imported

  data.frame(
    treatment = treatment,
    mediator_1 = mediator_1,
    mediator_2 = mediator_2,
    outcome = outcome
  )
}

# Helper function for DAG comparison (internal, not exported)
compare_dags <- function(researcher_dag, discovered_dag) {
  # igraph::as_edgelist is imported via NAMESPACE
  researcher_edges <- igraph::as_edgelist(researcher_dag)
  discovered_edges <- igraph::as_edgelist(discovered_dag)

  researcher_edges_df <- data.frame(
    From = researcher_edges[,1],
    To = researcher_edges[,2],
    Researcher = TRUE,
    stringsAsFactors = FALSE
  )

  discovered_edges_df <- data.frame(
    From = discovered_edges[,1],
    To = discovered_edges[,2],
    Discovered = TRUE,
    stringsAsFactors = FALSE
  )

  all_edges <- merge(researcher_edges_df, discovered_edges_df,
                     by = c("From", "To"), all = TRUE)

  all_edges$Researcher[is.na(all_edges$Researcher)] <- FALSE
  all_edges$Discovered[is.na(all_edges$Discovered)] <- FALSE

  all_edges
}

# Print method for bridge_analysis objects
#' @export
print.bridge_analysis <- function(x, ...) {
  cat("BriDGE Causal Analysis Results\n")
  cat("===============================\n\n")

  if (!is.null(x$summary)) {
    cat(x$summary)
  }

  cat("\n\nUse summary(object) for detailed summary text or plot(object) for visualizations.")
}

# Print method for bridge_summary objects
#' @export
print.bridge_summary <- function(x, ...) {
  cat(x)
}

# Plot method for bridge_analysis objects
#' @export
plot.bridge_analysis <- function(x, ...) {
  if (!is.null(x$plots)) {
    if (is.character(x$plots$dag_comparison_text)) {
      cat(x$plots$dag_comparison_text, "\n")
    }

    if (!is.null(x$plots$bootstrap_distributions) && !is.character(x$plots$bootstrap_distributions)) {
      print(x$plots$bootstrap_distributions)
    } else if (is.character(x$plots$bootstrap_distributions)) {
      cat(x$plots$bootstrap_distributions, "\n")
    }

    if (!is.null(x$plots$effect_sizes) && !is.character(x$plots$effect_sizes)) {
      print(x$plots$effect_sizes)
    } else if (is.character(x$plots$effect_sizes)) {
      cat(x$plots$effect_sizes, "\n")
    }

  } else {
    cat("No plots available. Set plot = TRUE in bridge_analyze().\n")
  }
}
