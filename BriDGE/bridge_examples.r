# BriDGE Package - Complete Examples and Usage Guide
# ================================================

# This script demonstrates all the features of the BriDGE package
# for causal analysis of RCT data

# Install and load the package
# devtools::install_github("username/BriDGE")
library(BriDGE)
library(ggplot2)

# =============================================================================
# EXAMPLE 1: Basic Usage with Generated Data
# =============================================================================

cat("Example 1: Basic Usage with Generated Data\n")
cat("==========================================\n\n")

# Generate synthetic RCT data
set.seed(123)
data <- bridge_generate_data(
  n = 1500,                    # Sample size
  nonlinear_strength = 0.5,    # Strength of nonlinear relationships
  seed = 123                   # For reproducibility
)

# Explore the generated data
cat("Data Structure:\n")
str(data)
cat("\nData Summary:\n")
summary(data)

# Quick visualization of the data
if (require(ggplot2)) {
  p1 <- ggplot(data, aes(x = treatment, y = outcome, fill = treatment)) +
    geom_boxplot() +
    theme_minimal() +
    ggtitle("Outcome by Treatment Group")
  
  print(p1)
}

# Perform complete causal analysis
cat("\nRunning complete BriDGE analysis...\n")
results_basic <- bridge_analyze(
  data = data,
  treatment = "treatment",
  mediators = c("mediator_1", "mediator_2"),
  outcome = "outcome",
  n_bootstraps = 200,          # Reduced for faster execution
  discovery_method = "mmhc",
  nonlinear = TRUE,
  parallel = TRUE,
  sensitivity = TRUE
)

# View results
cat("\nBasic Analysis Results:\n")
print(results_basic)

# =============================================================================
# EXAMPLE 2: Step-by-Step Analysis
# =============================================================================

cat("\n\nExample 2: Step-by-Step Analysis\n")
cat("=================================\n\n")

# Step 1: Causal Discovery
cat("Step 1: Causal Discovery\n")
discovery_results <- bridge_discover(
  data = data,
  treatment = "treatment",
  mediators = c("mediator_1", "mediator_2"),
  outcome = "outcome",
  method = "mmhc",
  discretize = TRUE,
  n_bins = 5
)

cat("Discovery Method:", discovery_results$method, "\n")
cat("DAG Comparison:\n")
print(discovery_results$comparison)

# Step 2: Mediation Analysis
cat("\nStep 2: Mediation Analysis\n")
mediation_results <- bridge_mediate(
  data = data,
  treatment = "treatment",
  mediators = c("mediator_1", "mediator_2"),
  outcome = "outcome",
  n_bootstraps = 300,
  nonlinear = TRUE,
  parallel = TRUE
)

cat("Mediation Effects:\n")
for (effect_name in names(mediation_results$summaries)) {
  effect <- mediation_results$summaries[[effect_name]]
  cat(sprintf("  %s: %.4f (95%% CI: %.4f to %.4f)\n", 
              effect_name, effect$mean, effect$ci_lower, effect$ci_upper))
}

# Step 3: Comparative Analysis
cat("\nStep 3: Comparative Analysis\n")
comparison_results <- bridge_compare(
  data = data,
  treatment = "treatment",
  mediators = c("mediator_1", "mediator_2"),
  outcome = "outcome"
)

print(comparison_results)

# Step 4: Sensitivity Analysis
cat("\nStep 4: Sensitivity Analysis\n")
sensitivity_results <- bridge_sensitivity(
  data = data,
  treatment = "treatment",
  mediators = c("mediator_1", "mediator_2"),
  outcome = "outcome",
  n_bootstraps = 100,
  perturbation_sd = 0.1
)

cat("Original vs Perturbed Results:\n")
original_effects <- mediation_results$summaries
perturbed_effects <- sensitivity_results$perturbed_results$summaries

for (effect_name in names(original_effects)) {
  original <- original_effects[[effect_name]]$mean
  perturbed <- perturbed_effects[[effect_name]]$mean
  difference <- abs(original - perturbed)
  cat(sprintf("  %s: Original=%.4f, Perturbed=%.4f, Diff=%.4f\n",
              effect_name, original, perturbed, difference))
}

# Step 5: Create Visualizations
cat("\nStep 5: Creating Visualizations\n")
plots <- bridge_plot(
  discovery_results = discovery_results,
  mediation_results = mediation_results,
  data = data,
  treatment = "treatment",
  mediators = c("mediator_1", "mediator_2"),
  outcome = "outcome"
)

# Display plots
if (!is.null(plots$bootstrap_distributions)) {
  print(plots$bootstrap_distributions)
}

if (!is.null(plots$effect_sizes)) {
  print(plots$effect_sizes)
}

# =============================================================================
# EXAMPLE 3: Comparing Different Methods
# =============================================================================

cat("\n\nExample 3: Comparing Different Methods\n")
cat("======================================\n\n")

# Compare different causal discovery methods
cat("Comparing Causal Discovery Methods:\n")

methods <- c("mmhc", "hc", "pc")
discovery_comparison <- list()

for (method in methods) {
  cat(paste("Testing method:", method, "\n"))
  
  tryCatch({
    discovery_comparison[[method]] <- bridge_discover(
      data = data,
      treatment = "treatment",
      mediators = c("mediator_1", "mediator_2"),
      outcome = "outcome",
      method = method
    )
    
    # Calculate agreement percentage
    comparison <- discovery_comparison[[method]]$comparison
    total_edges <- nrow(comparison)
    matching_edges <- sum(comparison$Researcher & comparison$Discovered)
    agreement <- (matching_edges / total_edges) * 100
    
    cat(paste("  Agreement with researcher's DAG:", round(agreement, 2), "%\n"))
    
  }, error = function(e) {
    cat(paste("  Error with method", method, ":", e$message, "\n"))
  })
}

# Compare linear vs nonlinear mediation models
cat("\nComparing Linear vs Nonlinear Models:\n")

linear_mediation <- bridge_mediate(
  data = data,
  treatment = "treatment",
  mediators = c("mediator_1", "mediator_2"),
  outcome = "outcome",
  n_bootstraps = 200,
  nonlinear = FALSE,
  parallel = TRUE
)

nonlinear_mediation <- bridge_mediate(
  data = data,
  treatment = "treatment",
  mediators = c("mediator_1", "mediator_2"),
  outcome = "outcome",
  n_bootstraps = 200,
  nonlinear = TRUE,
  parallel = TRUE
)

cat("Linear Model Results:\n")
for (effect_name in names(linear_mediation$summaries)) {
  effect <- linear_mediation$summaries[[effect_name]]
  cat(sprintf("  %s: %.4f\n", effect_name, effect$mean))
}

cat("Nonlinear Model Results:\n")
for (effect_name in names(nonlinear_mediation$summaries)) {
  effect <- nonlinear_mediation$summaries[[effect_name]]
  cat(sprintf("  %s: %.4f\n", effect_name, effect$mean))
}

# =============================================================================
# EXAMPLE 4: Working with Your Own Data
# =============================================================================

cat("\n\nExample 4: Working with Your Own Data\n")
cat("=====================================\n\n")

# Simulate loading your own data
cat("# Example of how to use BriDGE with your own RCT data:\n\n")

cat("# 1. Load your data\n")
cat("your_data <- read.csv('your_rct_data.csv')\n\n")

cat("# 2. Prepare your data\n")
cat("# Ensure treatment is a factor\n")
cat("your_data$treatment <- as.factor(your_data$treatment)\n\n")

cat("# Handle missing values if any\n")
cat("your_data <- na.omit(your_data)  # or use imputation\n\n")

cat("# 3. Run BriDGE analysis\n")
cat("results <- bridge_analyze(\n")
cat("  data = your_data,\n")
cat("  treatment = 'treatment_column_name',\n")
cat("  mediators = c('mediator1', 'mediator2', 'mediator3'),\n")
cat("  outcome = 'outcome_column_name',\n")
cat("  n_bootstraps = 500\n")
cat(")\n\n")

cat("# 4. Examine results\n")
cat("print(results)\n")
cat("plot(results)\n\n")

# Create a more complex example dataset
cat("Creating a more complex example dataset...\n")

set.seed(456)
complex_data <- data.frame(
  # Demographics
  age = rnorm(1000, 45, 15),
  gender = factor(rbinom(1000, 1, 0.5), labels = c("Female", "Male")),
  
  # Treatment assignment (randomized)
  treatment = factor(rbinom(1000, 1, 0.5), labels = c("Control", "Treatment")),
  
  # Multiple mediators
  stress_level = NA,
  sleep_quality = NA,
  social_support = NA,
  
  # Outcome
  wellbeing_score = NA
)

# Generate mediators with complex relationships
treat_numeric <- as.numeric(complex_data$treatment) - 1
age_centered <- scale(complex_data$age)[,1]
gender_numeric <- as.numeric(complex_data$gender) - 1

complex_data$stress_level <- 3 + 
  1.5 * treat_numeric + 
  0.3 * age_centered + 
  0.8 * gender_numeric +
  rnorm(1000, 0, 1)

complex_data$sleep_quality <- 7 - 
  0.8 * complex_data$stress_level + 
  1.2 * treat_numeric +
  0.2 * age_centered +
  rnorm(1000, 0, 0.8)

complex_data$social_support <- 5 + 
  1.0 * treat_numeric +
  0.3 * (complex_data$stress_level)^0.5 +
  0.4 * gender_numeric +
  rnorm(1000, 0, 0.9)

# Generate outcome with nonlinear relationships
complex_data$wellbeing_score <- 50 + 
  2.0 * treat_numeric +
  -1.5 * complex_data$stress_level +
  1.8 * complex_data$sleep_quality +
  1.2 * complex_data$social_support +
  0.5 * (complex_data$sleep_quality)^2 +
  -0.3 * complex_data$stress_level * complex_data$sleep_quality +
  0.8 * treat_numeric * complex_data$social_support +
  rnorm(1000, 0, 2)

# Analyze complex data
cat("Analyzing complex dataset...\n")
complex_results <- bridge_analyze(
  data = complex_data,
  treatment = "treatment",
  mediators = c("stress_level", "sleep_quality", "social_support"),
  outcome = "wellbeing_score",
  n_bootstraps = 200
)

print(complex_results)

# =============================================================================
# EXAMPLE 5: Advanced Customization
# =============================================================================

cat("\n\nExample 5: Advanced Customization\n")
cat("==================================\n\n")

# Custom sensitivity analysis with multiple perturbation levels
perturbation_levels <- c(0.05, 0.1, 0.15, 0.2)
sensitivity_comparison <- list()

cat("Testing sensitivity at different perturbation levels:\n")
for (pert_level in perturbation_levels) {
  cat(paste("Perturbation SD:", pert_level, "\n"))
  
  sens_result <- bridge_sensitivity(
    data = data,
    treatment = "treatment",
    mediators = c("mediator_1", "mediator_2"),
    outcome = "outcome",
    n_bootstraps = 100,
    perturbation_sd = pert_level
  )
  
  sensitivity_comparison[[paste0("pert_", pert_level)]] <- sens_result
  
  # Compare direct effect
  original_direct <- mediation_results$summaries$direct_effect$mean
  perturbed_direct <- sens_result$perturbed_results$summaries$direct_effect$mean
  difference <- abs(original_direct - perturbed_direct)
  
  cat(paste("  Direct effect difference:", round(difference, 4), "\n"))
}

# Bootstrap convergence analysis
cat("\nBootstrap Convergence Analysis:\n")
bootstrap_sizes <- c(50, 100, 200, 500)
convergence_results <- list()

for (n_boot in bootstrap_sizes) {
  cat(paste("Testing with", n_boot, "bootstrap samples...\n"))
  
  conv_result <- bridge_mediate(
    data = data,
    treatment = "treatment",
    mediators = c("mediator_1", "mediator_2"),
    outcome = "outcome",
    n_bootstraps = n_boot,
    nonlinear = TRUE,
    parallel = TRUE
  )
  
  convergence_results[[paste0("n_", n_boot)]] <- conv_result$summaries$direct_effect$mean
}

cat("Direct effect estimates by bootstrap sample size:\n")
for (n_boot in bootstrap_sizes) {
  cat(sprintf("  %d samples: %.4f\n", n_boot, convergence_results[[paste0("n_", n_boot)]]))
}

# =============================================================================
# EXAMPLE 6: Diagnostic and Validation
# =============================================================================

cat("\n\nExample 6: Diagnostic and Validation\n")
cat("====================================\n\n")

# Data quality checks
cat("Data Quality Diagnostics:\n")

# Check for missing values
missing_counts <- sapply(data, function(x) sum(is.na(x)))
cat("Missing values per variable:\n")
print(missing_counts)

# Check treatment balance
treatment_table <- table(data$treatment)
cat("\nTreatment group sizes:\n")
print(treatment_table)

# Check for outliers (using IQR method)
numeric_vars <- c("mediator_1", "mediator_2", "outcome")
for (var in numeric_vars) {
  Q1 <- quantile(data[[var]], 0.25)
  Q3 <- quantile(data[[var]], 0.75)
  IQR <- Q3 - Q1
  lower_bound <- Q1 - 1.5 * IQR
  upper_bound <- Q3 + 1.5 * IQR
  outliers <- sum(data[[var]] < lower_bound | data[[var]] > upper_bound)
  cat(paste("Outliers in", var, ":", outliers, "\n"))
}

# Model validation using cross-validation approach
cat("\nModel Validation using Data Splitting:\n")

set.seed(789)
train_indices <- sample(1:nrow(data), size = floor(0.7 * nrow(data)))
train_data <- data[train_indices, ]
test_data <- data[-train_indices, ]

# Train on training set
train_results <- bridge_mediate(
  data = train_data,
  treatment = "treatment",
  mediators = c("mediator_1", "mediator_2"),
  outcome = "outcome",
  n_bootstraps = 200,
  nonlinear = TRUE,
  parallel = TRUE
)

# Test on test set
test_results <- bridge_mediate(
  data = test_data,
  treatment = "treatment",
  mediators = c("mediator_1", "mediator_2"),
  outcome = "outcome",
  n_bootstraps = 200,
  nonlinear = TRUE,
  parallel = TRUE
)

# Compare results
cat("Training vs Test Set Results:\n")
for (effect_name in names(train_results$summaries)) {
  train_effect <- train_results$summaries[[effect_name]]$mean
  test_effect <- test_results$summaries[[effect_name]]$mean
  difference <- abs(train_effect - test_effect)
  cat(sprintf("  %s - Train: %.4f, Test: %.4f, Diff: %.4f\n",
              effect_name, train_effect, test_effect, difference))
}

# =============================================================================
# SUMMARY AND RECOMMENDATIONS
# =============================================================================

cat("\n\nSUMMARY AND RECOMMENDATIONS\n")
cat("============================\n\n")

cat("BriDGE Package Usage Summary:\n\n")

cat("1. BASIC USAGE:\n")
cat("   - Use bridge_analyze() for complete analysis pipeline\n")
cat("   - Ensure treatment variable is a factor\n")
cat("   - Start with reasonable bootstrap samples (200-500)\n\n")

cat("2. CAUSAL DISCOVERY:\n")
cat("   - MMHC generally works well for RCT data\n")
cat("   - Compare multiple methods when possible\n")
cat("   - Higher agreement indicates more reliable structure\n\n")

cat("3. MEDIATION ANALYSIS:\n")
cat("   - Use nonlinear models for complex relationships\n")
cat("   - Bootstrap confidence intervals provide uncertainty quantification\n")
cat("   - Consider multiple mediators simultaneously\n\n")

cat("4. SENSITIVITY ANALYSIS:\n")
cat("   - Small differences indicate robust results\n")
cat("   - Test multiple perturbation levels\n")
cat("   - Large sensitivity may indicate model instability\n\n")

cat("5. PERFORMANCE TIPS:\n")
cat("   - Use parallel processing for faster computation\n")
cat("   - Reduce bootstrap samples for initial exploration\n")
cat("   - Consider data size when setting parameters\n\n")

cat("6. INTERPRETATION:\n")
cat("   - Direct effects: treatment impact not through mediators\n")
cat("   - Indirect effects: treatment impact through specific mediators\n")
cat("   - Total effect: sum of all pathways\n")
cat("   - Confidence intervals indicate statistical significance\n\n")

cat("Analysis completed successfully!\n")
cat("Use the BriDGE package documentation for more detailed information.\n")