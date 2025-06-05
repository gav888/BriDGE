# BriDGE Package - Unit Tests
# ===========================
#
# This file contains unit tests for the BriDGE package using testthat
# Place this file in tests/testthat/ directory of the package

library(testthat)
library(BriDGE)

# =============================================================================
# Test Data Generation
# =============================================================================

test_that("bridge_generate_data works correctly", {
  # Basic functionality
  data <- bridge_generate_data(n = 100, seed = 123)

  # Check structure
  expect_s3_class(data, "data.frame")
  expect_equal(nrow(data), 100)
  expect_equal(ncol(data), 4)
  expect_named(data, c("treatment", "mediator_1", "mediator_2", "outcome"))

  # Check variable types
  expect_s3_class(data$treatment, "factor")
  expect_type(data$mediator_1, "double")
  expect_type(data$mediator_2, "double")
  expect_type(data$outcome, "double")

  # Check treatment levels
  expect_equal(levels(data$treatment), c("0", "1"))

  # Test reproducibility
  data1 <- bridge_generate_data(n = 50, seed = 456)
  data2 <- bridge_generate_data(n = 50, seed = 456)
  expect_identical(data1, data2)
})

# =============================================================================
# Test Causal Discovery
# =============================================================================

test_that("bridge_discover works correctly", {
  # Generate test data
  data <- bridge_generate_data(n = 200, seed = 123)

  # Test MMHC method
  result <- bridge_discover(
    data = data,
    treatment = "treatment",
    mediators = c("mediator_1", "mediator_2"),
    outcome = "outcome",
    method = "mmhc"
  )

  # Check structure
  expect_type(result, "list")
  expect_named(result, c("discovered_dag", "discovered_igraph", "researcher_dag", "comparison", "method"))

  # Check method
  expect_equal(result$method, "mmhc")

  # Check comparison structure
  expect_s3_class(result$comparison, "data.frame")
  expect_true(all(c("From", "To", "Researcher", "Discovered") %in% names(result$comparison)))

  # Test HC method
  result_hc <- bridge_discover(
    data = data,
    treatment = "treatment",
    mediators = c("mediator_1", "mediator_2"),
    outcome = "outcome",
    method = "hc"
  )
  expect_equal(result_hc$method, "hc")

  # Test invalid method
  expect_error(
    bridge_discover(
      data = data,
      treatment = "treatment",
      mediators = c("mediator_1", "mediator_2"),
      outcome = "outcome",
      method = "invalid_method"
    ),
    "Unsupported discovery method"
  )
})

test_that("bridge_discover input validation", {
  data <- bridge_generate_data(n = 100, seed = 123)

  # Test missing variables
  expect_error(
    bridge_discover(
      data = data,
      treatment = "nonexistent",
      mediators = c("mediator_1", "mediator_2"),
      outcome = "outcome"
    )
  )
})

# =============================================================================
# Test Mediation Analysis
# =============================================================================

test_that("bridge_mediate works correctly", {
  # Generate test data
  data <- bridge_generate_data(n = 200, seed = 123)

  # Test basic functionality
  result <- bridge_mediate(
    data = data,
    treatment = "treatment",
    mediators = c("mediator_1", "mediator_2"),
    outcome = "outcome",
    n_bootstraps = 10,  # Small number for testing
    nonlinear = TRUE,
    parallel = FALSE    # Disable parallel for testing
  )

  # Check structure
  expect_type(result, "list")
  expect_named(result, c("summaries", "bootstrap_results", "n_bootstraps", "method"))
  expect_equal(result$n_bootstraps, 10)
  expect_equal(result$method, "GAM")

  # Check summaries structure
  expect_type(result$summaries, "list")
  expect_true(length(result$summaries) >= 3)  # At least direct, total, and one indirect

  # Check each summary has required elements
  for (summary in result$summaries) {
    expect_named(summary, c("mean", "ci_lower", "ci_upper", "effects"))
    expect_type(summary$mean, "double")
    expect_type(summary$ci_lower, "double")
    expect_type(summary$ci_upper, "double")
    expect_type(summary$effects, "double")
    expect_equal(length(summary$effects), 10)
  }

  # Test linear method
  result_linear <- bridge_mediate(
    data = data,
    treatment = "treatment",
    mediators = c("mediator_1", "mediator_2"),
    outcome = "outcome",
    n_bootstraps = 10,
    nonlinear = FALSE,
    parallel = FALSE
  )
  expect_equal(result_linear$method, "Linear")
})

test_that("bridge_mediate handles single mediator", {
  data <- bridge_generate_data(n = 100, seed = 123)

  result <- bridge_mediate(
    data = data,
    treatment = "treatment",
    mediators = "mediator_1",
    outcome = "outcome",
    n_bootstraps = 5,
    parallel = FALSE
  )

  expect_type(result, "list")
  expect_true("nie_mediator_1" %in% names(result$summaries))
})

# =============================================================================
# Test Comparative Analysis
# =============================================================================

test_that("bridge_compare works correctly", {
  data <- bridge_generate_data(n = 200, seed = 123)

  result <- bridge_compare(
    data = data,
    treatment = "treatment",
    mediators = c("mediator_1", "mediator_2"),
    outcome = "outcome"
  )

  # Check structure
  expect_s3_class(result, "data.frame")
  expect_named(result, c("Variable", "Control_Mean", "Treatment_Mean", "Difference"))
  expect_equal(nrow(result), 3)  # Two mediators + outcome

  # Check data types
  expect_type(result$Control_Mean, "double")
  expect_type(result$Treatment_Mean, "double")
  expect_type(result$Difference, "double")

  # Check that differences are calculated correctly
  expect_equal(result$Difference, result$Treatment_Mean - result$Control_Mean)
})

# =============================================================================
# Test Sensitivity Analysis
# =============================================================================

test_that("bridge_sensitivity works correctly", {
  data <- bridge_generate_data(n = 100, seed = 123)

  result <- bridge_sensitivity(
    data = data,
    treatment = "treatment",
    mediators = c("mediator_1", "mediator_2"),
    outcome = "outcome",
    n_bootstraps = 5,
    parallel = FALSE,
    perturbation_sd = 0.1
  )

  # Check structure
  expect_type(result, "list")
  expect_named(result, c("perturbed_results", "perturbation_sd"))
  expect_equal(result$perturbation_sd, 0.1)

  # Check perturbed results structure
  expect_type(result$perturbed_results, "list")
  expect_named(result$perturbed_results, c("summaries", "bootstrap_results", "n_bootstraps", "method"))
})

# =============================================================================
# Test Complete Analysis Pipeline
# =============================================================================

test_that("bridge_analyze works correctly", {
  data <- bridge_generate_data(n = 150, seed = 123)

  # Test complete analysis
  result <- bridge_analyze(
    data = data,
    treatment = "treatment",
    mediators = c("mediator_1", "mediator_2"),
    outcome = "outcome",
    n_bootstraps = 10,
    discovery_method = "mmhc",
    nonlinear = TRUE,
    plot = FALSE,      # Disable plotting for testing
    parallel = FALSE,  # Disable parallel for testing
    sensitivity = TRUE
  )

  # Check structure
  expect_s3_class(result, "bridge_analysis")
  expect_named(result, c("discovery", "mediation", "comparison", "sensitivity", "plots", "summary", "call"))

  # Check individual components
  expect_type(result$discovery, "list")
  expect_type(result$mediation, "list")
  expect_s3_class(result$comparison, "data.frame")
  expect_type(result$sensitivity, "list")
  expect_s3_class(result$summary, "bridge_summary")

  # Check that call is stored
  expect_type(result$call, "language")
})

test_that("bridge_analyze input validation", {
  # Test with non-data.frame input
  expect_error(
    bridge_analyze(
      data = matrix(1:20, nrow = 5),
      treatment = "treatment",
      mediators = c("mediator_1", "mediator_2"),
      outcome = "outcome"
    ),
    "Data must be a data frame"
  )

  # Test with missing variables
  data <- bridge_generate_data(n = 100, seed = 123)
  expect_error(
    bridge_analyze(
      data = data,
      treatment = "nonexistent",
      mediators = c("mediator_1", "mediator_2"),
      outcome = "outcome"
    ),
    "Missing variables in data"
  )
})

# =============================================================================
# Test Plotting Functions
# =============================================================================

test_that("bridge_plot works correctly", {
  data <- bridge_generate_data(n = 100, seed = 123)

  # Generate required results
  discovery_results <- bridge_discover(
    data = data,
    treatment = "treatment",
    mediators = c("mediator_1", "mediator_2"),
    outcome = "outcome"
  )

  mediation_results <- bridge_mediate(
    data = data,
    treatment = "treatment",
    mediators = c("mediator_1", "mediator_2"),
    outcome = "outcome",
    n_bootstraps = 10,
    parallel = FALSE
  )

  # Test plotting
  plots <- bridge_plot(
    discovery_results = discovery_results,
    mediation_results = mediation_results,
    data = data,
    treatment = "treatment",
    mediators = c("mediator_1", "mediator_2"),
    outcome = "outcome"
  )

  expect_type(plots, "list")
  expect_true("bootstrap_distributions" %in% names(plots))
  expect_true("effect_sizes" %in% names(plots))
})

# =============================================================================
# Test Summary Functions
# =============================================================================

test_that("bridge_summary works correctly", {
  data <- bridge_generate_data(n = 100, seed = 123)

  # Generate required results
  discovery_results <- bridge_discover(
    data = data,
    treatment = "treatment",
    mediators = c("mediator_1", "mediator_2"),
    outcome = "outcome"
  )

  mediation_results <- bridge_mediate(
    data = data,
    treatment = "treatment",
    mediators = c("mediator_1", "mediator_2"),
    outcome = "outcome",
    n_bootstraps = 10,
    parallel = FALSE
  )

  comparison_results <- bridge_compare(
    data = data,
    treatment = "treatment",
    mediators = c("mediator_1", "mediator_2"),
    outcome = "outcome"
  )

  # Test summary
  summary_result <- bridge_summary(
    discovery_results = discovery_results,
    mediation_results = mediation_results,
    comparison_results = comparison_results,
    sensitivity_results = NULL
  )

  expect_s3_class(summary_result, "bridge_summary")
  expect_type(summary_result, "character")
  expect_true(nchar(summary_result) > 0)
})

# =============================================================================
# Test Print Methods
# =============================================================================

test_that("print methods work correctly", {
  data <- bridge_generate_data(n = 50, seed = 123)

  # Test bridge_analysis print method
  result <- bridge_analyze(
    data = data,
    treatment = "treatment",
    mediators = c("mediator_1", "mediator_2"),
    outcome = "outcome",
    n_bootstraps = 5,
    plot = FALSE,
    parallel = FALSE,
    sensitivity = FALSE
  )

  # These should not error
  expect_output(print(result))
  expect_output(print(result$summary))
})

# =============================================================================
# Test Error Handling and Edge Cases
# =============================================================================

test_that("functions handle edge cases", {
  # Test with very small sample size
  small_data <- bridge_generate_data(n = 20, seed = 123)

  # This should work but might give warnings
  expect_warning({
    result <- bridge_mediate(
      data = small_data,
      treatment = "treatment",
      mediators = c("mediator_1", "mediator_2"),
      outcome = "outcome",
      n_bootstraps = 5,
      parallel = FALSE
    )
  }, NA)  # NA means no warning expected, but we're allowing warnings

  # Test with single observation per group (should error or handle gracefully)
  tiny_data <- data.frame(
    treatment = factor(c("0", "1")),
    mediator_1 = c(1, 2),
    mediator_2 = c(1, 2),
    outcome = c(1, 2)
  )

  # This might error, which is acceptable
  expect_error({
    bridge_mediate(
      data = tiny_data,
      treatment = "treatment",
      mediators = c("mediator_1", "mediator_2"),
      outcome = "outcome",
      n_bootstraps = 2,
      parallel = FALSE
    )
  })
})

test_that("functions handle missing data appropriately", {
  data <- bridge_generate_data(n = 100, seed = 123)

  # Introduce missing values
  data$mediator_1[1:5] <- NA
  data$outcome[6:10] <- NA

  # Functions should handle this appropriately (either by removing NAs or erroring)
  # The exact behavior depends on implementation, but should not crash unexpectedly
  expect_error({
    bridge_compare(
      data = data,
      treatment = "treatment",
      mediators = c("mediator_1", "mediator_2"),
      outcome = "outcome"
    )
  })
})

# =============================================================================
# Test Package Integration
# =============================================================================

test_that("package functions work together", {
  # Test that all functions can be called in sequence without errors
  data <- bridge_generate_data(n = 100, seed = 123)

  # Discovery
  discovery <- bridge_discover(
    data = data,
    treatment = "treatment",
    mediators = c("mediator_1", "mediator_2"),
    outcome = "outcome"
  )

  # Mediation
  mediation <- bridge_mediate(
    data = data,
    treatment = "treatment",
    mediators = c("mediator_1", "mediator_2"),
    outcome = "outcome",
    n_bootstraps = 5,
    parallel = FALSE
  )

  # Comparison
  comparison <- bridge_compare(
    data = data,
    treatment = "treatment",
    mediators = c("mediator_1", "mediator_2"),
    outcome = "outcome"
  )

  # Summary
  summary_result <- bridge_summary(
    discovery_results = discovery,
    mediation_results = mediation,
    comparison_results = comparison
  )

  # All should work without error
  expect_type(discovery, "list")
  expect_type(mediation, "list")
  expect_s3_class(comparison, "data.frame")
  expect_s3_class(summary_result, "bridge_summary")
})

# =============================================================================
# Performance Tests
# =============================================================================

test_that("functions complete in reasonable time", {
  data <- bridge_generate_data(n = 200, seed = 123)

  # Test that mediation analysis completes in reasonable time
  start_time <- Sys.time()
  result <- bridge_mediate(
    data = data,
    treatment = "treatment",
    mediators = c("mediator_1", "mediator_2"),
    outcome = "outcome",
    n_bootstraps = 20,
    parallel = FALSE
  )
  end_time <- Sys.time()

  # Should complete within 30 seconds on most systems
  expect_lt(as.numeric(end_time - start_time, units = "secs"), 30)
})

# =============================================================================
# Run Tests
# =============================================================================

# To run these tests, use:
# devtools::test()
# or
# testthat::test_dir("tests/testthat/")

cat("BriDGE package unit tests defined.\n")
cat("Run with: devtools::test() or testthat::test_dir('tests/testthat/')\n")
