# Regression tests for generalized treatment coding and reproducibility
# =======================================================================
# Prior to v1.0.0, treatment levels "0"/"1" were hardcoded in bridge_mediate()
# and bridge_compare(), silently breaking data coded e.g. "control"/"treatment".

make_labeled_data <- function(n = 150, seed = 123) {
  data <- bridge_generate_data(n = n, seed = seed)
  data$treatment <- factor(
    ifelse(data$treatment == "1", "treatment", "control"),
    levels = c("control", "treatment")
  )
  data
}

test_that("bridge_mediate works with non-numeric treatment labels", {
  data <- make_labeled_data()

  result <- bridge_mediate(
    data = data,
    treatment = "treatment",
    mediators = c("mediator_1", "mediator_2"),
    outcome = "outcome",
    n_bootstraps = 5,
    nonlinear = FALSE,
    parallel = FALSE
  )

  expect_type(result, "list")
  for (summary in result$summaries) {
    expect_false(is.na(summary$mean))
  }

  # Labeled coding must give the same estimates as 0/1 coding of the same data
  data_numeric <- bridge_generate_data(n = 150, seed = 123)
  result_numeric <- bridge_mediate(
    data = data_numeric,
    treatment = "treatment",
    mediators = c("mediator_1", "mediator_2"),
    outcome = "outcome",
    n_bootstraps = 5,
    nonlinear = FALSE,
    parallel = FALSE
  )
  expect_equal(result$summaries$direct_effect$mean,
               result_numeric$summaries$direct_effect$mean)
  expect_equal(result$summaries$total_effect$mean,
               result_numeric$summaries$total_effect$mean)
})

test_that("bridge_compare works with non-numeric treatment labels", {
  data <- make_labeled_data()

  result <- bridge_compare(
    data = data,
    treatment = "treatment",
    mediators = c("mediator_1", "mediator_2"),
    outcome = "outcome"
  )

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 3)
  expect_false(any(is.na(result$Difference)))
})

test_that("treatment variables without exactly 2 levels are rejected", {
  data <- bridge_generate_data(n = 100, seed = 123)
  data$treatment <- factor(sample(c("a", "b", "c"), 100, replace = TRUE))

  expect_error(
    bridge_mediate(
      data = data,
      treatment = "treatment",
      mediators = c("mediator_1", "mediator_2"),
      outcome = "outcome",
      n_bootstraps = 2,
      parallel = FALSE
    ),
    "exactly 2 levels"
  )
  expect_error(
    bridge_compare(
      data = data,
      treatment = "treatment",
      mediators = c("mediator_1", "mediator_2"),
      outcome = "outcome"
    ),
    "exactly 2 levels"
  )
})

test_that("bootstrap results are reproducible via the seed argument", {
  data <- bridge_generate_data(n = 100, seed = 42)

  run <- function(seed) {
    bridge_mediate(
      data = data,
      treatment = "treatment",
      mediators = "mediator_1",
      outcome = "outcome",
      n_bootstraps = 5,
      nonlinear = FALSE,
      parallel = FALSE,
      seed = seed
    )
  }

  expect_equal(run(1)$summaries, run(1)$summaries)
  expect_false(identical(run(1)$bootstrap_results$direct_effect,
                         run(2)$bootstrap_results$direct_effect))
})

test_that("summary method returns the bridge_summary object", {
  data <- bridge_generate_data(n = 100, seed = 123)

  result <- bridge_analyze(
    data = data,
    treatment = "treatment",
    mediators = "mediator_1",
    outcome = "outcome",
    n_bootstraps = 5,
    plot = FALSE,
    parallel = FALSE,
    sensitivity = FALSE
  )

  s <- summary(result)
  expect_s3_class(s, "bridge_summary")
  expect_output(print(s), "BriDGE CAUSAL ANALYSIS SUMMARY")
})
