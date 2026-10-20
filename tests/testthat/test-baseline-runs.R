test_that("exact runs probabilities match enumeration of binary sequences", {
  for(n1 in 1:4) {
    for(n2 in 1:4) {
      n <- n1 + n2
      all_runs <- apply(combn(n, n1), 2, function(positions) {
        x <- integer(n)
        x[positions] <- 1L
        1L + sum(diff(x) != 0)
      })
      for(r in unique(all_runs)) {
        expected <- min(1, 2 * min(mean(all_runs <= r), mean(all_runs >= r)))
        expect_equal(.exact_runs_pvalue(r, n1, n2), expected,
                     tolerance = 1e-12)
      }
    }
  }
})

test_that("baseline runs detect clustering and alternation", {
  detail <- .detail_baseline_runs(data.frame(
    clustered = rep(0:1, each = 20),
    alternating = rep(0:1, 20)
  ), 0.05)
  expect_equal(detail$Runs, c(2, 40))
  expect_equal(detail$ExpectedRuns, c(21, 21))
  expect_true(all(detail$Significant))
  expect_true(all(grepl("normal approximation", detail$Method)))

  expect_warning(result <- run_checks(
    data.frame(id = 1:40, binary = rep(0:1, each = 20)),
    list(participantID = "id", baseline = list(dichotomous = "binary"))
  ), "Item 2.1: No usable randomisation date")
  item <- result$check_table[result$check_table$ItemNumber == "2.1", ]
  expect_equal(item$Status, "Potential integrity issue")
  expect_match(item$Details, "Wald-Wolfowitz runs test")
  expect_match(item$Details, "dataset row order")
  expect_equal(result$detail_tables[["2.1"]]$Runs, 2)
  expect_equal(result$detail_tables[["2.1"]]$PValue,
               .runs_test_binary(rep(0:1, each = 20))$p_value)
})

test_that("small counts use exact tails and preserve category invariance", {
  x <- c(rep("A", 4), rep("B", 4))
  detail <- .detail_baseline_runs(data.frame(binary = x), 0.05)
  expect_equal(detail$Runs, 2)
  expect_equal(detail$ExpectedRuns, 5)
  expect_equal(detail$PValue, 4 / choose(8, 4))
  expect_false(detail$Significant)
  expect_match(detail$Method, "Exact")
  reversed_levels <- factor(x, levels = c("B", "A"))
  expect_equal(.detail_baseline_runs(data.frame(binary = reversed_levels), 0.05)$PValue,
               detail$PValue)
  expect_equal(.detail_baseline_runs(data.frame(binary = c(0, 1)), 0.05)$PValue, 1)
  at_boundary <- .detail_baseline_runs(data.frame(binary = c(rep(0, 10), rep(1, 30))), 0.05)
  expect_match(at_boundary$Method, "Exact")
})

test_that("missing and untestable baseline sequences are explicit", {
  detail <- .detail_baseline_runs(data.frame(
    binary = c(0, NA, 1, 1, 0, NA),
    constant = rep(1, 6),
    absent = rep(NA_real_, 6),
    three_levels = rep(1:3, 2)
  ), 0.05)
  expect_equal(detail$NonMissing, c(4, 6, 0, 6))
  expect_equal(detail$Missing, c(2, 0, 6, 0))
  expect_equal(detail$Runs[1], 3)
  expect_equal(detail$PValue[1], .runs_test_binary(c(0, 1, 1, 0), exact = TRUE)$p_value)
  expect_true(all(is.na(detail$PValue[-1])))
  expect_true(all(is.na(detail$Significant[-1])))
  expect_true(all(detail$Method[-1] == "Not testable"))
  expect_true(all(grepl("exactly two", detail$Details[-1])))
  summary <- .excessivelly_homogenous_adjacent(
    data.frame(binary = c(0, NA, 1, 1, 0, NA), constant = rep(1, 6)),
    alpha = 0.05
  )
  expect_match(summary$Details, "Could not test variables: constant")
  expect_match(summary$Details, "Missing baseline values were omitted")
  skipped <- .excessivelly_homogenous_adjacent(
    data.frame(constant = rep(1, 6)), alpha = 0.05
  )
  expect_equal(skipped$Status, "Skipped")
})

test_that("the baseline flag uses the requested alpha without rounding", {
  x <- data.frame(binary = c(rep(0, 4), rep(1, 4)))
  p <- .detail_baseline_runs(x, 0.05)$PValue
  expect_equal(.excessivelly_homogenous_adjacent(x, alpha = p)$Status, "Pass")
  expect_equal(.excessivelly_homogenous_adjacent(x, alpha = p + 1e-8)$Status,
               "Potential integrity issue")
})

test_that("allocation sequence tests retain their existing behaviour", {
  x <- data.frame(allocation = rep(0:1, each = 20))
  detail <- .detail_allocation_pattern_tests(x, 0.05)
  expect_equal(detail$Test, c("Adjacent-pairs chi-squared test", "Runs test"))
  expect_equal(detail$PValue[2], signif(.runs_test_binary(x$allocation)$p_value, 4))
  expect_equal(.excessivelly_homogenous_adjacent(x, "intervention", 0.05)$Status,
               "Potential integrity issue")
})

test_that("baseline runs use chronological dates rather than input row order", {
  original <- data.frame(id = 1:40, binary = rep(0:1, each = 20),
                         date = as.Date("2020-01-01") + 1:40)
  shuffled <- original[as.vector(rbind(1:20, 21:40)), ]
  info <- list(participantID = "id", baseline = list(dichotomous = "binary"),
               enrollment = list(randomisation = "date"))
  result <- run_checks(shuffled, info)
  detail <- result$detail_tables[["2.1"]]
  expect_equal(detail$Runs, 2)
  expect_equal(detail$ExcludedMissingDates, 0)
  expect_match(detail$OrderBasis, "Sorted by randomisation date")
  item <- result$check_table[result$check_table$ItemNumber == "2.1", ]
  expect_match(item$Details, "randomisation-date order")
  expect_false(grepl("arrange records chronologically", item$Details))
  expect_identical(shuffled, original[as.vector(rbind(1:20, 21:40)), ])

  shuffled$date <- as.POSIXct(shuffled$date)
  expect_equal(run_checks(shuffled, info)$detail_tables[["2.1"]]$Runs, 2)
  shuffled$date <- rep(as.Date("2020-01-01"), nrow(shuffled))
  tied <- run_checks(shuffled, info)$detail_tables[["2.1"]]
  expect_equal(tied$Runs, 40)
  expect_match(tied$OrderBasis, "ties retain original row order")
})

test_that("missing or unusable randomisation dates warn about row-order fallback", {
  dataset <- data.frame(id = 1:40, binary = rep(0:1, 20))
  info <- list(participantID = "id", baseline = list(dichotomous = "binary"))
  for(date_type in c("absent", "text", "all_missing")) {
    if(date_type != "absent") {
      info$enrollment <- list(randomisation = "date")
      dataset$date <- if(date_type == "text") rep("2020-01-01", 40) else
        rep(as.Date(NA), 40)
    }
    expect_warning(result <- run_checks(dataset, info),
                   "Item 2.1: No usable randomisation date column")
    detail <- result$detail_tables[["2.1"]]
    expect_equal(detail$Runs, 40)
    expect_match(detail$OrderBasis, "Dataset row order")
    item <- result$check_table[result$check_table$ItemNumber == "2.1", ]
    expect_match(item$Details, "Warning: Item 2.1")
  }
})

test_that("partly missing dates are excluded explicitly from baseline runs", {
  dataset <- data.frame(id = 1:40, binary = rep(0:1, each = 20),
                        date = as.Date("2020-01-01") + 1:40)
  dataset$date[1] <- NA
  info <- list(participantID = "id", baseline = list(dichotomous = "binary"),
               enrollment = list(randomisation = "date"))
  expect_warning(result <- run_checks(dataset, info),
                 "Item 2.1: Excluded 1 record with missing randomisation dates")
  detail <- result$detail_tables[["2.1"]]
  expect_equal(detail$Runs, 2)
  expect_equal(detail$NonMissing, 39)
  expect_equal(detail$ExcludedMissingDates, 1)
  expect_match(detail$OrderBasis, "Sorted by randomisation date")
  item <- result$check_table[result$check_table$ItemNumber == "2.1", ]
  expect_match(item$Details, "Excluded 1 record")
})
