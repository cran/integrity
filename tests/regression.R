library(integrity)

internal <- function(name) getFromNamespace(name, "integrity")
item <- function(result, number) {
  result$check_table[result$check_table$ItemNumber == number, , drop = FALSE]
}

# Removing an all-missing column must preserve data-frame structure.
prepared <- internal(".prepare_data")(
  data.frame(id = 1:8, unused = rep(NA_real_, 8)),
  list(participantID = "id")
)
stopifnot(is.data.frame(prepared), identical(names(prepared), "id"))
result <- run_checks(
  data.frame(id = 1:8, unused = rep(NA_real_, 8)),
  list(participantID = "id", unexpected = list(id = "greater than 4"))
)
stopifnot(item(result, "6.1")$Status == "Potential integrity issue")

# Numeric metadata must override automatic categorical type inference.
numeric_data <- data.frame(group = rep(c("A", "B"), each = 6), value = rep(1:3, 4))
numeric_detail <- suppressWarnings(internal(".detail_excessivelly_different")(
  numeric_data, "group", "numeric"
))
stopifnot(
  nrow(numeric_detail) == 1L,
  numeric_detail$Variable == "value",
  !any(grepl("%", unlist(numeric_detail[, 2:3]))),
  is.finite(numeric_detail$PValue)
)

# Undefined tests must not produce an unconditional Pass.
result <- suppressWarnings(run_checks(
  data.frame(id = 1:12, constant = rep(0, 12)),
  list(participantID = "id", baseline = list(dichotomous = "constant"))
))
stopifnot(item(result, "2.1")$Status == "Skipped")
mixed_binary <- suppressWarnings(internal(".excessivelly_homogenous_adjacent")(
  data.frame(constant = rep(0, 12), variable = rep(0:1, 6)), alpha = 0.05
))
stopifnot(grepl("Could not test variables: constant", mixed_binary$Details))
missing_sequence <- internal(".excessivelly_homogenous_adjacent")(
  data.frame(binary = c(0, NA, 1)), alpha = 0.05
)
stopifnot(missing_sequence$Status == "Pass",
          grepl("Missing baseline values were omitted", missing_sequence$Details))

# A single participant must not collapse the rare-outcome matrix.
result <- run_checks(
  data.frame(id = 1, value = 10, event = 1),
  list(participantID = "id", baseline = list(numeric = "value"),
       outcome = list(rare = list(numeric = "event")))
)
stopifnot(nrow(item(result, "1.3")) == 1L)

# Allocation curves must retain group totals and aggregate tied dates.
allocation_data <- data.frame(
  id = 1:30,
  group = c(rep("A", 20), rep("B", 10)),
  date = as.Date("2020-01-01") + rep(0:4, 6)
)
allocation_info <- list(participantID = "id", intervention = "group",
                        enrollment = list(randomisation = "date"))
result <- suppressWarnings(run_checks(allocation_data, allocation_info))
counts <- result$images[["Cumulative Allocation"]]$data
stopifnot(
  nrow(counts) == 10L,
  max(counts$count[counts$intervention == "A"]) == 20,
  max(counts$count[counts$intervention == "B"]) == 10,
  counts$count[counts$intervention == "A"][1] == 4,
  counts$count[counts$intervention == "B"][1] == 2
)
allocation_info$enrollment$randomisation <- NULL
result <- suppressWarnings(run_checks(allocation_data, allocation_info))
stopifnot(
  item(result, "5.1")$Status == "Skipped",
  item(result, "5.3")$Status == "Skipped",
  !any(c("Cumulative Allocation", "Days") %in% names(result$images))
)

# Different missingness/frequencies should produce distinct panel scales.
terminal_plot <- internal(".terminal_digits")(
  data.frame(frequent = rep(1, 20), sparse = c(1:10, rep(NA, 10)))
)
terminal_build <- ggplot2::ggplot_build(terminal_plot)
panel_maxima <- vapply(terminal_build$layout$panel_params,
                       function(panel) max(panel$y.range), numeric(1))
stopifnot(terminal_plot$facet$params$free$y, length(unique(panel_maxima)) == 2L)
