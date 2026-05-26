dataset_info <- list(
  participantID = "infant_ID",
  enrollment = list(
    start = "enrol_start",
    randomisation = "rand_date",
    end = "enrol_end"
  ),
  baseline = list(
    dichotomous = "sex",
    numeric = c("mat_age", "GA_weeks", "birthweight")
  ),
  intervention = "treatment_cat",
  outcome = list(
    common = list(
      dichotomous = c("IVH", "NEC"),
      polytomous = "CLD"
    ),
    rare = list(
      dichotomous = "inf_death"
    )
  ),
  correlated = list(
    timeAndSize = c("GA_weeks", "birthweight")
  ),
  unexpected = list(
    days = list(
      names = c("Saturday", "Sunday"),
      locale = "C"
    ),
    mat_age = c("less than 10", "greater than 50")
  )
)
