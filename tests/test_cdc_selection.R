# Run from repository root: Rscript tests/test_cdc_selection.R
source("cdc_selection.R")
records <- data.frame(
  measure = c("Diabetes", "Diabetes", "Diabetes", "Asthma"),
  data_value_type = c("Age-adjusted prevalence", "Crude prevalence",
                      "Crude prevalence", "Crude prevalence"),
  year = c("2023", "2022", "2023", "2023"),
  data_value = c(8, 10, 12, 7),
  stringsAsFactors = FALSE
)
stopifnot(
  identical(select_cdc_measure_row(records, "Diabetes")$data_value, 12),
  identical(select_cdc_measure_row(records[c(3, 1, 2, 4), ], "Diabetes")$data_value, 12),
  identical(select_cdc_measure_row(records, "Asthma")$data_value, 7),
  is.null(select_cdc_measure_row(records, "Nonexistent")),
  is.null(select_cdc_measure_row(records[1, , drop = FALSE], "Diabetes")),
  is.null(select_cdc_measure_row(NULL, "Diabetes"))
)
message("CDC selection checks passed")
