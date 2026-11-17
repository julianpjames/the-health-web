# Deterministic selection of a CDC PLACES estimate from a ZCTA response.
# PLACES can return crude and age-adjusted estimates for the same measure.
# This application displays population prevalence, so prefer crude estimates.
select_cdc_measure_row <- function(data, pattern) {
  if (is.null(data) || !is.data.frame(data) || !nrow(data) ||
      !"measure" %in% names(data) || !length(pattern) || is.na(pattern[1])) {
    return(NULL)
  }
  matches <- which(!is.na(data$measure) &
    grepl(pattern[1], as.character(data$measure), ignore.case = TRUE))
  if (!length(matches)) return(NULL)
  candidates <- data[matches, , drop = FALSE]

  # Never silently substitute age-adjusted estimates for crude estimates.
  if ("data_value_type" %in% names(candidates)) {
    types <- tolower(trimws(as.character(candidates$data_value_type)))
    crude <- which(grepl("crude", types, fixed = TRUE))
    if (!length(crude)) return(NULL)
    candidates <- candidates[crude, , drop = FALSE]
  }

  # Exact measure names take precedence over partial/regex matches.
  exact <- which(tolower(trimws(as.character(candidates$measure))) ==
    tolower(trimws(pattern[1])))
  if (length(exact)) candidates <- candidates[exact, , drop = FALSE]

  # Prefer the latest available estimate year, when present.
  if ("year" %in% names(candidates)) {
    years <- suppressWarnings(as.numeric(as.character(candidates$year)))
    if (any(is.finite(years))) {
      candidates <- candidates[which(is.finite(years) &
        years == max(years, na.rm = TRUE)), , drop = FALSE]
    }
  }

  # Stable result even when the upstream API changes its row ordering.
  if (nrow(candidates) > 1L) {
    sort_columns <- intersect(c("measureid", "measure", "data_value_type", "year"), names(candidates))
    if (length(sort_columns)) {
      keys <- lapply(candidates[sort_columns], function(x) as.character(x))
      candidates <- candidates[do.call(order, c(keys, list(na.last = TRUE))), , drop = FALSE]
    }
  }
  candidates[1, , drop = FALSE]
}
