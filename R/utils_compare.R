# ---------------------------------------------------------------------------
# Comparison utilities for validation checks
# ---------------------------------------------------------------------------

#' Compare two numeric values with tolerance
#' @return "Pass" or "Fail"
compare_numeric <- function(derived, reported, tolerance = 1e-4) {
  if (is.na(derived) || is.na(reported)) return("Fail")
  if (abs(derived - reported) <= tolerance) "Pass" else "Fail"
}

#' Compare two character values (exact match, case-insensitive)
compare_character <- function(derived, reported) {
  if (is.na(derived) || is.na(reported)) return("Fail")
  if (toupper(trimws(derived)) == toupper(trimws(reported))) "Pass" else "Fail"
}

#' Compare two date values with optional day tolerance
compare_date <- function(derived, reported, day_tolerance = 0) {
  if (is.na(derived) || is.na(reported)) return("Fail")
  d <- as.Date(derived)
  r <- as.Date(reported)
  if (abs(as.numeric(d - r)) <= day_tolerance) "Pass" else "Fail"
}

#' Build a comparison summary row
#' @param metric Name of the metric
#' @param derived Re-derived value
#' @param reported Sponsor-reported value
#' @param result "Pass" or "Fail"
#' @return A one-row data.frame
comparison_row <- function(metric, derived, reported, result) {
  data.frame(
    Metric   = metric,
    Derived  = as.character(derived),
    Reported = as.character(reported),
    Diff     = if (is.numeric(derived) && is.numeric(reported)) {
      as.character(round(derived - reported, 8))
    } else {
      NA_character_
    },
    Result   = result,
    stringsAsFactors = FALSE
  )
}

#' Subject-level key-variable comparison between two datasets
#' @param derived_df Re-derived dataset (data.frame)
#' @param sponsor_df Sponsor's dataset (data.frame)
#' @param by_var Join key, typically "USUBJID"
#' @param key_vars Character vector of variable names to compare
#' @param num_tolerance Tolerance for numeric comparisons
#' @param date_tolerance Day tolerance for date comparisons
#' @return A data.frame of mismatches with columns: USUBJID, Variable, Derived, Sponsor, Diff
compare_datasets_by_subject <- function(derived_df, sponsor_df, by_var = "USUBJID",
                                        key_vars, num_tolerance = 1e-6,
                                        date_tolerance = 0) {
  merged <- merge(derived_df, sponsor_df, by = by_var, suffixes = c(".derived", ".sponsor"))
  mismatches <- list()

  for (v in key_vars) {
    col_d <- paste0(v, ".derived")
    col_s <- paste0(v, ".sponsor")
    if (!col_d %in% names(merged) || !col_s %in% names(merged)) next

    val_d <- merged[[col_d]]
    val_s <- merged[[col_s]]

    is_num <- is.numeric(val_d) && is.numeric(val_s)
    is_date <- inherits(val_d, "Date") && inherits(val_s, "Date")

    if (is_num) {
      mismatch_idx <- which(is.na(val_d) != is.na(val_s) |
                              (!is.na(val_d) & !is.na(val_s) & abs(val_d - val_s) > num_tolerance))
    } else if (is_date) {
      mismatch_idx <- which(is.na(val_d) != is.na(val_s) |
                              (!is.na(val_d) & !is.na(val_s) & abs(as.numeric(val_d - val_s)) > date_tolerance))
    } else {
      mismatch_idx <- which(is.na(val_d) != is.na(val_s) |
                              (!is.na(val_d) & !is.na(val_s) & toupper(trimws(as.character(val_d))) != toupper(trimws(as.character(val_s)))))
    }

    if (length(mismatch_idx) > 0) {
      diff_val <- if (is_num) {
        as.character(round(val_d[mismatch_idx] - val_s[mismatch_idx], 8))
      } else {
        NA_character_
      }
      mismatches[[length(mismatches) + 1]] <- data.frame(
        USUBJID  = merged[[by_var]][mismatch_idx],
        Variable = v,
        Derived  = as.character(val_d[mismatch_idx]),
        Sponsor  = as.character(val_s[mismatch_idx]),
        Diff     = diff_val,
        stringsAsFactors = FALSE
      )
    }
  }

  if (length(mismatches) == 0) {
    data.frame(USUBJID = character(), Variable = character(),
               Derived = character(), Sponsor = character(),
               Diff = character(), stringsAsFactors = FALSE)
  } else {
    do.call(rbind, mismatches)
  }
}
