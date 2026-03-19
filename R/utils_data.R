# ---------------------------------------------------------------------------
# Data I/O and validation utilities
# ---------------------------------------------------------------------------

#' Read a SAS dataset (.sas7bdat or .xpt) into a tibble
#' @param path File path
#' @return A tibble with SAS labels preserved as attributes
read_sas_file <- function(path) {
  ext <- tolower(tools::file_ext(path))
  if (ext == "sas7bdat") {
    haven::read_sas(path)
  } else if (ext == "xpt") {
    haven::read_xpt(path)
  } else {
    stop("Unsupported file type: ", ext, ". Expected .sas7bdat or .xpt")
  }
}

#' Validate that a dataset contains all required columns
#' @param data A data frame
#' @param required_cols Character vector of required column names
#' @return A list with `valid` (logical) and `missing` (character vector)
validate_required_cols <- function(data, required_cols) {
  cols_upper <- toupper(names(data))
  required_upper <- toupper(required_cols)
  missing <- required_cols[!required_upper %in% cols_upper]
  list(valid = length(missing) == 0, missing = missing)
}

#' Standardise column names to uppercase (ADaM/SDTM convention)
standardise_names <- function(data) {
  names(data) <- toupper(names(data))
  data
}

#' Extract SAS variable labels as a named character vector
get_sas_labels <- function(data) {
  vapply(data, function(col) {
    lbl <- attr(col, "label")
    if (is.null(lbl)) NA_character_ else lbl
  }, character(1))
}

# Schema definitions for common datasets
SCHEMA_ADTTE <- c("USUBJID", "PARAMCD", "PARAM", "AVAL", "CNSR", "STARTDT")
SCHEMA_ADSL  <- c("USUBJID", "TRT01P", "RANDDT")
SCHEMA_DM    <- c("USUBJID", "RFSTDTC", "ARM", "ACTARM")
SCHEMA_RS    <- c("USUBJID", "RSTESTCD", "RSSTRESC", "RSDTC", "RSEVAL")
SCHEMA_DS    <- c("USUBJID", "DSDECOD", "DSSTDTC")
