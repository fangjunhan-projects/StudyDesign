# ---------------------------------------------------------------------------
# Shared PFS derivation and survival analysis engine
# ---------------------------------------------------------------------------

library(survival)

#' Run the standard PFS survival analysis suite
#' @param data A data.frame with at minimum: AVAL (time), CNSR (0=event, 1=censor), TRT (treatment)
#' @param trt_var Name of the treatment variable
#' @param ref_group Reference treatment group label
#' @param strata_vars Optional character vector of stratification variable names
#' @return A list with components: km_fit, km_summary, logrank, cox, event_summary
run_pfs_analysis <- function(data, trt_var = "TRT", ref_group = NULL,
                             strata_vars = NULL) {
  data$EVENT <- 1 - data$CNSR
  data$TRT_FACTOR <- factor(data[[trt_var]])

  if (!is.null(ref_group) && ref_group %in% levels(data$TRT_FACTOR)) {
    data$TRT_FACTOR <- relevel(data$TRT_FACTOR, ref = ref_group)
  }

  surv_obj <- Surv(data$AVAL, data$EVENT)

  # Kaplan-Meier
  km_fit <- survfit(surv_obj ~ TRT_FACTOR, data = data)

  km_summary <- summarise_km(km_fit, data$TRT_FACTOR)

  # Log-rank test
  if (is.null(strata_vars)) {
    lr <- survdiff(surv_obj ~ TRT_FACTOR, data = data)
  } else {
    strata_formula <- as.formula(
      paste("surv_obj ~ TRT_FACTOR +",
            paste("strata(", strata_vars, ")", collapse = " + "))
    )
    lr <- survdiff(strata_formula, data = data)
  }
  logrank_p <- 1 - pchisq(lr$chisq, df = length(unique(data$TRT_FACTOR)) - 1)

  # Cox PH
  if (is.null(strata_vars)) {
    cox_fit <- coxph(surv_obj ~ TRT_FACTOR, data = data)
  } else {
    cox_formula <- as.formula(
      paste("surv_obj ~ TRT_FACTOR +",
            paste("strata(", strata_vars, ")", collapse = " + "))
    )
    cox_fit <- coxph(cox_formula, data = data)
  }
  cox_summary <- summary(cox_fit, conf.int = 0.95)

  # Event summary per arm
  event_summary <- do.call(rbind, lapply(levels(data$TRT_FACTOR), function(arm) {
    sub <- data[data$TRT_FACTOR == arm, ]
    data.frame(
      Treatment = arm,
      N         = nrow(sub),
      Events    = sum(sub$EVENT),
      Censored  = sum(sub$CNSR),
      stringsAsFactors = FALSE
    )
  }))

  list(
    km_fit        = km_fit,
    km_summary    = km_summary,
    logrank_p     = logrank_p,
    cox_fit       = cox_fit,
    cox_summary   = cox_summary,
    event_summary = event_summary
  )
}

#' Extract median survival and CI per arm from a survfit object
summarise_km <- function(km_fit, trt_factor) {
  arms <- levels(trt_factor)
  med <- summary(km_fit)$table

  if (length(arms) == 1) {
    med <- t(as.matrix(med))
  }

  result <- data.frame(
    Treatment  = arms,
    Median     = med[, "median"],
    LCL        = med[, "0.95LCL"],
    UCL        = med[, "0.95UCL"],
    stringsAsFactors = FALSE
  )
  result
}

#' Extract HR, CI, and p-value from Cox model summary
extract_cox_results <- function(cox_summary) {
  coef_tbl <- cox_summary$conf.int
  hr    <- coef_tbl[1, "exp(coef)"]
  hr_lo <- coef_tbl[1, "lower .95"]
  hr_up <- coef_tbl[1, "upper .95"]
  p_val <- cox_summary$coefficients[1, "Pr(>|z|)"]

  list(HR = hr, HR_LCL = hr_lo, HR_UCL = hr_up, p_value = p_val)
}
