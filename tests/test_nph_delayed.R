# =============================================================================
# Test: NPH Sample Size — Delayed Treatment Effect
#
# Inputs matching the app UI fields:
#   Number of Stages (K)          = 3
#   One-sided Alpha               = 0.02
#   Beta                          = 0.1
#   NPH Model                     = Delayed Treatment Effect
#   Delay Duration (months)       = 2
#   HR during delay               = 1.0
#   HR after delay                = 0.595
#   Accrual Duration (months)     = 18
#   Monthly Enrollment            = c(1,3,6,9,13,18,22,27,32,35,38,39,41,44,46,48,50,50)
#   Follow-up after Accrual (mo)  = 17
#   Annual Dropout Rate           = 5% (0.05)
#   Median Survival — Control     = 18.2 months
#   Allocation Ratio (trt:ctrl)   = 1
#   Analysis Calendar Times (mo)  = 20, 27.1, 35
#   Alpha Spending                = Lan-DeMets O'Brien-Fleming (sfLDOF)
#   Beta Spending                 = Hwang-Shih-DeCani (sfHSD, gamma = -4)
# =============================================================================

library(gsDesign2)
library(gsDesign)

# ---- Study parameters -------------------------------------------------------
alpha          <- 0.02
beta           <- 0.10
k              <- 3
delay          <- 2        # months
hr_early       <- 1.0
hr_late        <- 0.595
accrual_time   <- 18       # months
monthly_enroll <- c(1, 3, 6, 9, 13, 18, 22, 27, 32, 35, 38, 39, 41, 44, 46, 48, 50, 50)
followup_time  <- 17       # months
dropout_annual <- 0.05
med_ctrl       <- 18.2     # months
alloc_ratio    <- 1              # trt:ctrl
analysis_time  <- c(20, 27.1, 35)  # calendar times for each look (months)

# ---- Derived values ---------------------------------------------------------
total_time  <- accrual_time + followup_time   # 35 months
ctrl_lambda <- log(2) / med_ctrl              # monthly hazard (control)
dropout_mo  <- dropout_annual / 12            # monthly dropout rate

# ---- Enrollment rate (piecewise: 1 month per period) -----------------------
enroll_rate <- define_enroll_rate(
  duration = rep(1, length(monthly_enroll)),
  rate     = monthly_enroll
)

# ---- Failure rates (piecewise exponential, delayed effect) ------------------
fail_rate <- define_fail_rate(
  duration     = c(delay, Inf),
  fail_rate    = ctrl_lambda,
  hr           = c(hr_early, hr_late),
  dropout_rate = dropout_mo
)

# ---- Spending functions -----------------------------------------------------
upper_par <- list(sf = gsDesign::sfLDOF,   total_spend = alpha, param = NULL)
lower_par <- list(sf = gsDesign::sfHSD,    total_spend = beta,  param = -4)

# ---- Group sequential design ------------------------------------------------
result <- gs_design_ahr(
  enroll_rate   = enroll_rate,
  fail_rate     = fail_rate,
  ratio         = alloc_ratio,
  alpha         = alpha,
  beta          = beta,
  analysis_time = analysis_time,
  info_scale    = "h0_h1_info",
  upper         = gs_spending_bound,
  upar          = upper_par,
  lower         = gs_spending_bound,
  lpar          = lower_par
)

# ---- Print results ----------------------------------------------------------
cat("========== NPH Sample Size: Delayed Treatment Effect ==========\n\n")

cat("--- Analysis Summary ---\n")
print(result$analysis)

cat("\n--- Boundary Summary ---\n")
print(result$bound)

cat("\n--- Key Metrics ---\n")
total_n  <- ceiling(max(result$analysis$n))
tot_ev   <- ceiling(max(result$analysis$event))
duration <- round(max(result$analysis$time), 1)
n_trt    <- ceiling(total_n * alloc_ratio / (1 + alloc_ratio))
n_ctrl   <- total_n - n_trt

cat(sprintf("Total N          : %d\n",    total_n))
cat(sprintf("  Treatment arm  : %d\n",    n_trt))
cat(sprintf("  Control arm    : %d\n",    n_ctrl))
cat(sprintf("Required Events  : %d\n",    tot_ev))
cat(sprintf("Study Duration   : %.1f months\n", duration))
