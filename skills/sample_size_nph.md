# Skill: Sample Size Calculation — Non-Proportional Hazards (NPH)

## Purpose

Calculate the required sample size and event count for a survival endpoint clinical trial under **non-proportional hazards**, using a group sequential design. Currently supports the **delayed treatment effect** model, where the treatment benefit begins after an initial delay period.

**R package:** `gsDesign2`, `gsDesign`

---

## Execution

This skill requires R. How to proceed depends on the agent's capabilities:

**If the agent can execute R code (e.g. has a shell/code tool):**
Read the inputs from the user's prompt, substitute them into the R code in the Procedure section, run it, and return the formatted results directly.

**If the agent cannot execute R code (e.g. ChatGPT, Gemini without code tools):**
Read the inputs from the user's prompt, substitute them into the R code in the Procedure section, and present the complete ready-to-run R script to the user. Ask the user to run it in their R console and share the output.

**If the agent has no code capability at all:**
Walk the user through each step of the Procedure section in plain language, explaining what each function does and what output to expect.

### Requirements
- **R version:** ≥ 4.4.0
- **R packages:** `gsDesign2`, `gsDesign`
- **Install:** `install.packages(c("gsDesign2", "gsDesign"))`

---

## Inputs

### Trial Design

| Parameter | Type | Default | Constraints | Description |
|-----------|------|---------|-------------|-------------|
| `k` | integer | 3 | ≥ 1 | Number of stages (including final) |
| `alpha` | numeric | 0.025 | (0, 0.5) | Overall one-sided Type I error |
| `beta` | numeric | 0.2 | (0, 0.5) | Type II error (1 − power) |

### NPH Model: Delayed Treatment Effect

The hazard ratio changes at a specified delay point. Before the delay, HR = `hr_early` (typically 1.0, no effect); after the delay, HR = `hr_late` (the steady-state treatment benefit).

| Parameter | Type | Default | Constraints | Description |
|-----------|------|---------|-------------|-------------|
| `delay` | numeric | 6 | ≥ 0 | Duration of delay period (months) |
| `hr_early` | numeric | 1.0 | > 0 | HR during delay (typically 1.0 = no early effect) |
| `hr_late` | numeric | 0.65 | > 0 | HR after delay (steady-state treatment effect) |
| `med_ctrl` | numeric | 10 | > 0 | Median survival — control arm (months) |

Control arm monthly hazard rate is derived as: `ctrl_lambda = log(2) / med_ctrl`

### Accrual & Follow-up

Two ways to specify accrual:

**Option A — Constant rate:**

| Parameter | Type | Description |
|-----------|------|-------------|
| `accrual_time` | numeric | Accrual duration (months) |
| `accrual_rate` | numeric | Patients enrolled per month (constant) |

**Option B — Piecewise monthly enrollment (ramp-up):**

| Parameter | Type | Description |
|-----------|------|-------------|
| `accrual_time` | numeric | Total accrual duration (months); must equal `length(monthly_enroll)` |
| `monthly_enroll` | numeric vector | Number of patients enrolled each month (one value per month) |

| Parameter | Type | Default | Description |
|-----------|------|---------|-------------|
| `followup_time` | numeric | 12 | Additional follow-up after accrual ends (months) |
| `dropout_annual` | numeric | 0.02 | Annual dropout rate (both arms) |
| `alloc_ratio` | numeric | 1 | Allocation ratio treatment : control |

### Group Sequential Boundaries

**Analysis timing** — specify calendar times for each look (months from study start):

| Parameter | Type | Constraints | Description |
|-----------|------|-------------|-------------|
| `analysis_time` | numeric vector, length K | Strictly increasing; last value = `accrual_time + followup_time` | Calendar time of each analysis (months) |

**Efficacy boundary (upper):**

| `alpha_spending` value | Spending function | Extra parameter |
|------------------------|-------------------|-----------------|
| `"sfLDOF"` | Lan-DeMets O'Brien-Fleming | — |
| `"sfLDP"` | Lan-DeMets Pocock | — |
| `"sfHSD"` | Hwang-Shih-DeCani | `alpha_hsd_gamma` (default −4) |
| `"gs_b"` | Fixed z-bound (no spending) | `upar`: numeric vector of length K (z-scores) |

**Futility boundary (lower):**

| `beta_spending` value | Spending function | Extra parameter |
|-----------------------|-------------------|-----------------|
| `"none"` | No futility stopping | — |
| `"sfLDOF"` | Lan-DeMets O'Brien-Fleming | — |
| `"sfLDP"` | Lan-DeMets Pocock | — |
| `"sfHSD"` | Hwang-Shih-DeCani | `beta_hsd_gamma` (default −4) |
| `"gs_b"` | Fixed z-bound (no spending) | `lpar`: numeric vector of length K (z-scores) |

**test_upper** — logical vector of length K: whether to test the efficacy bound at each stage (default: all TRUE).

**`info_scale`** — always use `"h0_h1_info"` (default in gsDesign2).

---

## Procedure

### Step 1 — Derive control hazard and dropout rates

```r
library(gsDesign2)
library(gsDesign)

ctrl_lambda <- log(2) / med_ctrl       # monthly hazard rate (control arm)
dropout_mo  <- dropout_annual / 12     # convert annual dropout to monthly
total_time  <- accrual_time + followup_time
```

### Step 2 — Define enrollment rate

```r
# Option A: constant accrual
enroll_rate <- gsDesign2::define_enroll_rate(
  duration = accrual_time,
  rate     = accrual_rate
)

# Option B: piecewise monthly enrollment (ramp-up)
enroll_rate <- gsDesign2::define_enroll_rate(
  duration = rep(1, length(monthly_enroll)),
  rate     = monthly_enroll
)
```

### Step 3 — Define failure rates (delayed treatment effect)

```r
fail_rate <- gsDesign2::define_fail_rate(
  duration     = c(delay, Inf),     # two periods: [0, delay] and (delay, Inf)
  fail_rate    = ctrl_lambda,        # control arm hazard (same in both periods)
  hr           = c(hr_early, hr_late),
  dropout_rate = dropout_mo
)
```

### Step 4 — Define spending function parameters

```r
sf_lookup <- list(
  sfLDOF = list(sf = gsDesign::sfLDOF,    param = NULL),
  sfLDP  = list(sf = gsDesign::sfLDPocock, param = NULL),
  sfHSD  = list(sf = gsDesign::sfHSD,      param = alpha_hsd_gamma)  # e.g. -4
)

# Upper (efficacy)
if (alpha_spending == "gs_b") {
  upper_fn  <- gsDesign2::gs_b
  upper_par <- upar   # user-supplied z-score vector of length K
} else {
  su <- sf_lookup[[alpha_spending]]
  upper_fn  <- gsDesign2::gs_spending_bound
  upper_par <- list(sf = su$sf, total_spend = alpha, param = su$param)
}

# Lower (futility)
if (beta_spending == "none") {
  lower_fn  <- gsDesign2::gs_b
  lower_par <- -Inf
} else if (beta_spending == "gs_b") {
  lower_fn  <- gsDesign2::gs_b
  lower_par <- lpar   # user-supplied z-score vector of length K
} else {
  sl <- sf_lookup[[beta_spending]]
  lower_fn  <- gsDesign2::gs_spending_bound
  lower_par <- list(sf = sl$sf, total_spend = beta, param = sl$param)
}
```

### Step 5 — Run the design

```r
result <- gsDesign2::gs_design_ahr(
  enroll_rate   = enroll_rate,
  fail_rate     = fail_rate,
  ratio         = alloc_ratio,
  alpha         = alpha,
  beta          = beta,
  analysis_time = analysis_time,
  info_scale    = "h0_h1_info",
  upper         = upper_fn,
  upar          = upper_par,
  lower         = lower_fn,
  lpar          = lower_par,
  test_upper    = test_upper   # logical vector length K, default rep(TRUE, K)
)
```

---

## Output

### Key Metrics

| Output | How to compute |
|--------|----------------|
| Total N | `ceiling(max(result$analysis$n))` |
| Treatment arm N | `ceiling(total_n * alloc_ratio / (1 + alloc_ratio))` |
| Control arm N | `total_n - n_trt` |
| Required events | `ceiling(max(result$analysis$event))` |
| Study duration (months) | `round(max(result$analysis$time), 1)` |

### Analysis Summary Table (`result$analysis`)

| Column | Description |
|--------|-------------|
| `analysis` | Stage index (1, 2, …, K) |
| `time` | Calendar time of analysis (months) |
| `n` | Expected total N enrolled by this analysis |
| `event` | Expected cumulative events by this analysis |
| `ahr` | Average hazard ratio at this analysis |
| `info_frac` | Statistical information fraction (under H0/H1 average) |
| `theta` | Treatment effect parameter |

### Boundary Table (`result$bound`)

| Column | Description |
|--------|-------------|
| `analysis` | Stage index |
| `bound` | `"upper"` (efficacy) or `"lower"` (futility) |
| `z` | Z-score boundary |
| `probability` | Cumulative probability under H1 (power spend) |
| `probability0` | Cumulative probability under H0 (alpha spend) |
| `~hr at bound` | HR corresponding to the boundary z-score |
| `nominal p` | Nominal p-value at boundary |

---

## Validation Rules

- `analysis_time` must be strictly increasing; last value must equal `accrual_time + followup_time`
- `monthly_enroll` length must equal `accrual_time` (one value per month)
- `hr_early` is typically 1.0 (no effect during delay); must be > 0
- `hr_late` must be < 1 for a beneficial treatment effect
- `test_upper` must be a logical vector of length K

---

## Example

**Inputs:**
- K = 3, alpha = 0.02, beta = 0.1, one-sided
- Delay = 2 months, HR during delay = 1.0, HR after delay = 0.595
- Median survival control = 18.2 months
- Accrual: piecewise monthly `c(1,3,6,9,13,18,22,27,32,35,38,39,41,44,46,48,50,50)`
- Follow-up = 17 months; annual dropout = 5%
- Analysis times: 20, 27.1, 35 months
- Alpha spending: Lan-DeMets O'Brien-Fleming (`sfLDOF`)
- Beta spending: Hwang-Shih-DeCani (`sfHSD`, gamma = −4)
- test_upper: TRUE at all stages

**Expected output:**
- Total N = 446 (223 per arm)
- Required events = 230
- Study duration = 35.0 months

| Stage | Time (mo) | Events | AHR | Info Frac |
|-------|-----------|--------|-----|-----------|
| 1 | 20.0 | 124 | 0.682 | 0.534 |
| 2 | 27.1 | 181 | 0.653 | 0.783 |
| 3 | 35.0 | 230 | 0.640 | 1.000 |

| Stage | Bound | z | ~HR at Bound | Cum. Power |
|-------|-------|------|-------------|------------|
| 1 | upper | 2.96 | 0.587 | 20.5% |
| 1 | lower | −0.109 | 1.02 | — |
| 2 | upper | 2.40 | 0.700 | 67.8% |
| 2 | lower | 1.03 | 0.858 | — |
| 3 | upper | 2.11 | 0.757 | 90.0% |
| 3 | lower | 2.02 | 0.766 | — |
