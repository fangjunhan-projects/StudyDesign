# Skill: Sample Size Calculation — Proportional Hazards (PH)

## Purpose

Calculate the required sample size and event count for a survival endpoint clinical trial under the **proportional hazards assumption**, using a group sequential design. Supports interim analyses with flexible alpha (efficacy) and beta (futility) spending functions.

**R package:** `rpact`

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
- **R version:** ≥ 4.1.0
- **R packages:** `rpact`
- **Install:** `install.packages("rpact")`

---

## Inputs

### Trial Design

| Parameter | Type | Default | Constraints | Description |
|-----------|------|---------|-------------|-------------|
| `k` | integer | 3 | ≥ 1 | Number of stages (including final) |
| `alpha` | numeric | 0.025 | (0, 0.5) | Overall one-sided Type I error |
| `beta` | numeric | 0.2 | (0, 0.5) | Type II error (1 − power) |
| `sided` | integer | 1 | 1 or 2 | One-sided or two-sided test |

### Information Fractions

Specify via **one** of the following:

- `info_fractions`: numeric vector of length K, strictly increasing, last value = 1.0
  - e.g. `c(0.33, 0.67, 1.0)`
- `planned_events`: integer vector of length K, strictly increasing
  - Fractions computed as `planned_events / planned_events[K]`

### Interim Analysis Types

For each interim look i = 1, …, K−1, assign one of:

| Value | Meaning |
|-------|---------|
| `"Efficacy"` | Only efficacy (upper) bound tested |
| `"Futility"` | Only futility (lower) bound tested |
| `"Both"` | Both efficacy and futility tested |

### Efficacy Boundary (Alpha Spending)

| `alpha_spending` value | Description | Extra parameter |
|------------------------|-------------|-----------------|
| `"OF"` | O'Brien-Fleming (classic) | — |
| `"P"` | Pocock (classic) | — |
| `"WT"` | Wang-Tsiatis Delta | — |
| `"asOF"` | Lan-DeMets O'Brien-Fleming | — |
| `"asP"` | Lan-DeMets Pocock | — |
| `"asKD"` | Kim-DeMets power | `gamma_a` (default 1) |
| `"asHSD"` | Hwang-Shih-DeCani | `gamma_a` (default −4) |
| `"asUser"` | User-defined cumulative alpha | `user_alpha_spending`: vector of length K, non-decreasing, last = alpha |
| `"noEarlyEfficacy"` | No interim efficacy stopping | — |

> **Constraint:** Classic designs (`"OF"`, `"P"`, `"WT"`) do **not** support beta spending. Use a spending-function design (`"asOF"`, `"asP"`, etc.) to enable futility bounds.

### Futility Boundary (Beta Spending)

| `beta_spending` value | Description | Extra parameter |
|-----------------------|-------------|-----------------|
| `"none"` | No futility stopping | — |
| `"bsOF"` | O'Brien-Fleming type | — |
| `"bsP"` | Pocock type | — |
| `"bsKD"` | Kim-DeMets power | `gamma_b` (default 1) |
| `"bsHSD"` | Hwang-Shih-DeCani | `gamma_b` (default −4) |
| `"bsUser"` | User-defined cumulative beta | `user_beta_spending`: vector of length K, non-decreasing, last = beta |

| Parameter | Type | Default | Description |
|-----------|------|---------|-------------|
| `binding_futility` | logical | FALSE | Whether futility bound is binding |

### Survival Assumptions

Specify effect size via **one** of:

- `hazard_ratio`: numeric, HR of treatment vs control (e.g. 0.65)
- `median_trt` + `median_ctrl`: median survival times in months; HR derived as `log(2)/median_trt / (log(2)/median_ctrl)`

| Parameter | Type | Default | Description |
|-----------|------|---------|-------------|
| `alloc_ratio` | numeric | 1 | Allocation ratio treatment : control |

### Accrual & Follow-up

| Parameter | Type | Default | Description |
|-----------|------|---------|-------------|
| `accrual_time` | numeric | 24 | Accrual duration (months) |
| `accrual_rate` | numeric | 20 | Patients enrolled per month |
| `followup_time` | numeric | 12 | Additional follow-up after accrual ends (months) |
| `dropout1` | numeric | 0.02 | Annual dropout rate — treatment arm |
| `dropout2` | numeric | 0.02 | Annual dropout rate — control arm |
| `dropout_time` | numeric | 12 | Dropout reference time (months) |

---

## Procedure

### Step 1 — Build the group sequential design

```r
library(rpact)

design_args <- list(
  kMax             = k,
  alpha            = alpha,
  beta             = beta,
  sided            = sided,
  informationRates = info_fractions,   # resolved from info_fractions or planned_events
  typeOfDesign     = alpha_spending
)

# Add spending function parameters if needed
if (alpha_spending == "asKD")   design_args$gammaA <- gamma_a
if (alpha_spending == "asHSD")  design_args$gammaA <- gamma_a
if (alpha_spending == "asUser") design_args$userAlphaSpending <- user_alpha_spending

# Add beta spending (only for non-classic designs)
classic_designs <- c("OF", "P", "WT")
if (beta_spending != "none" && !(alpha_spending %in% classic_designs)) {
  design_args$typeBetaSpending <- beta_spending
  design_args$bindingFutility  <- binding_futility
  if (beta_spending == "bsKD")   design_args$gammaB <- gamma_b
  if (beta_spending == "bsHSD")  design_args$gammaB <- gamma_b
  if (beta_spending == "bsUser") design_args$userBetaSpending <- user_beta_spending
}

design <- do.call(rpact::getDesignGroupSequential, design_args)
```

### Step 2 — Calculate sample size

```r
ss_args <- list(
  design                 = design,
  allocationRatioPlanned = alloc_ratio,
  accrualTime            = c(0, accrual_time),
  accrualIntensity       = accrual_rate,
  followUpTime           = followup_time,
  dropoutRate1           = dropout1,
  dropoutRate2           = dropout2,
  dropoutTime            = dropout_time
)

# Effect size
if (!is.null(hazard_ratio)) {
  ss_args$hazardRatio <- hazard_ratio
} else {
  ss_args$median1 <- median_trt
  ss_args$median2 <- median_ctrl
}

ss <- do.call(rpact::getSampleSizeSurvival, ss_args)
```

### Step 3 — Resolve info fractions from planned events (if applicable)

```r
# If user provides planned_events instead of info_fractions:
info_fractions <- planned_events / planned_events[k]
```

### Step 4 — Save the executed script

After obtaining results, save the **complete R script you ran** (Steps 1–3 with all actual input values substituted) to a `.R` file. This file serves as an audit record of exactly what was executed.

- Suggested filename: `ph_sample_size_<study_name>_<YYYYMMDD>.R`
- Content: the full script exactly as run — not a template, but the actual code with real values
- Include a comment header with the date, analyst name, and study/context

---

## Output

### Key Metrics

| Output | How to compute |
|--------|----------------|
| Total N | `ceiling(max(ss$numberOfSubjects))` |
| Treatment arm N | `ceiling(max(ss$numberOfSubjects1))` |
| Control arm N | `ceiling(max(ss$numberOfSubjects2))` |
| Required events | `ceiling(ss$maxNumberOfEvents)` |
| Study duration (months) | `round(ss$studyDuration, 1)` |

### Stage Details Table

| Column | Source |
|--------|--------|
| Stage | `1:k` |
| Info Fraction | `design$informationRates` |
| Events at Stage | `ceiling(ss$eventsPerStage)` |
| Cumulative Events | `ceiling(ss$cumulativeEventsPerStage)` |
| N at Stage | `ceiling(ss$numberOfSubjects)` |
| Analysis Time (months) | `round(ss$analysisTime, 2)` |

### Boundary Tables

**Efficacy (z-scale):**

| Column | Source |
|--------|--------|
| Stage | `1:k` |
| Efficacy z | `round(design$criticalValues, 6)` |
| Nominal Alpha | `round(design$stageLevels, 6)` |
| Cumulative Alpha | `round(design$alphaSpent, 6)` |
| Futility z | `round(design$futilityBounds, 6)` — only if real futility bounds exist (check: `!all(design$futilityBounds <= -5)`) |

**HR scale** (from `ss$criticalValuesEffectScale`, `ss$futilityBoundsEffectScale`, `ss$criticalValuesPValueScale`, `ss$futilityBoundsPValueScale`):

| Column | Source |
|--------|--------|
| Efficacy HR | `round(ss$criticalValuesEffectScale, 6)` |
| Efficacy p | `ss$criticalValuesPValueScale` |
| Futility HR | `round(ss$futilityBoundsEffectScale, 6)` |

---

## Validation Rules

- `info_fractions` must be strictly increasing with last value = 1.0
- `alpha_spending %in% c("OF","P","WT")` → beta spending is not supported; warn and skip
- Futility bounds are real only when: `!is.null(design$futilityBounds) && !all(design$futilityBounds <= -5) && beta_spending != "none"`
- `user_alpha_spending`: non-decreasing, last value = `alpha`
- `user_beta_spending`: non-decreasing, last value = `beta`

---

## Unit Testing

After running the design, verify key outputs against expected values using the following checks.

```r
# --- Tolerance thresholds ---
tol_n <- 2      # acceptable rounding difference for N and events
tol_z <- 0.01   # acceptable difference for z-scores

# --- Helper ---
pass <- TRUE
check <- function(label, observed, expected, tol) {
  diff <- abs(observed - expected)
  ok   <- diff <= tol
  if (!ok) pass <<- FALSE
  cat(sprintf("[%s] %-35s observed=%.4f  expected=%.4f  diff=%.4f\n",
              if (ok) "PASS" else "FAIL", label, observed, expected, diff))
}

# --- Key metrics ---
total_n  <- ceiling(max(ss$numberOfSubjects))
tot_ev   <- ceiling(ss$maxNumberOfEvents)
duration <- round(ss$studyDuration, 1)

check("Total N",          total_n,  <expected_total_n>,  tol_n)
check("Required events",  tot_ev,   <expected_events>,   tol_n)
check("Study duration",   duration, <expected_duration>, 0.1)

# --- Per-stage boundaries ---
for (i in seq_len(k)) {
  check(paste("Stage", i, "efficacy z"),
        round(design$criticalValues[i], 4), <expected_efficacy_z_i>, tol_z)
}

# --- Futility bounds (if applicable) ---
if (!is.null(design$futilityBounds) && !all(design$futilityBounds <= -5)) {
  for (i in seq_len(k - 1)) {
    check(paste("Stage", i, "futility z"),
          round(design$futilityBounds[i], 4), <expected_futility_z_i>, tol_z)
  }
}

if (pass) cat("\n=== ALL TESTS PASSED ===\n") else cat("\n!!! SOME TESTS FAILED !!!\n")
```

Replace all `<expected_...>` placeholders with values from your verified reference run. The unit test code should also be included in the saved `.R` script (Step 4).

---

## Example

**Inputs:**
- K = 3, alpha = 0.025, beta = 0.2, one-sided
- Info fractions: 0.33, 0.67, 1.0
- Alpha spending: Lan-DeMets O'Brien-Fleming (`"asOF"`)
- Beta spending: O'Brien-Fleming type (`"bsOF"`), non-binding
- HR = 0.65, allocation ratio = 1
- Accrual: 24 months at 20 patients/month; follow-up: 12 months
- Dropout: 2% annual (both arms), reference time 12 months

**Expected output:**
- Total N = 480 (240 per arm)
- Required events = 187
- Study duration = 37.7 months
- Analysis times: 21.3, 32.7, 46.1 months
