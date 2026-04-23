# Skill: IA Boundary Re-calculation

## Purpose

Re-calculate group sequential **interim analysis (IA) boundaries** for a clinical trial given the observed or planned information fractions. Used to validate or reproduce protocol-specified boundaries, or to compute boundaries for a new design.

Outputs boundaries on the **z-score scale** and optionally on the **hazard ratio (HR) scale**.

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

### Design Parameters

| Parameter | Type | Default | Constraints | Description |
|-----------|------|---------|-------------|-------------|
| `k` | integer | 3 | ≥ 2 | Number of stages (including final) |
| `alpha` | numeric | 0.025 | (0, 0.5) | Overall one-sided Type I error |
| `beta` | numeric | 0.2 | (0, 0.5) | Type II error (1 − power) |
| `sided` | integer | 1 | 1 or 2 | One-sided or two-sided test |

### Information Fractions

Specify via **one** of:

- `info_fractions`: numeric vector of length K, strictly increasing, last value = 1.0
  - e.g. `c(0.33, 0.67, 1.0)`
- `observed_events`: integer vector of length K (observed events at each look)
  - Fractions computed as: `observed_events / observed_events[K]`

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
| `"asUser"` | User-defined cumulative alpha | `user_alpha_spending`: vector length K, non-decreasing, last = alpha |
| `"noEarlyEfficacy"` | No interim efficacy stopping | — |

> **Constraint:** Classic designs (`"OF"`, `"P"`, `"WT"`) do **not** support beta spending in rpact. Use a spending-function design to enable futility bounds.

### Futility Boundary (Beta Spending)

| `beta_spending` value | Description | Extra parameter |
|-----------------------|-------------|-----------------|
| `"none"` | No futility stopping | — |
| `"bsOF"` | O'Brien-Fleming type | — |
| `"bsP"` | Pocock type | — |
| `"bsKD"` | Kim-DeMets power | `gamma_b` (default 1) |
| `"bsHSD"` | Hwang-Shih-DeCani | `gamma_b` (default −4) |
| `"bsUser"` | User-defined cumulative beta | `user_beta_spending`: vector length K, non-decreasing, last = beta |

| Parameter | Type | Default | Description |
|-----------|------|---------|-------------|
| `binding_futility` | logical | FALSE | Whether futility bound is binding |

### HR Scale Boundaries (Optional)

Set `compute_hr_bounds = TRUE` to additionally output boundaries on the HR scale. Requires an effect size assumption:

- `hazard_ratio`: assumed HR of treatment vs control (e.g. 0.65)
- **or** `median_trt` + `median_ctrl`: median survival times in months

| Parameter | Type | Default | Description |
|-----------|------|---------|-------------|
| `allocation_ratio` | numeric | 1 | Allocation ratio treatment : control |

---

## Procedure

### Step 1 — Resolve information fractions

```r
# Option A: fractions provided directly
info_fractions <- c(0.33, 0.67, 1.0)

# Option B: derive from observed event counts
info_fractions <- observed_events / observed_events[k]
```

### Step 2 — Build the group sequential design

```r
library(rpact)

args <- list(
  kMax             = k,
  alpha            = alpha,
  beta             = beta,
  sided            = sided,
  informationRates = info_fractions,
  typeOfDesign     = alpha_spending
)

# Alpha spending parameters
if (alpha_spending == "asKD")   args$gammaA <- gamma_a
if (alpha_spending == "asHSD")  args$gammaA <- gamma_a
if (alpha_spending == "asUser") args$userAlphaSpending <- user_alpha_spending

# Beta spending (only for non-classic designs)
classic_designs <- c("OF", "P", "WT")
if (beta_spending != "none" && !(alpha_spending %in% classic_designs)) {
  args$typeBetaSpending <- beta_spending
  args$bindingFutility  <- binding_futility
  if (beta_spending == "bsKD")   args$gammaB <- gamma_b
  if (beta_spending == "bsHSD")  args$gammaB <- gamma_b
  if (beta_spending == "bsUser") args$userBetaSpending <- user_beta_spending
}

design <- do.call(rpact::getDesignGroupSequential, args)
```

### Step 3 — Compute HR scale boundaries (optional)

```r
if (compute_hr_bounds) {
  ss_args <- list(design = design, allocationRatioPlanned = allocation_ratio)

  if (!is.null(hazard_ratio)) {
    ss_args$hazardRatio <- hazard_ratio
  } else {
    ss_args$median1 <- median_trt
    ss_args$median2 <- median_ctrl
  }

  ss <- do.call(rpact::getSampleSizeSurvival, ss_args)
}
```

### Step 4 — Save the executed script

After obtaining results, save the **complete R script you ran** (Steps 1–3 with all actual input values substituted) to a `.R` file. This file serves as an audit record of exactly what was executed.

- Suggested filename: `ia_boundary_<study_name>_<YYYYMMDD>.R`
- Content: the full script exactly as run — not a template, but the actual code with real values
- Include a comment header with the date, analyst name, and study/context

---

## Output

### Efficacy Bounds (z-scale)

| Column | Source |
|--------|--------|
| Stage | `1:k` |
| Info Fraction | `design$informationRates` |
| Efficacy z | `round(design$criticalValues, 6)` |
| Nominal Alpha | `round(design$stageLevels, 6)` |
| Cumulative Alpha | `round(design$alphaSpent, 6)` |

### Futility Bounds (z-scale)

Only shown when real futility bounds exist: `!is.null(design$futilityBounds) && !all(design$futilityBounds <= -5) && beta_spending != "none"`

| Column | Source |
|--------|--------|
| Stage | `1:(k-1)` |
| Info Fraction | `design$informationRates[1:(k-1)]` |
| Futility z | `round(design$futilityBounds, 6)` |
| Cumulative Beta | `round(design$betaSpent[1:(k-1)], 6)` |

### HR Scale Boundaries (optional)

| Column | Source |
|--------|--------|
| Stage | `1:k` |
| Info Fraction | `design$informationRates` |
| Efficacy HR | `round(ss$criticalValuesEffectScale, 6)` |
| Efficacy p | `ss$criticalValuesPValueScale` |
| Futility HR | `round(ss$futilityBoundsEffectScale, 6)` — interim stages only |
| Futility p | `ss$futilityBoundsPValueScale` — interim stages only |

### Combined Summary

Combines all of the above into a single table per stage, with columns: Stage, Type (from IA types), Info Fraction, Efficacy z, Futility z, Cumulative Alpha, Cumulative Beta, Efficacy HR, Futility HR.

---

## Validation Rules

- `info_fractions` must be strictly increasing with last value = 1.0
- `alpha_spending %in% c("OF","P","WT")` → beta spending not supported; skip and warn
- `user_alpha_spending`: non-decreasing, last value = `alpha`
- `user_beta_spending`: non-decreasing, last value = `beta`
- Futility bounds sentinel check: rpact returns `≤ −6` when no real futility bound is set; always check `!all(design$futilityBounds <= -5)` before displaying

---

## Unit Testing

After running the design, verify boundary values against expected values using the following checks.

```r
# --- Tolerance thresholds ---
tol_z   <- 0.0001   # acceptable difference for z-scores (rpact is deterministic)
tol_p   <- 0.000001 # acceptable difference for p-values

# --- Helper ---
pass <- TRUE
check <- function(label, observed, expected, tol) {
  diff <- abs(observed - expected)
  ok   <- diff <= tol
  if (!ok) pass <<- FALSE
  cat(sprintf("[%s] %-35s observed=%.6f  expected=%.6f  diff=%.6f\n",
              if (ok) "PASS" else "FAIL", label, observed, expected, diff))
}

# --- Efficacy boundaries ---
for (i in seq_len(k)) {
  check(paste("Stage", i, "efficacy z"),
        round(design$criticalValues[i], 6), <expected_efficacy_z_i>, tol_z)
  check(paste("Stage", i, "nominal alpha"),
        round(design$stageLevels[i], 6),    <expected_nominal_alpha_i>, tol_p)
  check(paste("Stage", i, "cumul alpha"),
        round(design$alphaSpent[i], 6),     <expected_cumul_alpha_i>, tol_p)
}

# --- Futility boundaries (if applicable) ---
if (!is.null(design$futilityBounds) && !all(design$futilityBounds <= -5) && beta_spending != "none") {
  for (i in seq_len(k - 1)) {
    check(paste("Stage", i, "futility z"),
          round(design$futilityBounds[i], 6), <expected_futility_z_i>, tol_z)
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
- Alpha spending: O'Brien-Fleming (`"OF"`)
- Beta spending: none
- HR boundaries: HR = 0.65, allocation ratio = 1

**Expected output:**

| Stage | Info Frac | Efficacy z | Nominal Alpha | Cumul Alpha |
|-------|-----------|-----------|---------------|-------------|
| 1 | 0.33 | 3.4894 | 0.000242 | 0.000242 |
| 2 | 0.67 | 2.4489 | 0.007165 | 0.007263 |
| 3 | 1.00 | 2.0045 | 0.022509 | 0.025000 |

| Stage | Efficacy HR | Efficacy p |
|-------|------------|------------|
| 1 | 0.3962 | 2.421e-04 |
| 2 | 0.6338 | 7.165e-03 |
| 3 | 0.7367 | 2.251e-02 |
