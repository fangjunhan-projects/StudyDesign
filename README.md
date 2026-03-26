# BiostatVD -- Biostatistics Validation Dashboard

R Shiny application for automated validation of efficacy results in clinical trials.

## Modules

| Module | Sub-modules | Description |
|--------|-------------|-------------|
| M1: Study Design Calculation | IA Boundary | Re-calculate group sequential boundaries using rpact; supports multiple alpha/beta spending functions and HR-scale boundary output |
| | Sample Size | Sample size calculation for PH models (rpact); NPH models via gsDesign2 (coming soon) |
| M2: PFS Efficacy | Raw Data Path | Re-derive PFS from raw CRF/EDC data and compare against sponsor results |
| | SDTM Path | Re-derive PFS from SDTM datasets (RS, TU, DM, DS) |
| | ADaM Comparison | Subject-level key-variable comparison between re-derived and sponsor ADaM (ADTTE/ADSL) |
| M3: Event Projection | — | Predict when target event counts will be reached for OS/PFS using Moving Average, eventTrack Hybrid Piecewise Exponential, or Parametric distributions (Weibull/Exp/Log-normal/Gamma) |
| M4: Additional | — | Placeholder for future efficacy checks (ORR, OS, subgroup analyses) |

## Requirements

- R >= 4.1.0
- Core packages: `shiny`, `shinydashboard`, `haven`, `survival`, `survminer`, `rpact`, `DT`, `ggplot2`, `plotly`
- M3 Event Projection (eventTrack methods): `eventTrack`, `fitdistrplus`
- M1 Sample Size NPH (coming soon): `gsDesign2`

## Quick Start

```r
# Install core dependencies
install.packages(c("shiny", "shinydashboard", "haven", "survival",
                   "survminer", "rpact", "DT", "ggplot2", "plotly"))

# Install M3 event projection dependencies
install.packages(c("eventTrack", "fitdistrplus"))

# Run the app
shiny::runApp()
```

## Test Data

Sample datasets for testing M3 Event Projection are provided under `inst/extdata/`:

| File | Endpoint | N | Events | Distribution |
|------|----------|---|--------|--------------|
| `test_os_events.csv` | OS | 400 | ~298 (75%) | Weibull(shape=1.4, scale=28), median ~24 months |
| `test_pfs_events.csv` | PFS | 400 | ~296 (74%) | Weibull(shape=1.2, scale=14), median ~12 months |

Upload either file in M3, select the time and event columns, set time unit to Months.

## Project Structure

```
app.R                         Entry point
ui.R                          Dashboard UI layout
server.R                      Server orchestration
global.R                      Library loading and module sourcing
DESCRIPTION                   Package metadata
inst/
  extdata/
    test_os_events.csv         Sample OS dataset for M3 testing
    test_pfs_events.csv        Sample PFS dataset for M3 testing
R/
  mod_data_upload.R            Shared SAS file upload module
  mod_ia_boundary.R            M1a: IA boundary re-calculation (rpact)
  mod_sample_size.R            M1b: Sample size -- PH (rpact) + NPH skeleton (gsDesign2)
  mod_pfs_raw.R                M2a: PFS from raw CRF/EDC data
  mod_pfs_sdtm.R               M2b: PFS from SDTM datasets
  mod_pfs_adam_compare.R       M2c: ADaM key-variable comparison
  mod_event_projection.R       M3: Event projection (Moving Avg + eventTrack)
  utils_data.R                 Data I/O and validation helpers
  utils_compare.R              Comparison logic (tolerance-based)
  utils_pfs_derive.R           Shared survival analysis engine
  utils_report.R               Report generation helpers
tests/
  testthat/                    Unit tests (pending)
```
