# SampleSize -- Study Design Calculation Dashboard

R Shiny application for study design calculations including IA boundary re-calculation and sample size planning.

## Modules

| Module | Sub-modules | Description |
|--------|-------------|-------------|
| M1: Study Design Calculation | IA Boundary | Re-calculate group sequential boundaries using rpact; supports multiple alpha/beta spending functions and HR-scale boundary output |
| | Sample Size | Sample size calculation for PH models (rpact); NPH models via gsDesign2 (coming soon) |

## Requirements

- R >= 4.1.0
- Core packages: `shiny`, `shinydashboard`, `rpact`, `DT`, `ggplot2`, `plotly`
- Sample Size NPH (coming soon): `gsDesign2`

## Quick Start

```r
# Install core dependencies
install.packages(c("shiny", "shinydashboard", "rpact", "DT", "ggplot2", "plotly"))

# Run the app
shiny::runApp()
```

## Project Structure

```
app.R                         Entry point
ui.R                          Dashboard UI layout
server.R                      Server orchestration
global.R                      Library loading and module sourcing
DESCRIPTION                   Package metadata
R/
  mod_ia_boundary.R            M1a: IA boundary re-calculation (rpact)
  mod_sample_size.R            M1b: Sample size -- PH (rpact) + NPH skeleton (gsDesign2)
  utils_compare.R              Comparison logic (tolerance-based)
  utils_data.R                 Data I/O and validation helpers
  utils_pfs_derive.R           Shared survival analysis engine
  utils_report.R               Report generation helpers
```
