# SampleSize -- Study Design Calculation Dashboard

R Shiny application for study design calculations including IA boundary re-calculation and sample size planning.

## Modules

| Module | Sub-modules | Description |
|--------|-------------|-------------|
| M1: Study Design Calculation | IA Boundary | Re-calculate group sequential boundaries using rpact; supports multiple alpha/beta spending functions and HR-scale boundary output |
| | Sample Size (PH) | Sample size for PH survival models (rpact); group sequential with spending functions, HR-scale boundaries |
| | Sample Size (NPH) | Sample size for NPH survival models (gsDesign2); delayed treatment effect implemented; group sequential supported |

## Requirements

- R >= 4.1.0
- Core packages: `shiny`, `shinydashboard`, `rpact`, `DT`, `ggplot2`, `plotly`, `scales`
- Sample Size NPH: `gsDesign2`, `gsDesign`

## Quick Start

```r
# Install core dependencies
install.packages(c("shiny", "shinydashboard", "rpact", "DT", "ggplot2", "plotly", "scales"))

# Install NPH dependencies
install.packages(c("gsDesign2", "gsDesign"))

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
