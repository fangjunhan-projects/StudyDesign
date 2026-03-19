# BiostatVD -- Biostatistics Validation Dashboard

R Shiny application for automated validation of efficacy results in clinical trials.

## Modules

| Module | Description |
|--------|-------------|
| M1: IA Boundary | Re-calculate interim analysis boundaries using rpact and compare against sponsor values |
| M2: PFS Efficacy | Validate PFS results via three sub-tabs: Raw CRF data path, SDTM path, and ADaM key-variable comparison |
| M3: Additional | Placeholder for future efficacy checks (ORR, OS, subgroups) |

## Requirements

- R >= 4.1.0
- Key packages: `shiny`, `shinydashboard`, `haven`, `survival`, `survminer`, `rpact`, `DT`

## Quick Start

```r
# Install dependencies
install.packages(c("shiny", "shinydashboard", "haven", "survival",
                    "survminer", "rpact", "DT", "ggplot2", "plotly", "rmarkdown"))

# Run the app
shiny::runApp()
```

## Project Structure

```
app.R                        Entry point
ui.R                         Dashboard UI layout
server.R                     Server orchestration
R/
  mod_data_upload.R           Shared SAS file upload module
  mod_ia_boundary.R           Module 1: IA boundary re-calculation
  mod_pfs_raw.R               Module 2a: PFS from raw CRF/EDC data
  mod_pfs_sdtm.R              Module 2b: PFS from SDTM datasets
  mod_pfs_adam_compare.R      Module 2c: ADaM key-variable comparison
  utils_data.R                Data I/O and validation helpers
  utils_compare.R             Comparison logic (tolerance-based)
  utils_pfs_derive.R          Shared survival analysis engine
  utils_report.R              Report generation helpers
```
