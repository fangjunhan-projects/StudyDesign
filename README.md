# StudyDesign — Clinical Trial Study Design Toolkit

R-based toolkit for clinical trial study design calculations. Includes a Shiny dashboard for interactive use, agent-agnostic skill documents for AI-assisted workflows, and reference test files.

## Modules

| Module | Description |
|--------|-------------|
| IA Boundary | Re-calculate group sequential boundaries (rpact); alpha/beta spending functions, HR-scale output |
| Sample Size (PH) | Sample size for proportional hazards survival models (rpact); group sequential with spending functions |
| Sample Size (NPH) | Sample size for non-proportional hazards — delayed treatment effect (gsDesign2); piecewise enrollment, flexible boundaries |

## Skills

Agent-agnostic skill documents in `skills/` define how to perform each calculation. Any AI agent (or human) can follow them to reproduce results, regardless of the tool being used.

| Skill | File | Package |
|-------|------|---------|
| IA Boundary Re-calculation | `skills/ia_boundary.md` | `rpact` |
| Sample Size — Proportional Hazards | `skills/sample_size_ph.md` | `rpact` |
| Sample Size — Non-Proportional Hazards | `skills/sample_size_nph.md` | `gsDesign2` |

Each skill defines: Purpose, Execution (3-tier: run code / generate code / plain language), Inputs, Procedure (R code), Output, Validation Rules, and a Verified Example.

## Requirements

- R >= 4.4.0
- Core packages: `shiny`, `shinydashboard`, `rpact`, `DT`, `ggplot2`, `plotly`, `scales`
- NPH packages: `gsDesign2`, `gsDesign`

## Quick Start

```r
# Install core dependencies
install.packages(c("shiny", "shinydashboard", "rpact", "DT", "ggplot2", "plotly", "scales"))

# Install NPH dependencies
install.packages(c("gsDesign2", "gsDesign"))

# Run the Shiny app
shiny::runApp()
```

## Project Structure

```
app.R                         Shiny entry point
ui.R                          Dashboard UI layout
server.R                      Server orchestration
global.R                      Library loading and module sourcing
DESCRIPTION                   Package metadata
R/
  mod_ia_boundary.R            IA boundary re-calculation module
  mod_sample_size.R            Sample size module (PH + NPH)
  utils_compare.R              Comparison logic (tolerance-based)
  utils_data.R                 Data I/O and validation helpers
  utils_pfs_derive.R           Shared survival analysis engine
  utils_report.R               Report generation helpers
skills/
  ia_boundary.md               Skill: IA boundary re-calculation
  sample_size_ph.md            Skill: Sample size under PH
  sample_size_nph.md           Skill: Sample size under NPH (delayed effect)
tests/
  test_nph_delayed.R           Reference test: NPH delayed treatment effect
```
