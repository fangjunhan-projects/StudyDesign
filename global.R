library(shiny)
library(shinydashboard)
library(haven)
library(survival)
library(survminer)
library(rpact)
library(DT)
library(ggplot2)
library(plotly)
library(dplyr)
library(mvtnorm)

# M1b NPH dependencies (gsDesign2 + gsDesign for spending functions)
# install.packages(c("gsDesign2", "gsDesign", "scales"))

`%||%` <- function(x, y) if (is.null(x)) y else x

source("R/utils_data.R")
source("R/utils_compare.R")
source("R/utils_pfs_derive.R")
source("R/utils_report.R")
source("R/mod_sample_size.R")
source("R/mod_futility.R")
