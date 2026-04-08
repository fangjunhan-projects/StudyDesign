#!/usr/bin/env Rscript
# Run BiostatVD Shiny App

setwd(dirname(rstudioapi::getSourceEditorContext()$path))
shiny::runApp(host = "0.0.0.0", port = 3838)
