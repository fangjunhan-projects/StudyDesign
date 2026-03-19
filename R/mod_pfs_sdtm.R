# ---------------------------------------------------------------------------
# Module 2b: PFS Validation from SDTM Datasets
# ---------------------------------------------------------------------------

mod_pfs_sdtm_ui <- function(id) {
  ns <- NS(id)
  fluidRow(
    box(
      title = "PFS Validation -- SDTM Path", status = "primary",
      solidHeader = TRUE, width = 4,
      mod_data_upload_ui(ns("sdtm_upload"), label = "Upload SDTM Datasets (DM, RS, TU, DS)", multiple = TRUE),
      hr(),
      h5("Analysis Settings"),
      selectInput(ns("rs_criteria"), "Response Criteria (RSTESTCD)",
                  choices = c("OVRLRESP" = "OVRLRESP", "BORRESP" = "BORRESP"),
                  selected = "OVRLRESP"),
      selectInput(ns("trt_var"), "Treatment Variable",
                  choices = c("ARM" = "ARM", "ACTARM" = "ACTARM"), selected = "ARM"),
      textInput(ns("ref_group"), "Reference Group", placeholder = "e.g. Placebo"),
      textInput(ns("population_filter"), "Population Filter (optional)",
                placeholder = "e.g. ITTFL == 'Y'"),
      hr(),
      h5("Sponsor's Reported Results"),
      numericInput(ns("sponsor_hr"), "Reported HR", value = NA, step = 0.01),
      numericInput(ns("sponsor_hr_lcl"), "HR Lower 95% CI", value = NA, step = 0.01),
      numericInput(ns("sponsor_hr_ucl"), "HR Upper 95% CI", value = NA, step = 0.01),
      numericInput(ns("sponsor_logrank_p"), "Reported Log-rank p-value", value = NA, step = 0.0001),
      numericInput(ns("tolerance"), "Comparison Tolerance", value = 1e-4, step = 1e-4),
      actionButton(ns("run_analysis"), "Run PFS Analysis", class = "btn-primary")
    ),

    box(
      title = "Results", status = "success", solidHeader = TRUE, width = 8,
      tabsetPanel(
        tabPanel("Event Summary", DT::dataTableOutput(ns("event_table"))),
        tabPanel("KM Summary", DT::dataTableOutput(ns("km_table"))),
        tabPanel("Comparison", DT::dataTableOutput(ns("comparison_table"))),
        tabPanel("KM Plot", plotOutput(ns("km_plot"), height = "550px")),
        tabPanel("Details", verbatimTextOutput(ns("cox_summary")))
      )
    )
  )
}

mod_pfs_sdtm_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    sdtm_data <- mod_data_upload_server("sdtm_upload")

    analysis_result <- eventReactive(input$run_analysis, {
      req(sdtm_data())
      datasets <- sdtm_data()

      validate(need("DM" %in% names(datasets), "DM dataset is required"))
      validate(need("RS" %in% names(datasets) || "TU" %in% names(datasets),
                    "RS or TU dataset is required"))

      dm <- datasets[["DM"]]
      rs <- datasets[["RS"]]
      ds <- datasets[["DS"]]

      # Derive PFS from SDTM
      pfs_data <- derive_pfs_from_sdtm(dm, rs, ds,
                                        trt_var = input$trt_var,
                                        rs_criteria = input$rs_criteria)

      if (nzchar(input$population_filter)) {
        pfs_data <- tryCatch(
          subset(pfs_data, eval(parse(text = input$population_filter))),
          error = function(e) { showNotification(paste("Filter error:", e$message), type = "error"); pfs_data }
        )
      }

      ref <- if (nzchar(input$ref_group)) input$ref_group else NULL
      run_pfs_analysis(pfs_data, trt_var = "TRT", ref_group = ref)
    })

    output$event_table <- DT::renderDataTable({
      req(analysis_result())
      DT::datatable(analysis_result()$event_summary, rownames = FALSE,
                    options = list(dom = "t"))
    })

    output$km_table <- DT::renderDataTable({
      req(analysis_result())
      DT::datatable(analysis_result()$km_summary, rownames = FALSE,
                    options = list(dom = "t")) |>
        DT::formatRound(columns = c("Median", "LCL", "UCL"), digits = 3)
    })

    output$comparison_table <- DT::renderDataTable({
      req(analysis_result())
      res <- analysis_result()
      cox_res <- extract_cox_results(res$cox_summary)
      tol <- input$tolerance

      rows <- list()
      if (!is.na(input$sponsor_hr)) {
        rows[[length(rows) + 1]] <- comparison_row("Hazard Ratio", cox_res$HR, input$sponsor_hr,
                                                    compare_numeric(cox_res$HR, input$sponsor_hr, tol))
      }
      if (!is.na(input$sponsor_hr_lcl)) {
        rows[[length(rows) + 1]] <- comparison_row("HR Lower 95% CI", cox_res$HR_LCL, input$sponsor_hr_lcl,
                                                    compare_numeric(cox_res$HR_LCL, input$sponsor_hr_lcl, tol))
      }
      if (!is.na(input$sponsor_hr_ucl)) {
        rows[[length(rows) + 1]] <- comparison_row("HR Upper 95% CI", cox_res$HR_UCL, input$sponsor_hr_ucl,
                                                    compare_numeric(cox_res$HR_UCL, input$sponsor_hr_ucl, tol))
      }
      if (!is.na(input$sponsor_logrank_p)) {
        rows[[length(rows) + 1]] <- comparison_row("Log-rank p-value", res$logrank_p, input$sponsor_logrank_p,
                                                    compare_numeric(res$logrank_p, input$sponsor_logrank_p, tol))
      }

      if (length(rows) == 0) {
        comp_df <- data.frame(Metric = "No sponsor values entered", Derived = "", Reported = "",
                              Diff = "", Result = "", stringsAsFactors = FALSE)
      } else {
        comp_df <- do.call(rbind, rows)
      }
      render_comparison_dt(comp_df)
    })

    output$km_plot <- renderPlot({
      req(analysis_result())
      survminer::ggsurvplot(
        analysis_result()$km_fit,
        data = NULL,
        risk.table = TRUE,
        pval = TRUE,
        conf.int = TRUE,
        xlab = "Time",
        ylab = "PFS Probability",
        title = "Kaplan-Meier Curve -- PFS (SDTM Path)"
      )
    })

    output$cox_summary <- renderPrint({
      req(analysis_result())
      print(analysis_result()$cox_summary)
    })
  })
}

# ---------------------------------------------------------------------------
# SDTM -> PFS derivation helper
# ---------------------------------------------------------------------------
derive_pfs_from_sdtm <- function(dm, rs = NULL, ds = NULL,
                                  trt_var = "ARM",
                                  rs_criteria = "OVRLRESP") {
  dm <- standardise_names(dm)

  # Randomisation / reference start date
  if ("RFSTDTC" %in% names(dm)) {
    dm$STARTDT <- as.Date(substr(dm$RFSTDTC, 1, 10))
  } else if ("RFICDTC" %in% names(dm)) {
    dm$STARTDT <- as.Date(substr(dm$RFICDTC, 1, 10))
  } else {
    stop("DM must contain RFSTDTC or RFICDTC for start date derivation")
  }

  dm$TRT <- dm[[trt_var]]
  base <- dm[, c("USUBJID", "STARTDT", "TRT"), drop = FALSE]

  # Progressive disease date from RS
  prog_date <- NULL
  if (!is.null(rs)) {
    rs <- standardise_names(rs)
    pd_records <- rs[rs$RSTESTCD == rs_criteria & toupper(rs$RSSTRESC) == "PD", ]
    if (nrow(pd_records) > 0) {
      pd_records$RSDTC_D <- as.Date(substr(pd_records$RSDTC, 1, 10))
      prog_date <- aggregate(RSDTC_D ~ USUBJID, data = pd_records, FUN = min)
      names(prog_date)[2] <- "PROGDT"
    }
  }

  # Death date from DS
  death_date <- NULL
  if (!is.null(ds)) {
    ds <- standardise_names(ds)
    death_records <- ds[toupper(ds$DSDECOD) == "DEATH", ]
    if (nrow(death_records) > 0) {
      death_records$DSSTDTC_D <- as.Date(substr(death_records$DSSTDTC, 1, 10))
      death_date <- aggregate(DSSTDTC_D ~ USUBJID, data = death_records, FUN = min)
      names(death_date)[2] <- "DEATHDT"
    }
  }

  # Last known alive / assessment date from RS (for censoring)
  last_assess <- NULL
  if (!is.null(rs)) {
    rs$RSDTC_D <- as.Date(substr(rs$RSDTC, 1, 10))
    last_assess <- aggregate(RSDTC_D ~ USUBJID, data = rs, FUN = max)
    names(last_assess)[2] <- "LASTDT"
  }

  # Merge
  pfs <- base
  if (!is.null(prog_date))  pfs <- merge(pfs, prog_date, by = "USUBJID", all.x = TRUE)
  if (!is.null(death_date)) pfs <- merge(pfs, death_date, by = "USUBJID", all.x = TRUE)
  if (!is.null(last_assess)) pfs <- merge(pfs, last_assess, by = "USUBJID", all.x = TRUE)

  # Ensure columns exist
  if (!"PROGDT"  %in% names(pfs)) pfs$PROGDT  <- as.Date(NA)
  if (!"DEATHDT" %in% names(pfs)) pfs$DEATHDT <- as.Date(NA)
  if (!"LASTDT"  %in% names(pfs)) pfs$LASTDT  <- as.Date(NA)

  # PFS event: earliest of progression or death
  pfs$EVENTDT <- pmin(pfs$PROGDT, pfs$DEATHDT, na.rm = TRUE)
  pfs$HAS_EVENT <- !is.na(pfs$EVENTDT)

  # Censor date: last assessment if no event
  pfs$CNSR <- ifelse(pfs$HAS_EVENT, 0, 1)
  pfs$ADT  <- ifelse(pfs$HAS_EVENT, pfs$EVENTDT, pfs$LASTDT)
  pfs$ADT  <- as.Date(pfs$ADT, origin = "1970-01-01")
  pfs$AVAL <- as.numeric(pfs$ADT - pfs$STARTDT)

  # Remove rows without calculable time
  pfs <- pfs[!is.na(pfs$AVAL) & pfs$AVAL >= 0, ]

  pfs
}
