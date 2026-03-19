# ---------------------------------------------------------------------------
# Module 2a: PFS Validation from CRF/EDC Raw Data
# ---------------------------------------------------------------------------

mod_pfs_raw_ui <- function(id) {
  ns <- NS(id)
  fluidRow(
    box(
      title = "PFS Validation -- Raw CRF/EDC Data Path", status = "primary",
      solidHeader = TRUE, width = 4,
      mod_data_upload_ui(ns("raw_upload"), label = "Upload Raw CRF/EDC Export Files", multiple = TRUE),
      hr(),
      h5("Field Mapping"),
      p("Map uploaded dataset columns to required PFS derivation fields.
         Select the dataset and column for each field below."),
      selectInput(ns("ds_subject"), "Subject ID Dataset", choices = NULL),
      selectInput(ns("col_subject"), "Subject ID Column", choices = NULL),
      selectInput(ns("ds_randdt"), "Randomisation Date Dataset", choices = NULL),
      selectInput(ns("col_randdt"), "Randomisation Date Column", choices = NULL),
      selectInput(ns("ds_trt"), "Treatment Dataset", choices = NULL),
      selectInput(ns("col_trt"), "Treatment Column", choices = NULL),
      selectInput(ns("ds_progdt"), "Progression Date Dataset", choices = NULL),
      selectInput(ns("col_progdt"), "Progression Date Column", choices = NULL),
      selectInput(ns("ds_deathdt"), "Death Date Dataset", choices = NULL),
      selectInput(ns("col_deathdt"), "Death Date Column", choices = NULL),
      selectInput(ns("ds_lastdt"), "Last Assessment Date Dataset", choices = NULL),
      selectInput(ns("col_lastdt"), "Last Assessment Date Column", choices = NULL),
      hr(),
      textInput(ns("ref_group"), "Reference Treatment Group", placeholder = "e.g. Placebo"),
      numericInput(ns("tolerance"), "Comparison Tolerance", value = 1e-4, step = 1e-4),
      hr(),
      h5("Sponsor's Reported Results"),
      numericInput(ns("sponsor_hr"), "Reported HR", value = NA, step = 0.01),
      numericInput(ns("sponsor_hr_lcl"), "HR Lower 95% CI", value = NA, step = 0.01),
      numericInput(ns("sponsor_hr_ucl"), "HR Upper 95% CI", value = NA, step = 0.01),
      numericInput(ns("sponsor_logrank_p"), "Reported Log-rank p-value", value = NA, step = 0.0001),
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

mod_pfs_raw_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    raw_data <- mod_data_upload_server("raw_upload")

    # Update dataset and column choices when data is uploaded
    observe({
      req(raw_data())
      ds_names <- names(raw_data())
      for (sel_id in c("ds_subject", "ds_randdt", "ds_trt", "ds_progdt", "ds_deathdt", "ds_lastdt")) {
        updateSelectInput(session, sel_id, choices = ds_names)
      }
    })

    # Update column choices based on selected dataset
    observe_col_update <- function(ds_input, col_input) {
      observe({
        req(raw_data(), input[[ds_input]])
        ds <- raw_data()[[input[[ds_input]]]]
        if (!is.null(ds)) {
          updateSelectInput(session, col_input, choices = names(ds))
        }
      })
    }
    observe_col_update("ds_subject", "col_subject")
    observe_col_update("ds_randdt",  "col_randdt")
    observe_col_update("ds_trt",     "col_trt")
    observe_col_update("ds_progdt",  "col_progdt")
    observe_col_update("ds_deathdt", "col_deathdt")
    observe_col_update("ds_lastdt",  "col_lastdt")

    analysis_result <- eventReactive(input$run_analysis, {
      req(raw_data())
      datasets <- raw_data()

      pfs_data <- derive_pfs_from_raw(
        datasets    = datasets,
        subject_ds  = input$ds_subject,  subject_col  = input$col_subject,
        randdt_ds   = input$ds_randdt,   randdt_col   = input$col_randdt,
        trt_ds      = input$ds_trt,      trt_col      = input$col_trt,
        progdt_ds   = input$ds_progdt,   progdt_col   = input$col_progdt,
        deathdt_ds  = input$ds_deathdt,  deathdt_col  = input$col_deathdt,
        lastdt_ds   = input$ds_lastdt,   lastdt_col   = input$col_lastdt
      )

      ref <- if (nzchar(input$ref_group)) input$ref_group else NULL
      run_pfs_analysis(pfs_data, trt_var = "TRT", ref_group = ref)
    })

    output$event_table <- DT::renderDataTable({
      req(analysis_result())
      DT::datatable(analysis_result()$event_summary, rownames = FALSE, options = list(dom = "t"))
    })

    output$km_table <- DT::renderDataTable({
      req(analysis_result())
      DT::datatable(analysis_result()$km_summary, rownames = FALSE, options = list(dom = "t")) |>
        DT::formatRound(columns = c("Median", "LCL", "UCL"), digits = 3)
    })

    output$comparison_table <- DT::renderDataTable({
      req(analysis_result())
      res <- analysis_result()
      cox_res <- extract_cox_results(res$cox_summary)
      tol <- input$tolerance

      rows <- list()
      if (!is.na(input$sponsor_hr))
        rows[[length(rows) + 1]] <- comparison_row("Hazard Ratio", cox_res$HR, input$sponsor_hr,
                                                    compare_numeric(cox_res$HR, input$sponsor_hr, tol))
      if (!is.na(input$sponsor_hr_lcl))
        rows[[length(rows) + 1]] <- comparison_row("HR Lower 95% CI", cox_res$HR_LCL, input$sponsor_hr_lcl,
                                                    compare_numeric(cox_res$HR_LCL, input$sponsor_hr_lcl, tol))
      if (!is.na(input$sponsor_hr_ucl))
        rows[[length(rows) + 1]] <- comparison_row("HR Upper 95% CI", cox_res$HR_UCL, input$sponsor_hr_ucl,
                                                    compare_numeric(cox_res$HR_UCL, input$sponsor_hr_ucl, tol))
      if (!is.na(input$sponsor_logrank_p))
        rows[[length(rows) + 1]] <- comparison_row("Log-rank p-value", res$logrank_p, input$sponsor_logrank_p,
                                                    compare_numeric(res$logrank_p, input$sponsor_logrank_p, tol))

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
        risk.table = TRUE, pval = TRUE, conf.int = TRUE,
        xlab = "Time", ylab = "PFS Probability",
        title = "Kaplan-Meier Curve -- PFS (Raw Data Path)"
      )
    })

    output$cox_summary <- renderPrint({
      req(analysis_result())
      print(analysis_result()$cox_summary)
    })
  })
}

# ---------------------------------------------------------------------------
# Raw CRF -> PFS derivation helper
# ---------------------------------------------------------------------------
derive_pfs_from_raw <- function(datasets,
                                 subject_ds, subject_col,
                                 randdt_ds, randdt_col,
                                 trt_ds, trt_col,
                                 progdt_ds, progdt_col,
                                 deathdt_ds, deathdt_col,
                                 lastdt_ds, lastdt_col) {

  get_col <- function(ds_name, col_name) {
    if (is.null(ds_name) || !nzchar(ds_name)) return(NULL)
    if (!ds_name %in% names(datasets)) return(NULL)
    df <- datasets[[ds_name]]
    if (!col_name %in% names(df)) return(NULL)
    df[, c("USUBJID_RAW" , col_name), drop = FALSE]
  }

  # Build subject-level base from subject ID dataset
  subj_df <- datasets[[subject_ds]]
  subj_df$USUBJID <- subj_df[[subject_col]]

  # Randomisation date
  rand_df <- datasets[[randdt_ds]]
  rand_df$USUBJID <- rand_df[[subject_col]]
  rand_df$STARTDT <- as.Date(rand_df[[randdt_col]])

  # Treatment
  trt_df <- datasets[[trt_ds]]
  trt_df$USUBJID <- trt_df[[subject_col]]
  trt_df$TRT <- trt_df[[trt_col]]

  base <- merge(
    rand_df[, c("USUBJID", "STARTDT"), drop = FALSE],
    trt_df[, c("USUBJID", "TRT"), drop = FALSE],
    by = "USUBJID"
  )

  # Progression date
  prog_df <- datasets[[progdt_ds]]
  prog_df$USUBJID <- prog_df[[subject_col]]
  prog_df$PROGDT <- as.Date(prog_df[[progdt_col]])
  prog_agg <- aggregate(PROGDT ~ USUBJID, data = prog_df[!is.na(prog_df$PROGDT), ], FUN = min)
  base <- merge(base, prog_agg, by = "USUBJID", all.x = TRUE)

  # Death date
  death_df <- datasets[[deathdt_ds]]
  death_df$USUBJID <- death_df[[subject_col]]
  death_df$DEATHDT <- as.Date(death_df[[deathdt_col]])
  death_agg <- aggregate(DEATHDT ~ USUBJID, data = death_df[!is.na(death_df$DEATHDT), ], FUN = min)
  base <- merge(base, death_agg, by = "USUBJID", all.x = TRUE)

  # Last assessment date (for censoring)
  last_df <- datasets[[lastdt_ds]]
  last_df$USUBJID <- last_df[[subject_col]]
  last_df$LASTDT <- as.Date(last_df[[lastdt_col]])
  last_agg <- aggregate(LASTDT ~ USUBJID, data = last_df[!is.na(last_df$LASTDT), ], FUN = max)
  base <- merge(base, last_agg, by = "USUBJID", all.x = TRUE)

  # Derive PFS
  base$EVENTDT <- pmin(base$PROGDT, base$DEATHDT, na.rm = TRUE)
  base$HAS_EVENT <- !is.na(base$EVENTDT)
  base$CNSR <- ifelse(base$HAS_EVENT, 0, 1)
  base$ADT  <- ifelse(base$HAS_EVENT, base$EVENTDT, base$LASTDT)
  base$ADT  <- as.Date(base$ADT, origin = "1970-01-01")
  base$AVAL <- as.numeric(base$ADT - base$STARTDT)

  base <- base[!is.na(base$AVAL) & base$AVAL >= 0, ]
  base
}
