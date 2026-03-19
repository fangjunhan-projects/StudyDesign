# ---------------------------------------------------------------------------
# Module 2c: ADaM Key-Variable Comparison
# ---------------------------------------------------------------------------

ADTTE_KEY_VARS <- c("AVAL", "CNSR", "ADT", "STARTDT", "EVNTDESC", "PARAMCD")
ADSL_KEY_VARS  <- c("TRT01P", "TRT01A", "RANDDT", "ITTFL", "SAFFL")

mod_pfs_adam_compare_ui <- function(id) {
  ns <- NS(id)
  fluidRow(
    box(
      title = "ADaM Key-Variable Comparison", status = "primary",
      solidHeader = TRUE, width = 4,
      h5("Upload Re-derived Dataset"),
      mod_data_upload_ui(ns("derived_upload"), label = "Upload Re-derived ADTTE / ADSL", multiple = TRUE),
      hr(),
      h5("Upload Sponsor's ADaM"),
      mod_data_upload_ui(ns("sponsor_upload"), label = "Upload Sponsor ADTTE / ADSL", multiple = TRUE),
      hr(),
      h5("Comparison Settings"),
      selectInput(ns("dataset_choice"), "Dataset to Compare",
                  choices = c("ADTTE", "ADSL"), selected = "ADTTE"),
      checkboxGroupInput(
        ns("key_vars_adtte"), "ADTTE Key Variables",
        choices = ADTTE_KEY_VARS, selected = ADTTE_KEY_VARS
      ),
      checkboxGroupInput(
        ns("key_vars_adsl"), "ADSL Key Variables",
        choices = ADSL_KEY_VARS, selected = ADSL_KEY_VARS
      ),
      numericInput(ns("num_tolerance"), "Numeric Tolerance", value = 1e-6, step = 1e-6),
      numericInput(ns("date_tolerance"), "Date Tolerance (days)", value = 0, min = 0, step = 1),
      actionButton(ns("run_compare"), "Run Comparison", class = "btn-primary")
    ),

    box(
      title = "Comparison Results", status = "success", solidHeader = TRUE, width = 8,
      fluidRow(
        valueBoxOutput(ns("vb_total"), width = 3),
        valueBoxOutput(ns("vb_matched"), width = 3),
        valueBoxOutput(ns("vb_mismatched"), width = 3),
        valueBoxOutput(ns("vb_pct"), width = 3)
      ),
      tabsetPanel(
        tabPanel("Mismatch Details", DT::dataTableOutput(ns("mismatch_table"))),
        tabPanel("Full Merge Preview", DT::dataTableOutput(ns("merge_preview"))),
        tabPanel(
          "Download",
          br(),
          downloadButton(ns("download_mismatches"), "Download Mismatches (CSV)")
        )
      )
    )
  )
}

mod_pfs_adam_compare_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    derived_data <- mod_data_upload_server("derived_upload")
    sponsor_data <- mod_data_upload_server("sponsor_upload")

    compare_result <- eventReactive(input$run_compare, {
      req(derived_data(), sponsor_data())

      ds_name <- input$dataset_choice
      validate(need(ds_name %in% names(derived_data()),
                    paste("Re-derived", ds_name, "not found in uploaded files")))
      validate(need(ds_name %in% names(sponsor_data()),
                    paste("Sponsor", ds_name, "not found in uploaded files")))

      derived_df <- derived_data()[[ds_name]]
      sponsor_df <- sponsor_data()[[ds_name]]

      key_vars <- if (ds_name == "ADTTE") input$key_vars_adtte else input$key_vars_adsl

      # Filter to common key vars that exist in both datasets
      key_vars <- intersect(key_vars, intersect(names(derived_df), names(sponsor_df)))

      mismatches <- compare_datasets_by_subject(
        derived_df  = derived_df,
        sponsor_df  = sponsor_df,
        by_var      = "USUBJID",
        key_vars    = key_vars,
        num_tolerance  = input$num_tolerance,
        date_tolerance = input$date_tolerance
      )

      n_subjects <- length(intersect(derived_df$USUBJID, sponsor_df$USUBJID))
      n_mismatched <- length(unique(mismatches$USUBJID))
      n_matched <- n_subjects - n_mismatched

      list(
        mismatches   = mismatches,
        n_subjects   = n_subjects,
        n_matched    = n_matched,
        n_mismatched = n_mismatched,
        derived_df   = derived_df,
        sponsor_df   = sponsor_df
      )
    })

    output$vb_total <- renderValueBox({
      req(compare_result())
      summary_value_box("Total Subjects Compared", compare_result()$n_subjects,
                        icon_name = "users", color = "blue")
    })

    output$vb_matched <- renderValueBox({
      req(compare_result())
      summary_value_box("Matched", compare_result()$n_matched,
                        icon_name = "check", color = "green")
    })

    output$vb_mismatched <- renderValueBox({
      req(compare_result())
      n <- compare_result()$n_mismatched
      summary_value_box("Mismatched", n,
                        icon_name = if (n == 0) "check" else "exclamation-triangle",
                        color = if (n == 0) "green" else "red")
    })

    output$vb_pct <- renderValueBox({
      req(compare_result())
      res <- compare_result()
      pct <- if (res$n_subjects > 0) round(100 * res$n_matched / res$n_subjects, 1) else 0
      summary_value_box("Agreement %", paste0(pct, "%"),
                        icon_name = "percentage",
                        color = if (pct >= 100) "green" else "yellow")
    })

    output$mismatch_table <- DT::renderDataTable({
      req(compare_result())
      DT::datatable(
        compare_result()$mismatches,
        rownames = FALSE,
        filter = "top",
        options = list(pageLength = 25, scrollX = TRUE)
      )
    })

    output$merge_preview <- DT::renderDataTable({
      req(compare_result())
      res <- compare_result()
      merged <- merge(res$derived_df, res$sponsor_df, by = "USUBJID",
                      suffixes = c(".derived", ".sponsor"))
      DT::datatable(
        head(merged, 200),
        rownames = FALSE,
        options = list(scrollX = TRUE, pageLength = 20)
      )
    })

    output$download_mismatches <- downloadHandler(
      filename = function() {
        paste0("mismatches_", input$dataset_choice, "_", Sys.Date(), ".csv")
      },
      content = function(file) {
        write.csv(compare_result()$mismatches, file, row.names = FALSE)
      }
    )
  })
}
