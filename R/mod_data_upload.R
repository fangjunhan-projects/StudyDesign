# ---------------------------------------------------------------------------
# Shared reusable data upload module
# ---------------------------------------------------------------------------

#' Data Upload Module UI
#' @param id Module namespace id
#' @param label Label for the file input
#' @param multiple Allow multiple file uploads
mod_data_upload_ui <- function(id, label = "Upload SAS Dataset(s)",
                               multiple = TRUE) {
  ns <- NS(id)
  tagList(
    fileInput(
      ns("file_input"),
      label = label,
      multiple = multiple,
      accept = c(".sas7bdat", ".xpt")
    ),
    uiOutput(ns("upload_status"))
  )
}

#' Data Upload Module Server
#' @param id Module namespace id
#' @param required_datasets Optional named list: name -> required columns vector.
#'   e.g. list(ADTTE = c("USUBJID","AVAL","CNSR"), ADSL = c("USUBJID","TRT01P"))
#' @return A reactive list of loaded datasets (named by file stem, uppercased)
mod_data_upload_server <- function(id, required_datasets = NULL) {
  moduleServer(id, function(input, output, session) {
    datasets <- reactiveVal(list())

    observeEvent(input$file_input, {
      req(input$file_input)
      files <- input$file_input
      loaded <- list()
      messages <- list()

      for (i in seq_len(nrow(files))) {
        name <- toupper(tools::file_path_sans_ext(files$name[i]))
        tryCatch({
          df <- read_sas_file(files$datapath[i])
          df <- standardise_names(df)
          loaded[[name]] <- df
          messages[[length(messages) + 1]] <- tags$li(
            icon("check-circle", class = "text-success"),
            paste0(name, ": ", nrow(df), " rows, ", ncol(df), " cols")
          )
        }, error = function(e) {
          messages[[length(messages) + 1]] <<- tags$li(
            icon("times-circle", class = "text-danger"),
            paste0(files$name[i], ": ", e$message)
          )
        })
      }

      # Validate schemas if required_datasets specified
      if (!is.null(required_datasets)) {
        for (ds_name in names(required_datasets)) {
          if (ds_name %in% names(loaded)) {
            check <- validate_required_cols(loaded[[ds_name]], required_datasets[[ds_name]])
            if (!check$valid) {
              messages[[length(messages) + 1]] <- tags$li(
                icon("exclamation-triangle", class = "text-warning"),
                paste0(ds_name, " missing columns: ", paste(check$missing, collapse = ", "))
              )
            }
          }
        }
      }

      datasets(loaded)

      output$upload_status <- renderUI({
        tags$ul(style = "list-style: none; padding-left: 0;", messages)
      })
    })

    return(datasets)
  })
}
