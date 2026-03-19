# ---------------------------------------------------------------------------
# Report generation helpers
# ---------------------------------------------------------------------------

#' Format a comparison table for display with DT
#' Highlights Pass/Fail cells with colour
render_comparison_dt <- function(comp_df) {
  DT::datatable(
    comp_df,
    escape = FALSE,
    options = list(
      dom = "t",
      pageLength = 50,
      columnDefs = list(
        list(className = "dt-center", targets = "_all")
      )
    ),
    rownames = FALSE
  ) |>
    DT::formatStyle(
      "Result",
      color = DT::styleEqual(c("Pass", "Fail"), c("#28a745", "#dc3545")),
      fontWeight = "bold"
    )
}

#' Create a summary info box for counts
summary_value_box <- function(title, value, icon_name = "check", color = "green") {
  shinydashboard::valueBox(
    value = value,
    subtitle = title,
    icon = icon(icon_name),
    color = color
  )
}
