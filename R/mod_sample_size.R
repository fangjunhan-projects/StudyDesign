# ---------------------------------------------------------------------------
# Module: Sample Size Calculation
#   Sub-tab 1: Proportional Hazard (PH)  — powered by rpact
#   Sub-tab 2: Non-Proportional Hazard (NPH) — powered by gsDesign2
# ---------------------------------------------------------------------------

mod_sample_size_ui <- function(id) {
  ns <- NS(id)

  fluidRow(
    column(
      width = 12,
      tabBox(
        id    = ns("ss_tabs"),
        width = NULL,
        title = "Sample Size Calculation",

        # ================================================================
        # Tab 1: Proportional Hazard (rpact)
        # ================================================================
        tabPanel(
          title = tagList(icon("equals"), " Proportional Hazard (rpact)"),
          value = "ph",

          fluidRow(

            # ---- Left: Inputs ------------------------------------------
            column(
              width = 4,
              box(
                title = "Design Parameters", status = "primary",
                solidHeader = TRUE, width = NULL,

                # ---- Group sequential design ----
                h5("Trial Design"),
                numericInput(ns("ph_k"),    "Number of Stages (K)",          value = 3,     min = 1, max = 10),
                numericInput(ns("ph_alpha"), "Overall Alpha (one-sided)",     value = 0.025, min = 0.001, max = 0.5, step = 0.005),
                numericInput(ns("ph_beta"),  "Beta (Type II error, 1-power)", value = 0.2,   min = 0.01,  max = 0.5, step = 0.05),
                selectInput(ns("ph_sided"), "Sidedness",
                            choices = c("One-sided" = 1, "Two-sided" = 2), selected = 1),

                hr(),

                # ---- Information fractions ----
                h5("Information Fractions"),
                radioButtons(ns("ph_info_method"), "Input Method",
                             choices = c("Enter fractions directly"          = "fractions",
                                         "Enter planned events at each look" = "events"),
                             selected = "fractions", inline = TRUE),
                conditionalPanel(
                  condition = sprintf("input['%s'] == 'fractions'", ns("ph_info_method")),
                  textInput(ns("ph_info_fractions"), "Fractions (comma-separated)",
                            value = "0.33, 0.67, 1.0",
                            placeholder = "e.g. 0.33, 0.67, 1.0")
                ),
                conditionalPanel(
                  condition = sprintf("input['%s'] == 'events'", ns("ph_info_method")),
                  textInput(ns("ph_planned_events"), "Planned events at each look (comma-separated)",
                            placeholder = "e.g. 100, 200, 300"),
                  verbatimTextOutput(ns("ph_computed_fractions"))
                ),

                hr(),

                # ---- IA types ----
                h5("Interim Analysis Types"),
                p("Assign each interim look (except the final) as Futility, Efficacy, or Both.",
                  style = "font-size: 0.9em; color: #666;"),
                uiOutput(ns("ph_ia_type_inputs")),

                hr(),

                # ---- Efficacy spending ----
                h5("Efficacy Boundary"),
                selectInput(ns("ph_alpha_spending"), "Alpha Spending Function",
                            choices = c(
                              "O'Brien-Fleming"              = "OF",
                              "Pocock"                       = "P",
                              "Wang-Tsiatis Delta"           = "WT",
                              "Lan-DeMets (O'Brien-Fleming)" = "asOF",
                              "Lan-DeMets (Pocock)"          = "asP",
                              "Kim-DeMets (power)"           = "asKD",
                              "Hwang-Shih-DeCani"            = "asHSD",
                              "User-defined"                 = "asUser",
                              "No early efficacy"            = "noEarlyEfficacy"
                            ), selected = "OF"),
                conditionalPanel(
                  condition = sprintf("input['%s'] == 'asKD'", ns("ph_alpha_spending")),
                  numericInput(ns("ph_gamma_a"),     "Gamma (alpha spending)", value = 1,  step = 0.1)
                ),
                conditionalPanel(
                  condition = sprintf("input['%s'] == 'asHSD'", ns("ph_alpha_spending")),
                  numericInput(ns("ph_gamma_a_hsd"), "Gamma (alpha HSD)",      value = -4, step = 0.5)
                ),
                conditionalPanel(
                  condition = sprintf("input['%s'] == 'asUser'", ns("ph_alpha_spending")),
                  textInput(ns("ph_user_alpha_spending"),
                            "Cumulative alpha spent at each look (comma-separated)",
                            placeholder = "e.g. 0.0001, 0.0062, 0.025"),
                  p("Must be non-decreasing; last value = overall alpha.",
                    style = "font-size: 0.85em; color: #888;")
                ),

                hr(),

                # ---- Futility spending ----
                h5("Futility Boundary"),
                selectInput(ns("ph_beta_spending"), "Futility Boundary Method",
                            choices = c(
                              "None (no futility)"           = "none",
                              "O'Brien-Fleming β-spending"   = "bsOF",
                              "Pocock β-spending"            = "bsP",
                              "Kim-DeMets β-spending"        = "bsKD",
                              "Hwang-Shih-DeCani β-spending" = "bsHSD",
                              "Manual (z-score bounds)"      = "manual"
                            ), selected = "bsOF"),
                conditionalPanel(
                  condition = sprintf("input['%s'] == 'bsKD'", ns("ph_beta_spending")),
                  numericInput(ns("ph_gamma_b"),     "Gamma (beta spending)", value = 1,  step = 0.1)
                ),
                conditionalPanel(
                  condition = sprintf("input['%s'] == 'bsHSD'", ns("ph_beta_spending")),
                  numericInput(ns("ph_gamma_b_hsd"), "Gamma (beta HSD)",      value = -4, step = 0.5)
                ),
                conditionalPanel(
                  condition = sprintf("input['%s'] == 'manual'", ns("ph_beta_spending")),
                  textInput(ns("ph_futility_bounds"),
                            "Futility z-score bounds at each interim (kMax-1 values, comma-separated)",
                            placeholder = "e.g. 0.0, 0.5"),
                  p("One z-score per interim (excluding the final). Lower z = more aggressive futility stopping. Use a very negative value (e.g. -6) to disable futility at a specific interim.",
                    style = "font-size: 0.85em; color: #888;")
                ),
                conditionalPanel(
                  condition = sprintf("input['%s'] != 'none'", ns("ph_beta_spending")),
                  checkboxInput(ns("ph_binding_futility"), "Binding Futility", value = FALSE)
                ),

                hr(),

                # ---- Survival assumptions ----
                h5("Survival Assumptions"),
                radioButtons(ns("ph_effect_input"), "Effect size via:",
                             choices = c("Hazard Ratio"          = "hr",
                                         "Median survival times" = "medians"),
                             selected = "hr", inline = TRUE),
                conditionalPanel(
                  condition = sprintf("input['%s'] == 'hr'", ns("ph_effect_input")),
                  numericInput(ns("ph_hr"),       "Hazard Ratio (trt / ctrl)",           value = 0.65, min = 0.01, max = 5,   step = 0.01),
                  numericInput(ns("ph_med_ctrl_hr"), "Median Survival — Control (months)", value = 10,   min = 0.1,  max = 1000, step = 0.5)
                ),
                conditionalPanel(
                  condition = sprintf("input['%s'] == 'medians'", ns("ph_effect_input")),
                  numericInput(ns("ph_med_trt"),  "Median Survival — Treatment (months)", value = 15, min = 0.1, step = 0.5),
                  numericInput(ns("ph_med_ctrl"), "Median Survival — Control (months)",   value = 10, min = 0.1, step = 0.5)
                ),
                numericInput(ns("ph_alloc"), "Allocation Ratio (trt : ctrl)", value = 1, min = 0.1, step = 0.1),

                hr(),

                # ---- Accrual & Follow-up ----
                h5("Accrual & Follow-up"),
                radioButtons(ns("ph_accrual_method"), "Accrual Input Method",
                             choices = c("Constant rate"             = "constant",
                                         "Monthly enrollment counts" = "monthly"),
                             selected = "constant", inline = TRUE),
                conditionalPanel(
                  condition = sprintf("input['%s'] == 'constant'", ns("ph_accrual_method")),
                  numericInput(ns("ph_accrual_time"), "Accrual Duration (months)",       value = 24,  min = 1),
                  numericInput(ns("ph_accrual_rate"), "Accrual Rate (patients / month)", value = 20,  min = 0.1, step = 1)
                ),
                conditionalPanel(
                  condition = sprintf("input['%s'] == 'monthly'", ns("ph_accrual_method")),
                  textInput(ns("ph_monthly_enroll"),
                            "Monthly Enrollment Counts (comma-separated)",
                            placeholder = "e.g. 2, 3, 4, 5, 23, 67, 2, 45, 8, 12, 11, 12"),
                  p("One value per month. Accrual duration = length of this vector.",
                    style = "font-size: 0.85em; color: #888;")
                ),
                numericInput(ns("ph_followup_time"), "Additional Follow-up after Accrual (months)", value = 12,   min = 0),
                numericInput(ns("ph_max_events"), "Max Number of Events (optional)",
                             value = NA, min = 1, step = 1),
                p("If provided, power is computed for this fixed event count (follow-up time is ignored).",
                  style = "font-size: 0.85em; color: #888;"),
                numericInput(ns("ph_dropout1"),      "Annual Dropout Rate — Treatment",             value = 0.02, min = 0, max = 1, step = 0.01),
                numericInput(ns("ph_dropout2"),      "Annual Dropout Rate — Control",               value = 0.02, min = 0, max = 1, step = 0.01),
                numericInput(ns("ph_dropout_time"),  "Dropout Reference Time (months)",             value = 12,   min = 1),

                hr(),
                actionButton(ns("ph_run"), "Calculate Sample Size",
                             class = "btn-primary btn-block", icon = icon("calculator"))
              )
            ),

            # ---- Right: Results ----------------------------------------
            column(
              width = 8,

              box(
                title = "Input Summary", status = "info",
                solidHeader = TRUE, width = NULL,
                uiOutput(ns("ph_input_summary"))
              ),

              # Key metrics boxes
              uiOutput(ns("ph_summary_boxes")),

              box(
                title = "Detailed Results", status = "success",
                solidHeader = TRUE, width = NULL,
                tabsetPanel(
                  tabPanel("Stage Details",     DT::dataTableOutput(ns("ph_stage_table"))),
                  tabPanel("Boundaries (z)",    DT::dataTableOutput(ns("ph_boundary_table"))),
                  tabPanel("Boundaries (HR)",   DT::dataTableOutput(ns("ph_hr_table"))),
                  tabPanel("Boundary Plot",     plotOutput(ns("ph_boundary_plot"), height = "420px")),
                  tabPanel("Design Details",    verbatimTextOutput(ns("ph_design_summary")))
                )
              )
            )
          )
        ),

        # ================================================================
        # Tab 2: Non-Proportional Hazard (gsDesign2)
        # ================================================================
        tabPanel(
          title = tagList(icon("chart-line"), " Non-Proportional Hazard (gsDesign2)"),
          value = "nph",

          fluidRow(
            column(
              width = 4,
              box(
                title = "Design Parameters", status = "warning",
                solidHeader = TRUE, width = NULL,

                h5("Trial Design"),
                numericInput(ns("nph_k"),    "Number of Stages (K)",          value = 1,     min = 1, max = 10),
                numericInput(ns("nph_alpha"), "One-sided Alpha",               value = 0.025, min = 0.001, max = 0.5, step = 0.005),
                numericInput(ns("nph_beta"),  "Beta (Type II error, 1-power)", value = 0.2,   min = 0.01,  max = 0.5, step = 0.05),

                hr(),
                h5("NPH Hazard Model"),
                selectInput(ns("nph_model"), "NPH Model Type",
                            choices = c(
                              "Delayed Treatment Effect" = "delayed",
                              "Crossing Survival Curves" = "crossing",
                              "Piecewise Exponential"    = "piecewise"
                            )),

                conditionalPanel(
                  condition = sprintf("input['%s'] == 'delayed'", ns("nph_model")),
                  numericInput(ns("nph_delay"),    "Delay Duration (months)",          value = 6,    min = 0, step = 1),
                  numericInput(ns("nph_hr_early"), "HR during delay period",           value = 1.0,  min = 0.01, step = 0.01),
                  numericInput(ns("nph_hr_late"),  "HR after delay (steady-state HR)", value = 0.65, min = 0.01, step = 0.01)
                ),
                conditionalPanel(
                  condition = sprintf("input['%s'] == 'crossing'", ns("nph_model")),
                  p("Crossing survival curves model — inputs coming soon.",
                    style = "color: #888; font-size: 0.9em;")
                ),
                conditionalPanel(
                  condition = sprintf("input['%s'] == 'piecewise'", ns("nph_model")),
                  p("Piecewise exponential model — inputs coming soon.",
                    style = "color: #888; font-size: 0.9em;")
                ),

                hr(),
                h5("Accrual & Follow-up"),
                radioButtons(ns("nph_accrual_method"), "Accrual Input Method",
                             choices = c("Constant rate"             = "constant",
                                         "Monthly enrollment counts" = "monthly"),
                             selected = "constant", inline = TRUE),
                conditionalPanel(
                  condition = sprintf("input['%s'] == 'constant'", ns("nph_accrual_method")),
                  numericInput(ns("nph_accrual_time"), "Accrual Duration (months)",       value = 24,  min = 1),
                  numericInput(ns("nph_accrual_rate"), "Accrual Rate (patients / month)", value = 20,  min = 0.1, step = 1)
                ),
                conditionalPanel(
                  condition = sprintf("input['%s'] == 'monthly'", ns("nph_accrual_method")),
                  textInput(ns("nph_monthly_enroll"),
                            "Monthly Enrollment Counts (comma-separated)",
                            placeholder = "e.g. 1, 3, 6, 9, 13, 18, 22, 27"),
                  p("One value per month. Rates are converted to relative proportions (sum to 1) for gsDesign2.",
                    style = "font-size: 0.85em; color: #888;")
                ),
                numericInput(ns("nph_followup_time"), "Additional Follow-up after Accrual (months)", value = 12,   min = 0),
                numericInput(ns("nph_dropout"),       "Annual Dropout Rate",                         value = 0.02, min = 0, max = 1, step = 0.01),
                numericInput(ns("nph_med_ctrl"),      "Median Survival — Control (months)",          value = 10,   min = 0.1, step = 0.5),
                numericInput(ns("nph_alloc"),         "Allocation Ratio (trt : ctrl)",               value = 1,    min = 0.1, step = 0.1),

                hr(),
                h5("Group Sequential Boundaries"),
                helpText("For K = 1 (fixed design), boundary inputs are ignored."),
                conditionalPanel(
                  condition = sprintf("input['%s'] > 1", ns("nph_k")),
                  textInput(ns("nph_analysis_times"), "Analysis Calendar Times (months, comma-separated)",
                            value = "17.5, 35",
                            placeholder = "e.g. 16, 26, 35"),
                  p("Last value must equal Accrual Duration + Follow-up.",
                    style = "font-size: 0.85em; color: #888;"),
                  selectInput(ns("nph_alpha_spending"), "Alpha Spending (Efficacy)",
                              choices = c(
                                "Lan-DeMets (O'Brien-Fleming)" = "sfLDOF",
                                "Lan-DeMets (Pocock)"          = "sfLDP",
                                "Hwang-Shih-DeCani (gamma=-4)" = "sfHSD",
                                "Fixed bound (gs_b)"           = "gs_b"
                              ), selected = "sfLDOF"),
                  conditionalPanel(
                    condition = sprintf("input['%s'] == 'gs_b'", ns("nph_alpha_spending")),
                    textInput(ns("nph_upar"), "Upper bounds — upar (z-scores, comma-separated)",
                              placeholder = "e.g. 3.0, 2.5, 2.1"),
                    p("One z-score per stage (K values).", style = "font-size: 0.85em; color: #888;")
                  ),
                  h6("Test Upper Bound per Stage"),
                  uiOutput(ns("nph_test_upper_inputs")),
                  selectInput(ns("nph_beta_spending"), "Beta Spending (Futility)",
                              choices = c(
                                "None (no futility, gs_b)"     = "none",
                                "Lan-DeMets (O'Brien-Fleming)" = "sfLDOF",
                                "Lan-DeMets (Pocock)"          = "sfLDP",
                                "Hwang-Shih-DeCani (gamma=-4)" = "sfHSD",
                                "Fixed bound (gs_b)"           = "gs_b"
                              ), selected = "none"),
                  conditionalPanel(
                    condition = sprintf("input['%s'] == 'gs_b'", ns("nph_beta_spending")),
                    textInput(ns("nph_lpar"), "Lower bounds — lpar (z-scores, comma-separated)",
                              placeholder = "e.g. -0.5, 0.5, 1.0"),
                    p("One z-score per stage (K values).", style = "font-size: 0.85em; color: #888;")
                  )
                ),

                hr(),
                actionButton(ns("nph_run"), "Calculate Sample Size",
                             class = "btn-warning btn-block", icon = icon("calculator"))
              )
            ),
            column(
              width = 8,

              box(
                title = "Input Summary", status = "info",
                solidHeader = TRUE, width = NULL,
                uiOutput(ns("nph_input_summary"))
              ),

              uiOutput(ns("nph_summary_boxes")),

              box(
                title = "Detailed Results", status = "success",
                solidHeader = TRUE, width = NULL,
                tabsetPanel(
                  tabPanel("Stage Summary",   DT::dataTableOutput(ns("nph_stage_table"))),
                  tabPanel("Boundaries",      DT::dataTableOutput(ns("nph_bound_table"))),
                  tabPanel("Survival Curves", plotOutput(ns("nph_surv_plot"), height = "420px")),
                  tabPanel("Design Details",  verbatimTextOutput(ns("nph_design_details")))
                )
              )
            )
          )
        ),

        # ================================================================
        # Tab 3: Futility Bounds (rpact)
        # ================================================================
        tabPanel(
          title = tagList(icon("ban"), " Futility Bounds (rpact)"),
          value = "fb",

          fluidRow(

            # ---- Left: Inputs ------------------------------------------
            column(
              width = 4,
              box(
                title = "Design Parameters", status = "primary",
                solidHeader = TRUE, width = NULL,

                h5("Trial Design"),
                numericInput(ns("fb_k"),     "Number of Stages (K)",          value = 2,     min = 1, max = 10),
                numericInput(ns("fb_alpha"), "Overall Alpha (one-sided)",      value = 0.025, min = 0.001, max = 0.5,  step = 0.005),
                numericInput(ns("fb_beta"),  "Beta (Type II error, 1 − power)", value = 0.2,   min = 0.01,  max = 0.5,  step = 0.05),
                selectInput(ns("fb_sided"), "Sidedness",
                            choices = c("One-sided" = 1, "Two-sided" = 2), selected = 1),

                hr(),
                h5("Efficacy Spending"),
                selectInput(ns("fb_alpha_spending"), "Alpha Spending Function",
                            choices = c(
                              "O'Brien-Fleming"       = "OF",
                              "Pocock"                = "P",
                              "Wang-Tsiatis"          = "WT",
                              "Lan-DeMets (OBF)"      = "asOF",
                              "Lan-DeMets (Pocock)"   = "asP",
                              "Kim-DeMets"            = "asKD",
                              "Hwang-Shih-DeCani"     = "asHSD",
                              "No Early Efficacy"     = "noEarlyEfficacy"
                            ), selected = "asOF"),
                conditionalPanel(
                  condition = sprintf("input['%s'] > 1", ns("fb_k")),
                  textInput(ns("fb_info_rates"), "Information Rates (comma-separated)",
                            value = "0.5, 1.0",
                            placeholder = "e.g. 0.33, 0.67, 1.0")
                ),

                hr(),
                h5("Futility Spending"),
                selectInput(ns("fb_beta_spending"), "Beta Spending Function",
                            choices = c(
                              "None"                        = "none",
                              "O'Brien-Fleming type"        = "bsOF",
                              "Pocock type"                 = "bsP",
                              "Kim-DeMets"                  = "bsKD",
                              "Hwang-Shih-DeCani"           = "bsHSD",
                              "Manual z-values"             = "manual"
                            ), selected = "bsOF"),
                conditionalPanel(
                  condition = sprintf("input['%s'] != 'none' && input['%s'] != 'manual'",
                                      ns("fb_beta_spending"), ns("fb_beta_spending")),
                  checkboxInput(ns("fb_binding"), "Binding Futility", value = FALSE)
                ),
                conditionalPanel(
                  condition = sprintf("input['%s'] == 'manual'", ns("fb_beta_spending")),
                  textInput(ns("fb_manual_fut"), "Futility Z-values at each IA (comma-separated)",
                            value = "0.0",
                            placeholder = "e.g. 0.5, 1.0"),
                  p("One value per interim (excluding the final). Use a very negative number (e.g. −6) to skip futility at a stage.",
                    style = "font-size: 0.85em; color: #888;")
                ),

                hr(),
                h5("Futility Scale Conversion"),
                p("Specify a futility threshold on one scale to see it converted to all other scales.",
                  style = "font-size: 0.85em; color: #888;"),
                selectInput(ns("fb_source_scale"), "Input Scale",
                            choices = c(
                              "Z-statistic"                    = "zValue",
                              "P-value"                        = "pValue",
                              "Cond. Power (observed effect)"  = "condPowerAtObserved",
                              "Cond. Power (specified effect)" = "condPowerAtSpecified",
                              "Predictive Power"               = "predictivePower",
                              "Reverse Cond. Power"            = "reverseCondPower",
                              "Effect Estimate (δ)"            = "effectEstimate"
                            ), selected = "condPowerAtObserved"),
                numericInput(ns("fb_source_val"), "Threshold Value", value = 0.2, step = 0.05),
                conditionalPanel(
                  condition = sprintf(
                    "input['%s'] == 'effectEstimate' || input['%s'] == 'condPowerAtSpecified'",
                    ns("fb_source_scale"), ns("fb_source_scale")),
                  p(HTML("Fisher Information at Stage 1 (I<sub>1</sub>) — required for this scale."),
                    style = "font-size: 0.85em; color: #555; margin-bottom: 4px;"),
                  numericInput(ns("fb_max_events"), "Max Number of Events (optional)",
                               value = NA, min = 1, step = 1),
                  conditionalPanel(
                    condition = sprintf("!isNaN(input['%s']) && input['%s'] !== null",
                                       ns("fb_max_events"), ns("fb_max_events")),
                    numericInput(ns("fb_alloc"), "Allocation Ratio (trt : ctrl)",
                                 value = 1, min = 0.1, step = 0.1),
                    p(HTML("I<sub>1</sub> is computed as (r/(1+r)<sup>2</sup>) &times; D &times; t<sub>1</sub>, where D = max events and t<sub>1</sub> = first information rate."),
                      style = "font-size: 0.82em; color: #888;")
                  ),
                  conditionalPanel(
                    condition = sprintf("isNaN(input['%s']) || input['%s'] === null",
                                       ns("fb_max_events"), ns("fb_max_events")),
                    numericInput(ns("fb_info1"), "Fisher Information at Stage 1 (I₁)",
                                 value = NA, min = 0, step = 1),
                    p(HTML("Or provide Max Number of Events above to compute I<sub>1</sub> automatically."),
                      style = "font-size: 0.82em; color: #888;")
                  )
                ),

                hr(),
                actionButton(ns("fb_run"), "Calculate",
                             class = "btn-primary btn-block", icon = icon("calculator"))
              )
            ),

            # ---- Right: Outputs ----------------------------------------
            column(
              width = 8,
              box(
                title = "Detailed Results", status = "success",
                solidHeader = TRUE, width = NULL,
                tabsetPanel(
                  tabPanel("Efficacy Bounds",
                           br(),
                           DT::dataTableOutput(ns("fb_eff_table"))),
                  tabPanel("Futility Bounds",
                           br(),
                           p(HTML("<b>Note:</b> Conditional power / predictive power conversions require K = 2. Effect estimate scale requires I<sub>1</sub>."),
                             style = "font-size: 0.85em; color: #888;"),
                           DT::dataTableOutput(ns("fb_fut_table"))),
                  tabPanel("Scale Conversions",
                           br(),
                           p("Conversion of the threshold entered under 'Futility Scale Conversion'.",
                             style = "font-size: 0.85em; color: #888;"),
                           DT::dataTableOutput(ns("fb_conv_table"))),
                  tabPanel("Design Details",
                           verbatimTextOutput(ns("fb_design_summary")))
                )
              )
            )
          )
        ),

      )
    )
  )
}


mod_sample_size_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ================================================================
    # PH: Dynamic IA type selectors
    # ================================================================
    output$ph_ia_type_inputs <- renderUI({
      k <- input$ph_k
      req(k >= 2)
      n_interim <- k - 1

      defaults <- if (n_interim == 1) {
        "Both"
      } else if (n_interim == 2) {
        c("Futility", "Efficacy")
      } else {
        c("Futility", rep("Both", n_interim - 2), "Efficacy")
      }

      lapply(seq_len(n_interim), function(i) {
        selectInput(ns(paste0("ph_ia_type_", i)),
                    paste0("IA ", i, " Type"),
                    choices  = c("Futility", "Efficacy", "Both"),
                    selected = defaults[min(i, length(defaults))])
      })
    })

    # ================================================================
    # PH: Computed fractions preview
    # ================================================================
    output$ph_computed_fractions <- renderText({
      req(input$ph_planned_events)
      ev <- suppressWarnings(as.numeric(trimws(strsplit(input$ph_planned_events, ",")[[1]])))
      if (any(is.na(ev)) || length(ev) == 0) return("Enter valid event counts")
      paste0("Computed fractions: ", paste(round(ev / ev[length(ev)], 4), collapse = ", "))
    })

# ================================================================
    # PH: Resolve info fractions
    # ================================================================
    ph_info_frac <- reactive({
      k <- input$ph_k
      req(k)
      if (input$ph_info_method == "fractions") {
        req(input$ph_info_fractions)
        vals <- as.numeric(trimws(strsplit(input$ph_info_fractions, ",")[[1]]))
        validate(need(length(vals) == k, "Number of fractions must equal K."))
        validate(need(all(!is.na(vals)), "All fractions must be numeric."))
        vals
      } else {
        req(input$ph_planned_events)
        ev <- as.numeric(trimws(strsplit(input$ph_planned_events, ",")[[1]]))
        validate(need(length(ev) == k,          "Number of event counts must equal K."))
        validate(need(all(!is.na(ev) & ev > 0), "All event counts must be positive."))
        ev / ev[length(ev)]
      }
    })

    # ================================================================
    # PH: Main calculation
    # ================================================================
    ph_result <- eventReactive(input$ph_run, {

      info_frac <- ph_info_frac()
      k         <- input$ph_k

      ia_types <- if (k > 1) {
        sapply(seq_len(k - 1), function(i) input[[paste0("ph_ia_type_", i)]])
      } else character(0)

      # ---- Build group sequential design ----
      design_args <- list(
        kMax             = k,
        alpha            = input$ph_alpha,
        beta             = input$ph_beta,
        sided            = as.numeric(input$ph_sided),
        informationRates = info_frac,
        typeOfDesign     = input$ph_alpha_spending
      )

      if (input$ph_alpha_spending == "asKD")
        design_args$gammaA <- input$ph_gamma_a
      if (input$ph_alpha_spending == "asHSD")
        design_args$gammaA <- input$ph_gamma_a_hsd
      if (input$ph_alpha_spending == "asUser") {
        req(input$ph_user_alpha_spending)
        ua <- as.numeric(trimws(strsplit(input$ph_user_alpha_spending, ",")[[1]]))
        validate(need(length(ua) == k && all(!is.na(ua)), "Check user alpha spending values."))
        design_args$userAlphaSpending <- ua
      }

      classic_designs <- c("OF", "P", "WT")

      if (input$ph_beta_spending != "none") {

        if (input$ph_alpha_spending %in% classic_designs) {
          showNotification(
            paste0("Classic bound designs (O\u2019Brien-Fleming, Pocock, Wang-Tsiatis) do not support ",
                   "futility bounds in rpact. Switch Alpha Spending to a spending-based design ",
                   "(e.g., Lan-DeMets) to enable futility bounds."),
            type = "warning", duration = 12
          )

        } else if (input$ph_beta_spending == "manual") {
          # ---- Manual z-score futility bounds ----
          req(input$ph_futility_bounds)
          raw <- suppressWarnings(
            as.numeric(trimws(strsplit(input$ph_futility_bounds, ",")[[1]]))
          )
          validate(
            need(length(raw) == k - 1,
                 paste0("Manual futility bounds require ", k - 1,
                        " value(s) (one per interim, excluding final).")),
            need(all(!is.na(raw)), "All futility bound values must be numeric.")
          )
          # Apply IA types: "Efficacy-only" IAs get -Inf (disabled futility)
          fut_vec <- raw
          for (i in seq_len(k - 1)) {
            if (length(ia_types) >= i && ia_types[i] == "Efficacy") fut_vec[i] <- -6
          }
          design_args$futilityBounds  <- fut_vec
          design_args$bindingFutility <- isTRUE(input$ph_binding_futility)

        } else {
          # ---- Beta-spending function futility ----
          efficacy_only_idx <- which(ia_types == "Efficacy")

          if (length(efficacy_only_idx) == 0) {
            # Standard: spending function applies to all IAs
            design_args$typeBetaSpending <- input$ph_beta_spending
            design_args$bindingFutility  <- isTRUE(input$ph_binding_futility)
            if (input$ph_beta_spending == "bsKD")  design_args$gammaB <- input$ph_gamma_b
            if (input$ph_beta_spending == "bsHSD") design_args$gammaB <- input$ph_gamma_b_hsd

          } else {
            # Two-pass: compute spending-function bounds, zero out "Efficacy-only" IAs
            temp_args                   <- design_args
            temp_args$typeBetaSpending  <- input$ph_beta_spending
            temp_args$bindingFutility   <- FALSE
            if (input$ph_beta_spending == "bsKD")  temp_args$gammaB <- input$ph_gamma_b
            if (input$ph_beta_spending == "bsHSD") temp_args$gammaB <- input$ph_gamma_b_hsd

            temp_design <- tryCatch(
              do.call(rpact::getDesignGroupSequential, temp_args),
              error = function(e) NULL
            )

            if (!is.null(temp_design) && !is.null(temp_design$futilityBounds)) {
              fut_vec <- as.numeric(temp_design$futilityBounds)
              fut_vec[efficacy_only_idx] <- -6
              design_args$futilityBounds  <- fut_vec
              design_args$bindingFutility <- isTRUE(input$ph_binding_futility)
            } else {
              # Fallback to standard spending function
              design_args$typeBetaSpending <- input$ph_beta_spending
              design_args$bindingFutility  <- isTRUE(input$ph_binding_futility)
              if (input$ph_beta_spending == "bsKD")  design_args$gammaB <- input$ph_gamma_b
              if (input$ph_beta_spending == "bsHSD") design_args$gammaB <- input$ph_gamma_b_hsd
            }
          }
        }
      }

      design <- do.call(rpact::getDesignGroupSequential, design_args)

      # ---- Resolve accrual args ----
      if (input$ph_accrual_method == "constant") {
        accrual_time_arg      <- c(0, input$ph_accrual_time)
        accrual_intensity_arg <- input$ph_accrual_rate
      } else {
        req(input$ph_monthly_enroll)
        monthly <- as.numeric(trimws(strsplit(input$ph_monthly_enroll, ",")[[1]]))
        validate(need(length(monthly) > 0 && all(!is.na(monthly)) && all(monthly >= 0),
                      "Monthly enrollment counts must be non-negative numbers."))
        n_months              <- length(monthly)
        accrual_time_arg      <- seq(0, n_months - 1)
        accrual_intensity_arg <- monthly
      }

      # ---- Determine mode: fixed events (getPowerSurvival) or compute events (getSampleSizeSurvival) ----
      max_events <- input$ph_max_events
      use_power_mode <- !is.null(max_events) && !is.na(max_events) && max_events > 0

      # ---- Build survival args ----
      ss_args <- list(
        design                 = design,
        allocationRatioPlanned = input$ph_alloc,
        accrualTime            = accrual_time_arg,
        accrualIntensity       = accrual_intensity_arg,
        dropoutRate1           = input$ph_dropout1,
        dropoutRate2           = input$ph_dropout2,
        dropoutTime            = input$ph_dropout_time
      )

      if (input$ph_effect_input == "hr") {
        ss_args$hazardRatio <- input$ph_hr
        ss_args$median2     <- input$ph_med_ctrl_hr
      } else {
        ss_args$median1 <- input$ph_med_trt
        ss_args$median2 <- input$ph_med_ctrl
      }

      if (use_power_mode) {
        # getPowerSurvival requires maxNumberOfSubjects when accrual is piecewise
        max_subjects <- if (input$ph_accrual_method == "constant") {
          input$ph_accrual_rate * input$ph_accrual_time
        } else {
          sum(accrual_intensity_arg)
        }
        ss_args$maxNumberOfEvents   <- max_events
        ss_args$maxNumberOfSubjects <- max_subjects
        ss_args$directionUpper      <- FALSE
        rpact_fn <- rpact::getPowerSurvival
      } else {
        ss_args$followUpTime <- input$ph_followup_time
        rpact_fn <- rpact::getSampleSizeSurvival
      }

      ss <- tryCatch(
        do.call(rpact_fn, ss_args),
        error = function(e) {
          showNotification(paste("rpact error:", e$message), type = "error", duration = 8)
          NULL
        }
      )
      validate(need(!is.null(ss), "Calculation failed — check inputs."))

      list(design = design, ss = ss, ia_types = ia_types, k = k,
           use_power_mode = use_power_mode)
    })

    # ================================================================
    # PH: Summary boxes
    # ================================================================
    output$ph_summary_boxes <- renderUI({
      res <- ph_result()
      req(res)
      ss <- res$ss

      total_n  <- ceiling(max(ss$numberOfSubjects,        na.rm = TRUE))
      n_arm1   <- ceiling(max(ss$numberOfSubjects1,       na.rm = TRUE))
      n_arm2   <- ceiling(max(ss$numberOfSubjects2,       na.rm = TRUE))
      tot_ev   <- ceiling(max(ss$cumulativeEventsPerStage, na.rm = TRUE))
      duration <- round(max(ss$studyDuration,             na.rm = TRUE), 1)

      if (res$use_power_mode) {
        power_pct <- round(ss$overallReject * 100, 1)
        ev_label  <- "Max Events (fixed)"
        box4_label <- paste0("Power: ", power_pct, "%")
        box4_icon  <- icon("bolt")
        box4_class <- "info-box bg-purple"
      } else {
        ev_label   <- "Required Events"
        box4_label <- paste0(n_arm1, " / ", n_arm2)
        box4_icon  <- icon("vials")
        box4_class <- "info-box bg-red"
      }

      fluidRow(
        column(3, div(class = "info-box bg-blue",
          div(class = "info-box-icon", icon("users")),
          div(class = "info-box-content",
            span(class = "info-box-text",  "Total N"),
            span(class = "info-box-number", total_n)
          )
        )),
        column(3, div(class = "info-box bg-green",
          div(class = "info-box-icon", icon("calendar-check")),
          div(class = "info-box-content",
            span(class = "info-box-text",  ev_label),
            span(class = "info-box-number", tot_ev)
          )
        )),
        column(3, div(class = "info-box bg-yellow",
          div(class = "info-box-icon", icon("clock")),
          div(class = "info-box-content",
            span(class = "info-box-text",  "Study Duration (mo)"),
            span(class = "info-box-number", duration)
          )
        )),
        column(3, div(class = box4_class,
          div(class = "info-box-icon", box4_icon),
          div(class = "info-box-content",
            span(class = "info-box-text",  if (res$use_power_mode) "Computed Power" else "Per Arm (trt / ctrl)"),
            span(class = "info-box-number", box4_label)
          )
        ))
      )
    })

    # ================================================================
    # PH: Stage details table
    # ================================================================
    output$ph_stage_table <- DT::renderDataTable({
      res <- ph_result()
      req(res)
      ss <- res$ss
      k  <- res$k

      df <- data.frame(
        Stage            = seq_len(k),
        Info_Fraction    = round(res$design$informationRates, 4),
        Events_at_Stage  = ceiling(ss$singleEventsPerStage),
        Cum_Events       = ceiling(ss$cumulativeEventsPerStage),
        N_at_Stage       = ceiling(ss$numberOfSubjects),
        Analysis_Time_mo = round(ss$analysisTime, 2),
        stringsAsFactors = FALSE
      )

      DT::datatable(df, rownames = FALSE,
                    options = list(dom = "t", pageLength = 20,
                                   columnDefs = list(list(className = "dt-center", targets = "_all"))))
    })

    # ================================================================
    # PH: Boundary table (z-scale)
    # ================================================================
    output$ph_boundary_table <- DT::renderDataTable({
      res <- ph_result()
      req(res)
      design <- res$design
      k      <- res$k

      has_real_fut <- !is.null(design$futilityBounds) &&
                      !all(design$futilityBounds <= -5) &&
                      input$ph_beta_spending != "none"
      fut <- if (has_real_fut) {
        c(round(design$futilityBounds, 6), NA)
      } else rep(NA_real_, k)

      df <- data.frame(
        Stage          = seq_len(k),
        Info_Fraction  = round(design$informationRates, 4),
        Efficacy_z     = round(design$criticalValues, 6),
        Nominal_Alpha  = round(design$stageLevels, 6),
        Cumul_Alpha    = round(design$alphaSpent, 6),
        Futility_z     = fut,
        stringsAsFactors = FALSE
      )

      if (!is.null(design$betaSpent) && input$ph_beta_spending != "none")
        df$Cumul_Beta <- round(design$betaSpent, 6)

      DT::datatable(df, rownames = FALSE,
                    options = list(dom = "t", pageLength = 20,
                                   columnDefs = list(list(className = "dt-center", targets = "_all"))))
    })

    # ================================================================
    # PH: Boundary table (HR scale)
    # ================================================================
    output$ph_hr_table <- DT::renderDataTable({
      res <- ph_result()
      req(res)
      ss <- res$ss
      k  <- res$k

      eff_hr <- round(ss$criticalValuesEffectScale, 6)
      eff_p  <- format(ss$criticalValuesPValueScale, digits = 4, scientific = TRUE)

      has_fut <- !is.null(ss$futilityBoundsEffectScale) &&
                 input$ph_beta_spending != "none"
      fut_hr  <- if (has_fut) c(round(ss$futilityBoundsEffectScale, 6), NA) else rep(NA_real_, k)
      fut_p   <- if (has_fut) c(format(ss$futilityBoundsPValueScale, digits = 4, scientific = TRUE), NA) else rep(NA_character_, k)

      df <- data.frame(
        Stage         = seq_len(k),
        Info_Fraction = round(res$design$informationRates, 4),
        Efficacy_HR   = eff_hr,
        Efficacy_p    = eff_p,
        Futility_HR   = fut_hr,
        Futility_p    = fut_p,
        stringsAsFactors = FALSE
      )

      DT::datatable(df, rownames = FALSE,
                    options = list(dom = "t", pageLength = 20,
                                   columnDefs = list(list(className = "dt-center", targets = "_all"))))
    })

    # ================================================================
    # PH: Boundary plot
    # ================================================================
    output$ph_boundary_plot <- renderPlot({
      res <- ph_result()
      req(res)
      rpact::plot(res$design, type = 1)
    })

    # ================================================================
    # PH: Design details (raw rpact output)
    # ================================================================
    output$ph_design_summary <- renderPrint({
      res <- ph_result()
      req(res)
      cat("========== Group Sequential Design ==========\n\n")
      print(summary(res$design))
      cat("\n\n========== Sample Size / Survival ==========\n\n")
      print(summary(res$ss))
    })

    # ================================================================
    # NPH: Dynamic test_upper checkboxes (one per stage)
    # ================================================================
    output$nph_test_upper_inputs <- renderUI({
      k <- input$nph_k
      req(k >= 1)
      lapply(seq_len(k), function(i) {
        checkboxInput(ns(paste0("nph_test_upper_", i)),
                      paste0("Stage ", i),
                      value = TRUE)
      })
    })

    # ================================================================
    # NPH: Main calculation (gsDesign2)
    # ================================================================
    nph_result <- eventReactive(input$nph_run, {

      if (!requireNamespace("gsDesign2", quietly = TRUE)) {
        showNotification(
          "Package 'gsDesign2' is required. Install with: install.packages('gsDesign2')",
          type = "error", duration = 12
        )
        return(NULL)
      }
      if (!requireNamespace("gsDesign", quietly = TRUE)) {
        showNotification(
          "Package 'gsDesign' is required for spending functions. Install with: install.packages('gsDesign')",
          type = "error", duration = 12
        )
        return(NULL)
      }

      req(input$nph_model == "delayed",
          cancelOutput = TRUE)

      k           <- input$nph_k
      ctrl_lambda <- log(2) / input$nph_med_ctrl

      # Enrollment rate
      if (input$nph_accrual_method == "constant") {
        total_time  <- input$nph_accrual_time + input$nph_followup_time
        enroll_rate <- gsDesign2::define_enroll_rate(
          duration = input$nph_accrual_time,
          rate     = input$nph_accrual_rate
        )
      } else {
        req(input$nph_monthly_enroll)
        monthly <- as.numeric(trimws(strsplit(input$nph_monthly_enroll, ",")[[1]]))
        validate(need(length(monthly) > 0 && all(!is.na(monthly)) && all(monthly >= 0),
                      "Monthly enrollment counts must be non-negative numbers."))
        total_time  <- length(monthly) + input$nph_followup_time
        enroll_rate <- gsDesign2::define_enroll_rate(
          duration = rep(1, length(monthly)),
          rate     = monthly / sum(monthly)
        )
      }

      # Failure / dropout rates: piecewise for delayed treatment effect
      fail_rate <- gsDesign2::define_fail_rate(
        duration     = c(input$nph_delay, Inf),
        fail_rate    = ctrl_lambda,
        hr           = c(input$nph_hr_early, input$nph_hr_late),
        dropout_rate = input$nph_dropout / 12  # convert annual → monthly
      )

      # Resolve analysis times
      test_upper_vec <- TRUE   # default for K=1; overridden below for K>1

      if (k == 1) {
        nph_analysis_time <- total_time
        upper_fn  <- gsDesign2::gs_b
        upper_par <- stats::qnorm(1 - input$nph_alpha)
        lower_fn  <- gsDesign2::gs_b
        lower_par <- -Inf

      } else {
        req(input$nph_analysis_times)
        nph_analysis_time <- as.numeric(trimws(strsplit(input$nph_analysis_times, ",")[[1]]))
        validate(
          need(length(nph_analysis_time) == k,          "Number of analysis times must equal K."),
          need(all(!is.na(nph_analysis_time)),           "All analysis times must be numeric."),
          need(all(nph_analysis_time > 0),               "All analysis times must be positive."),
          need(all(diff(nph_analysis_time) > 0),         "Analysis times must be strictly increasing."),
          need(abs(nph_analysis_time[k] - total_time) < 0.01,
               sprintf("Last analysis time must equal total study duration (%.1f months).", total_time))
        )

        # Each entry: list(sf = <function>, param = <gamma or NULL>)
        sf_lookup <- list(
          sfLDOF = list(sf = gsDesign::sfLDOF,    param = NULL),
          sfLDP  = list(sf = gsDesign::sfLDPocock, param = NULL),
          sfHSD  = list(sf = gsDesign::sfHSD,      param = -4)
        )

        # Upper bound
        if (input$nph_alpha_spending == "gs_b") {
          req(input$nph_upar)
          upar_vals <- as.numeric(trimws(strsplit(input$nph_upar, ",")[[1]]))
          validate(
            need(length(upar_vals) == k,   "upar must have K values (one per stage)."),
            need(all(!is.na(upar_vals)),   "All upar values must be numeric.")
          )
          upper_fn  <- gsDesign2::gs_b
          upper_par <- upar_vals
        } else {
          su <- sf_lookup[[input$nph_alpha_spending]]
          upper_fn  <- gsDesign2::gs_spending_bound
          upper_par <- list(sf = su$sf, total_spend = input$nph_alpha, param = su$param)
        }

        # Lower bound
        if (input$nph_beta_spending == "none") {
          lower_fn  <- gsDesign2::gs_b
          lower_par <- -Inf
        } else if (input$nph_beta_spending == "gs_b") {
          req(input$nph_lpar)
          lpar_vals <- as.numeric(trimws(strsplit(input$nph_lpar, ",")[[1]]))
          validate(
            need(length(lpar_vals) == k,   "lpar must have K values (one per stage)."),
            need(all(!is.na(lpar_vals)),   "All lpar values must be numeric.")
          )
          lower_fn  <- gsDesign2::gs_b
          lower_par <- lpar_vals
        } else {
          sl <- sf_lookup[[input$nph_beta_spending]]
          lower_fn  <- gsDesign2::gs_spending_bound
          lower_par <- list(sf = sl$sf, total_spend = input$nph_beta, param = sl$param)
        }

        # test_upper: one checkbox per stage
        test_upper_vec <<- sapply(seq_len(k), function(i) {
          val <- input[[paste0("nph_test_upper_", i)]]
          if (is.null(val)) TRUE else isTRUE(val)
        })
      }

      result <- tryCatch(
        gsDesign2::gs_design_ahr(
          enroll_rate   = enroll_rate,
          fail_rate     = fail_rate,
          ratio         = input$nph_alloc,
          alpha         = input$nph_alpha,
          beta          = input$nph_beta,
          analysis_time = nph_analysis_time,
          info_scale    = "h0_h1_info",
          upper         = upper_fn,
          upar          = upper_par,
          lower         = lower_fn,
          lpar          = lower_par,
          test_upper    = test_upper_vec
        ),
        error = function(e) {
          showNotification(paste("gsDesign2 error:", e$message), type = "error", duration = 10)
          NULL
        }
      )
      validate(need(!is.null(result), "Calculation failed — check inputs."))

      list(
        result      = result,
        k           = k,
        total_time  = total_time,
        ctrl_lambda = ctrl_lambda,
        model       = input$nph_model
      )
    })

    # ================================================================
    # NPH: Summary boxes
    # ================================================================
    output$nph_summary_boxes <- renderUI({
      res <- nph_result()
      req(res)
      an <- res$result$analysis

      total_n  <- ceiling(max(an$n,     na.rm = TRUE))
      tot_ev   <- ceiling(max(an$event, na.rm = TRUE))
      duration <- round(max(an$time,    na.rm = TRUE), 1)
      ratio    <- input$nph_alloc
      n_trt    <- ceiling(total_n * ratio / (1 + ratio))
      n_ctrl   <- total_n - n_trt

      fluidRow(
        column(3, div(class = "info-box bg-blue",
          div(class = "info-box-icon", icon("users")),
          div(class = "info-box-content",
            span(class = "info-box-text",  "Total N"),
            span(class = "info-box-number", total_n)
          )
        )),
        column(3, div(class = "info-box bg-green",
          div(class = "info-box-icon", icon("calendar-check")),
          div(class = "info-box-content",
            span(class = "info-box-text",  "Required Events"),
            span(class = "info-box-number", tot_ev)
          )
        )),
        column(3, div(class = "info-box bg-yellow",
          div(class = "info-box-icon", icon("clock")),
          div(class = "info-box-content",
            span(class = "info-box-text",  "Study Duration (mo)"),
            span(class = "info-box-number", duration)
          )
        )),
        column(3, div(class = "info-box bg-red",
          div(class = "info-box-icon", icon("vials")),
          div(class = "info-box-content",
            span(class = "info-box-text",  "Per Arm (trt / ctrl)"),
            span(class = "info-box-number", paste0(n_trt, " / ", n_ctrl))
          )
        ))
      )
    })

    # ================================================================
    # NPH: Stage summary table
    # ================================================================
    output$nph_stage_table <- DT::renderDataTable({
      res <- nph_result()
      req(res)
      an <- res$result$analysis

      df <- data.frame(
        Stage         = an$analysis,
        Analysis_Time = round(an$time,      2),
        N             = ceiling(an$n),
        Events        = ceiling(an$event),
        AHR           = round(an$ahr,       4),
        Info_Frac     = round(an$info_frac, 4),
        stringsAsFactors = FALSE
      )

      DT::datatable(df, rownames = FALSE,
                    options = list(dom = "t", pageLength = 20,
                                   columnDefs = list(list(className = "dt-center", targets = "_all"))))
    })

    # ================================================================
    # NPH: Boundaries table
    # ================================================================
    output$nph_bound_table <- DT::renderDataTable({
      res <- nph_result()
      req(res)
      bd <- res$result$bound

      # Keep columns that exist
      keep_cols <- intersect(names(bd), c("analysis", "bound", "z", "probability", "~hr at bound"))
      df <- as.data.frame(bd[, keep_cols, drop = FALSE])

      # Round numeric columns
      numeric_cols <- sapply(df, is.numeric)
      df[numeric_cols] <- lapply(df[numeric_cols], round, digits = 6)

      DT::datatable(df, rownames = FALSE,
                    options = list(dom = "t", pageLength = 20,
                                   columnDefs = list(list(className = "dt-center", targets = "_all"))))
    })

    # ================================================================
    # NPH: Survival curves plot
    # ================================================================
    output$nph_surv_plot <- renderPlot({
      res <- nph_result()
      req(res, res$model == "delayed")

      ctrl_lambda <- res$ctrl_lambda
      delay       <- input$nph_delay
      hr_early    <- input$nph_hr_early
      hr_late     <- input$nph_hr_late
      total_time  <- res$total_time

      t <- seq(0, total_time, length.out = 500)

      s_ctrl <- exp(-ctrl_lambda * t)

      s_trt <- exp(-ctrl_lambda * (
        hr_early * pmin(t, delay) + hr_late * pmax(t - delay, 0)
      ))

      plot_df <- rbind(
        data.frame(Time = t, Survival = s_ctrl, Arm = "Control"),
        data.frame(Time = t, Survival = s_trt,  Arm = "Treatment")
      )

      ggplot2::ggplot(plot_df, ggplot2::aes(x = Time, y = Survival, colour = Arm)) +
        ggplot2::geom_line(linewidth = 1) +
        ggplot2::geom_vline(xintercept = delay, linetype = "dashed", colour = "grey50") +
        ggplot2::annotate("text", x = delay, y = 0.95,
                          label = paste0("Delay = ", delay, " mo"),
                          hjust = -0.1, size = 3.5, colour = "grey40") +
        ggplot2::scale_y_continuous(limits = c(0, 1), labels = scales::percent_format()) +
        ggplot2::labs(
          title   = "Assumed Survival Curves (Delayed Treatment Effect)",
          x       = "Time (months)",
          y       = "Survival Probability",
          colour  = NULL
        ) +
        ggplot2::theme_bw(base_size = 13) +
        ggplot2::theme(legend.position = "bottom")
    })

    # ================================================================
    # NPH: Design details
    # ================================================================
    output$nph_design_details <- renderPrint({
      res <- nph_result()
      req(res)
      cat("========== NPH Design (gsDesign2 :: gs_design_ahr) ==========\n\n")
      print(res$result)
    })

    # ================================================================
    # PH: Input Summary (reactive, no button required)
    # ================================================================
    output$ph_input_summary <- renderUI({
      k     <- input$ph_k
      alpha <- input$ph_alpha
      beta  <- input$ph_beta
      power <- round((1 - beta) * 100, 1)

      sided_label <- if (input$ph_sided == "1") "one-sided" else "two-sided"

      effect_str <- if (input$ph_effect_input == "hr") {
        med_trt <- round(input$ph_med_ctrl_hr / input$ph_hr, 1)
        paste0("Hazard ratio = ", input$ph_hr,
               " (control median = ", input$ph_med_ctrl_hr,
               " mo, implied treatment median ≈ ", med_trt, " mo)")
      } else {
        hr_derived <- round(input$ph_med_ctrl / input$ph_med_trt, 3)
        paste0("Median survival: treatment = ", input$ph_med_trt,
               " mo, control = ", input$ph_med_ctrl,
               " mo (implied HR ≈ ", hr_derived, ")")
      }

      spending_labels <- c(
        "OF" = "O’Brien-Fleming", "P" = "Pocock", "WT" = "Wang-Tsiatis",
        "asOF" = "Lan-DeMets (OBF)", "asP" = "Lan-DeMets (Pocock)",
        "asKD" = "Kim-DeMets", "asHSD" = "Hwang-Shih-DeCani",
        "asUser" = "User-defined", "noEarlyEfficacy" = "No early efficacy"
      )
      alpha_sf <- spending_labels[input$ph_alpha_spending]

      futility_str <- if (input$ph_beta_spending == "none") {
        "No futility boundary."
      } else {
        beta_labels <- c(
          "bsOF" = "O’Brien-Fleming type", "bsP" = "Pocock type",
          "bsKD" = "Kim-DeMets", "bsHSD" = "Hwang-Shih-DeCani", "bsUser" = "User-defined"
        )
        bind_str <- if (isTRUE(input$ph_binding_futility)) " (binding)" else " (non-binding)"
        paste0("Futility: ", beta_labels[input$ph_beta_spending], bind_str, ".")
      }

      if (input$ph_accrual_method == "constant") {
        accrual_str <- paste0(input$ph_accrual_rate, " patients/month over ",
                              input$ph_accrual_time, " months")
      } else {
        monthly <- suppressWarnings(
          as.numeric(trimws(strsplit(input$ph_monthly_enroll %||% "", ",")[[1]]))
        )
        if (length(monthly) > 0 && !any(is.na(monthly))) {
          accrual_str <- paste0("monthly schedule (", length(monthly),
                                " months, total ≈ ", sum(monthly, na.rm = TRUE), " patients)")
        } else {
          accrual_str <- "monthly schedule (enter counts above)"
        }
      }

      info_frac_str <- tryCatch({
        if (input$ph_info_method == "fractions") {
          vals <- as.numeric(trimws(strsplit(input$ph_info_fractions %||% "", ",")[[1]]))
          paste(round(vals, 3), collapse = ", ")
        } else {
          ev <- as.numeric(trimws(strsplit(input$ph_planned_events %||% "", ",")[[1]]))
          if (length(ev) > 0 && !any(is.na(ev)) && ev[length(ev)] > 0)
            paste(round(ev / ev[length(ev)], 3), collapse = ", ")
          else "—"
        }
      }, error = function(e) "—")

      HTML(paste0(
        "<b>Design:</b> K = ", k, " stage", if (k > 1) "s" else "",
        ", &alpha; = ", alpha, " (", sided_label, "), power = ", power, "%.<br/>",
        "<b>Effect:</b> ", effect_str, ", allocation ratio ", input$ph_alloc, ":1.<br/>",
        "<b>Accrual:</b> ", accrual_str,
        "; follow-up = ", input$ph_followup_time, " months.<br/>",
        "<b>Dropout:</b> treatment ", input$ph_dropout1 * 100, "%/year, ",
        "control ", input$ph_dropout2 * 100, "%/year ",
        "(reference time ", input$ph_dropout_time, " months).<br/>",
        "<b>Efficacy spending:</b> ", alpha_sf, ". ", futility_str, "<br/>",
        "<b>Information fractions:</b> ", info_frac_str, "."
      ))
    })

    # ================================================================
    # NPH: Input Summary (reactive, no button required)
    # ================================================================
    output$nph_input_summary <- renderUI({
      k     <- input$nph_k
      alpha <- input$nph_alpha
      beta  <- input$nph_beta
      power <- round((1 - beta) * 100, 1)

      model_label <- switch(input$nph_model,
        delayed   = "Delayed Treatment Effect",
        crossing  = "Crossing Survival Curves",
        piecewise = "Piecewise Exponential",
        input$nph_model
      )

      effect_str <- if (input$nph_model == "delayed") {
        paste0("Control median = ", input$nph_med_ctrl,
               " mo; delay = ", input$nph_delay,
               " mo; HR during delay = ", input$nph_hr_early,
               "; HR after delay = ", input$nph_hr_late)
      } else {
        paste0("Model: ", model_label, " (see inputs at left)")
      }

      if (input$nph_accrual_method == "constant") {
        accrual_str <- paste0(input$nph_accrual_rate, " patients/month over ",
                              input$nph_accrual_time, " months")
      } else {
        monthly <- suppressWarnings(
          as.numeric(trimws(strsplit(input$nph_monthly_enroll %||% "", ",")[[1]]))
        )
        if (length(monthly) > 0 && !any(is.na(monthly))) {
          accrual_str <- paste0("monthly schedule (", length(monthly),
                                " months, relative rates sum to 1)")
        } else {
          accrual_str <- "monthly schedule (enter counts above)"
        }
      }

      boundary_str <- if (k == 1) {
        "Fixed design (no interim analyses)."
      } else {
        sf_labels <- c(
          sfLDOF = "Lan-DeMets (OBF)", sfLDP = "Lan-DeMets (Pocock)",
          sfHSD  = "Hwang-Shih-DeCani (gamma=-4)", gs_b = "Fixed bounds (gs_b)"
        )
        alpha_sf <- sf_labels[input$nph_alpha_spending]
        fut_str  <- if (input$nph_beta_spending == "none") "no futility" else sf_labels[input$nph_beta_spending]
        times_str <- input$nph_analysis_times %||% "—"
        paste0("Analysis times: ", times_str, " months. ",
               "Efficacy: ", alpha_sf, "; futility: ", fut_str, ".")
      }

      HTML(paste0(
        "<b>Design:</b> K = ", k, " stage", if (k > 1) "s" else "",
        ", &alpha; = ", alpha, " (one-sided), power = ", power, "%.<br/>",
        "<b>NPH model:</b> ", model_label, ".<br/>",
        "<b>Effect:</b> ", effect_str, ".<br/>",
        "<b>Accrual:</b> ", accrual_str,
        "; follow-up = ", input$nph_followup_time, " months.<br/>",
        "<b>Dropout:</b> ", input$nph_dropout * 100, "%/year; ",
        "allocation ratio ", input$nph_alloc, ":1.<br/>",
        "<b>Boundaries:</b> ", boundary_str
      ))
    })

    # ================================================================
    # Futility Bounds: Main calculation
    # ================================================================
    fb_result <- eventReactive(input$fb_run, {
      k     <- input$fb_k
      alpha <- input$fb_alpha
      beta  <- input$fb_beta
      sided <- as.numeric(input$fb_sided)
      alpha_sf  <- input$fb_alpha_spending
      beta_sf   <- input$fb_beta_spending

      # Parse information rates
      if (k > 1) {
        req(input$fb_info_rates)
        ir <- suppressWarnings(
          as.numeric(trimws(strsplit(input$fb_info_rates, ",")[[1]]))
        )
        validate(need(length(ir) == k,        "Number of information rates must equal K."))
        validate(need(all(!is.na(ir)),         "All information rates must be numeric."))
        validate(need(abs(ir[k] - 1) < 0.001, "Last information rate must be 1.0."))
      } else {
        ir <- 1.0
      }

      # Build design args
      design_args <- list(
        kMax             = k,
        alpha            = alpha,
        beta             = beta,
        sided            = sided,
        typeOfDesign     = alpha_sf,
        informationRates = ir
      )

      if (beta_sf == "manual") {
        req(input$fb_manual_fut)
        fv <- suppressWarnings(
          as.numeric(trimws(strsplit(input$fb_manual_fut, ",")[[1]]))
        )
        n_interim <- k - 1
        validate(need(length(fv) == n_interim,  "Number of futility z-values must equal K − 1."))
        validate(need(all(!is.na(fv)),           "All futility z-values must be numeric."))
        design_args$futilityBounds <- fv
      } else if (beta_sf != "none") {
        classic <- c("OF", "P", "WT")
        if (alpha_sf %in% classic) {
          showNotification(
            "Classic designs (OF/P/WT) do not support beta spending in rpact. Futility bounds skipped.",
            type = "warning", duration = 8)
        } else {
          design_args$typeBetaSpending <- beta_sf
          design_args$bindingFutility  <- isTRUE(input$fb_binding)
        }
      }

      design <- tryCatch(
        do.call(rpact::getDesignGroupSequential, design_args),
        error = function(e) {
          showNotification(paste("Design error:", e$message), type = "error", duration = 8)
          NULL
        }
      )
      validate(need(!is.null(design), "Failed to build group sequential design — check inputs."))

      # ---- Efficacy boundary table ----
      crit_z <- round(design$criticalValues, 4)
      crit_p <- format(1 - pnorm(crit_z), digits = 4, scientific = TRUE)
      eff_df <- data.frame(
        Stage          = seq_len(k),
        Info_Fraction  = round(design$informationRates, 4),
        Efficacy_z     = crit_z,
        Efficacy_p     = crit_p,
        stringsAsFactors = FALSE
      )

      # ---- Futility boundary table (multi-scale) ----
      has_fut <- !is.null(design$futilityBounds) && !all(design$futilityBounds <= -5)
      needs_2stage <- c("condPowerAtObserved", "condPowerAtSpecified",
                        "predictivePower", "reverseCondPower")
      needs_info1  <- c("effectEstimate", "condPowerAtSpecified")

      # Resolve Fisher information I₁: compute from maxNumberOfEvents when provided
      max_events <- input$fb_max_events
      if (!is.null(max_events) && !is.na(max_events) && max_events > 0) {
        alloc <- input$fb_alloc
        if (is.null(alloc) || is.na(alloc)) alloc <- 1
        ir1   <- ir[1]
        info1 <- (alloc / (1 + alloc)^2) * max_events * ir1
      } else {
        info1 <- input$fb_info1
        if (is.null(info1) || is.na(info1)) info1 <- NULL
      }

      if (has_fut) {
        fut_z_all <- design$futilityBounds  # length k-1
        # helper: convert one z-value to another scale
        convert_one <- function(zval, target_scale) {
          if (target_scale %in% needs_2stage && k != 2) return(NA_real_)
          if (target_scale %in% needs_info1 && is.null(info1)) return(NA_real_)
          fb_args <- list(design,
                          sourceValue = zval,
                          sourceScale = "zValue",
                          targetScale = target_scale)
          if (!is.null(info1)) fb_args$information1 <- info1
          tryCatch(
            as.numeric(do.call(rpact::getFutilityBounds, fb_args)),
            error = function(e) NA_real_
          )
        }

        fut_rows <- lapply(seq_along(fut_z_all), function(i) {
          zv <- fut_z_all[i]
          active <- zv > -5
          data.frame(
            Stage            = i,
            Info_Fraction    = round(design$informationRates[i], 4),
            Futility_z       = if (active) round(zv, 4) else NA_real_,
            Futility_p       = if (active) format(pnorm(zv), digits = 4, scientific = TRUE) else "—",
            Cond_Power_Obs   = if (active) round(convert_one(zv, "condPowerAtObserved"),  4) else NA_real_,
            Predictive_Power = if (active) round(convert_one(zv, "predictivePower"),       4) else NA_real_,
            Rev_Cond_Power   = if (active) round(convert_one(zv, "reverseCondPower"),      4) else NA_real_,
            stringsAsFactors = FALSE
          )
        })
        fut_df <- do.call(rbind, fut_rows)
        if (k != 2) {
          fut_df$Cond_Power_Obs   <- "K≠2"
          fut_df$Predictive_Power <- "K≠2"
          fut_df$Rev_Cond_Power   <- "K≠2"
        }
      } else {
        fut_df <- data.frame(Message = "No futility bounds in this design.")
      }

      # ---- Single-threshold scale conversion (getFutilityBounds) ----
      source_scale <- input$fb_source_scale
      source_val   <- input$fb_source_val

      all_scales <- list(
        zValue               = "Z-statistic",
        pValue               = "P-value",
        condPowerAtObserved  = "Cond. Power (at observed effect)",
        condPowerAtSpecified = "Cond. Power (at specified effect)",
        predictivePower      = "Predictive Power",
        reverseCondPower     = "Reverse Cond. Power",
        effectEstimate       = "Effect Estimate (δ)"
      )

      conv_rows <- lapply(names(all_scales), function(ts) {
        label <- all_scales[[ts]]
        if (ts == source_scale)
          return(data.frame(Scale = label, Value = format(round(source_val, 6)),
                            Note = "(input)", stringsAsFactors = FALSE))
        if (ts %in% needs_2stage && k != 2)
          return(data.frame(Scale = label, Value = "—", Note = "Requires K = 2",
                            stringsAsFactors = FALSE))
        if (ts %in% needs_info1 && is.null(info1))
          return(data.frame(Scale = label, Value = "—", Note = "Provide I₁",
                            stringsAsFactors = FALSE))

        fb_args <- list(design, sourceValue = source_val,
                        sourceScale = source_scale, targetScale = ts)
        if (!is.null(info1)) fb_args$information1 <- info1

        val_raw <- tryCatch(
          as.numeric(do.call(rpact::getFutilityBounds, fb_args)),
          error = function(e) NA_real_
        )
        data.frame(
          Scale = label,
          Value = if (is.na(val_raw)) "—" else format(round(val_raw, 6)),
          Note  = if (is.na(val_raw)) "Conversion failed" else "",
          stringsAsFactors = FALSE
        )
      })
      conv_df <- do.call(rbind, conv_rows)

      list(design = design, eff_df = eff_df, fut_df = fut_df, conv_df = conv_df)
    })

    make_dt <- function(df) {
      DT::datatable(df, rownames = FALSE,
                    options = list(dom = "t", pageLength = 20,
                                   columnDefs = list(list(className = "dt-center", targets = "_all"))))
    }

    output$fb_eff_table <- DT::renderDataTable({
      res <- fb_result(); req(res); make_dt(res$eff_df)
    })
    output$fb_fut_table <- DT::renderDataTable({
      res <- fb_result(); req(res); make_dt(res$fut_df)
    })
    output$fb_conv_table <- DT::renderDataTable({
      res <- fb_result(); req(res); make_dt(res$conv_df)
    })

    output$fb_design_summary <- renderPrint({
      res <- fb_result()
      req(res)
      print(summary(res$design))
    })

  })
}
