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
                selectInput(ns("ph_beta_spending"), "Beta Spending Function",
                            choices = c(
                              "None (no futility)"  = "none",
                              "O'Brien-Fleming type" = "bsOF",
                              "Pocock type"          = "bsP",
                              "Kim-DeMets (power)"   = "bsKD",
                              "Hwang-Shih-DeCani"    = "bsHSD",
                              "User-defined"         = "bsUser"
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
                  condition = sprintf("input['%s'] == 'bsUser'", ns("ph_beta_spending")),
                  textInput(ns("ph_user_beta_spending"),
                            "Cumulative beta spent at each look (comma-separated)",
                            placeholder = "e.g. 0.026, 0.117, 0.200"),
                  p("Must be non-decreasing; last value = overall beta.",
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
                  numericInput(ns("ph_hr"), "Hazard Ratio (trt / ctrl)",
                               value = 0.65, min = 0.01, max = 5, step = 0.01)
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
        # Tab 2: Non-Proportional Hazard (gsDesign2) — skeleton
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
        # Tab 3: NPH — Stratified (gsDesign2)
        # ================================================================
        tabPanel(
          title = tagList(icon("layer-group"), " NPH — Stratified (gsDesign2)"),
          value = "nph_strat",

          fluidRow(
            column(
              width = 4,
              box(
                title = "Design Parameters", status = "danger",
                solidHeader = TRUE, width = NULL,

                h5("Trial Design"),
                numericInput(ns("nph_s_k"),     "Number of Stages (K)",          value = 1,     min = 1, max = 10),
                numericInput(ns("nph_s_alpha"),  "One-sided Alpha",               value = 0.025, min = 0.001, max = 0.5, step = 0.005),
                numericInput(ns("nph_s_beta"),   "Beta (Type II error, 1-power)", value = 0.2,   min = 0.01,  max = 0.5, step = 0.05),

                hr(),
                h5("Strata"),
                numericInput(ns("nph_s_n_strata"), "Number of Strata", value = 2, min = 2, max = 6, step = 1),
                p("Define each stratum's enrollment proportion and hazard model below.",
                  style = "font-size: 0.85em; color: #888;"),
                uiOutput(ns("nph_strat_stratum_inputs")),

                hr(),
                h5("Accrual & Follow-up"),
                numericInput(ns("nph_s_accrual_time"),  "Accrual Duration (months)",                   value = 24,   min = 1),
                numericInput(ns("nph_s_accrual_rate"),  "Accrual Rate (patients / month)",             value = 20,   min = 0.1, step = 1),
                numericInput(ns("nph_s_followup_time"), "Additional Follow-up after Accrual (months)", value = 12,   min = 0),
                numericInput(ns("nph_s_dropout"),       "Annual Dropout Rate (all strata)",            value = 0.02, min = 0, max = 1, step = 0.01),
                numericInput(ns("nph_s_alloc"),         "Allocation Ratio (trt : ctrl)",               value = 1,    min = 0.1, step = 0.1),

                hr(),
                h5("Group Sequential Boundaries"),
                helpText("For K = 1 (fixed design), boundary inputs are ignored."),
                conditionalPanel(
                  condition = sprintf("input['%s'] > 1", ns("nph_s_k")),
                  textInput(ns("nph_s_analysis_times"), "Analysis Calendar Times (months, comma-separated)",
                            placeholder = "e.g. 16, 26, 35"),
                  p("Last value must equal Accrual Duration + Follow-up.",
                    style = "font-size: 0.85em; color: #888;"),
                  selectInput(ns("nph_s_alpha_spending"), "Alpha Spending (Efficacy)",
                              choices = c(
                                "Lan-DeMets (O'Brien-Fleming)" = "sfLDOF",
                                "Lan-DeMets (Pocock)"          = "sfLDP",
                                "Hwang-Shih-DeCani (gamma=-4)" = "sfHSD"
                              ), selected = "sfLDOF"),
                  selectInput(ns("nph_s_beta_spending"), "Beta Spending (Futility)",
                              choices = c(
                                "None (no futility)" = "none",
                                "Lan-DeMets (O'Brien-Fleming)" = "sfLDOF",
                                "Lan-DeMets (Pocock)"          = "sfLDP",
                                "Hwang-Shih-DeCani (gamma=-4)" = "sfHSD"
                              ), selected = "none")
                ),

                hr(),
                actionButton(ns("nph_s_run"), "Calculate Sample Size",
                             class = "btn-danger btn-block", icon = icon("calculator"))
              )
            ),

            column(
              width = 8,
              uiOutput(ns("nph_s_summary_boxes")),
              box(
                title = "Detailed Results", status = "success",
                solidHeader = TRUE, width = NULL,
                tabsetPanel(
                  tabPanel("Stage Summary",  DT::dataTableOutput(ns("nph_s_stage_table"))),
                  tabPanel("Boundaries",     DT::dataTableOutput(ns("nph_s_bound_table"))),
                  tabPanel("Design Details", verbatimTextOutput(ns("nph_s_design_details")))
                )
              )
            )
          )
        )

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
      if (input$ph_beta_spending != "none" && input$ph_alpha_spending %in% classic_designs) {
        showNotification(
          paste0("Classic bound designs (O\u2019Brien-Fleming, Pocock, Wang-Tsiatis) do not support ",
                 "beta spending in rpact. Switch the Alpha Spending Function to a spending-based ",
                 "design (e.g., Lan-DeMets) to enable futility bounds."),
          type = "warning", duration = 12
        )
      } else if (input$ph_beta_spending != "none") {
        design_args$typeBetaSpending <- input$ph_beta_spending
        design_args$bindingFutility  <- input$ph_binding_futility
        if (input$ph_beta_spending == "bsKD")
          design_args$gammaB <- input$ph_gamma_b
        if (input$ph_beta_spending == "bsHSD")
          design_args$gammaB <- input$ph_gamma_b_hsd
        if (input$ph_beta_spending == "bsUser") {
          req(input$ph_user_beta_spending)
          ub <- as.numeric(trimws(strsplit(input$ph_user_beta_spending, ",")[[1]]))
          validate(need(length(ub) == k && all(!is.na(ub)), "Check user beta spending values."))
          design_args$userBetaSpending <- ub
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

      # ---- Build sample size / survival args ----
      ss_args <- list(
        design                  = design,
        allocationRatioPlanned  = input$ph_alloc,
        accrualTime             = accrual_time_arg,
        accrualIntensity        = accrual_intensity_arg,
        followUpTime            = input$ph_followup_time,
        dropoutRate1            = input$ph_dropout1,
        dropoutRate2            = input$ph_dropout2,
        dropoutTime             = input$ph_dropout_time
      )

      if (input$ph_effect_input == "hr") {
        ss_args$hazardRatio <- input$ph_hr
      } else {
        ss_args$median1 <- input$ph_med_trt
        ss_args$median2 <- input$ph_med_ctrl
      }

      ss <- tryCatch(
        do.call(rpact::getSampleSizeSurvival, ss_args),
        error = function(e) {
          showNotification(paste("rpact error:", e$message), type = "error", duration = 8)
          NULL
        }
      )
      validate(need(!is.null(ss), "Sample size calculation failed — check inputs."))

      list(design = design, ss = ss, ia_types = ia_types, k = k)
    })

    # ================================================================
    # PH: Summary boxes
    # ================================================================
    output$ph_summary_boxes <- renderUI({
      res <- ph_result()
      req(res)
      ss <- res$ss

      total_n   <- ceiling(max(ss$numberOfSubjects,  na.rm = TRUE))
      n_arm1    <- ceiling(max(ss$numberOfSubjects1, na.rm = TRUE))
      n_arm2    <- ceiling(max(ss$numberOfSubjects2, na.rm = TRUE))
      tot_ev    <- ceiling(max(ss$numberOfEvents,    na.rm = TRUE))
      duration  <- round(max(ss$studyDuration,       na.rm = TRUE), 1)

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
            span(class = "info-box-number", paste0(n_arm1, " / ", n_arm2))
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
        Events_at_Stage  = ceiling(ss$eventsPerStage),
        Cum_Events       = ceiling(ss$numberOfEvents),
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
                      input$ph_beta_spending != "none" &&
                      !(input$ph_alpha_spending %in% c("OF", "P", "WT"))
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
                 input$ph_beta_spending != "none" &&
                 !(input$ph_alpha_spending %in% c("OF", "P", "WT"))
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
    # NPH Stratified: Dynamic per-stratum inputs
    # ================================================================
    output$nph_strat_stratum_inputs <- renderUI({
      n <- input$nph_s_n_strata
      req(n >= 2)

      lapply(seq_len(n), function(i) {
        tagList(
          hr(),
          h6(paste0("Stratum ", i)),
          textInput(ns(paste0("nph_s_name_",     i)), "Stratum Name",                    value = paste0("S", i)),
          numericInput(ns(paste0("nph_s_prop_",  i)), "Enrollment Proportion (%)",        value = round(100 / n, 1), min = 0.1, max = 99.9, step = 0.1),
          numericInput(ns(paste0("nph_s_ctrl_",  i)), "Median Survival — Control (mo)",   value = 10,   min = 0.1, step = 0.5),
          numericInput(ns(paste0("nph_s_delay_", i)), "Delay Duration (months)",          value = 6,    min = 0,   step = 1),
          numericInput(ns(paste0("nph_s_hre_",   i)), "HR during delay",                  value = 1.0,  min = 0.01, step = 0.01),
          numericInput(ns(paste0("nph_s_hrl_",   i)), "HR after delay (steady-state HR)", value = 0.65, min = 0.01, step = 0.01)
        )
      })
    })

    # ================================================================
    # NPH Stratified: Main calculation
    # ================================================================
    nph_s_result <- eventReactive(input$nph_s_run, {

      n_strata <- input$nph_s_n_strata
      k        <- input$nph_s_k

      # Collect per-stratum inputs
      strat_names <- sapply(seq_len(n_strata), function(i) input[[paste0("nph_s_name_", i)]])
      props       <- sapply(seq_len(n_strata), function(i) input[[paste0("nph_s_prop_", i)]])
      med_ctrls   <- sapply(seq_len(n_strata), function(i) input[[paste0("nph_s_ctrl_", i)]])
      delays      <- sapply(seq_len(n_strata), function(i) input[[paste0("nph_s_delay_",i)]])
      hr_earlys   <- sapply(seq_len(n_strata), function(i) input[[paste0("nph_s_hre_",  i)]])
      hr_lates    <- sapply(seq_len(n_strata), function(i) input[[paste0("nph_s_hrl_",  i)]])

      validate(
        need(all(!is.na(props)) && sum(props) > 0, "Enrollment proportions must be positive numbers."),
        need(all(med_ctrls > 0), "Median survival must be positive for all strata.")
      )

      rel_props <- props / sum(props)   # normalise to sum to 1

      total_time <- input$nph_s_accrual_time + input$nph_s_followup_time
      dropout_mo <- input$nph_s_dropout / 12

      # Build enroll_rate across strata
      enroll_rate <- gsDesign2::define_enroll_rate(
        duration = rep(input$nph_s_accrual_time, n_strata),
        rate     = rel_props,
        stratum  = strat_names
      )

      # Build fail_rate across strata (delayed treatment effect per stratum)
      durations_all    <- c()
      fail_rates_all   <- c()
      hrs_all          <- c()
      dropout_all      <- c()
      strata_all       <- c()

      for (i in seq_len(n_strata)) {
        ctrl_lambda_i <- log(2) / med_ctrls[i]
        durations_all  <- c(durations_all,  delays[i], Inf)
        fail_rates_all <- c(fail_rates_all, ctrl_lambda_i, ctrl_lambda_i)
        hrs_all        <- c(hrs_all,        hr_earlys[i],  hr_lates[i])
        dropout_all    <- c(dropout_all,    dropout_mo,    dropout_mo)
        strata_all     <- c(strata_all,     strat_names[i], strat_names[i])
      }

      fail_rate <- gsDesign2::define_fail_rate(
        duration     = durations_all,
        fail_rate    = fail_rates_all,
        hr           = hrs_all,
        dropout_rate = dropout_all,
        stratum      = strata_all
      )

      # Spending functions
      sf_lookup <- list(
        sfLDOF = list(sf = gsDesign::sfLDOF,    param = NULL),
        sfLDP  = list(sf = gsDesign::sfLDPocock, param = NULL),
        sfHSD  = list(sf = gsDesign::sfHSD,      param = -4)
      )

      if (k == 1) {
        nph_s_analysis_time <- total_time
        upper_fn  <- gsDesign2::gs_b
        upper_par <- stats::qnorm(1 - input$nph_s_alpha)
        lower_fn  <- gsDesign2::gs_b
        lower_par <- -Inf
      } else {
        req(input$nph_s_analysis_times)
        nph_s_analysis_time <- as.numeric(trimws(strsplit(input$nph_s_analysis_times, ",")[[1]]))
        validate(
          need(length(nph_s_analysis_time) == k,        "Number of analysis times must equal K."),
          need(all(!is.na(nph_s_analysis_time)),         "All analysis times must be numeric."),
          need(all(diff(nph_s_analysis_time) > 0),       "Analysis times must be strictly increasing."),
          need(abs(nph_s_analysis_time[k] - total_time) < 0.01,
               sprintf("Last analysis time must equal total study duration (%.1f months).", total_time))
        )

        su <- sf_lookup[[input$nph_s_alpha_spending]]
        upper_fn  <- gsDesign2::gs_spending_bound
        upper_par <- list(sf = su$sf, total_spend = input$nph_s_alpha, param = su$param)

        if (input$nph_s_beta_spending == "none") {
          lower_fn  <- gsDesign2::gs_b
          lower_par <- -Inf
        } else {
          sl <- sf_lookup[[input$nph_s_beta_spending]]
          lower_fn  <- gsDesign2::gs_spending_bound
          lower_par <- list(sf = sl$sf, total_spend = input$nph_s_beta, param = sl$param)
        }
      }

      result <- tryCatch(
        gsDesign2::gs_design_ahr(
          enroll_rate   = enroll_rate,
          fail_rate     = fail_rate,
          ratio         = input$nph_s_alloc,
          alpha         = input$nph_s_alpha,
          beta          = input$nph_s_beta,
          analysis_time = nph_s_analysis_time,
          info_scale    = "h0_h1_info",
          upper         = upper_fn,
          upar          = upper_par,
          lower         = lower_fn,
          lpar          = lower_par
        ),
        error = function(e) {
          showNotification(paste("gsDesign2 error:", e$message), type = "error", duration = 10)
          NULL
        }
      )
      validate(need(!is.null(result), "Calculation failed — check inputs."))

      list(result = result, k = k, total_time = total_time,
           strat_names = strat_names, rel_props = rel_props)
    })

    # ================================================================
    # NPH Stratified: Summary boxes
    # ================================================================
    output$nph_s_summary_boxes <- renderUI({
      res <- nph_s_result()
      req(res)
      an <- res$result$analysis

      total_n  <- ceiling(max(an$n,     na.rm = TRUE))
      tot_ev   <- ceiling(max(an$event, na.rm = TRUE))
      duration <- round(max(an$time,    na.rm = TRUE), 1)
      ratio    <- input$nph_s_alloc
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
    # NPH Stratified: Stage summary table
    # ================================================================
    output$nph_s_stage_table <- DT::renderDataTable({
      res <- nph_s_result()
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
    # NPH Stratified: Boundaries table
    # ================================================================
    output$nph_s_bound_table <- DT::renderDataTable({
      res <- nph_s_result()
      req(res)
      bd <- res$result$bound

      keep_cols <- intersect(names(bd), c("analysis", "bound", "z", "probability", "~hr at bound"))
      df <- as.data.frame(bd[, keep_cols, drop = FALSE])
      numeric_cols <- sapply(df, is.numeric)
      df[numeric_cols] <- lapply(df[numeric_cols], round, digits = 6)

      DT::datatable(df, rownames = FALSE,
                    options = list(dom = "t", pageLength = 20,
                                   columnDefs = list(list(className = "dt-center", targets = "_all"))))
    })

    # ================================================================
    # NPH Stratified: Design details
    # ================================================================
    output$nph_s_design_details <- renderPrint({
      res <- nph_s_result()
      req(res)
      cat("========== NPH Stratified Design (gsDesign2 :: gs_design_ahr) ==========\n\n")
      cat("Strata:", paste(res$strat_names, collapse = ", "), "\n")
      cat("Relative enrollment proportions:",
          paste(round(res$rel_props * 100, 1), collapse = "%, "), "%\n\n")
      print(res$result)
    })

  })
}
