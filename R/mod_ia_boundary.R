# ---------------------------------------------------------------------------
# Module 1: IA Boundary Re-calculation (rpact)
# ---------------------------------------------------------------------------

mod_ia_boundary_ui <- function(id) {
  ns <- NS(id)
  fluidRow(
    box(
      title = "IA Boundary Re-calculation", status = "primary",
      solidHeader = TRUE, width = 4,

      h5("Design Parameters"),
      numericInput(ns("n_stages"), "Number of Stages (K)", value = 3, min = 2, max = 10),
      numericInput(ns("alpha"), "Overall Alpha (one-sided)", value = 0.025, min = 0.001, max = 0.1, step = 0.005),
      numericInput(ns("beta"), "Beta (Type II error)", value = 0.2, min = 0.01, max = 0.5, step = 0.05),
      selectInput(ns("sided"), "Sidedness", choices = c("One-sided" = 1, "Two-sided" = 2), selected = 1),

      hr(),
      h5("Information Fractions"),
      radioButtons(ns("info_method"), "Input Method",
                   choices = c("Enter fractions directly" = "fractions",
                               "Enter observed events at each look" = "events"),
                   selected = "fractions", inline = TRUE),
      conditionalPanel(
        condition = sprintf("input['%s'] == 'fractions'", ns("info_method")),
        textInput(ns("info_fractions"), "Fractions (comma-separated)",
                  value = "0.33, 0.67, 1.0",
                  placeholder = "e.g. 0.33, 0.67, 1.0")
      ),
      conditionalPanel(
        condition = sprintf("input['%s'] == 'events'", ns("info_method")),
        textInput(ns("observed_events"), "Observed events at each look (comma-separated)",
                  placeholder = "e.g. 100, 200, 300"),
        verbatimTextOutput(ns("computed_fractions"))
      ),

      hr(),
      h5("Interim Analysis Types"),
      p("Assign each interim look (except the final) as Futility, Efficacy, or Both.",
        style = "font-size: 0.9em; color: #666;"),
      uiOutput(ns("ia_type_inputs")),

      hr(),
      h5("Efficacy Boundary"),
      selectInput(
        ns("alpha_spending"), "Alpha Spending Function (Efficacy)",
        choices = c(
          "O'Brien-Fleming" = "OF",
          "Pocock" = "P",
          "Wang-Tsiatis Delta" = "WT",
          "Lan-DeMets (O'Brien-Fleming)" = "asOF",
          "Lan-DeMets (Pocock)" = "asP",
          "Kim-DeMets (power)" = "asKD",
          "Hwang-Shih-DeCani" = "asHSD",
          "User-defined" = "asUser",
          "No early efficacy" = "noEarlyEfficacy"
        ),
        selected = "OF"
      ),
      conditionalPanel(
        condition = sprintf("input['%s'] == 'asKD'", ns("alpha_spending")),
        numericInput(ns("gamma_a"), "Gamma (alpha spending)", value = 1, step = 0.1)
      ),
      conditionalPanel(
        condition = sprintf("input['%s'] == 'asHSD'", ns("alpha_spending")),
        numericInput(ns("gamma_a_hsd"), "Gamma (alpha HSD)", value = -4, step = 0.5)
      ),
      conditionalPanel(
        condition = sprintf("input['%s'] == 'asUser'", ns("alpha_spending")),
        textInput(ns("user_alpha_spending"), "Cumulative alpha spent at each look (comma-separated)",
                  placeholder = "e.g. 0.0001, 0.0062, 0.025"),
        p("Must be non-decreasing; last value should equal overall alpha.",
          style = "font-size: 0.85em; color: #888;")
      ),

      hr(),
      h5("Futility Boundary"),
      selectInput(
        ns("beta_spending"), "Beta Spending Function (Futility)",
        choices = c(
          "None (no futility)" = "none",
          "O'Brien-Fleming type" = "bsOF",
          "Pocock type" = "bsP",
          "Kim-DeMets (power)" = "bsKD",
          "Hwang-Shih-DeCani" = "bsHSD",
          "User-defined" = "bsUser"
        ),
        selected = "bsOF"
      ),
      conditionalPanel(
        condition = sprintf("input['%s'] == 'bsKD'", ns("beta_spending")),
        numericInput(ns("gamma_b"), "Gamma (beta spending)", value = 1, step = 0.1)
      ),
      conditionalPanel(
        condition = sprintf("input['%s'] == 'bsHSD'", ns("beta_spending")),
        numericInput(ns("gamma_b_hsd"), "Gamma (beta HSD)", value = -4, step = 0.5)
      ),
      conditionalPanel(
        condition = sprintf("input['%s'] == 'bsUser'", ns("beta_spending")),
        textInput(ns("user_beta_spending"), "Cumulative beta spent at each look (comma-separated)",
                  placeholder = "e.g. 0.026, 0.117, 0.200"),
        p("Must be non-decreasing; last value should equal overall beta.",
          style = "font-size: 0.85em; color: #888;")
      ),
      conditionalPanel(
        condition = sprintf("input['%s'] != 'none'", ns("beta_spending")),
        checkboxInput(ns("binding_futility"), "Binding Futility", value = FALSE)
      ),

      hr(),
      h5("Effect Size Scale (HR Boundaries)"),
      checkboxInput(ns("compute_hr_bounds"), "Compute boundaries on HR scale", value = FALSE),
      conditionalPanel(
        condition = sprintf("input['%s']", ns("compute_hr_bounds")),
        radioButtons(ns("hr_input_method"), "Specify effect size via:",
                     choices = c("Hazard Ratio" = "hr", "Median survival times" = "medians"),
                     selected = "hr", inline = TRUE),
        conditionalPanel(
          condition = sprintf("input['%s'] == 'hr'", ns("hr_input_method")),
          numericInput(ns("hazard_ratio"), "Assumed Hazard Ratio (treatment vs control)",
                       value = 0.65, min = 0.01, max = 2.0, step = 0.01)
        ),
        conditionalPanel(
          condition = sprintf("input['%s'] == 'medians'", ns("hr_input_method")),
          numericInput(ns("median_trt"), "Median survival -- Treatment", value = 15, min = 0.1, step = 0.5),
          numericInput(ns("median_ctrl"), "Median survival -- Control", value = 10, min = 0.1, step = 0.5)
        ),
        numericInput(ns("allocation_ratio"), "Allocation Ratio (trt:ctrl)", value = 1, min = 0.1, max = 10, step = 0.1)
      ),

      hr(),
      actionButton(ns("run_calc"), "Calculate Boundaries", class = "btn-primary", width = "100%")
    ),

    box(
      title = "Results", status = "success", solidHeader = TRUE, width = 8,
      tabsetPanel(
        tabPanel("Efficacy Bounds (z)", DT::dataTableOutput(ns("efficacy_table"))),
        tabPanel("Futility Bounds (z)", DT::dataTableOutput(ns("futility_table"))),
        tabPanel("HR Scale Boundaries", DT::dataTableOutput(ns("hr_bounds_table"))),
        tabPanel("Combined Summary", DT::dataTableOutput(ns("combined_table"))),
        tabPanel("Boundary Plot", plotOutput(ns("boundary_plot"), height = "500px")),
        tabPanel("Design Details", verbatimTextOutput(ns("design_summary")))
      )
    )
  )
}

mod_ia_boundary_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Dynamic UI: generate IA type selectors based on K
    output$ia_type_inputs <- renderUI({
      k <- input$n_stages
      req(k >= 2)
      n_interim <- k - 1

      default_types <- if (n_interim == 1) {
        c("Both")
      } else if (n_interim == 2) {
        c("Futility", "Efficacy")
      } else {
        c("Futility", rep("Both", n_interim - 2), "Efficacy")
      }

      lapply(seq_len(n_interim), function(i) {
        selectInput(
          ns(paste0("ia_type_", i)),
          paste0("IA ", i, " Type"),
          choices = c("Futility" = "Futility", "Efficacy" = "Efficacy", "Both" = "Both"),
          selected = default_types[min(i, length(default_types))]
        )
      })
    })

    # Reactive: resolve info fractions from either input method
    resolved_info_frac <- reactive({
      k <- input$n_stages
      req(k)

      if (input$info_method == "fractions") {
        req(input$info_fractions)
        vals <- as.numeric(trimws(strsplit(input$info_fractions, ",")[[1]]))
        validate(need(length(vals) == k, "Number of fractions must equal K"))
        validate(need(all(!is.na(vals)), "All fractions must be numeric"))
        return(vals)
      } else {
        req(input$observed_events)
        events <- as.numeric(trimws(strsplit(input$observed_events, ",")[[1]]))
        validate(need(length(events) == k, "Number of event counts must equal K"))
        validate(need(all(!is.na(events) & events > 0), "All event counts must be positive numbers"))
        return(events / events[length(events)])
      }
    })

    output$computed_fractions <- renderText({
      req(input$observed_events)
      events <- suppressWarnings(as.numeric(trimws(strsplit(input$observed_events, ",")[[1]])))
      if (any(is.na(events)) || length(events) == 0) return("Enter valid event counts")
      fracs <- events / events[length(events)]
      paste0("Computed fractions: ", paste(round(fracs, 4), collapse = ", "))
    })

    design_result <- eventReactive(input$run_calc, {
      req(input$n_stages, input$alpha)

      info_frac <- resolved_info_frac()
      k <- input$n_stages

      # Collect IA types
      ia_types <- sapply(seq_len(k - 1), function(i) {
        input[[paste0("ia_type_", i)]]
      })

      # Build rpact arguments
      args <- list(
        kMax             = k,
        alpha            = input$alpha,
        beta             = input$beta,
        sided            = as.numeric(input$sided),
        informationRates = info_frac,
        typeOfDesign     = input$alpha_spending
      )

      # Alpha spending parameters
      if (input$alpha_spending == "asKD") {
        args$gammaA <- input$gamma_a
      } else if (input$alpha_spending == "asHSD") {
        args$gammaA <- input$gamma_a_hsd
      } else if (input$alpha_spending == "asUser") {
        req(input$user_alpha_spending)
        user_alpha <- as.numeric(trimws(strsplit(input$user_alpha_spending, ",")[[1]]))
        validate(need(length(user_alpha) == k, "User alpha spending must have K values"))
        validate(need(all(!is.na(user_alpha)), "All alpha spending values must be numeric"))
        args$userAlphaSpending <- user_alpha
      }

      # Beta spending (futility)
      if (input$beta_spending != "none") {
        args$typeBetaSpending <- input$beta_spending
        args$bindingFutility  <- input$binding_futility

        if (input$beta_spending == "bsKD") {
          args$gammaB <- input$gamma_b
        } else if (input$beta_spending == "bsHSD") {
          args$gammaB <- input$gamma_b_hsd
        } else if (input$beta_spending == "bsUser") {
          req(input$user_beta_spending)
          user_beta <- as.numeric(trimws(strsplit(input$user_beta_spending, ",")[[1]]))
          validate(need(length(user_beta) == k, "User beta spending must have K values"))
          validate(need(all(!is.na(user_beta)), "All beta spending values must be numeric"))
          args$userBetaSpending <- user_beta
        }
      }

      design <- do.call(rpact::getDesignGroupSequential, args)

      # Compute HR-scale boundaries if requested
      ss_result <- NULL
      if (isTRUE(input$compute_hr_bounds)) {
        ss_args <- list(design = design)

        if (input$hr_input_method == "hr") {
          ss_args$hazardRatio <- input$hazard_ratio
        } else {
          ss_args$median1 <- input$median_trt
          ss_args$median2 <- input$median_ctrl
        }

        ss_args$allocationRatioPlanned <- input$allocation_ratio

        ss_result <- tryCatch(
          do.call(rpact::getSampleSizeSurvival, ss_args),
          error = function(e) {
            showNotification(paste("HR scale error:", e$message), type = "warning")
            NULL
          }
        )
      }

      list(design = design, ia_types = ia_types, ss_result = ss_result)
    })

    # --- Efficacy bounds table (z-scale) ---
    output$efficacy_table <- DT::renderDataTable({
      req(design_result())
      res <- design_result()
      design <- res$design
      k <- input$n_stages

      df <- data.frame(
        Stage          = seq_len(k),
        Info_Fraction  = design$informationRates,
        Efficacy_z     = round(design$criticalValues, 6),
        Nominal_Alpha  = round(design$stageLevels, 6),
        Cumul_Alpha    = round(design$alphaSpent, 6),
        stringsAsFactors = FALSE
      )

      DT::datatable(df, rownames = FALSE,
                    options = list(dom = "t", pageLength = 20,
                                  columnDefs = list(list(className = "dt-center", targets = "_all"))))
    })

    # --- Futility bounds table (z-scale) ---
    output$futility_table <- DT::renderDataTable({
      req(design_result())
      res <- design_result()
      design <- res$design
      k <- input$n_stages

      if (input$beta_spending == "none" || is.null(design$futilityBounds)) {
        df <- data.frame(Note = "No futility boundaries (beta spending set to 'None')",
                         stringsAsFactors = FALSE)
        return(DT::datatable(df, rownames = FALSE, options = list(dom = "t")))
      }

      fut_bounds <- design$futilityBounds
      n_fut <- length(fut_bounds)

      df <- data.frame(
        Stage          = seq_len(n_fut),
        Info_Fraction  = design$informationRates[seq_len(n_fut)],
        Futility_z     = round(fut_bounds, 6),
        Cumul_Beta     = round(design$betaSpent[seq_len(n_fut)], 6),
        stringsAsFactors = FALSE
      )

      DT::datatable(df, rownames = FALSE,
                    options = list(dom = "t", pageLength = 20,
                                  columnDefs = list(list(className = "dt-center", targets = "_all"))))
    })

    # --- HR scale boundaries table ---
    output$hr_bounds_table <- DT::renderDataTable({
      req(design_result())
      res <- design_result()
      ss <- res$ss_result
      k <- input$n_stages

      if (is.null(ss)) {
        df <- data.frame(Note = "Enable 'Compute boundaries on HR scale' and provide effect size inputs.",
                         stringsAsFactors = FALSE)
        return(DT::datatable(df, rownames = FALSE, options = list(dom = "t")))
      }

      eff_hr <- ss$criticalValuesEffectScale
      eff_p  <- ss$criticalValuesPValueScale

      has_futility <- !is.null(ss$futilityBoundsEffectScale) && input$beta_spending != "none"
      fut_hr <- if (has_futility) c(ss$futilityBoundsEffectScale, NA) else rep(NA, k)
      fut_p  <- if (has_futility) c(ss$futilityBoundsPValueScale, NA) else rep(NA, k)

      df <- data.frame(
        Stage         = seq_len(k),
        Info_Fraction = res$design$informationRates,
        Efficacy_HR   = round(eff_hr, 6),
        Efficacy_p    = format(eff_p, digits = 4, scientific = TRUE),
        Futility_HR   = round(fut_hr, 6),
        Futility_p    = ifelse(is.na(fut_p), NA, format(fut_p, digits = 4, scientific = TRUE)),
        stringsAsFactors = FALSE
      )

      DT::datatable(df, rownames = FALSE,
                    options = list(dom = "t", pageLength = 20,
                                  columnDefs = list(list(className = "dt-center", targets = "_all"))))
    })

    # --- Combined summary table ---
    output$combined_table <- DT::renderDataTable({
      req(design_result())
      res <- design_result()
      design <- res$design
      k <- input$n_stages
      ia_types <- res$ia_types

      stage_type <- c(ia_types, "Final")

      efficacy <- round(design$criticalValues, 6)

      futility <- if (!is.null(design$futilityBounds) && input$beta_spending != "none") {
        c(round(design$futilityBounds, 6), NA)
      } else {
        rep(NA, k)
      }

      comp_df <- data.frame(
        Stage              = seq_len(k),
        Type               = stage_type,
        Info_Fraction      = design$informationRates,
        Efficacy_z         = efficacy,
        Futility_z         = futility,
        Cumul_Alpha        = round(design$alphaSpent, 6),
        stringsAsFactors   = FALSE
      )

      if (!is.null(design$betaSpent) && input$beta_spending != "none") {
        comp_df$Cumul_Beta <- round(design$betaSpent, 6)
      }

      ss <- res$ss_result
      if (!is.null(ss)) {
        comp_df$Efficacy_HR <- round(ss$criticalValuesEffectScale, 6)

        has_fut_hr <- !is.null(ss$futilityBoundsEffectScale) && input$beta_spending != "none"
        if (has_fut_hr) {
          comp_df$Futility_HR <- c(round(ss$futilityBoundsEffectScale, 6), NA)
        } else {
          comp_df$Futility_HR <- rep(NA, k)
        }
      }

      DT::datatable(comp_df, rownames = FALSE,
                    options = list(dom = "t", pageLength = 20,
                                  columnDefs = list(list(className = "dt-center", targets = "_all"))))
    })

    output$boundary_plot <- renderPlot({
      req(design_result())
      rpact::plot(design_result()$design, type = 1)
    })

    output$design_summary <- renderPrint({
      req(design_result())
      res <- design_result()
      cat("========== Group Sequential Design ==========\n\n")
      summary(res$design)
      if (!is.null(res$ss_result)) {
        cat("\n\n========== Survival Sample Size / HR Boundaries ==========\n\n")
        summary(res$ss_result)
      }
    })
  })
}
