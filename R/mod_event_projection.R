# ---------------------------------------------------------------------------
# Module 4: Event Projection
#
#   Method 1 : Moving Average (base R)
#   Method 2a: eventTrack — Hybrid Piecewise Exponential (native/recommended)
#   Method 2b: eventTrack — Parametric (Weibull / Exponential / Log-normal / Gamma)
#
#   Input: CSV with event time + event indicator columns (for now)
#   Time unit: Days or Months (auto-converted to months internally)
# ---------------------------------------------------------------------------

mod_event_projection_ui <- function(id) {
  ns <- NS(id)

  fluidRow(

    # ---- Left panel: Inputs ------------------------------------------------
    column(
      width = 4,

      box(
        title = "Data Input", status = "primary", solidHeader = TRUE, width = NULL,

        radioButtons(ns("endpoint_type"), "Endpoint Type",
                     choices = c("OS", "PFS"), inline = TRUE),

        fileInput(ns("data_file"), "Upload Event Data (CSV)",
                  accept = ".csv",
                  placeholder = "CSV with time & event indicator columns"),

        helpText("CSV must have at least two numeric columns: event time and event indicator (1 = event, 0 = censored)."),

        uiOutput(ns("col_selectors")),

        selectInput(ns("time_unit"), "Time Unit in Data",
                    choices = c("Months" = "months", "Days" = "days"),
                    selected = "months"),

        hr(),

        dateInput(ns("ccod_date"), "Current Cutoff Date (CCOD)", value = Sys.Date()),
        numericInput(ns("target_events"), "Target Number of Events", value = 200, min = 1),

        hr(),

        # ---- Method selection -----------------------------------------------
        radioButtons(ns("method"), "Projection Method",
                     choices = c(
                       "Moving Average"                   = "ma",
                       "eventTrack: Hybrid Piecewise Exp" = "hybrid",
                       "eventTrack: Parametric"           = "parametric"
                     )),

        # Moving average options
        conditionalPanel(
          condition = sprintf("input['%s'] == 'ma'", ns("method")),
          numericInput(ns("ma_window"), "Window (months)", value = 3, min = 1, max = 24)
        ),

        # Hybrid piecewise exponential options
        conditionalPanel(
          condition = sprintf("input['%s'] == 'hybrid'", ns("method")),
          numericInput(ns("max_k"), "Max Changepoints to Test", value = 5, min = 1, max = 10),
          numericInput(ns("n_boot_hybrid"), "Bootstrap Iterations", value = 500, min = 100, max = 2000, step = 100),
          numericInput(ns("future_months_hybrid"), "Months to Project Forward", value = 36, min = 6, max = 120)
        ),

        # Parametric options
        conditionalPanel(
          condition = sprintf("input['%s'] == 'parametric'", ns("method")),
          selectInput(ns("dist_choice"), "Distribution",
                      choices = c(
                        "Weibull"       = "weibull",
                        "Exponential"   = "exp",
                        "Log-normal"    = "lnorm",
                        "Gamma"         = "gamma"
                      )),
          numericInput(ns("n_boot_param"), "Bootstrap Iterations", value = 500, min = 100, max = 2000, step = 100),
          numericInput(ns("future_months_param"), "Months to Project Forward", value = 36, min = 6, max = 120)
        ),

        actionButton(ns("run"), "Run Projection",
                     class = "btn-primary btn-block", icon = icon("play"))
      )
    ),

    # ---- Right panel: Results ----------------------------------------------
    column(
      width = 8,

      box(
        title = "Data Preview", status = "info", solidHeader = TRUE,
        width = NULL, collapsible = TRUE, collapsed = FALSE,
        DT::dataTableOutput(ns("data_preview"))
      ),

      box(
        title = "Projection Results", status = "success", solidHeader = TRUE, width = NULL,
        uiOutput(ns("results_summary")),
        hr(),
        plotOutput(ns("projection_plot"), height = "400px")
      )
    )
  )
}


mod_event_projection_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ---- Load CSV ----------------------------------------------------------
    evt_data <- reactive({
      req(input$data_file)
      tryCatch(
        read.csv(input$data_file$datapath, stringsAsFactors = FALSE),
        error = function(e) {
          showNotification("Failed to read CSV.", type = "error")
          NULL
        }
      )
    })

    # ---- Dynamic column selectors ------------------------------------------
    output$col_selectors <- renderUI({
      df <- evt_data()
      req(df)
      cols <- names(df)
      tagList(
        selectInput(ns("time_col"),  "Event Time Column",
                    choices = cols, selected = cols[1]),
        selectInput(ns("event_col"), "Event Indicator Column (1=event, 0=censored)",
                    choices = cols, selected = if (length(cols) >= 2) cols[2] else cols[1])
      )
    })

    # ---- Data preview ------------------------------------------------------
    output$data_preview <- DT::renderDataTable({
      df <- evt_data()
      req(df)
      DT::datatable(head(df, 20),
                    options = list(pageLength = 5, scrollX = TRUE),
                    rownames = FALSE)
    })

    # ---- Run on button click -----------------------------------------------
    results <- eventReactive(input$run, {
      df <- evt_data()
      req(df, input$time_col, input$event_col)

      time_raw  <- suppressWarnings(as.numeric(df[[input$time_col]]))
      event_vec <- suppressWarnings(as.integer(df[[input$event_col]]))

      validate(
        need(!any(is.na(time_raw)),            "Time column contains missing / non-numeric values."),
        need(all(event_vec %in% c(0L, 1L)),    "Event column must contain only 0 and 1."),
        need(sum(event_vec, na.rm = TRUE) > 5, "Too few events to project (need > 5).")
      )

      # Standardise to months
      time_months <- if (input$time_unit == "days") time_raw / 30.4375 else time_raw

      n_current <- sum(event_vec)
      target    <- input$target_events
      ccod      <- input$ccod_date

      validate(
        need(target > n_current,
             sprintf("Target events (%d) must exceed current events (%d).", target, n_current))
      )

      withProgress(message = "Running projection...", value = 0.1, {
        if (input$method == "ma") {
          run_moving_average(time_months, event_vec, target, n_current, ccod,
                             input$ma_window)

        } else if (input$method == "hybrid") {
          incProgress(0.1, detail = "Fitting piecewise exponential model...")
          run_eventtrack_hybrid(time_months, event_vec, target, n_current, ccod,
                                input$max_k, input$n_boot_hybrid, input$future_months_hybrid)

        } else {
          incProgress(0.1, detail = paste("Fitting", input$dist_choice, "distribution..."))
          run_eventtrack_parametric(time_months, event_vec, target, n_current, ccod,
                                    input$dist_choice, input$n_boot_param, input$future_months_param)
        }
      })
    })

    # ---- Summary boxes -----------------------------------------------------
    output$results_summary <- renderUI({
      res <- results()
      req(res)

      tagList(
        fluidRow(
          column(4, div(style = "text-align:center;",
            h5(strong("Current Events")), p(class = "info-value", res$n_current))),
          column(4, div(style = "text-align:center;",
            h5(strong("Target Events")),  p(class = "info-value", res$n_target))),
          column(4, div(style = "text-align:center;",
            h5(strong("Remaining")),      p(class = "info-value", res$n_target - res$n_current)))
        ),
        hr(),
        fluidRow(
          column(4, div(style = "text-align:center;",
            h5(strong("Predicted Date")),
            p(class = "info-value pass-flag", format(res$pred_date, "%Y-%m-%d")))),
          column(4, div(style = "text-align:center;",
            h5(strong("95% CI Lower")),
            p(class = "info-value", format(res$ci_lower, "%Y-%m-%d")))),
          column(4, div(style = "text-align:center;",
            h5(strong("95% CI Upper")),
            p(class = "info-value", format(res$ci_upper, "%Y-%m-%d"))))
        ),
        if (!is.null(res$method_note))
          p(class = "text-muted", style = "margin-top:8px;", em(res$method_note))
      )
    })

    # ---- Projection plot ---------------------------------------------------
    output$projection_plot <- renderPlot({
      res <- results()
      req(res, !is.null(res$monthly_df))

      df_obs <- res$monthly_df
      ccod   <- input$ccod_date
      target <- res$n_target

      x_max <- max(res$ci_upper, res$pred_date, na.rm = TRUE)
      x_range <- range(c(df_obs$date, x_max), na.rm = TRUE)
      y_max   <- max(target * 1.08, max(df_obs$cum_events, na.rm = TRUE) * 1.1, na.rm = TRUE)

      method_label <- switch(input$method,
        ma          = "Moving Average",
        hybrid      = "eventTrack: Hybrid Piecewise Exponential",
        parametric  = paste0("eventTrack: ",
                             c(weibull="Weibull", exp="Exponential",
                               lnorm="Log-normal", gamma="Gamma")[input$dist_choice])
      )
      main_title <- paste0(input$endpoint_type, " Event Projection — ", method_label)

      par(mar = c(4, 4.5, 2.5, 1))
      plot(df_obs$date, df_obs$cum_events,
           type = "l", lwd = 2.5, col = "#2c7bb6",
           xlab = "Date", ylab = "Cumulative Events",
           main = main_title,
           xlim = x_range, ylim = c(0, y_max),
           las = 1)

      # Projected trajectory
      pd <- res$proj_df
      if (!is.null(pd)) {
        if (!is.null(pd$lower) && !all(is.na(pd$lower))) {
          polygon(c(pd$date, rev(pd$date)),
                  c(pd$upper, rev(pd$lower)),
                  col = adjustcolor("#d7191c", alpha.f = 0.12), border = NA)
        }
        lines(pd$date, pd$cum_events, lwd = 2, lty = 2, col = "#d7191c")
      }

      # Target line
      abline(h = target, lty = 3, lwd = 1.5, col = "darkgreen")
      text(x_range[1], target * 1.02, paste0("Target: ", target),
           pos = 4, col = "darkgreen", cex = 0.82)

      # CCOD marker
      abline(v = as.numeric(ccod), lty = 2, col = "gray60", lwd = 1.2)
      mtext("CCOD", side = 3, at = as.numeric(ccod), col = "gray50", cex = 0.75, line = 0.1)

      # Predicted date + CI
      abline(v = as.numeric(res$pred_date), col = "#d7191c", lwd = 2)
      if (!is.na(res$ci_lower))
        abline(v = as.numeric(res$ci_lower), col = "#d7191c", lwd = 1, lty = 3)
      if (!is.na(res$ci_upper))
        abline(v = as.numeric(res$ci_upper), col = "#d7191c", lwd = 1, lty = 3)

      legend("topleft",
             legend = c("Observed", "Projected", "95% CI band", "Target", "Pred. date / CI"),
             col    = c("#2c7bb6", "#d7191c",
                        adjustcolor("#d7191c", 0.3), "darkgreen", "#d7191c"),
             lty    = c(1, 2, NA, 3, 1),
             lwd    = c(2.5, 2, NA, 1.5, 1.5),
             pch    = c(NA, NA, 15, NA, NA), pt.cex = 1.5,
             bty = "n", cex = 0.82)
    })
  })
}


# ===========================================================================
# Helper: Moving Average
# ===========================================================================
run_moving_average <- function(time_months, event_vec, target, n_current, ccod, window) {

  ev_months  <- time_months[event_vec == 1]
  max_month  <- ceiling(max(ev_months, na.rm = TRUE))
  monthly_ev <- tabulate(ceiling(ev_months), nbins = max_month)

  # Trim leading zero months
  first_ev  <- which(monthly_ev > 0)[1]
  active_ev <- monthly_ev[first_ev:length(monthly_ev)]

  window  <- min(window, length(active_ev))
  recent  <- tail(active_ev, window)
  rate    <- mean(recent)
  if (rate <= 0) stop("Moving average event rate is zero — cannot project.")

  sd_rate <- if (window > 1) sd(recent) else rate * 0.2
  if (is.na(sd_rate) || sd_rate == 0) sd_rate <- rate * 0.2

  se       <- sd_rate / sqrt(window)
  rate_hi  <- max(rate + 1.96 * se, 0.01)
  rate_lo  <- max(rate - 1.96 * se, 0.01)

  months_pt <- (target - n_current) / rate
  months_lo <- (target - n_current) / rate_hi
  months_hi <- (target - n_current) / rate_lo

  pred_date <- ccod + months_pt * 30.4375
  ci_lower  <- ccod + months_lo * 30.4375
  ci_upper  <- ccod + months_hi * 30.4375

  # Observed cumulative events anchored at CCOD
  cum_ev    <- cumsum(monthly_ev)
  obs_dates <- ccod - (max_month - seq_along(monthly_ev)) * 30.4375
  monthly_df <- data.frame(date = obs_dates, cum_events = cum_ev)

  # Projected trajectory + CI band
  proj_seq <- seq(0, months_hi * 1.05, by = 0.5)
  proj_df  <- data.frame(
    date       = ccod + proj_seq * 30.4375,
    cum_events = pmin(n_current + proj_seq * rate,    target * 1.05),
    lower      = pmin(n_current + proj_seq * rate_hi, target * 1.05),
    upper      = pmin(n_current + proj_seq * rate_lo, target * 1.05)
  )

  list(
    n_current   = n_current,
    n_target    = target,
    pred_date   = pred_date,
    ci_lower    = ci_lower,
    ci_upper    = ci_upper,
    monthly_df  = monthly_df,
    proj_df     = proj_df,
    method_note = sprintf(
      "Moving average over last %d active month(s). Monthly event rate: %.1f events/month (SD: %.1f).",
      window, rate, sd_rate
    )
  )
}


# ===========================================================================
# Helper: eventTrack — Hybrid Piecewise Exponential
# ===========================================================================
run_eventtrack_hybrid <- function(time_months, event_vec, target, n_current, ccod,
                                  max_k, n_boot, future_months) {

  for (pkg in c("eventTrack")) {
    if (!requireNamespace(pkg, quietly = TRUE))
      stop(sprintf("Package '%s' must be installed for this method.", pkg))
  }
  library(eventTrack)

  # 1. Fit piecewise exponential and detect changepoints
  pe_fit  <- piecewiseExp_MLE(time_months, event_vec, K = max_k)
  pe_test <- piecewiseExp_test_changepoint(pe_fit, alpha = 0.05)

  # Number of significant changepoints: last row where test is significant
  sig_rows   <- which(pe_test[, ncol(pe_test)] < 0.05)   # p-value column
  n_sig      <- length(sig_rows)
  changepoint <- if (n_sig > 0) pe_fit$tau[1] else pe_fit$tau[1]   # use first changepoint

  # 2. Hybrid survival function: KM up to changepoint, exponential beyond
  St_hybrid <- function(t0) hybrid_Exponential(t0, time_months, event_vec, changepoint)

  # 3. Point estimate
  pred_pt   <- predictEvents(
    time         = time_months,
    event        = event_vec,
    St           = St_hybrid,
    accrual.in   = NULL,
    start.date   = ccod,
    future.units = future_months
  )
  pred_date <- exactDatesFromMonths(pred_pt, target)

  # 4. Bootstrap CI
  set.seed(42)
  bs <- bootSurvivalSample(
    surv.obj = survival::Surv(time_months, event_vec),
    n = length(time_months),
    M = n_boot
  )

  boot_dates <- vapply(seq_len(n_boot), function(i) {
    bi <- as.matrix(bs[, i])
    t_i <- bi[, 1]; e_i <- bi[, 2]

    pe_i <- tryCatch(piecewiseExp_MLE(t_i, e_i, K = 1), error = function(e) NULL)
    if (is.null(pe_i)) return(NA_real_)

    cp_i <- pe_i$tau[1]
    St_i <- function(t0) hybrid_Exponential(t0, t_i, e_i, cp_i)

    pred_i <- tryCatch(
      predictEvents(time = t_i, event = e_i, St = St_i,
                    accrual.in = NULL, start.date = ccod,
                    future.units = future_months),
      error = function(e) NULL
    )
    if (is.null(pred_i)) return(NA_real_)
    tryCatch(as.numeric(exactDatesFromMonths(pred_i, target)), error = function(e) NA_real_)
  }, numeric(1))

  boot_valid <- boot_dates[!is.na(boot_dates)]
  if (length(boot_valid) < 10)
    stop("Too few valid bootstrap samples. Try reducing target events or check data.")

  ci_dates <- as.Date(quantile(boot_valid, c(0.025, 0.975)), origin = "1970-01-01")

  # Build monthly observed + projected data for plot
  monthly_df <- build_monthly_obs(time_months, event_vec, ccod)
  proj_df    <- build_proj_from_pred(pred_pt, ccod)

  list(
    n_current   = n_current,
    n_target    = target,
    pred_date   = pred_date,
    ci_lower    = ci_dates[1],
    ci_upper    = ci_dates[2],
    monthly_df  = monthly_df,
    proj_df     = proj_df,
    method_note = sprintf(
      "Hybrid piecewise exponential: changepoint at %.1f months. %d valid bootstrap samples (of %d).",
      changepoint, length(boot_valid), n_boot
    )
  )
}


# ===========================================================================
# Helper: eventTrack — Parametric Distribution
# ===========================================================================
run_eventtrack_parametric <- function(time_months, event_vec, target, n_current, ccod,
                                      dist, n_boot, future_months) {

  for (pkg in c("eventTrack", "fitdistrplus")) {
    if (!requireNamespace(pkg, quietly = TRUE))
      stop(sprintf("Package '%s' must be installed for this method.", pkg))
  }
  library(eventTrack)
  library(fitdistrplus)

  # 1. Fit selected distribution to censored data
  cens_df <- data.frame(left = time_months, right = time_months)
  cens_df[event_vec == 0, "right"] <- NA

  fit <- tryCatch(
    suppressWarnings(fitdistcens(cens_df, dist)),
    error = function(e) stop(sprintf("Failed to fit %s distribution: %s", dist, e$message))
  )

  # 2. Build survival function from fitted parameters
  St_fn <- make_St(dist, fit$estimate)

  # 3. Point estimate
  pred_pt <- predictEvents(
    time         = time_months,
    event        = event_vec,
    St           = St_fn,
    accrual.in   = NULL,
    start.date   = ccod,
    future.units = future_months
  )
  pred_date <- exactDatesFromMonths(pred_pt, target)

  # 4. Bootstrap CI
  set.seed(42)
  bs <- bootSurvivalSample(
    surv.obj = survival::Surv(time_months, event_vec),
    n = length(time_months),
    M = n_boot
  )

  boot_dates <- vapply(seq_len(n_boot), function(i) {
    bi <- as.matrix(bs[, i])
    d2 <- data.frame(left = bi[, 1], right = bi[, 1])
    d2[bi[, 2] == 0, "right"] <- NA

    fit_i <- tryCatch(
      suppressWarnings(fitdistcens(d2, dist)),
      error = function(e) NULL
    )
    if (is.null(fit_i)) return(NA_real_)

    St_i   <- make_St(dist, fit_i$estimate)
    pred_i <- tryCatch(
      predictEvents(time = bi[, 1], event = bi[, 2], St = St_i,
                    accrual.in = NULL, start.date = ccod,
                    future.units = future_months),
      error = function(e) NULL
    )
    if (is.null(pred_i)) return(NA_real_)
    tryCatch(as.numeric(exactDatesFromMonths(pred_i, target)), error = function(e) NA_real_)
  }, numeric(1))

  boot_valid <- boot_dates[!is.na(boot_dates)]
  if (length(boot_valid) < 10)
    stop("Too few valid bootstrap samples. Try a different distribution or check data.")

  ci_dates <- as.Date(quantile(boot_valid, c(0.025, 0.975)), origin = "1970-01-01")

  # Format parameter note
  param_str <- paste(
    mapply(function(nm, val) sprintf("%s = %.3f", nm, val),
           names(fit$estimate), fit$estimate),
    collapse = ", "
  )
  dist_label <- c(weibull = "Weibull", exp = "Exponential",
                  lnorm = "Log-normal", gamma = "Gamma")[dist]

  monthly_df <- build_monthly_obs(time_months, event_vec, ccod)
  proj_df    <- build_proj_from_pred(pred_pt, ccod)

  list(
    n_current   = n_current,
    n_target    = target,
    pred_date   = pred_date,
    ci_lower    = ci_dates[1],
    ci_upper    = ci_dates[2],
    monthly_df  = monthly_df,
    proj_df     = proj_df,
    method_note = sprintf(
      "%s fit: %s. Bootstrap CI from %d valid samples (of %d).",
      dist_label, param_str, length(boot_valid), n_boot
    )
  )
}


# ===========================================================================
# Utility: build survival function from fitdistcens estimates
# ===========================================================================
make_St <- function(dist, params) {
  switch(dist,
    weibull = {
      shape <- params["shape"]; scale <- params["scale"]
      function(t0) pweibull(t0, shape = shape, scale = scale, lower.tail = FALSE)
    },
    exp = {
      rate <- params["rate"]
      function(t0) pexp(t0, rate = rate, lower.tail = FALSE)
    },
    lnorm = {
      meanlog <- params["meanlog"]; sdlog <- params["sdlog"]
      function(t0) plnorm(t0, meanlog = meanlog, sdlog = sdlog, lower.tail = FALSE)
    },
    gamma = {
      shape <- params["shape"]; rate <- params["rate"]
      function(t0) pgamma(t0, shape = shape, rate = rate, lower.tail = FALSE)
    },
    stop("Unsupported distribution: ", dist)
  )
}


# ===========================================================================
# Utility: build monthly observed cumulative event data frame for plot
# ===========================================================================
build_monthly_obs <- function(time_months, event_vec, ccod) {
  ev_months  <- time_months[event_vec == 1]
  max_month  <- ceiling(max(ev_months, na.rm = TRUE))
  monthly_ev <- tabulate(ceiling(ev_months), nbins = max_month)
  cum_ev     <- cumsum(monthly_ev)
  obs_dates  <- ccod - (max_month - seq_along(monthly_ev)) * 30.4375
  data.frame(date = obs_dates, cum_events = cum_ev)
}


# ===========================================================================
# Utility: extract projected trajectory from predictEvents() output
#   predictEvents returns: t.predict (months or dates), predict.events (count)
# ===========================================================================
build_proj_from_pred <- function(pred_pt, ccod) {
  tryCatch({
    # When start.date is supplied, first column contains Date values
    col1 <- pred_pt[[1]]
    col2 <- pred_pt[[2]]
    if (inherits(col1, "Date")) {
      dates <- col1
    } else {
      # col1 is months from baseline — convert relative to ccod
      dates <- ccod + col1 * 30.4375
    }
    data.frame(
      date       = dates,
      cum_events = col2,
      lower      = NA_real_,
      upper      = NA_real_
    )
  }, error = function(e) NULL)
}
