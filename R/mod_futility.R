# ---------------------------------------------------------------------------
# Module: Futility IA in Trial Design
# Adapted from Jay Zhang & Fanni Zhang (app_updated.R)
# ---------------------------------------------------------------------------

# ---- Statistical helper functions (power-function accrual) ----
rct.r    <- function(t, rpow, rct.T) (t <= rct.T) * (t / rct.T)^rpow + (t > rct.T)
rct.d    <- function(t, rpow, rct.T) (t <= rct.T) * rpow * (t / rct.T)^(rpow - 1) / rct.T + 0
cen.s    <- function(t, hzc) 1 - pexp(t, hzc)
survv    <- function(t, hz) 1 - pexp(t, hz)
atrsk    <- function(t, hz, hzc) survv(t, hz) * cen.s(t, hzc)
Atrsk    <- function(tt, hz, hzc, rpow, rct.T) {
  tt1 <- min(tt, rct.T)
  integrate(function(t) { rct.d(t, rpow, rct.T) * atrsk(tt - t, hz, hzc) }, 0, tt1)$value
}
Atrsk    <- Vectorize(Atrsk, "tt")
dmm      <- function(tt, hz, hzc, rpow, rct.T) hz * Atrsk(tt, hz, hzc, rpow, rct.T)
dmmt     <- function(tt, hz0, hz1, hzc0, hzc1, rpow, rct.T, rw) {
  rw[1] * dmm(tt, hz0, hzc0, rpow, rct.T) + rw[2] * dmm(tt, hz1, hzc1, rpow, rct.T)
}
cmmt     <- function(dcot, hz0, hz1, hzc0, hzc1, rpow, rct.T, rw) {
  integrate(function(tt) dmmt(tt, hz0, hz1, hzc0, hzc1, rpow, rct.T, rw), 0, dcot)$value
}
cMMt     <- Vectorize(cmmt, "dcot")
Tdco     <- function(Mat.r, hz0, hz1, hzc0, hzc1, rpow, rct.T, rw) {
  uniroot(function(t) cMMt(t, hz0, hz1, hzc0, hzc1, rpow, rct.T, rw) - Mat.r, c(0, 1e2))$root
}
Tdco.hz1 <- Vectorize(Tdco, "hz1")

# ---- Fast piecewise (monthly) accrual helpers ----
make_pw_schedule <- function(rates, durations) {
  stopifnot(length(rates) == length(durations))
  total_N <- sum(rates * durations)
  if (total_N <= 0) stop("Total planned accrual from monthly rates must be > 0.")
  edges  <- c(0, cumsum(durations))
  starts <- head(edges, -1)
  ends   <- tail(edges, -1)
  cum_N  <- cumsum(rates * durations)
  list(rates = rates, durations = durations, starts = starts, ends = ends,
       total_T = sum(durations), total_N = total_N,
       dens = rates / total_N, cum_N = cum_N / total_N)
}

rct.r_pw_fast <- function(t, sched) {
  t   <- as.numeric(t)
  out <- numeric(length(t))
  out[t <= 0] <- 0
  out[t >= sched$total_T] <- 1
  mid <- which(t > 0 & t < sched$total_T)
  if (length(mid) > 0) {
    idx <- findInterval(t[mid], c(0, sched$ends), rightmost.closed = TRUE)
    idx[idx < 1] <- 1
    idx[idx > length(sched$rates)] <- length(sched$rates)
    t_prev   <- c(0, sched$ends)[idx]
    n_prev   <- c(0, sched$cum_N)[idx]
    out[mid] <- n_prev + sched$dens[idx] * (t[mid] - t_prev)
  }
  out
}

pw_cum_events_single <- function(t, hz, hzc, sched) {
  lam <- hz + hzc
  t   <- as.numeric(t)
  vapply(t, function(tt) {
    if (tt <= 0) return(0)
    m      <- pmin(sched$ends, tt)
    active <- m > sched$starts
    if (!any(active)) return(0)
    a       <- sched$starts[active]
    b       <- m[active]
    d       <- sched$dens[active]
    L       <- b - a
    exp_int <- (exp(-lam * (tt - b)) - exp(-lam * (tt - a))) / lam
    sum(d * hz / lam * (L - exp_int))
  }, numeric(1))
}

pw_cum_events_total <- function(t, hz0, hz1, hzc0, hzc1, rw, sched) {
  rw[1] * pw_cum_events_single(t, hz0, hzc0, sched) +
    rw[2] * pw_cum_events_single(t, hz1, hzc1, sched)
}

pw_Tdco_fast <- function(Mat.r, hz0, hz1, hzc0, hzc1, rw, sched) {
  f     <- function(t) pw_cum_events_total(t, hz0, hz1, hzc0, hzc1, rw, sched) - Mat.r
  lower <- 0
  upper <- max(sched$total_T, 1)
  f_upper <- f(upper)
  iter    <- 0
  while (f_upper < 0 && upper < 5000 && iter < 25) {
    upper   <- upper * 2
    f_upper <- f(upper)
    iter    <- iter + 1
  }
  if (f_upper < 0) stop("Target maturity is not reachable within the search range.")
  uniroot(f, c(lower, upper))$root
}

make_pw_accrual_fns <- function(rates, durations) {
  sched <- make_pw_schedule(rates, durations)
  list(
    Tdco  = function(Mat.r, hz0, hz1, hzc0, hzc1, rw) {
      pw_Tdco_fast(Mat.r, hz0, hz1, hzc0, hzc1, rw, sched)
    },
    rct.r = function(t) rct.r_pw_fast(t, sched),
    schedule = sched
  )
}

# ---- Statistical functions ----
pfstp      <- function(mm, hr, zcut, ifia, za, finf) {
  pmvnorm(c(zcut, za), Inf, -sqrt(mm * finf * c(ifia, 1)) * log(hr),
          sigma = diag(1 - sqrt(ifia), 2) + sqrt(ifia))[1]
}
pfstp.hr   <- Vectorize(pfstp, "hr")
pfnstp     <- function(mm, hr, zcut, ifia, za, finf) pnorm(-sqrt(mm * finf) * log(hr) - za)
pfnstp.hr  <- Vectorize(pfnstp, "hr")
m.pow      <- function(mm, hr, zcut, ifia, za, finf) {
  pww <- pfnstp(mm, hr, zcut, ifia, za, finf)
  uniroot(function(x) pfstp(x, hr, zcut, ifia, za, finf) - pww, mm * c(1, 9))$root
}
m.pow.hr   <- Vectorize(m.pow, "hr")
pjfun      <- function(mm, hr, zcut, ifia, za, finf) {
  sigm <- diag(1 - sqrt(ifia), 2) + sqrt(ifia)
  mzs  <- -sqrt(mm * finf * c(ifia, 1)) * log(hr)
  c(
    pmvnorm(-Inf, c(zcut, za), mzs, sigma = sigm)[1],
    pmvnorm(c(-Inf, za), c(zcut, Inf), mzs, sigma = sigm)[1]
  )
}
pjfun.hr   <- Vectorize(pjfun, "hr")

DRC.Template <- function(mos, HR, cen.r0, cen.r1, alp, pow = NULL, ranr, N_DM, N_DM_v,
                         rct.T, rpow, IA.delay, ifia, fMtc, stp.cut, format = 0,
                         nevent = NULL, accrual_type = "power",
                         monthly_rates = NULL, monthly_durations = NULL) {
  ran.r <- c(1, ranr)
  os.m  <- c(1, 1 / HR) * mos
  hzi   <- log(2) / os.m
  lhr   <- log(HR)
  hzc0  <- -log(1 - cen.r0) / 12
  hzc1  <- -log(1 - cen.r1) / 12
  rw    <- ran.r / sum(ran.r)
  finf  <- prod(rw)

  if (accrual_type == "monthly" && !is.null(monthly_rates) && !is.null(monthly_durations)) {
    pw_fns    <- make_pw_accrual_fns(monthly_rates, monthly_durations)
    use_Tdco  <- pw_fns$Tdco
    use_rct_r <- pw_fns$rct.r
    use_pw    <- TRUE
  } else {
    use_Tdco  <- Tdco.hz1
    use_rct_r <- function(t) rct.r(t, rpow, rct.T)
    use_pw    <- FALSE
  }

  z.ab <- qnorm(c(1 - alp, pow))
  za   <- z.ab[1]
  zb   <- z.ab[2]
  zab  <- sum(z.ab)
  lhri <- lhr * c(0, za, zab) / zab
  HRi  <- exp(lhri)

  if (!is.null(pow))    M <- (zab / lhr)^2 / finf
  if (!is.null(nevent)) M <- nevent

  if (N_DM == "Sample Size") {
    N     <- N_DM_v
    mat.r <- M / N
  } else {
    mat.r <- N_DM_v
    N     <- M / mat.r
  }

  ifj <- c(ifia, 1)
  ifi <- diff(c(0, ifj))

  if (fMtc == 1) {
    zcut <- qnorm(stp.cut, za, sqrt((1 - ifia) / ifia)) * sqrt(ifia)
  } else if (fMtc == 2) {
    zcut <- qnorm(stp.cut, za, sqrt(1 - ifia)) * sqrt(ifia)
  } else if (fMtc == 3) {
    zcut <- -log(stp.cut) * sqrt(ifia * M * finf)
  } else if (fMtc == 4) {
    zcut <- stp.cut
  }

  ppcv       <- pnorm(zcut / sqrt(ifia), za, sqrt((1 - ifia) / ifia))
  hr1cv      <- exp(-zcut / sqrt(ifia * M * finf))
  zcut.back  <- qnorm(ppcv, za, sqrt((1 - ifia) / ifia)) * sqrt(ifia)
  cpcv       <- pnorm(zcut.back / sqrt(ifia), za, sqrt(1 - ifia))

  Template        <- list()
  Template$rules  <- matrix(c(ifia, ppcv, cpcv, hr1cv, zcut.back), 5, 1)
  if (format == 1) {
    Template$rules[2, 1] <- paste0(sprintf('%.1f', as.numeric(Template$rules[2, 1]) * 100), "%")
    Template$rules[3, 1] <- paste0(sprintf('%.1f', as.numeric(Template$rules[3, 1]) * 100), "%")
    Template$rules[c(1, 4, 5), 1] <- round(c(ifia, hr1cv, zcut.back), 3)
  }
  rownames(Template$rules) <- c("Information Fraction:", "Predictive Power Boundary:",
                                "Corresponding Conditional Power:",
                                "Corresponding HR Observation:",
                                "Corresponding Z-statistic:")
  colnames(Template$rules) <- "Values"

  Template$err.ind <- c(0, 0)
  Template$err.str <- c("No error", "No error")

  Tdco.IA <- NULL; T.IA <- NULL; N.IA <- NULL
  try({
    Tdco.IA <- if (use_pw) {
      sapply(hzi[1] * HRi, function(hz1i) use_Tdco(mat.r * ifia, hzi[1], hz1i, hzc0, hzc1, rw))
    } else {
      use_Tdco(mat.r * ifia, hzi[1], hzi[1] * HRi, hzc0, hzc1, rpow, rct.T, rw)
    }
  }, silent = TRUE)

  if (is.null(Tdco.IA)) {
    Template$err.ind[1] <- 1
    Template$err.str[1] <- paste0(
      "Given the specified input parameters, data will never reach the desired maturity at IA. ",
      "Possible reasons: too long median OS, too high censoring rate, small sample size. ",
      "Please consider reasonable input values.")
  } else {
    T.IA <- Tdco.IA + IA.delay
    N.IA <- use_rct_r(T.IA) * N
  }

  Tdco.FA <- NULL; T.FA <- NULL; N.FA <- NULL
  try({
    Tdco.FA <- if (use_pw) {
      sapply(hzi[1] * HRi, function(hz1i) use_Tdco(mat.r, hzi[1], hz1i, hzc0, hzc1, rw))
    } else {
      use_Tdco(mat.r, hzi[1], hzi[1] * HRi, hzc0, hzc1, rpow, rct.T, rw)
    }
  }, silent = TRUE)

  if (is.null(Tdco.FA)) {
    Template$err.ind[2] <- 1
    Template$err.str[2] <- paste0(
      "Given the specified input parameters, data will never reach the desired maturity at FA. ",
      "Possible reasons: too long median OS/PFS, too high censoring rate, small sample size. ",
      "Please consider reasonable input values.")
  } else {
    T.FA <- Tdco.FA
    N.FA <- use_rct_r(T.FA) * N
  }

  if (sum(Template$err.ind) == 0) {
    stppr  <- pjfun.hr(M, hr = HRi, zcut, ifia, za, finf)
    Stppr  <- colSums(stppr)
    pw.Fut  <- pfstp.hr(M, HRi, zcut, ifia, za, finf)
    pw.nFut <- pfnstp(M, HRi, zcut, ifia, za, finf)
    Minc    <- m.pow.hr(M, HRi[2:3], zcut, ifia, za, finf)

    Template$OCs <- round(t(matrix(c(
      T.IA, N.IA, T.FA, N.FA,
      Stppr * T.IA + (1 - Stppr) * T.FA,
      Stppr * N.IA + (1 - Stppr) * N.FA,
      pw.nFut, pw.Fut, c(NA, Minc - M), Stppr, stppr[1, ], stppr[2, ]
    ), 3)), 4)

    rnames <- c("Time to Futility IA (months)", "N Enrolled by IA",
                "Time to Final Analysis (months)", "Total N Enrolled",
                "Average Trial Duration (months)", "Average Sample Size",
                "Power without Futility", "Power with Futility",
                "Additional Events to Recoup Power", "Probability of Futility Stop",
                "Probability of Correct Futility Stop", "Probability of Incorrect Futility Stop")

    if (format == 1) {
      rows.d1 <- which(rnames %in% c("Time to Futility IA (months)",
                                     "Time to Final Analysis (months)",
                                     "Average Trial Duration (months)"))
      rows.d0 <- which(rnames %in% c("N Enrolled by IA", "Total N Enrolled",
                                     "Average Sample Size",
                                     "Additional Events to Recoup Power"))
      rows.pc <- which(rnames %in% c("Power without Futility", "Power with Futility",
                                     "Probability of Futility Stop",
                                     "Probability of Correct Futility Stop",
                                     "Probability of Incorrect Futility Stop"))
      Template$OCs             <- as.data.frame(apply(Template$OCs, 2, as.numeric))
      fmt.d1                   <- apply(Template$OCs[rows.d1, ], 2, sprintf, fmt = '%.1f')
      fmt.d0                   <- apply(Template$OCs[rows.d0, ], 2, ceiling)
      fmt.pc                   <- apply(Template$OCs[rows.pc, ] * 100, 2, sprintf, fmt = '%.1f')
      Template$OCs[rows.d1, ]  <- fmt.d1
      Template$OCs[rows.d0, ]  <- fmt.d0
      Template$OCs[rows.pc, ]  <- paste0(fmt.pc, "%")
    }

    rownames(Template$OCs) <- rnames
    colnames(Template$OCs) <- c("Null: HR=1",
                                paste0("Critical Value: HR=", round(HRi[2], 3)),
                                paste0("Alternative: HR=", HRi[3]))
  }

  return(Template)
}

# ---------------------------------------------------------------------------
# Module UI
# ---------------------------------------------------------------------------
mod_futility_ui <- function(id) {
  ns <- NS(id)

  fluidRow(
    column(
      width = 4,

      box(
        title = "Fixed Sample Design", status = "primary",
        solidHeader = TRUE, width = NULL,

        h5("Hypotheses and Assumptions"),
        numericInput(ns("mos"), "Control Arm Median (months):", 12, min = 0, max = 120, step = 3),
        numericInput(ns("HR"),  "Alternative HR:", 0.65, min = 0.1, max = 0.999, step = 0.05),

        checkboxGroupInput(ns("dropind"), "Include Subject Dropout",
                           choices = c("Yes" = 1), selected = ""),
        conditionalPanel(
          condition = sprintf("input['%s'] == 1", ns("dropind")),
          numericInput(ns("droptime"), "Dropout in the First (months):", 12, min = 0, max = 90, step = 1),
          fluidRow(
            column(6, numericInput(ns("drop0"), "% Control Dropout:",      5, min = 0, max = 50, step = 5)),
            column(6, numericInput(ns("drop1"), "% Experimental Dropout:", 5, min = 0, max = 50, step = 5))
          )
        ),

        hr(),
        h5("Statistical Design Parameters"),
        fluidRow(
          column(6, numericInput(ns("ran.r"), "Randomization Ratio:",  1,     min = 0.5, max = 3,   step = 0.05)),
          column(6, numericInput(ns("alp"),   "Alpha (One-sided):",    0.025, min = 0.005, max = 0.10, step = 0.001))
        ),

        radioButtons(ns("dcotype"), "DCO Determination",
                     choices  = c("Power" = "pow", "Total Number of Events" = "dcoevent"),
                     selected = "pow"),
        conditionalPanel(
          condition = sprintf("input['%s'] == 'pow'", ns("dcotype")),
          numericInput(ns("pow"), "Power:", 0.90, min = 0.3, max = 0.999, step = 0.001)
        ),
        conditionalPanel(
          condition = sprintf("input['%s'] == 'dcoevent'", ns("dcotype")),
          numericInput(ns("dcoevent"), "Total Number of Events:", 200, min = 1, max = 1000, step = 1)
        ),

        fluidRow(
          column(6, radioButtons(ns("N_DM"), "Select One:", c("Sample Size", "Data Maturity"))),
          column(6,
            conditionalPanel(
              condition = sprintf("input['%s'] == 'Sample Size'", ns("N_DM")),
              numericInput(ns("N_DM_v_ss"), "Value:", 500, min = 0, max = 10000, step = 100)
            ),
            conditionalPanel(
              condition = sprintf("input['%s'] == 'Data Maturity'", ns("N_DM")),
              numericInput(ns("N_DM_v_dm"), "Value:", 0.65, min = 0, max = 1, step = 0.05)
            )
          )
        ),

        hr(),
        h5("Operational Variables"),
        radioButtons(ns("accrual_type"), "Accrual Type:",
                     choices  = c("Power Function" = "power", "Monthly Rates" = "monthly"),
                     selected = "power", inline = TRUE),
        conditionalPanel(
          condition = sprintf("input['%s'] == 'power'", ns("accrual_type")),
          fluidRow(
            column(6, numericInput(ns("rct.T"), "Accrual Period (months):", 18, min = 6,  max = 120, step = 3)),
            column(6, numericInput(ns("rpow"),  "Accrual Power Parameter:", 1.5, min = 1, max = 5,   step = 0.5))
          )
        ),
        conditionalPanel(
          condition = sprintf("input['%s'] == 'monthly'", ns("accrual_type")),
          textAreaInput(ns("monthly_rates_input"),
                        "Monthly Enrollment Counts (comma-separated, one per month):",
                        value = "7,12,15,18,21,23,25,27,29,30,32,33,35,36,37,39,40,41",
                        rows = 3, width = "100%")
        ),

        numericInput(ns("IA.delay"), "Time from DCO to futility decision (months):", 2, min = 0, max = 6, step = 1)
      ),

      box(
        title = "IA Futility Rules", status = "warning",
        solidHeader = TRUE, width = NULL,

        numericInput(ns("ifia"), "Information Fraction:", 0.35, min = 0.1, max = 0.999, step = 0.05),

        h5("Stopping Metric and Boundary"),
        radioButtons(ns("fMtc"), "Choose a Metric:",
                     choices = list("Predictive Probability"  = 1,
                                    "Conditional Probability" = 2,
                                    "Hazard Ratio"            = 3,
                                    "IA Z-statistic"          = 4),
                     selected = 1),
        conditionalPanel(
          condition = sprintf("input['%s'] == 1", ns("fMtc")),
          numericInput(ns("stp.cut_pp"), "Stopping Boundary:", 0.3,  min = 0.01, max = 0.90, step = 0.05)
        ),
        conditionalPanel(
          condition = sprintf("input['%s'] == 2", ns("fMtc")),
          numericInput(ns("stp.cut_cp"), "Stopping Boundary:", 0.2,  min = 0.01, max = 0.90, step = 0.05)
        ),
        conditionalPanel(
          condition = sprintf("input['%s'] == 3", ns("fMtc")),
          numericInput(ns("stp.cut_hr"), "Stopping Boundary:", 1.0,  min = 0.5,  max = 1.5,  step = 0.05)
        ),
        conditionalPanel(
          condition = sprintf("input['%s'] == 4", ns("fMtc")),
          numericInput(ns("stp.cut_z"),  "Stopping Boundary:", 0.7,  min = 0,    max = 1.65, step = 0.05)
        )
      )
    ),

    column(
      width = 8,

      box(
        title = "Input Summary", status = "info",
        solidHeader = TRUE, width = NULL,
        uiOutput(ns("text"))
      ),

      box(
        title = "Rules", status = "success",
        solidHeader = TRUE, width = NULL,
        tableOutput(ns("rules"))
      ),

      box(
        title = "DRC Template", status = "primary",
        solidHeader = TRUE, width = NULL,
        div(style = "overflow-x: scroll", tableOutput(ns("table"))),
        downloadButton(ns("download_table"), "Download table")
      )
    )
  )
}

# ---------------------------------------------------------------------------
# Module Server
# ---------------------------------------------------------------------------
mod_futility_server <- function(id) {
  moduleServer(id, function(input, output, session) {

    # Sync N input when monthly accrual mode is active
    observe({
      req(input$accrual_type == "monthly", input$monthly_rates_input)
      mr <- tryCatch(
        as.numeric(trimws(strsplit(input$monthly_rates_input, ",")[[1]])),
        error = function(e) NULL
      )
      if (!is.null(mr) && length(mr) >= 1 && all(!is.na(mr)) && all(mr >= 0) && sum(mr) > 0) {
        updateNumericInput(session, "N_DM_v_ss", value = sum(mr))
        updateRadioButtons(session, "N_DM", selected = "Sample Size")
      }
    })

    input.update <- reactive({
      validate(
        need(input$HR >= 0.2 && input$HR < 1,
             "Warning: Please set a hazard ratio within [0.2, 1)."),
        need(input$alp > 0 && input$alp <= 0.1,
             "Warning: Alpha should be within (0, 0.1]."),
        need(input$pow >= 0.3 && input$pow <= 0.999,
             "Warning: Power should be within [0.3, 0.999]."),
        need(input$rct.T >= 6 && input$rct.T <= 120 || isTRUE(input$accrual_type == "monthly"),
             "Warning: Please set an accrual time within [6, 120]."),
        need(input$rpow >= 1 && input$rpow <= 5 || isTRUE(input$accrual_type == "monthly"),
             "Warning: Please set an accrual power parameter within [1, 5]."),
        need(input$IA.delay >= 0 && input$IA.delay <= 6,
             "Warning: Please set an IA delay time within [0, 6]."),
        need(input$ifia >= 0.1 && input$ifia < 1,
             "Warning: Please set an information fraction within [0.1, 1)."),
        need(input$stp.cut_pp >= 0.01 && input$stp.cut_pp <= 0.9,
             "Warning: Predictive power boundary within [0.01, 0.90]."),
        need(input$stp.cut_cp >= 0.01 && input$stp.cut_cp <= 0.9,
             "Warning: Conditional power boundary within [0.01, 0.90]."),
        need(input$stp.cut_hr >= 0.5 && input$stp.cut_hr <= 1.5,
             "Warning: HR stopping boundary within [0.5, 1.5]."),
        need(input$stp.cut_z >= 0 && input$stp.cut_z <= 1.65,
             "Warning: Z-statistic boundary within [0, 1.65].")
      )
      if (isTRUE(input$accrual_type == "monthly")) {
        rates_check <- tryCatch(
          as.numeric(trimws(strsplit(input$monthly_rates_input, ",")[[1]])),
          error = function(e) NULL
        )
        validate(
          need(!is.null(rates_check) && length(rates_check) >= 1 &&
                 all(!is.na(rates_check)) && all(rates_check >= 0) && sum(rates_check) > 0,
               "Warning: Please enter valid non-negative monthly enrollment counts with total > 0.")
        )
      }

      input.par <- list()
      input.par$mos <- input$mos
      input.par$HR  <- input$HR

      input.par$dropind <- 0
      if (is.null(input$dropind)) {
        input.par$droptime <- 1000
        input.par$drop0    <- 0.0001
        input.par$drop1    <- 0.0001
      } else {
        validate(
          need(input$drop0 >= 0 && input$drop0 <= 50,
               "Warning: Please set dropout percentage within [0, 50] for control."),
          need(input$drop1 >= 0 && input$drop1 <= 50,
               "Warning: Please set dropout percentage within [0, 50] for treatment.")
        )
        input.par$dropind  <- 1
        input.par$droptime <- input$droptime
        input.par$drop0    <- input$drop0 / 100
        input.par$drop1    <- input$drop1 / 100
      }

      input.par$alp   <- input$alp
      input.par$pow   <- input$pow
      input.par$ran.r <- input$ran.r
      input.par$N_DM  <- input$N_DM
      input.par$N_DM_v <- input$N_DM_v_dm

      finf <- prod(c(1, input.par$ran.r) / sum(c(1, input.par$ran.r)))

      if (input$dcotype == "pow") {
        input.par$pow <- input$pow
        M <- (sum(qnorm(c(1 - input.par$alp, input.par$pow))) / log(input.par$HR))^2 / finf
      }
      if (input$dcotype == "dcoevent") {
        M <- input$dcoevent
        input.par$pow <- pnorm(-sqrt(M * finf) * log(input.par$HR) - qnorm(1 - input.par$alp))
      }

      z.ab <- qnorm(c(1 - input$alp, input$pow))
      za   <- z.ab[1]; zb <- z.ab[2]; zab <- sum(z.ab)
      input.par$HRi <- exp(log(input$HR) * c(0, za, zab) / zab)

      if (input.par$N_DM == "Sample Size") {
        input.par$N_DM_v <- input$N_DM_v_ss
        input.par$ss     <- input$N_DM_v_ss
      } else if (input.par$N_DM == "Data Maturity") {
        input.par$ss <- M / input$N_DM_v_dm
      }

      input.par$rct.T <- input$rct.T
      input.par$rpow  <- input$rpow

      input.par$accrual_type <- if (isTRUE(input$accrual_type == "monthly")) "monthly" else "power"
      if (input.par$accrual_type == "monthly") {
        mr <- as.numeric(trimws(strsplit(input$monthly_rates_input, ",")[[1]]))
        input.par$monthly_rates     <- mr
        input.par$monthly_durations <- rep(1, length(mr))
        input.par$rct.T             <- sum(input.par$monthly_durations)
        input.par$rpow              <- NA
      } else {
        input.par$monthly_rates     <- NULL
        input.par$monthly_durations <- NULL
      }

      input.par$IA.delay <- input$IA.delay
      input.par$ifia     <- input$ifia
      input.par$fMtc     <- input$fMtc

      if (input.par$fMtc == 1) {
        input.par$stp.cut <- input$stp.cut_pp
        zcut <- qnorm(input.par$stp.cut, za, sqrt((1 - input.par$ifia) / input.par$ifia)) * sqrt(input.par$ifia)
      } else if (input.par$fMtc == 2) {
        input.par$stp.cut <- input$stp.cut_cp
        zcut <- qnorm(input.par$stp.cut, za, sqrt(1 - input.par$ifia)) * sqrt(input.par$ifia)
      } else if (input.par$fMtc == 3) {
        input.par$stp.cut <- input$stp.cut_hr
        zcut <- -log(input.par$stp.cut) * sqrt(input.par$ifia * M * finf)
      } else if (input.par$fMtc == 4) {
        input.par$stp.cut <- input$stp.cut_z
        zcut <- input.par$stp.cut
      }

      ppcv <- pnorm(zcut / sqrt(input.par$ifia), za, sqrt((1 - input.par$ifia) / input.par$ifia))

      input.par$M        <- M
      input.par$ppcv     <- ppcv
      input.par$hr1cv    <- exp(-zcut / sqrt(input.par$ifia * M * finf))
      input.par$zcut.back <- qnorm(ppcv, za, sqrt((1 - input.par$ifia) / input.par$ifia)) * sqrt(input.par$ifia)
      input.par$cpcv     <- pnorm(input.par$zcut.back / sqrt(input.par$ifia), za, sqrt(1 - input.par$ifia))

      return(input.par)
    })

    tableDRC <- reactive({
      input.par <- input.update()
      DRC.Template(
        mos              = input.par$mos,
        HR               = input.par$HR,
        cen.r0           = input.par$drop0,
        cen.r1           = input.par$drop1,
        alp              = input.par$alp,
        pow              = input.par$pow,
        ranr             = input.par$ran.r,
        N_DM             = input.par$N_DM,
        N_DM_v           = input.par$N_DM_v,
        rct.T            = input.par$rct.T,
        rpow             = input.par$rpow,
        IA.delay         = input.par$IA.delay,
        ifia             = input.par$ifia,
        fMtc             = input.par$fMtc,
        stp.cut          = input.par$stp.cut,
        format           = 1,
        accrual_type     = input.par$accrual_type,
        monthly_rates    = input.par$monthly_rates,
        monthly_durations = input.par$monthly_durations
      )
    })

    output$text <- renderUI({
      input.par <- input.update()
      ppcv      <- input.par$ppcv

      str1 <- paste0(
        "The study is designed to test the hypothesized HR=", round(input.par$HRi[3], 3),
        " with a significance level of alpha(1-side)=", input.par$alp,
        " and a power of ", round(input.par$pow * 100, 1), "%. <br/>"
      )

      hazrate.c <- round(log(2) / input.par$mos, 4)
      hazrate.e <- round(log(2) / input.par$mos * input.par$HR, 4)
      mos.e     <- round(log(2) / (log(2) / input.par$mos * input.par$HR), 1)

      if (input.par$accrual_type == "monthly") {
        accr_detail <- paste0(
          "Enrollment follows a user-specified monthly rate schedule over ",
          length(input.par$monthly_rates), " months ",
          "(rates: ", paste(round(input.par$monthly_rates, 1), collapse = ","), "). "
        )
        accr.str <- paste0(input.par$rct.T, " months, ")
      } else {
        accr_detail <- paste0(
          "Cumulative accruals will follow a power function ",
          "F(t)=(t/AccrualPeriod)^k where k=", input.par$rpow, ". "
        )
        accr.str <- paste0(input.par$rct.T, " months, ")
      }

      drop.str <- if (input.par$dropind == 1) {
        paste0("The non-informative exponential dropout is assumed to occur with ",
               input.par$drop0 * 100, "% in control group and ",
               input.par$drop1 * 100, "% in experimental group by ",
               input.par$droptime, " months. ")
      } else ""

      str2 <- paste0(
        "<br/> Over a period of ", accr.str, ceiling(input.par$ss),
        " patients will be recruited in a ", input.par$ran.r,
        ":1 ratio of experimental to control groups. ", accr_detail,
        "The expected control median =", input.par$mos,
        " months, corresponding to a hazard rate &lambda;=", hazrate.c,
        ". The median of experimental group =", mos.e,
        " months with &lambda;=", hazrate.e,
        " based on the hypothesized HR=", round(input.par$HRi[3], 3),
        ". ", drop.str,
        "Based on the above design, the estimated HR critical value =", round(input.par$HRi[2], 3),
        " and the estimated number of events =", ceiling(input.par$M), ". <br/>"
      )

      str3 <- paste0(
        "<br/> The futility analysis will be executed at ", input.par$ifia * 100,
        "% information time and to stop the study if a predictive probability <",
        round(ppcv, 3) * 100, "%. Otherwise, the study will continue up to the final analysis. <br/>"
      )

      HTML(paste0(str1, str2, str3))
    })

    output$rules <- renderTable({
      tableDRC()$rules
    }, rownames = TRUE, colnames = FALSE)

    output$table <- renderTable({
      tbl       <- tableDRC()
      err.check <- tbl$err.ind
      if (sum(err.check) == 0) {
        tbl$OCs
      } else {
        matrix(tbl$err.str[which(err.check == 1)], ncol = 1,
               dimnames = list(NULL, "Error"))
      }
    }, rownames = TRUE)

    output$download_table <- downloadHandler(
      filename = function() paste0("DRC_output_", Sys.Date(), ".csv"),
      content  = function(file) {
        input.par <- input.update()
        out       <- tableDRC()$OCs
        colnames(out) <- gsub(" ", "",   colnames(out))
        colnames(out) <- gsub(":|=", "_", colnames(out))

        input.info <- c("Control Arm Median", "Alternative HR",
                        "Censoring Rate per Year (control)", "Censoring Rate per Year (treatment)",
                        "Alpha (One-sided)", "Power", "Randomization Ratio",
                        "Sample Size", "Data Maturity",
                        "Accrual Period (months)", "Accrual Power Parameter",
                        "Time from DCO to IA (months)",
                        "Information Fraction", "Predictive Probability",
                        "Conditional Probability", "Hazard Ratio", "IA Z-statistics")
        input.table      <- matrix(NA, nrow = length(input.info), ncol = 2)
        colnames(input.table) <- c("Input", "Value")
        input.table[, 1] <- input.info
        input.table[, 2] <- c(
          input.par$mos, input.par$HR, input.par$drop0, input.par$drop1,
          input.par$alp, input.par$pow, input.par$ran.r,
          input.par$ss, round(input.par$M / input.par$ss, 3),
          input.par$rct.T, input.par$rpow, input.par$IA.delay, input.par$ifia,
          paste0(sprintf('%.1f', as.numeric(input.par$ppcv) * 100), "%"),
          paste0(sprintf('%.1f', as.numeric(input.par$cpcv) * 100), "%"),
          round(input.par$hr1cv, 3), round(input.par$zcut.back, 3)
        )

        input.table <- rbind(colnames(input.table), input.table)
        out.table   <- cbind(Output = rownames(out), data.frame(out, row.names = NULL))
        out.table   <- rbind(colnames(out.table), out.table)

        dl.table <- matrix("", nrow = nrow(out.table) + nrow(input.table) + 1,
                           ncol = max(ncol(out.table), ncol(input.table)))
        dl.table <- data.frame(dl.table)
        dl.table[1:nrow(out.table), 1:ncol(out.table)] <- out.table
        dl.table[(nrow(out.table) + 2):nrow(dl.table), 1:ncol(input.table)] <- input.table
        colnames(dl.table) <- c("Output/Input", rep("", ncol(dl.table) - 1))

        write.csv(dl.table, file, row.names = FALSE, quote = FALSE)
      }
    )

  })
}
