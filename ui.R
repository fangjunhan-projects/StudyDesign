ui <- dashboardPage(
  skin = "blue",

  dashboardHeader(title = "BiostatVD"),

  dashboardSidebar(
    sidebarMenu(
      id = "sidebar_menu",
      menuItem("Home", tabName = "home", icon = icon("home")),
      menuItem("M1: Study Design Calculation", icon = icon("calculator"),
        menuSubItem("Sample Size", tabName = "sample_size"),
        menuSubItem("Futility IA", tabName = "futility")
      )
    )
  ),

  dashboardBody(
    tags$head(
      tags$style(HTML("
        .content-wrapper { background-color: #f4f6f9; }
        .box-header { font-weight: 600; }
        .pass-flag { color: #28a745; font-weight: bold; }
        .fail-flag { color: #dc3545; font-weight: bold; }
        .info-value { font-size: 1.3em; font-weight: 600; }
      "))
    ),

    tabItems(
      tabItem(
        tabName = "home",
        fluidRow(
          box(
            title = "BiostatVD -- Biostatistics Validation Dashboard",
            status = "primary", solidHeader = TRUE, width = 12,
            h4("Automated validation of efficacy results"),
            p("Select a module from the sidebar to begin:"),
            tags$ul(
              tags$li(strong("Sample Size"), " -- Sample size calculation for survival endpoints (PH via rpact, NPH via gsDesign2)"),
              tags$li(strong("Futility IA"), " -- Futility interim analysis design (predictive probability, conditional power, HR, Z-statistic)")
            )
          )
        )
      ),

      tabItem(tabName = "sample_size", mod_sample_size_ui("sample_size")),
      tabItem(tabName = "futility",    mod_futility_ui("futility"))
    )
  )
)
