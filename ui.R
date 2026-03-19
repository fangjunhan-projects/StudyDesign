ui <- dashboardPage(
  skin = "blue",

  dashboardHeader(title = "BiostatVD"),

  dashboardSidebar(
    sidebarMenu(
      id = "sidebar_menu",
      menuItem("Home", tabName = "home", icon = icon("home")),
      menuItem("M1: IA Boundary", tabName = "ia_boundary", icon = icon("chart-line")),
      menuItem(
        "M2: PFS Efficacy", icon = icon("heartbeat"),
        menuSubItem("Raw Data Path", tabName = "pfs_raw"),
        menuSubItem("SDTM Path", tabName = "pfs_sdtm"),
        menuSubItem("ADaM Comparison", tabName = "pfs_adam_compare")
      ),
      menuItem("M3: Additional", tabName = "additional", icon = icon("plus-circle")),
      menuItem("M4: Event Projection", tabName = "event_projection", icon = icon("chart-bar"))
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
              tags$li(strong("M1: IA Boundary"), " -- Re-calculate interim analysis boundaries using rpact"),
              tags$li(strong("M2: PFS Efficacy"), " -- Validate PFS results via Raw Data, SDTM, or ADaM comparison"),
              tags$li(strong("M3: Additional"), " -- Coming soon"),
              tags$li(strong("M4: Event Projection"), " -- Placeholder for PFS / OS event projection (e.g., using RAVE)")
            )
          )
        )
      ),

      tabItem(tabName = "ia_boundary", mod_ia_boundary_ui("ia_boundary")),
      tabItem(tabName = "pfs_raw", mod_pfs_raw_ui("pfs_raw")),
      tabItem(tabName = "pfs_sdtm", mod_pfs_sdtm_ui("pfs_sdtm")),
      tabItem(tabName = "pfs_adam_compare", mod_pfs_adam_compare_ui("pfs_adam_compare")),

      tabItem(
        tabName = "additional",
        fluidRow(
          box(
            title = "Module 3: Additional Results", status = "info",
            solidHeader = TRUE, width = 12,
            h4("Coming Soon"),
            p("This module will support additional efficacy checks (e.g., ORR, OS, subgroup analyses).")
          )
        )
      ),

      tabItem(
        tabName = "event_projection",
        mod_event_projection_ui("event_projection")
      )
    )
  )
)
