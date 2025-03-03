#' The application User-Interface
#' 
#' @param request Internal parameter for `{shiny}`. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    
    # Use packages for feedback and popovers
    shinyFeedback::useShinyFeedback(),
    bsplus::use_bs_popover(),
    
    # Your application UI logic 
    shinydashboardPlus::dashboardPage(
      
      # Title header, with button that links to GitHub
      header = shinydashboard::dashboardHeader(
        title = "GlycoDash v1.6.6",
        tags$li(a(
          onclick = "onclick =window.open('https://github.com/Center-for-Proteomics-and-Metabolomics/GlycoDash')",
          href = NULL, icon("github"), title = "GitHub", style = "cursor: pointer;"
        ), class = "dropdown"),
        tags$li(a(
          onclick = "onclick =window.open('https://github.com/Center-for-Proteomics-and-Metabolomics/GlycoDash/issues')",
          href = NULL, icon("bug"), title = "Known issues", style = "cursor: pointer;"
        ), class = "dropdown"),
        tags$li(ManualButton("download_manual"), title = "Download manual", class = "dropdown"),
        tags$li(ChangelogButton("download_changelog"), title = "Download changelog", class = "dropdown")
      ),
      
      sidebar = shinydashboardPlus::dashboardSidebar(
        minified = TRUE,  # To keep icons visible when collapsing sidebar
        shinydashboard::sidebarMenu(
          id = "tabs",
          # HTML with &nbsp is to add some space between icon and text
          div(class = "sidebar-header", style = "font-weight: bold; padding: 10px; margin-left: 10px; text-decoration: underline;", "Required steps"),
          shinydashboard::menuItem(HTML("&nbspData import"), 
                                   tabName = "data_import",
                                   icon = icon("upload")),
          shinydashboard::menuItem(HTML("&nbspSpectra curation"), 
                                   tabName = "spectra_curation",
                                   icon = icon("area-chart")),
          shinydashboard::menuItem(HTML("&nbspAnalyte curation"), 
                                   tabName = "analyte_curation",
                                   icon = icon("bar-chart")),
          shinydashboard::menuItem(HTML("&nbspNormalized data"), 
                                   tabName = "normalization",
                                   icon = icon("table")),
          div(class = "sidebar-header", style = "font-weight: bold; padding: 10px; margin-left: 10px; text-decoration: underline;", "Optional steps"),
          shinydashboard::menuItem(HTML("&nbspIgG1 quantitation"),
                                   tabName = "quantitation",
                                   icon = icon("balance-scale")),
          shinydashboard::menuItem(HTML("&nbsp&nbspGlycosylation traits"), 
                                   tabName = "derived_traits",
                                   icon = icon("flask")),
          shinydashboard::menuItem(HTML("&nbspRepeatability"), 
                                   tabName = "repeatability",
                                   icon = icon("eye")),
          shinydashboard::menuItem(HTML("&nbspData exploration"), 
                                   tabName = "data_exploration",
                                   icon = icon("magnifying-glass")),
          div(class = "sidebar-header", style = "font-weight: bold; padding: 10px; margin-left: 10px; text-decoration: underline;", "Data export"),
          shinydashboard::menuItem(HTML("&nbspExport results"),
                                   tabName = "export",
                                   icon = icon("download"))
          )
        ),
      
      body = shinydashboard::dashboardBody(
        # Below causes the title to remain visible entirely when collapsing
        # the sidebar. Only tab names are collapsed, icons remain visible.
        # Taken from: https://stackoverflow.com/questions/69591309/shinydashboard-vs-shinydashboardplus-dashboardsidebar-title-differences
        tags$style(
            '
          @media (min-width: 768px){
            .sidebar-mini.sidebar-collapse .main-header .logo {
                width: 230px; 
            }
            .sidebar-mini.sidebar-collapse .main-header .navbar {
                margin-left: 230px;
            }
          }
          '
        ),
        shinyjs::useShinyjs(),
        shinydashboard::tabItems(
          shinydashboard::tabItem(
            "data_import",
            mod_data_import_ui("data_import_ui_1")
          ),
          shinydashboard::tabItem(
            "spectra_curation",
            mod_spectra_curation_ui("spectra_curation_ui_1")
          ),
          shinydashboard::tabItem(
            "analyte_curation",
            mod_analyte_curation_ui("analyte_curation_ui_1")
          ),
          shinydashboard::tabItem(
            "normalization",
            mod_normalization_ui("normalization_ui_1")
          ),
          shinydashboard::tabItem(
            "quantitation",
            mod_quantitation_ui("quantitation_ui_1")
          ),
          shinydashboard::tabItem(
            "derived_traits",
            mod_derived_traits_ui("derived_traits_ui_1")
          ),
          shinydashboard::tabItem(
            "repeatability",
            mod_repeatability_ui("repeatability_ui_1")
          ),
          shinydashboard::tabItem(
            "data_exploration",
            mod_data_exploration_ui("data_exploration_ui_1")
          ),
          shinydashboard::tabItem(
            "export",
            mod_export_ui("export_ui_1")
          )
        ),
      ),
      title = "GlycoDash"
    ),
    # Add the CSS for hiding the headers when the sidebar is collapsed.
    # Written by GPT-4
    tags$head(
      tags$style(HTML("
        /* Hide the sidebar headers when the sidebar is collapsed */
        .sidebar-collapse .sidebar-header {
          display: none;  /* Hide the headers when the sidebar is collapsed */
        }
      "))
    )
  )
}

#' Add external Resources to the Application
#' 
#' This function is internally used to add external 
#' resources inside the Shiny application. 
#' 
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function(){
  
  add_resource_path(
    'www', app_sys('app/www')
  )
 
  tags$head(
    favicon(
      ico = "glycodash_logo",
      ext = "png"
    ),
    bundle_resources(
      path = app_sys('app/www'),
      app_title = 'GlycoDash'
    )
  )
}

