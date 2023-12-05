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
    
    # Your application UI logic 
    shinydashboard::dashboardPage(
      
      # Title header, with button that links to GitHub
      header = shinydashboard::dashboardHeader(
        title = "GlycoDash v1.3.2",
        tags$li(a(
          onclick = "onclick =window.open('https://github.com/Center-for-Proteomics-and-Metabolomics/GlycoDash')",
          href = NULL, icon("github"), title = "GitHub", style = "cursor: pointer;"
        ), class = "dropdown"),
        tags$li(customDownloadbutton("download_md"), class = "dropdown")
      ),
      
      sidebar = shinydashboard::dashboardSidebar(
        shinydashboard::sidebarMenu(
          id = "tabs",
          
          shinydashboard::menuItem("Data Import", 
                                   tabName = "data_import"),
          shinydashboard::menuItem("Spectra Curation", 
                                   tabName = "spectra_curation"),
          shinydashboard::menuItem("Analyte Curation", 
                                   tabName = "analyte_curation"),
          shinydashboard::menuItem("Normalization", 
                                   tabName = "normalization"),
          shinydashboard::menuItem("IgG1 quantitation (optional)",
                                   tabName = "quantitation"),
          shinydashboard::menuItem("Glycosylation Traits (optional)", 
                                   tabName = "derived_traits"),
          shinydashboard::menuItem("Repeatability (optional)", 
                                   tabName = "repeatability"),
          shinydashboard::menuItem("Data Exploration (optional)", 
                                   tabName = "data_exploration"),
          shinydashboard::menuItem("Export results",
                                   tabName = "export")
          )
        ),
      
      body = shinydashboard::dashboardBody(
        shinyjs::useShinyjs(),
        
        #dashboardthemes::shinyDashboardThemes(theme = "poor_mans_flatly"),
        
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
        )
      ),
      title = "GlycoDash"
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
    favicon(),
    bundle_resources(
      path = app_sys('app/www'),
      app_title = 'GlycoDash'
    )
  )
}

