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
      header = shinydashboard::dashboardHeader(
        title = "Glyco data processing dashboard"
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
          shinydashboard::menuItem("Derived Traits", 
                                   tabName = "derived_traits"),
          shinydashboard::menuItem("Repeatability", 
                                   tabName = "repeatability"),
          shinydashboard::menuItem("Data Exploration", 
                                   tabName = "data_exploration")
          )
        ),
      
      body = shinydashboard::dashboardBody(
        shinyalert::useShinyalert(),
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
          )
        )
      ),
      title = "Glyco data processing dashboard"
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
      app_title = 'glycodash'
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert() 
  )
}

