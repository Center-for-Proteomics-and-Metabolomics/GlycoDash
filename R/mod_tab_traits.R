#' mod_tab_traits UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_tab_traits_ui <- function(id) {
  
  ns <- NS(id)
  
  tagList(
    br(),
    # Human IgG traits
    shinyWidgets::awesomeCheckboxGroup(
      ns("human_IgG_traits"),
      "Select the traits you want to calculate for human IgG:",
      choices = c(
        "Fucosylation of complex-type glycans",
        "Bisection of complex-type glycans",
        "Galactosylation of complex-type glycans",
        "Sialylation of complex type-glycans",
        "Monoantennarity of complex-type glycans",
        "Percentage of hybrid-type glycans",
        "Percentage of oligomannose-type glycans"
      )
    ),
    selectizeInput(
      ns("human_IgG_clusters"),
      "For which clusters in your data should the human IgG traits be calculated?",
      choices = c(""),
      multiple = TRUE
    ),
    # Mouse IgG traits
    shinyWidgets::awesomeCheckboxGroup(
      ns("mouse_IgG_traits"),
      "Select the traits you want to calculate for mouse IgG:",
      choices = c(
        "Mouse Trait 1",
        "Mouse Trait 2",
        "Mouse Trait 3"
      )
    ),
    selectizeInput(
      ns("mouse_IgG_clusters"),
      "For which clusters in your data should the mouse IgG traits be calculated?",
      choices = c(""),
      multiple = TRUE
    )
  )
}



#' tab_curated_analytes Server Function
#'
#' @noRd 
mod_tab_traits_server <- function(id, antibody_type) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    observe({
      if (antibody_type == "Human IgG") {
        shinyjs::show("human_IgG_traits")
        shinyjs::show("human_IgG_clusters")
      } else {
        shinyjs::hide("human_IgG_traits")
        shinyjs::hide("human_IgG_clusters")
      }
      
      if (antibody_type == "Mouse IgG") {
        shinyjs::show("mouse_IgG_traits")
        shinyjs::show("mouse_IgG_clusters")
      } else {
        shinyjs::hide("mouse_IgG_traits")
        shinyjs::hide("mouse_IgG_clusters")
      }
    })
  })
}