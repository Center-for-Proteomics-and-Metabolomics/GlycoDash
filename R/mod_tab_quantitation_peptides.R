#' tab_quantitation_peptides UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_tab_quantitation_peptides_ui <- function(id) {
  ns <- NS(id)
  tagList(
    
  )
}



#' tab_quantitation_peptides Server Functions
#'
#' @noRd 
mod_tab_quantitation_peptides_server <- function(id, peptides_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    observe({
      req(peptides_data)
      browser()
    })
    
  })
}

