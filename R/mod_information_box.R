#' information_box UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_information_box_ui <- function(id){
  ns <- NS(id)
  uiOutput(ns("infobox"))
}
    
#' information_box Server Function
#'
#' @noRd 
mod_information_box_server <- function(id, info, clusters){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    output$infobox <- renderUI({
      do.call(
        shinydashboard::tabBox,
        append(
          list(
            title = "Information on analyte curation per cluster",
            width = 8,
            side = "right"
          ),
          values = purrr::map(clusters(),
                              function(cluster) {
                                tabPanel(
                                  title = cluster,
                                  plotOutput(ns(paste0("info_plot_", cluster))),
                                  br(),
                                  DT::dataTableOutput(ns(paste0("info_table_", cluster)))
                                )
                              })
        )
      )
    })
    
    observe({
      req(info$curated_analytes())
      req(info$cut_off())
      
      purrr::map(clusters(),
                 function(cluster) {
                   plotname <- paste0("info_plot_", cluster)
                   
                   output[[plotname]] <- renderPlot({
                     
                     
                     plot_analyte_curation(curated_analytes = info$curated_analytes(),
                                           cut_off_percentage = info$cut_off(),
                                           selected_cluster = cluster)
                   })
                 })
    })
    
    observe({
      req(info$analyte_curated_data())
      
      purrr::map(clusters(),
                 function(cluster) {
                   tablename <- paste0("info_table_", cluster)
                   
                   output[[tablename]] <- DT::renderDT({
                     
                     create_analyte_curation_table(analyte_curated_data = info$analyte_curated_data(),
                                                   selected_cluster = cluster)
                   })
                 })
    })
 
  })
}
    
## To be copied in the UI
# mod_information_box_ui("information_box_ui_1")
    
## To be copied in the server
# mod_information_box_server("information_box_ui_1")
