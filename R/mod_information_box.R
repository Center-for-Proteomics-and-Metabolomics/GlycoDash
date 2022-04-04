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
  tagList(
    plotOutput(ns("plot")),
    br(),
    DT::dataTableOutput(ns("table"))
  )
}
    
#' information_box Server Function
#'
#' @noRd 
mod_information_box_server <- function(id, info, cluster){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    info_plot <- reactive({
      req(info$curated_analytes())
      req(info$cut_off())
      
      plot_analyte_curation(curated_analytes = info$curated_analytes(),
                            cut_off_percentage = info$cut_off(),
                            selected_cluster = cluster)
    })
    
    output$plot <- renderPlot({
      info_plot()
    })
    
    info_table <- reactive({
      req(info$analyte_curated_data())
      
      prepare_analyte_curation_table(analyte_curated_data = info$analyte_curated_data(),
                                     selected_cluster = cluster)
    })
    
    output$table <- DT::renderDT({
      create_analyte_curation_table(dataframe_for_table = info_table())
    })
    
    return(list(plot = info_plot,
                table = info_table))
    
  })
}
    
## To be copied in the UI
# mod_information_box_ui("information_box_ui_1")
    
## To be copied in the server
# mod_information_box_server("information_box_ui_1")
