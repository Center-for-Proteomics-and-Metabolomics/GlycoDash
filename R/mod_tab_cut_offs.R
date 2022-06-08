#' tab_cut_offs UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_tab_cut_offs_ui <- function(id){
  ns <- NS(id)
  tagList(
    plotOutput(ns("plot")),
    "Based on the selected samples the cut-off values are:",
    textOutput(ns("sum_int_cut_off")),
    textOutput(ns("prop_cut_off"))
  )
}
    
#' tab_cut_offs Server Functions
#'
#' @noRd 
mod_tab_cut_offs_server <- function(id, cluster, checked_spectra, cut_off_basis){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    spectra_check_filtered <- reactive({
      req(checked_spectra())
      
      print("check1")
      
      checked_spectra() %>% 
        dplyr::filter(cluster == cluster)
    })
    
    observe({
      req(spectra_check_filtered())
      print(spectra_check_filtered())
    })
    
    plot <- reactive({
      req(spectra_check_filtered())
      print("check2")
      create_cut_off_plot2(spectra_check_filtered())
    })
    
    output$plot <- renderPlot({
      req(plot())
      print(plot())
      plot()
    })
    
    cut_off_values <- reactive({
      req(checked_spectra(),
          cut_off_basis())
      
      cut_offs <- filter_cut_off_basis(cut_off_basis(),
                                       checked_spectra())
      return(list(
        sum_int = unique(cut_offs$cut_off_sum_int),
        prop = unique(cut_offs$cut_off_prop)
      ))
    })
    
    output$sum_int_cut_off <- renderText({
      paste("Sum intensity:",
            ifelse(is_truthy(cut_off_values()), cut_off_values()$sum_int, ""))
    })
    
    output$prop_cut_off <- renderText({
      paste("Percentage of passing analytes:",
            ifelse(is_truthy(cut_off_values()), cut_off_values()$prop, ""))
    })
    
  })
}
    
## To be copied in the UI
# mod_tab_cut_offs_ui("tab_cut_offs_ui_1")
    
## To be copied in the server
# mod_tab_cut_offs_server("tab_cut_offs_ui_1")
