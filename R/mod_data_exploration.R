#' data_exploration UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_data_exploration_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidPage(
      fluidRow(
        h1("Data exploration"),
        actionButton(ns("button"),
                     "Create a new plot.")
      ),
      fluidRow(
        uiOutput(ns("boxes"))
      )
    )
    
  )
}

#' data_exploration Server Functions
#'
#' @noRd 
mod_data_exploration_server <- function(id, results_derived_traits){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    my_data <- reactive({
      req(results_derived_traits$data_with_derived_traits())
      results_derived_traits$data_with_derived_traits()
    })
    
    r <- reactiveValues(all_boxes = list(),
                        all_plots = list())
    
    observe({
      req(input$button > 0)
      # Run the server part of the module that creates a box with a plot:
      mod_box_with_plot_server(input$button,
                               my_data = my_data)
      
      # Run the UI part of the module that creates a box with a plot and save it
      # in the reactiveValue all_boxes:
      r$all_boxes[[input$button]] <- mod_box_with_plot_ui(ns(input$button))
    }) %>% bindEvent(input$button)
    
    observe({
      req (input$button > 0)
      for (box in 1:input$button) {
        
      }
    })
    
    output$boxes <- renderUI(
      tagList(r$all_boxes)
    )
    
  })
}

## To be copied in the UI
# mod_data_exploration_ui("data_exploration_ui_1")

## To be copied in the server
# mod_data_exploration_server("data_exploration_ui_1")
