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
        h1("Data exploration")
      ),
      fluidRow(
        shinydashboard::box(
          title = "Boxplot",
          width = 12,
          solidHeader = TRUE,
          status = "primary",
          shinyWidgets::dropdownButton(
            icon = icon("gear"),
            status = "primary",
            size = "sm",
            selectizeInput(ns("yvar"),
                           choices = "",
                           selected = NULL,
                           label = "Choose what variable should be on the y-axis:",
                           options = list(placeholder = "select a continuous variable")),
            selectizeInput(ns("xvar"),
                           choices = "",
                           selected = NULL,
                           label = "Choose what variable should be on the x-axis:",
                           options = list(placeholder = "select a discrete variable")),
            selectizeInput(ns("facets"),
                           choices = "",
                           selected = NULL,
                           label = "Choose what variable to facet by:",
                           options = list(placeholder = "select a discrete variable",
                                          maxItems = 2)),
            selectizeInput(ns("color"),
                           choices = "",
                           selected = NULL,
                           label = "Choose what variable to color by:",
                           options = list(placeholder = "select a discrete variable"))
          ),
          plotOutput(ns("boxplot"))
        )
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
    
    x <- reactiveValues()
    
    observe({
      req(results_derived_traits$data_with_derived_traits())
      x$data <- results_derived_traits$data_with_derived_traits()
    })
    
    observe({
      req(x$data)
      updateSelectizeInput(inputId = "yvar",
                           choices = c("", colnames(x$data)))
      updateSelectizeInput(inputId = "xvar",
                           choices = c("", colnames(x$data)))
      updateSelectizeInput(inputId = "facets",
                           choices = c("", colnames(x$data)))
      updateSelectizeInput(inputId = "color",
                           choices = c("", colnames(x$data)))
    })
    
    output$boxplot <- renderPlot({
      req(x$data,
          input$xvar,
          input$yvar)
      
      plot <- x$data %>% 
        ggplot2::ggplot() +
        ggplot2::geom_boxplot(ggplot2::aes(x = .data[[input$xvar]],
                                           y = .data[[input$yvar]]),
                              outlier.shape = NA) +
        ggplot2::theme_classic() +
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45,
                                                           hjust = 1),
                       text = ggplot2::element_text(size = 16)) +
        ggpubr::border(size = 0.5)
      
      if (isTruthy(input$color)) {
        plot <- plot +
          ggplot2::geom_jitter(ggplot2::aes(x = .data[[input$xvar]],
                                            y = .data[[input$yvar]],
                                            color = .data[[input$color]]))
      } else {
        plot <- plot +
          ggplot2::geom_jitter(ggplot2::aes(x = .data[[input$xvar]],
                                            y = .data[[input$yvar]]),
                               color = "red")
      }
      
      if (isTruthy(input$facets)) {
        plot <- plot +
          ggplot2::facet_wrap(input$facets)
      }
      return(plot)
    })
    
  })
}

## To be copied in the UI
# mod_data_exploration_ui("data_exploration_ui_1")

## To be copied in the server
# mod_data_exploration_server("data_exploration_ui_1")
