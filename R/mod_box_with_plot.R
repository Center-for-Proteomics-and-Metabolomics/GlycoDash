#' box_with_plot UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_box_with_plot_ui <- function(id){
  ns <- NS(id)
  tagList(
    shinydashboard::box(
      title = "Boxplot",
      width = 12,
      solidHeader = TRUE,
      status = "primary",
      shinyWidgets::dropdownButton(
        icon = icon("gears",
                    verify_fa = FALSE),
        status = "primary",
        size = "sm",
        selectizeInput(ns("filter"),
                       choices = "",
                       selected = NULL,
                       multiple = TRUE,
                       label = "Choose which sample types should be excluded from the plot:",
                       options = list(placeholder = "select one or more sample type(s)")),
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
}
    
#' box_with_plot Server Functions
#'
#' @noRd 
mod_box_with_plot_server <- function(id, my_data){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    observe({
      req(my_data())
      updateSelectizeInput(inputId = "yvar",
                           choices = c("", colnames(my_data())))
      updateSelectizeInput(inputId = "xvar",
                           choices = c("", colnames(my_data())))
      updateSelectizeInput(inputId = "facets",
                           choices = c("", colnames(my_data())))
      updateSelectizeInput(inputId = "color",
                           choices = c("", colnames(my_data())))
      updateSelectizeInput(inputId = "filter",
                           choices = c("", unique(my_data()$sample_type)))
    })
    
    filtered_data <- reactive({
      req(my_data())
      
      if(isTruthy(input$filter)) {
        filtered_data <- my_data() %>% 
          dplyr::filter(!(sample_type %in% input$filter))
      } else {
        filtered_data <- my_data()
      }
      
      return(filtered_data)
      
    })
    
    my_plot <- reactive({
      req(filtered_data(),
          input$xvar,
          input$yvar)
      
      plot <- filtered_data() %>% 
        ggplot2::ggplot() +
        ggplot2::geom_boxplot(ggplot2::aes(x = .data[[input$xvar]],
                                           y = .data[[input$yvar]]),
                              outlier.shape = NA) +
        ggplot2::theme_classic() +
        ggplot2::theme(panel.border = ggplot2::element_rect(colour = "black", fill=NA, size=0.5)) +
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45,
                                                           hjust = 1),
                       text = ggplot2::element_text(size = 16))
      
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
    
    output$boxplot <- renderPlot({
      my_plot()
    })
    
    return(list(
      plot = my_plot
    ))
    
  })
}
    
## To be copied in the UI
# mod_box_with_plot_ui("box_with_plot_ui_1")
    
## To be copied in the server
# mod_box_with_plot_server("box_with_plot_ui_1")
