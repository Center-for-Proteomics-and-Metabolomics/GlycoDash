#' tab_data_exploration UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_tab_data_exploration_ui <- function(id) {
  ns <- NS(id)
  tagList(
    br(),
    fluidPage(
      fluidRow(
        shinydashboardPlus::box(
          title = "Settings",
          width = 12,
          solidHeader = FALSE,
          status = "primary",
          collapsible = TRUE,
          selectizeInput(
            ns("plot_type"),
            choices = c("Boxplot", "Scatter plot", "Histogram"),
            label = "Type of plot:"
          ),
          selectizeInput(
            ns("filter"),
            choices = "",
            selected = NULL,
            multiple = TRUE,
            label = "Sample types to exclude from figure:"
          ),
          selectizeInput(
            ns("yvar"),
            choices = "",
            selected = NULL,
            label = "y-axis variable:"
          ),
          selectizeInput(
            ns("xvar"),
            choices = "",
            selected = NULL,
            label = "x-axis variable:"
          ),
          selectizeInput(
            ns("facets"),
            choices = "",
            selected = NULL,
            label = "Variable to facet by:"
          ),
          selectizeInput(
            ns("color"),
            choices = "",
            selected = NULL,
            label = "Variable to color by:"
          )
        )
      ),
      br(),
      fluidRow(
        width = 12,
        shinyjqui::jqui_resizable(plotly::plotlyOutput(ns("plot")))
      )
    )
  )
}
    

#' tab_data_exploration Server Functions
#'
#' @noRd 
mod_tab_data_exploration_server <- function(
    id, 
    my_data, 
    trigger  
  ) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Hide y-variable when user selects histogram.
    observe({
      shinyjs::toggle(
        id = "yvar", 
        condition = input$plot_type != "Histogram"
      )
    })
    
    observe({
      req(my_data())
      
      updateSelectizeInput(
        inputId = "yvar",
        choices = c("", colnames(my_data()))
      )
      updateSelectizeInput(
        inputId = "xvar",
        choices = c("", colnames(my_data()))
      )
      updateSelectizeInput(
        inputId = "facets",
        choices = c("", colnames(my_data()))
      )
      updateSelectizeInput(
        inputId = "color",
        choices = c("", colnames(my_data()))
      )
      updateSelectizeInput(
        inputId = "filter",
        choices = c("", unique(my_data()["sample_type"]))
      )
    }) %>% bindEvent(trigger()) # Only once the trigger has become TRUE (and 
    # thus when the selectizeInputs have been rendered) are the selectizeInputs
    # updated.
    
    
    filtered_data <- reactive({
      req(my_data())
      
      if(isTruthy(input$filter)) {
        my_data() %>% 
          dplyr::filter(!(sample_type %in% input$filter))
      } else {
        my_data()
      }
    })
    
    
    my_plot <- reactive({
      req(filtered_data())
      
      if (is_truthy(input$color)) {
        color <- input$color
      } else {
        color <- NULL
      }
      
      if (is_truthy(input$facets)) {
        facets <- input$facets
      } else {
        facets <- NULL
      }
      
      if (input$plot_type == "Boxplot") {
        req(input$xvar, input$yvar)
        my_boxplot(
          filtered_data(),
          xvar = input$xvar,
          yvar = input$yvar,
          color = color,
          facets = facets
        )
      } else if (input$plot_type == "Scatter plot") {
        req(input$xvar, input$yvar)
        my_scatter_plot(
          filtered_data(),
          xvar = input$xvar,
          yvar = input$yvar,
          color = color,
          facets = facets
        )
      } else if (input$plot_type == "Histogram") {
        req(input$xvar)
        my_histogram(
          filtered_data(),
          xvar = input$xvar,
          color = color,
          facets = facets
        )
      }
    })
    
    
    output$plot <- plotly::renderPlotly({
      
      plotly_object <- plotly::ggplotly(my_plot(), tooltip = "text") 
      
      if (input$plot_type == "Boxplot") {
        # Determine the number of facets n, the first n traces will correspond to
        # the boxplot traces in the ggplotly object:
        boxplot_traces <- 1:nfacets(my_plot())
        
        plotly_object <- plotly_object %>% # Use "text" as hoverinfo for the points,
          # but use the default hoverinfo for the boxplot traces:
          plotly::style(hoverinfo = "y", traces = boxplot_traces) %>% 
          # Hide the outliers (needed because plotly ignores "outlier.shape = NA"):
          hide_outliers(.) 
      }
      
      plotly_object
    })
    
    
    
    return(list(
      plot = my_plot
    ))
    
  })
}
    
