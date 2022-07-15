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
        tags$style(
          HTML(paste0("#",
                      ns("box_title"),
                      " .btn {float: right; padding-top: 2px; border-color: #fff; border: 1.5px solid; padding-bottom: 2px}",
                      "#",
                      ns("tabbed_box"),
                      " .box-title {width: 100%;}",
                      "#",
                      ns("box_title"),
                      " .fa {float: right; margin-top: 3px; margin-left: 5px; font-size: 12px;}"))
        ),
        div(
          id = ns("tabbed_box"),
          shinydashboard::box(
            title = span(
              id = ns("box_title"),
              "Data exploration",
              actionButton(ns("add_tab"),
                           "Add a tab",
                           icon = icon("plus"))
            ),
            width = NULL,
            solidHeader = TRUE,
            status = "primary",
            tabsetPanel(
              id = ns("tabs"),
              tabPanel(title = "Figure 1",
                       uiOutput(ns("first_tab")))
            )
          )
        )
      )
      # fluidRow(
      #   h1("Data exploration"),
      #   actionButton(ns("button"),
      #                "Create a new plot.")
      # ),
      # fluidRow(
      #   uiOutput(ns("boxes"))
      # )
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
    
    tab_results <- reactiveValues()
    
    output$first_tab <- renderUI({
      mod_tab_data_exploration_ui(ns("tab1"))
    }) %>% bindEvent(my_data())
    
    observe({
    tab_results$tab1 <- mod_tab_data_exploration_server("tab1",
                                                        my_data = my_data)
    }) %>% bindEvent(my_data())
    
    observe({
      req(input$add_tab > 0)
      tab_id <- paste0("tab", (input$add_tab + 1))
      
      appendTab(inputId = "tabs",
                tabPanel(title = paste("Figure",
                                       input$add_tab + 1),
                         mod_tab_data_exploration_ui(
                           ns(tab_id)
                         )
                )
      )
      
      tab_results[[tab_id]] <- mod_tab_data_exploration_server(
        id = tab_id,
        my_data = my_data
      )
      
    }) %>% bindEvent(input$add_tab)
    
    # r <- reactiveValues(all_boxes = list(),
    #                     all_plots = list())
    # 
    # observe({
    #   req(input$button > 0)
    #   # Run the server part of the module that creates a box with a plot:
    #   mod_box_with_plot_server(input$button,
    #                            my_data = my_data)
    #   
    #   # Run the UI part of the module that creates a box with a plot and save it
    #   # in the reactiveValue all_boxes:
    #   r$all_boxes[[input$button]] <- mod_box_with_plot_ui(ns(input$button))
    # }) %>% bindEvent(input$button)
    # 
    # observe({
    #   req (input$button > 0)
    #   for (box in 1:input$button) {
    #     
    #   }
    # })
    # 
    # output$boxes <- renderUI(
    #   tagList(r$all_boxes)
    # )
    
  })
}

## To be copied in the UI
# mod_data_exploration_ui("data_exploration_ui_1")

## To be copied in the server
# mod_data_exploration_server("data_exploration_ui_1")
