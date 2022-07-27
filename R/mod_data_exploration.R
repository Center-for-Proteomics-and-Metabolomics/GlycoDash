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
    
    # Creating a trigger to track when UI of the first tab is rendered. This is
    # needed to prevent the server of the module from trying to update
    # selectizeInputs that don't exist yet. 
    r <- reactiveValues(trigger = FALSE)
    
    output$first_tab <- renderUI({
      # The trigger is set to true when the UI of the first tab is being
      # rendered:
      r$trigger <- TRUE
      mod_tab_data_exploration_ui(ns("tab1"))
    })
    
    # Creating a reactiveValues list to store the plots that are created in the
    # tabs:
    tab_results <- reactiveValues()
    
    # The server part of the first tab receives the trigger as an argument, so
    # that the observer where the selectizeInputs are updated is only ran after
    # the trigger has become TRUE:
    observe({
    req(my_data())
    tab_results$tab1 <- mod_tab_data_exploration_server("tab1",
                                                        my_data = my_data,
                                                        trigger = reactive({ r$trigger }))
    })
    
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
        my_data = my_data,
        trigger = reactive({ r$trigger })
      )
      
    }) %>% bindEvent(input$add_tab,
                     ignoreInit = TRUE) # Maybe unnecessary? according to 
    # documentation of actionButton they are 'falsy' after initial load.
    
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
    
    return(list(
      tab_results = tab_results
    ))
    
  })
}

## To be copied in the UI
# mod_data_exploration_ui("data_exploration_ui_1")

## To be copied in the server
# mod_data_exploration_server("data_exploration_ui_1")
