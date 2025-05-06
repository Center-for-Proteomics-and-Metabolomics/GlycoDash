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
                      " .fas {float: right; margin-top: 3px; margin-left: 5px; font-size: 12px;}"))
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
    )
    
  )
}

#' data_exploration Server Functions
#'
#' @noRd 
mod_data_exploration_server <- function(id, 
                                        results_quantitation,
                                        results_derived_traits,
                                        results_site_occupancy,
                                        results_normalization){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    my_data <- reactive({
      req(results_normalization$normalized_data_wide())
      if (is_truthy(results_site_occupancy$site_occupancy_data())) {
        results_site_occupancy$site_occupancy_data()
      } else if (is_truthy(results_derived_traits$data_with_traits())) {
        results_derived_traits$data_with_traits()
      } else if (is_truthy(results_quantitation$data_with_quantities())) {
        results_quantitation$data_with_quantities()
      } else {
        results_normalization$normalized_data_wide()
      }
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
    
    tab_counter <- reactiveVal(1)
    
    observe({
      tab_counter(tab_counter() + 1)
    }) %>% bindEvent(input$add_tab)
    
    observe({
      tab_id <- paste0("tab", (tab_counter()))
      
      appendTab(inputId = "tabs",
                tabPanel(
                  title = tags$span(
                    paste("Figure", tab_counter()),
                    # Adding a clickable x-mark icon to close the tab:
                    tags$span(icon("times"),
                              style = "margin-left: 5px;",
                              # When the x-mark icon is clicked a Shiny input
                              # named input$remove_tab is created with the value
                              # being the value argument of the tab on which the
                              # icon was clicked:
                              onclick = paste0("Shiny.setInputValue(\"", 
                                               ns("remove_tab"), 
                                               "\", \"", 
                                               paste("Figure", tab_counter()), 
                                               "\", {priority: \"event\"})"))
                  ),
                  value = paste("Figure", tab_counter()), # By giving this value 
                  # as an argument to removeTab() the tab can be closed.
                  mod_tab_data_exploration_ui(ns(tab_id))
                )
      )
      
      tab_results[[tab_id]] <- mod_tab_data_exploration_server(
        id = tab_id,
        my_data = my_data,
        trigger = reactive({ r$trigger })
      )
      
    }) %>% bindEvent(input$add_tab#, 
                     #ignoreInit = TRUE
                     ) # Maybe unnecessary? according to 
    # documentation of actionButton they are 'falsy' after initial load.
    
    
    # When an x-mark icon is clicked (so when input$remove_tab changes) that tab
    # is closed:
    observe({
      # The tab where the cross was clicked is removed:
      removeTab(inputId = "tabs", target = input$remove_tab, session = session)
      
      # If the tab that is removed is the tab with the highest number,
      # tab_counter$n is reduced with one so that if a new tab is created there
      # is no gap in numbering:
      if (as.numeric(stringr::str_extract(
        input$remove_tab,
        "[[:digit:]]"
      )) >= tab_counter()) {
        tab_counter(tab_counter() - 1)
      }
      
      tab_results[[stringr::str_replace(input$remove_tab,
                                        "Figure ",
                                        "tab")]] <- NULL
      
    }) %>% bindEvent(input$remove_tab)
    
    return(list(
      tab_results = tab_results
    ))
    
  })
}

## To be copied in the UI
# mod_data_exploration_ui("data_exploration_ui_1")

## To be copied in the server
# mod_data_exploration_server("data_exploration_ui_1")
