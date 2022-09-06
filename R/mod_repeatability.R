#' repeatability UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_repeatability_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidPage(
      fluidRow(
        h1("Repeatability")
      ),
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
              "Assess repeatability",
              actionButton(ns("add_tab"),
                           "Add a tab",
                           icon = icon("plus"))
            ),
            width = NULL,
            solidHeader = TRUE,
            status = "primary",
            tabsetPanel(
              id = ns("tabs"),
              tabPanel(title = "Standard 1",
                       uiOutput(ns("first_tab")))
            )
          )
        )
      )
    )
  )
}

#' repeatability Server Functions
#'
#' @noRd 
mod_repeatability_server <- function(id, results_normalization, results_data_import){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    tab_results <- reactiveValues()
    
    data <- reactive({
      results_normalization$normalized_data()
    })

    Ig_data <- reactive({
      results_data_import$Ig_data()
    })

    output$first_tab <- renderUI({
      mod_tab_repeatability_ui(ns("tab1"))
    })
    
    tab_results$tab1 <- mod_tab_repeatability_server("tab1",
                                                     my_data = data,
                                                     Ig_data = Ig_data)
    
    tab_counter <- reactiveValues(n = 1)
    
    observe({
      tab_counter$n <- tab_counter$n + 1
    }) %>% bindEvent(input$add_tab)
    
    observe({
      tab_id <- paste0("tab", (tab_counter$n))
      
      # Add a tab:
      appendTab(inputId = "tabs",
                tabPanel(
                  value = paste("Standard",
                                tab_counter$n), # By giving this value as an 
                  # argument to removeTab() the tab can be closed.
                  title = tags$span(
                    paste("Standard",
                          tab_counter$n),
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
                                               paste("Standard",
                                                     tab_counter$n), 
                                               "\", {priority: \"event\"})"))
                  ),
                  # The content of the tab is created in the tab_repeatability module:
                  mod_tab_repeatability_ui(ns(tab_id))
                )
      )
      
      # The contents of the tab are saved in the reactiveValues list tab_results,
      # so they can be passed on to the report:
      tab_results[[tab_id]] <- mod_tab_repeatability_server(
        id = tab_id,
        my_data = data,
        Ig_data = Ig_data
      )
      
    }) %>% bindEvent(input$add_tab)
    
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
        )) >= tab_counter$n) {
        tab_counter$n <- tab_counter$n - 1
      }
      
      tab_results[[stringr::str_replace(input$remove_tab,
                                        "Standard ",
                                        "tab")]] <- NULL
      
    }) %>% bindEvent(input$remove_tab)
    
    return(list(
      tab_results = tab_results
    ))
    
  })
}
