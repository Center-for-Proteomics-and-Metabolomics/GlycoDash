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
      
      appendTab(inputId = "tabs",
                tabPanel(
                  value = paste("Standard", 
                                tab_counter$n),
                  title = 
                  # tags$style(HTML(paste0(
                  #   "#",
                  #   ns(paste0("tabtitle", tab_id)),
                  #   " .btn {padding: 0pt; margin-left: 3pt; border-color: #ffffff; background-color: #ffffff; size: 12pt}"
                  # ))),
                  tags$span(
                    paste("Standard",
                          tab_counter$n),
                    tags$span(icon("times"),
                              style = "margin-left: 5px;",
                              onclick = paste0("Shiny.setInputValue(\"", 
                                               ns("remove_tab"), 
                                               "\", \"", 
                                               paste("Standard",
                                                      tab_counter$n), 
                                               "\", {priority: \"event\"})"))
                ),
                # actionButton(ns(paste0(tab_id, "button")),
                #                label = "",
                #                icon = icon("times"))
                mod_tab_repeatability_ui(
                  ns(tab_id)
                )
                )
      )
      
      tab_results[[tab_id]] <- mod_tab_repeatability_server(
        id = tab_id,
        my_data = data,
        Ig_data = Ig_data
      )
      
    }) %>% bindEvent(input$add_tab)
    
    ## remove a dataset
    observe({
      removeTab(inputId = "tabs", target = input$remove_tab, session = session)
      
      tab_counter$n <- tab_counter$n - 1
      # Problem: when a middle tab is removed and then a new tab is added there
      # will be a duplicate tab name, Solution maybe: change tab_counter$n only
      # when the remove_tab is the highest number
      
    }) %>% bindEvent(input$remove_tab)
    
    
    return(list(
      tab_results = tab_results
    ))
    
  })
}
