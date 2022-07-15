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
    
    observeEvent(input$add_tab, {
      tab_id <- paste0("tab", (input$add_tab + 1))
      
      appendTab(inputId = "tabs",
                tabPanel(title = paste("Standard",
                                       input$add_tab + 1),
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
      
    })
    
    return(list(
      tab_results = reactive({ tab_results })
    ))
    
  })
}
