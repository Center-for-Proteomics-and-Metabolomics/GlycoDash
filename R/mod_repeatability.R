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
                      ns("tabbed_box"),
                      " .btn {float: right;}",
                      "#",
                      ns("tabbed_box"),
                      " .box-title {width: 100%;}",
                      "#",
                      ns("tabbed_box"),
                      " .fa {float: right; margin-top: 3px; margin-left: 5px; font-size: 12px;}"))
        ),
        div(
          id = ns("tabbed_box"),
          shinydashboard::box(
            title = span(
              "Assess repeatability",
              actionButton(ns("add_tab"),
                           "Add a tab",
                           icon = icon("plus"))),
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
    
    data <- reactive({
      results_normalization$normalized_data()
    })

    Ig_data <- reactive({
      results_data_import$Ig_data()
    })

    output$first_tab <- renderUI({
      mod_tab_repeatability_ui(ns("tab1"))
    })

    observe({
      mod_tab_repeatability_server("tab1",
                                   data = data,
                                   Ig_data = Ig_data)
    })
    
    observeEvent(input$add_tab, {
      appendTab(inputId = "tabs",
                tabPanel(title = paste("Standard",
                                       input$add_tab + 1),
                         mod_tab_repeatability_ui(ns("tab2")))
                )
      
      mod_tab_repeatability_server("tab2",
                                   data = data,
                                   Ig_data = Ig_data)
      
      print(input$add_tab)

    })
    
  })
}
