#' derived_traits UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_derived_traits_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidPage(
      fluidRow(
        h1("Derived traits")
      ),
      fluidRow(
        shinydashboard::box(
          title = "Calculate derived traits",
          width = 3,
          solidHeader = TRUE,
          status = "primary",
          shinyWidgets::awesomeCheckboxGroup(ns("traits_menu"),
                                             "Select the derived traits that should be calculated:",
                                             choices = c("Fucosylation",
                                                         "Bisection",
                                                         "Galactosylation",
                                                         "Sialylation")),
          # checkboxGroupInput(ns("traits_menu"),
          #                    "Choose which derived traits should be calculated",
          #                    choices = c("Fucosylation",
          #                                "Bisection",
          #                                "Galactosylation",
          #                                "Sialylation")),
          actionButton(ns("do_calculation"),
                       "Calculate derived traits")
        )
      ),
      fluidRow(
        shinydashboard::box(
          title = "View data with derived traits",
          width = 12,
          solidHeader = TRUE,
          status = "primary",
          DT::dataTableOutput(ns("data_table"))
        )
      )
    )
  )
}
    
#' derived_traits Server Functions
#'
#' @noRd 
mod_derived_traits_server <- function(id, results_normalization){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    x <- reactiveValues()
    
    observe({
      req(results_normalization$normalized_data())
      x$data <- results_normalization$normalized_data()
    })
    
    observe({
      shinyjs::toggleState("do_calculation", 
                           condition = isTruthy(input$traits_menu))
    })
    
    observeEvent(input$do_calculation, {
      derived_traits <- calculate_derived_traits(data = x$data,
                                                 selected_derived_traits = input$traits_menu) %>% 
        tidyr::pivot_wider(names_from = cluster,
                           values_from = dplyr::any_of(input$traits_menu),
                           names_glue = "{cluster}_{.value}")
      
      x$data_with_derived_traits <- dplyr::full_join(results_normalization$normalized_data_wide(),
                                                     derived_traits)
      
    })
    
    output$data_table <- DT::renderDT({
      req(x$data_with_derived_traits)
      
      DT::datatable(data = x$data_with_derived_traits,
                    options = list(scrollX = TRUE))
    })
    
    return(
      list(
        data_with_derived_traits = reactive({x$data_with_derived_traits}),
        normalized_data = reactive({x$data})
      )
    )
 
  })
}
    
## To be copied in the UI
# mod_derived_traits_ui("derived_traits_ui_1")
    
## To be copied in the server
# mod_derived_traits_server("derived_traits_ui_1")
