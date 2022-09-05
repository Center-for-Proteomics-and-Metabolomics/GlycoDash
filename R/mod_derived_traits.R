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
        h1("Derived traits"),
              ),
      fluidRow(
        shinydashboard::box(
          title = "Calculate derived traits",
          width = 3,
          solidHeader = TRUE,
          status = "primary",
          "Attention: derived traits calculation can only be used on IgG data for now!",
          br(),
          shinyWidgets::awesomeCheckboxGroup(ns("traits_menu"),
                                             "Select the derived traits that should be calculated:",
                                             choices = c("Fucosylation",
                                                         "Bisection",
                                                         "Galactosylation",
                                                         "Sialylation")),
          actionButton(ns("do_calculation"),
                       "Calculate derived traits")
        ),
        shinydashboard::box(
          title = "Formulas used to calculate the derived traits:",
          width = 9,
          solidHeader = TRUE,
          status = "primary",
        DT::dataTableOutput(ns("formulas"))
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
                           condition = all(isTruthy(input$traits_menu),
                                           isTruthy(x$data)))
    })
    
    derived_traits <- reactive({
      req(x$data)
      
      calculate_derived_traits(data = x$data,
                               selected_derived_traits = input$traits_menu)
    }) %>% bindEvent(input$do_calculation)
    
    data_with_derived_traits <- reactive({
      req(derived_traits())
      dplyr::full_join(results_normalization$normalized_data_wide(),
                       derived_traits()) %>% 
        dplyr::select(-tidyselect::ends_with("formula")) %>% 
        tidyr::pivot_wider(names_from = cluster,
                           values_from = dplyr::any_of(input$traits_menu),
                           names_glue = "{cluster}_{.value}")
    })
  
    # observeEvent(input$do_calculation, {
    #   derived_traits <- calculate_derived_traits(data = x$data,
    #                                              selected_derived_traits = input$traits_menu) %>% 
    #     tidyr::pivot_wider(names_from = cluster,
    #                        values_from = dplyr::any_of(input$traits_menu),
    #                        names_glue = "{cluster}_{.value}")
    #   
    #   x$data_with_derived_traits <- dplyr::full_join(results_normalization$normalized_data_wide(),
    #                                                  derived_traits)
    #   
    # })
    
    output$data_table <- DT::renderDT({
      req(data_with_derived_traits())
      
      DT::datatable(data = data_with_derived_traits(),
                    options = list(scrollX = TRUE))
    })
    
    formulas <- reactive({
      req(derived_traits())
      derived_traits() %>% 
        dplyr::ungroup() %>% 
        dplyr::select(tidyselect::ends_with("formula")) %>% 
        dplyr::distinct() %>% 
        tidyr::pivot_longer(cols = tidyselect::everything(),
                            names_to = "Derived trait",
                            values_to = "Formula") %>% 
        dplyr::mutate(`Derived trait` = dplyr::recode(`Derived trait`,
                                                      fuc_formula = "Fucosylation",
                                                      gal_formula = "Galactosylation",
                                                      sial_formula = "Sialylation",
                                                      bis_formula = "Bisection"))
    })
    
    output$formulas <- DT::renderDT({
      req(formulas())
      
      DT::datatable(formulas(),
                    rownames = FALSE, 
                    options = list(paging = FALSE,
                                   ordering = FALSE,
                                   searching = FALSE))
    })
    
    return(
      list(
        data_with_derived_traits = data_with_derived_traits,
        normalized_data = reactive({x$data}),
        derived_traits = reactive({input$traits_menu}),
        formulas = formulas
      )
    )
 
  })
}
    
## To be copied in the UI
# mod_derived_traits_ui("derived_traits_ui_1")
    
## To be copied in the server
# mod_derived_traits_server("derived_traits_ui_1")
