#' curate_based_on_controls UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_curate_based_on_controls_ui <- function(id){
  ns <- NS(id)
  tagList(
    selectizeInput(ns("cut_off_basis"),
                   "Choose which spectra to use as negative controls:",
                   choices = c(""),
                   selected = NULL,
                   multiple = TRUE),
    div(
      id = ns("cut_off_basis_total_and_specific"),
      selectInput(ns("cut_off_basis_specific"),
                  "Choose which specific Ig spectra should be used as negative controls:",
                  choices = c(""),
                  selected = NULL,
                  multiple = TRUE),
      selectInput(ns("cut_off_basis_total"),
                     "Choose which total Ig spectra should be used as negative controls:",
                     choices = c(""),
                     selected = NULL,
                     multiple = TRUE)
    ),
    numericInput(ns("percentile"),
                 "At what percentile of the negative controls should the cut-offs be set?",
                 value = 95,
                 min = 0,
                 max = 100,
                 step = 1),
    shinyWidgets::awesomeCheckbox(ns("show_advanced_settings"),
                                  "Show advanced settings.",
                                  status = "primary"),
    div(
      id = ns("advanced_settings"),
      shinyWidgets::materialSwitch(
        ns("use_mean_SD"),
        label = paste(
          "To calculate the sum intensity cut-off use the mean", 
          "and standard deviation (SD) instead of percentiles."
        ),
        right = TRUE,
        status = "primary"),
      div(
        id = ns("mean_sd_settings"),
        tags$p(icon("warning"), "Using this method at low sample sizes can", 
               "lead to inflated cut-offs."),
        tags$p(paste(
          "The cut-off for the percentage of passing analytes will still be", 
          "calculated using the percentile chosen above."),
          br(),
          paste("The cut-off for the sum intensity will be calculated using", 
                "the following formula:")),
        tags$p("cut-off", 
               tags$sub("sum intensity"), 
               "= mean", 
               tags$sub("sum intensity in negative controls"), 
               "+ factor * SD", tags$sub("sum intensity in negative controls")),
        numericInput(ns("factor"),
                     "Choose the value of the factor with which the SD is multiplied:",
                     value = 3,
                     step = 1,
                     min = 0)
      )
    )
  )
}
    
#' curate_based_on_controls Server Functions
#'
#' @noRd 
mod_curate_based_on_controls_server <- function(id, 
                                                results_data_import,
                                                summarized_checks,
                                                uncalibrated_as_NA){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    observe({
      shinyjs::toggle("cut_off_basis_total_and_specific",
                      condition = results_data_import$contains_total_and_specific_samples() == TRUE)
      
      shinyjs::toggle("cut_off_basis",
                      condition = results_data_import$contains_total_and_specific_samples() == FALSE)
    })
    
    observe({
      shinyjs::toggle("advanced_settings",
                      condition = input$show_advanced_settings)
    })
    
    observe({
      shinyjs::toggle("mean_sd_settings",
                      condition = input$use_mean_SD)
    })
    
    # Creating a reactiveValue with the sample type options for the selectInput
    # menu to choose negative control samples:
    r <- reactiveValues()
    
    observe({
      req(summarized_checks)
      r$sample_types <- summarized_checks() %>% 
        dplyr::select(tidyselect::any_of(c("sample_type", "group"))) %>% 
        dplyr::distinct()
    })
    # I'm using a reactiveValue instead of a reactive expression, because a
    # reactiveValue is not invalidated if its value stays the same. Using a
    # reactiveValue here prevents the observer below (where the menus are updated)
    # from re-executing when it's not needed.
    
    # The selection menu for input$cut_off_basis is updated so that the choices
    # are all combinations of sample_types and groups that are present in the
    # data.
    observe({
      req(r$sample_types)
      if (results_data_import$contains_total_and_specific_samples() == FALSE) {
        options <- unique(r$sample_types$sample_type)
        
        names(options) <- paste(options, "samples")
        
        updateSelectizeInput(inputId = "cut_off_basis",
                             choices = options)
        
      } else {
        
        options_specific <- get_sample_type_options(
          summarized_checks = r$sample_types,
          total_or_specific_keyword = results_data_import$keyword_specific()
        )
        
        updateSelectizeInput(inputId = "cut_off_basis_specific",
                             choices = options_specific)
        
        options_total <- get_sample_type_options(
          summarized_checks = r$sample_types,
          total_or_specific_keyword = results_data_import$keyword_total()
        )
        
        updateSelectizeInput(inputId = "cut_off_basis_total",
                             choices = options_total)
        
      }
    })
    
    cut_offs <- reactive({
      req(summarized_checks(),
          input$percentile,
          any(all(results_data_import$contains_total_and_specific_samples() == FALSE,
                  is_truthy(input$cut_off_basis)),
              all(results_data_import$contains_total_and_specific_samples() == TRUE,
                  is_truthy(input$cut_off_basis_specific),
                  is_truthy(input$cut_off_basis_total))))
      
      if (results_data_import$contains_total_and_specific_samples() == TRUE) {
        
        cut_offs_specific <- calculate_cut_offs(
          summarized_checks(),
          control_sample_types = input$cut_off_basis_specific,
          group_keyword = results_data_import$keyword_specific(),
          percentile = input$percentile,
          use_mean_SD = input$use_mean_SD,
          SD_factor = input$factor,
          uncalibrated_as_NA = uncalibrated_as_NA()
        )
        
        cut_offs_total <- calculate_cut_offs(
          summarized_checks(),
          control_sample_types = input$cut_off_basis_total,
          group_keyword = results_data_import$keyword_total(),
          percentile = input$percentile,
          use_mean_SD = input$use_mean_SD,
          SD_factor = input$factor,
          uncalibrated_as_NA = uncalibrated_as_NA()
        )
        
        dplyr::full_join(cut_offs_specific,
                         cut_offs_total)
        
      } else {
        calculate_cut_offs(
          summarized_checks(),
          control_sample_types = input$cut_off_basis,
          percentile = input$percentile,
          use_mean_SD = input$use_mean_SD,
          SD_factor = input$factor,
          uncalibrated_as_NA = uncalibrated_as_NA()
        )
      }
    })
    
    return(
      cut_offs
    )
 
  })
}
