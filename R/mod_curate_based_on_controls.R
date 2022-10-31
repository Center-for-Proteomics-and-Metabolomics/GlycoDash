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
                   multiple = TRUE#,
                   #options = list(placeholder = "Select which samples to use as a basis for cut-off.")
    ) %>% 
      bsplus::bs_embed_popover(
        title = "Explanation",
        content = HTML(paste0(
          tags$p(paste(
            "Choose a group of samples that should not pass curation",
            "(e.g. Specific Ig negative control samples)."
          )),
          tags$p(paste(
            "The average proportion of passing analytes and average sum intensity",
            "in this group of samples will be used as cut-off values;"
          )),
          tags$p(paste(
            "All spectra that have a proportion of passing analytes and a sum intensity",
            "higher than these cut-off values will pass spectra curation."
          ))
        )),
        trigger = "hover",
        placement = "right",
        html = "true"),
    div(id = ns("cut_off_basis_Ig_data"),
        # shinydashboardPlus::box(
        #   title = "Specific Ig samples",
        #   width = 6,
        #   status = "primary",
        #   solidHeader = TRUE,
        column(
          width = 6,
          selectInput(ns("cut_off_basis_specific"),
                      "Choose which specific Ig spectra should be used as negative controls:",
                      choices = c(""),
                      selected = NULL,
                      multiple = TRUE#,
                      #options = list(placeholder = "select which samples to use as a basis for cut-off")
          ) #%>%
          #   bsplus::bs_embed_popover(
          #     title = "Explanation",
          #     content = HTML(paste0(
          #       tags$p(paste(
          #         "Choose a group of samples that should not pass curation",
          #         "(e.g. Specific Ig negative control samples)."
          #       )),
          #       tags$p(paste(
          #         "The average proportion of passing analytes and average sum intensity",
          #         "in this group of samples will be used as cut-off values;"
          #       )),
          #       tags$p(paste(
          #         "All spectra that have a proportion of passing analytes and a sum intensity",
          #         "higher than these cut-off values will pass spectra curation."
          #       ))
          #     )),
          #     trigger = "hover",
          #     placement = "right",
          #     html = "true")
        ),
        # ),
        # shinydashboardPlus::box(
        #   title = "Total Ig samples",
        #   width = 6,
        #   status = "primary",
        #   solidHeader = TRUE,
        column(
          width = 6,
          selectizeInput(ns("cut_off_basis_total"),
                         "Choose which total Ig spectra should be used as negative controls:",
                         choices = c(""),
                         selected = NULL,
                         multiple = TRUE#,
                         #options = list(placeholder = "select which samples to use as a basis for cut-off")
          ) #%>% 
        #     bsplus::bs_embed_popover(
        #       title = "Explanation",
        #       content = HTML(paste0(
        #         tags$p(paste(
        #           "Choose a group of samples that should not pass curation",
        #           "(e.g. Specific Ig negative control samples)."
        #         )),
        #         tags$p(paste(
        #           "The average proportion of passing analytes and average sum intensity",
        #           "in this group of samples will be used as cut-off values;"
        #         )),
        #         tags$p(paste(
        #           "All spectra that have a proportion of passing analytes and a sum intensity",
        #           "higher than these cut-off values will pass spectra curation."
        #         ))
        #       )),
        #       trigger = "hover",
        #       placement = "right",
        #       html = "true")
        #   # )
        # )
    )
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
    div(id = ns("advanced_settings"),
        shinyWidgets::materialSwitch(
          ns("use_mean_SD"),
          label = paste(
            "To calculate the sum intensity cut-off use the mean", 
            "and standard deviation (SD) instead of percentiles."
          ),
          right = TRUE,
          status = "primary"),
        div(id = ns("mean_sd_settings"),
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
                                                summarized_checks){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    observe({
      shinyjs::toggle("cut_off_basis_Ig_data",
                      condition = results_data_import$Ig_data() == "Yes")
      shinyjs::toggle("cut_off_basis",
                      condition = results_data_import$Ig_data() == "No")
    })
    
    observe({
      shinyjs::toggle("advanced_settings",
                      condition = input$show_advanced_settings)
    })
    
    observe({
      shinyjs::toggle("mean_sd_settings",
                      condition = input$use_mean_SD)
    })
    
    # The selection menu for input$cut_off_basis is updated so that the choices
    # are all combinations of sample_types and groups that are present in the
    # data.
    observe({
      req(summarized_checks())
      if (results_data_import$Ig_data() == "No") {
        options <- unique(summarized_checks()$sample_type)
        
        names(options) <- paste(options, "samples")
        
        updateSelectizeInput(inputId = "cut_off_basis",
                             choices = options)
        
      } else {
        
        is_specific <- summarized_checks()$group == results_data_import$keyword_specific()
        # Weird tibble behavior: tibble[rows, single_column] results in a
        # tibble, even though dataframe[rows, single_column] results in a
        # vector. We need a vector here and summarized_checks() is a tibble so I
        # have to use $sample_type instead of [is_specific, "sample_type"]:
        options_specific <- unique(summarized_checks()[is_specific, ]$sample_type)
        
        # The names is what will be shown in the selection menu:
        names(options_specific) <- paste(results_data_import$keyword_specific(),
                                         options_specific,
                                         "samples")
        
        updateSelectInput(inputId = "cut_off_basis_specific",
                          choices = options_specific)
        
        options_total <- summarized_checks() %>% 
          dplyr::filter(group == results_data_import$keyword_total()) %>% 
          dplyr::pull(sample_type) %>% 
          unique()
        
        names(options_total) <- paste(results_data_import$keyword_total(),
                                      options_total,
                                      "samples")
        
        updateSelectizeInput(inputId = "cut_off_basis_total",
                             choices = c("", options_total))
      }
    })
    
    cut_offs <- reactive({
      NULL
      req(summarized_checks(),
          input$percentile,
          any(all(results_data_import$Ig_data() == "No",
                  is_truthy(input$cut_off_basis)),
              all(results_data_import$Ig_data() == "Yes",
                  is_truthy(input$cut_off_basis_specific),
                  is_truthy(input$cut_off_basis_total))))
      
      if (results_data_import$Ig_data() == "Yes") {
        
        cut_offs_specific <- calculate_cut_offs(
          summarized_checks(),
          control_sample_types = input$cut_off_basis_specific,
          group_keyword = results_data_import$keyword_specific(),
          percentile = input$percentile,
          use_mean_SD = input$use_mean_SD,
          SD_factor = input$factor,
          uncalibrated_as_NA = FALSE
        )
        
        cut_offs_total <- calculate_cut_offs(
          summarized_checks(),
          control_sample_types = input$cut_off_basis_total,
          group_keyword = results_data_import$keyword_total(),
          percentile = input$percentile,
          use_mean_SD = input$use_mean_SD,
          SD_factor = input$factor,
          uncalibrated_as_NA = FALSE
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
          uncalibrated_as_NA = FALSE
        )
      }
    })
    
    return(
      cut_offs
    )
 
  })
}
    
## To be copied in the UI
# mod_curate_based_on_controls_ui("curate_based_on_controls_ui_1")
    
## To be copied in the server
# mod_curate_based_on_controls_server("curate_based_on_controls_ui_1")
