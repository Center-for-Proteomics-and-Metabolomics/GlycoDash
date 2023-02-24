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
          title = "Calculate derived traits automatically",
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
          title = "Calculate custom derived traits",
          width = 3,
          solidHeader = TRUE,
          status = "primary",
          fileInput(ns("custom_traits_file"), 
                    "Upload Excel file with custom derived traits formulas",
                    accept = c(".xlsx"))
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
mod_derived_traits_server <- function(id, results_normalization, results_data_import){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    normalized_data <- reactive({
      req(results_normalization$normalized_data())
      results_normalization$normalized_data()
    })
    
    colnames_metadata <- reactive({
      req(results_data_import$colnames_metadata())
      results_data_import$colnames_metadata()
    })
    
    observe({
      req(colnames_metadata())
      print(colnames_metadata())
    })
    
    
    ####################  Custom derived traits  ####################
    traits_excel <- reactive({
      req(input$custom_traits_file)
      readxl::read_excel(input$custom_traits_file$datapath, col_names = FALSE)
    })
    
    custom_traits <- reactive({
      req(traits_excel(), normalized_data())
      calculate_custom_traits(normalized_data(), traits_excel())
    })
    
    data_with_custom_traits <- reactive({
      req(custom_traits())
      dplyr::full_join(results_normalization$normalized_data_wide(),
                       custom_traits()) %>% 
      dplyr::select(-tidyselect::ends_with("formula"))
    })
    

  
    ####################  Default derived traits  ####################
    
    observe({
      shinyjs::toggleState("do_calculation", 
                           condition = all(is_truthy(input$traits_menu),
                                           is_truthy(normalized_data())))
    })
    
    derived_traits <- reactive({
      #req(normalized_data())
      
      calculate_derived_traits(data = normalized_data(),
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
    
    
    
    ############### Combined default + custom traits ###############
    
    # Combine default traits with custom traits
    data_with_all_traits <- reactive({
      req(data_with_derived_traits(), data_with_custom_traits())
      dplyr::full_join(
        data_with_derived_traits(), data_with_custom_traits()
      )
    })
    
    
    # If only default traits were calculated: use "data_with_derived_traits()" here
    # If only custom traits were calculated: use "data_with_custom_traits()" here
    # If both default and custom traits were calculated: use "data_with_all_traits()" here
    
    traits_data_table <- reactive({
      if (is_truthy(data_with_all_traits())) {
        return(data_with_all_traits())  
      } else if (is_truthy(data_with_derived_traits())) {
        return(data_with_derived_traits())
      } else if (is_truthy(data_with_custom_traits())) {
        return(data_with_custom_traits())
      }
    })

    output$data_table <- DT::renderDT({
      req(traits_data_table())
      # req(data_with_derived_traits())
      DT::datatable(data = traits_data_table(),
                    # data = data_with_derived_traits(),
                    options = list(scrollX = TRUE))
    })
    
    
    
    
    ############### Formulas of derived traits ###############
    
    ##TODO Add a box with "Formulas used to calculate custom traits"
    
    formulas <- reactive({
      req(derived_traits())
      derived_traits() %>% 
        dplyr::select(tidyselect::ends_with("formula"), cluster) %>% 
        dplyr::distinct() %>% 
        tidyr::pivot_longer(cols = -cluster,
                            names_to = "Derived trait",
                            values_to = "Formula") %>% 
        dplyr::rename_with(.fn = firstupper, 
                           .cols = cluster) %>% 
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
        normalized_data = normalized_data,
        derived_traits = reactive({ input$traits_menu }),
        formulas = formulas
      )
    )
 
  })
}
