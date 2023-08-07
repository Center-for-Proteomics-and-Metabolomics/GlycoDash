#' tab_curated_analytes UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_tab_curated_analytes_ui <- function(id){
  ns <- NS(id)
  
  tagList(
    column(
      width= 12,
      shinycssloaders::withSpinner(plotly::plotlyOutput(ns("plot"))),
      br(),
      shinyWidgets::materialSwitch(ns("check_all"),
                                   "If one charge state has passed curation, also select all other charge states for further analysis.",
                                   right = TRUE,
                                   status = "primary"),
      br(),
      shinycssloaders::withSpinner(DT::dataTableOutput(ns("table")))
    )
  )
}
    
#' tab_curated_analytes Server Function
#'
#' @noRd 
mod_tab_curated_analytes_server <- function(id, info, cluster, biogroup_column){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    curated_analytes_plot <- reactive({
      req(info$method()  == "Curate analytes based on data")
      req(info$curated_analytes())
      req(info$cut_off())
      
      plot_analyte_curation(curated_analytes = info$curated_analytes(),
                            cut_off_percentage = info$cut_off(),
                            selected_cluster = cluster,
                            bio_groups_colname = biogroup_column)
    })
    
    output$plot <- plotly::renderPlotly({
      req(curated_analytes_plot())
      plotly_object <- plotly::ggplotly(curated_analytes_plot(), tooltip = "text")
      
      plotly_object[["x"]][["layout"]][["annotations"]][[2]][["xshift"]] <- -50
      
      plotly_object[["x"]][["layout"]][["annotations"]][[1]][["yshift"]] <- -90
      
      return(plotly_object)
      
    })
    
    observe({
      shinyjs::toggle(id = "plot",
                      condition = is_truthy(curated_analytes_plot()))
    })
    
    # The code to create a datatable with checkboxes is based on:
    # https://stackoverflow.com/questions/63343676/r-shiny-set-check-box-controls-value-to-values-in-specific-column-of-dataframe
    # These functions are defined within this module instead of in
    # fct_analyte_curation.R, because inside the function ns() is used and the
    # ns() function is only available within the module:
    create_multiple_shinyInputs <- function(shinyInput_function, 
                                            number_of_inputs, 
                                            inputId, 
                                            values) {
      vector_with_inputs <- character(number_of_inputs)
      for (i in seq_len(number_of_inputs)) {
        vector_with_inputs[i] <- as.character(
          shinyInput_function(
            inputId = ns(paste0(inputId, i)), 
            label = NULL, 
            value = values[i]
          )
        )
      }
      return(vector_with_inputs)
    }
    
    retrieve_shinyInput_values <- function(inputId, 
                                           number_of_inputs) {
      unlist(lapply(
        seq_len(number_of_inputs), 
        function(i) {
          value <- input[[paste0(inputId, i)]]
          if (is.null(value)) NA else value
        }
      ))
    }
    
    curated_analytes_table <- reactive({
      req(info$analyte_curated_data())
      
      table <- prepare_analyte_curation_table(
        analyte_curated_data = info$analyte_curated_data(),
        selected_cluster = cluster,
        by_group = ifelse(
          test = (!is.null(biogroup_column) & biogroup_column != ""),
          yes = TRUE, no = FALSE
          )
        )

      charge_columns <- colnames(table)[-1]
      
      table_with_checkboxes <- table %>% 
        dplyr::mutate(
          dplyr::across(tidyselect::all_of(charge_columns),
                        ~ create_multiple_shinyInputs(
                          shinyInput_function = checkboxInput,
                          number_of_inputs = nrow(table),
                          inputId = paste0("checkbox", dplyr::cur_column()),
                          values = dplyr::if_else(.x == "Yes", 
                                                  TRUE, 
                                                  FALSE)
                        ),
                        .names = "Include {col} in further analysis")
        ) %>% 
        dplyr::select(analyte,
                      tidyselect::contains(charge_columns))
      
      return(table_with_checkboxes)
    })
    
    observe({
      req(curated_analytes_table())
      
      if (is_truthy(input$check_all)) {
        
        charge_columns <- stringr::str_subset(colnames(curated_analytes_table())[-1],
                                              "Include",
                                              negate = TRUE)
        checkbox_ids <- paste0("checkbox", charge_columns)
        
        to_check <- curated_analytes_table() %>% 
          dplyr::filter(dplyr::if_any(tidyselect::all_of(charge_columns),
                                      ~ .x == "Yes")) %>% 
          dplyr::pull(analyte)
        
        to_check_indices <- which(curated_analytes_table()$analyte %in% to_check)
        
        ids_to_check <- sapply(paste0("checkbox", charge_columns), 
                               paste0,
                               to_check_indices) %>% 
          c()
        
        purrr::map(ids_to_check,
                   ~ updateCheckboxInput(session = session,
                                         inputId = .x,
                                         value = TRUE))
      } else {
        charge_columns <- stringr::str_subset(colnames(curated_analytes_table())[-1],
                                              "Include",
                                              negate = TRUE)
        
        purrr::map(charge_columns,
                   function(charge_column) {
                     purrr::map(1:nrow(curated_analytes_table()),
                                function(row_index) {
                                  updateCheckboxInput(session = session,
                                                      inputId = paste0("checkbox", 
                                                                       charge_column, 
                                                                       row_index),
                                                      value = dplyr::if_else(
                                                        curated_analytes_table()[[row_index, charge_column]] == "Yes",
                                                        TRUE,
                                                        FALSE)
                                  )
                                })
                   })
      }
      
    }) %>% bindEvent(input$check_all)
      
    output$table <- DT::renderDT(server = FALSE, expr = {
      req(curated_analytes_table())
      
      create_analyte_curation_table(dataframe_for_table = curated_analytes_table())
    })
    
    
    analytes_to_include <- reactive({
      req(curated_analytes_table())
      
      charge_columns <- stringr::str_subset(colnames(curated_analytes_table())[-1],
                                            "Include",
                                            negate = TRUE)
      
      analytes_to_include_per_charge <- rlang::set_names(charge_columns) %>% 
        purrr::map_dfc(.,
                       function(charge_column) {
                         checkbox_values <- retrieve_shinyInput_values(
                           inputId = paste0("checkbox", charge_column),
                           number_of_inputs = nrow(curated_analytes_table())
                         )
                         ifelse(checkbox_values,
                                curated_analytes_table()$analyte,
                                NA)
                       }) %>% 
        tidyr::pivot_longer(
          tidyselect::everything(),
          names_to = "charge",
          values_to = "analyte"
        ) %>% 
        dplyr::filter(!is.na(analyte)) %>% 
        dplyr::mutate(dplyr::across(analyte, as.character))
      
      
      # Test if analytes_to_include_per_charge is empty.
      # This is the case when the cluster tab has not yet been clicked.
      to_return <- if (nrow(analytes_to_include_per_charge) > 0) {
        analytes_to_include_per_charge
      } else {
        curated_analytes_table() %>% 
          dplyr::select(., "analyte", charge_columns) %>% 
          tidyr::pivot_longer(., cols = charge_columns, names_to = "charge") %>% 
          dplyr::filter(., value == "Yes") %>% 
          dplyr::select(., -value)
      }
      
      return(to_return)
    })
    
    
    # Remove "Curating analytes..." spinner
    observe({
      req(curated_analytes_table())
      shinybusy::remove_modal_spinner()
    })

    
    return(list(plot = curated_analytes_plot,
                analytes_to_include = analytes_to_include))
    
  })
}
