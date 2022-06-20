#' information_box UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_information_box_ui <- function(id){
  ns <- NS(id)
  tagList(
    plotly::plotlyOutput(ns("plot")),
    #plotOutput(ns("plot")),
    br(),
    DT::dataTableOutput(ns("table"))
  )
}
    
#' information_box Server Function
#'
#' @noRd 
mod_information_box_server <- function(id, info, cluster){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    info_plot <- reactive({
      req(info$curated_analytes())
      req(info$cut_off())
      
      plot_analyte_curation(curated_analytes = info$curated_analytes(),
                            cut_off_percentage = info$cut_off(),
                            selected_cluster = cluster)
    })
    
    output$plot <- plotly::renderPlotly({
      req(info_plot())
      plotly_object <- plotly::ggplotly(info_plot(), tooltip = "text")
      
      plotly_object[["x"]][["layout"]][["annotations"]][[2]][["xshift"]] <- -50
      
      plotly_object[["x"]][["layout"]][["annotations"]][[1]][["yshift"]] <- -90
      
      return(plotly_object)
      
    })
    
    # create a character vector of shiny inputs
    shinyInput <- function(FUN, len, id, values) {
      inputs <- character(len)
      for (i in seq_len(len)) {
        inputs[i] <- as.character(FUN(ns(paste0(id, i)), label = NULL, value = values[i]))
      }
      inputs
    }
    
    # obtain the values of inputs
    shinyValue <- function(id, len) {
      unlist(lapply(seq_len(len), function(i) {
        value <- input[[paste0(id, i)]]
        if (is.null(value)) NA else value
      }))
    }
    
    info_table <- reactive({
      req(info$analyte_curated_data())
      
      table <- prepare_analyte_curation_table(analyte_curated_data = info$analyte_curated_data(),
                                              selected_cluster = cluster)
      
      charge_columns <- colnames(table)[-1]
      
      table_with_checkboxes <- table %>% 
        dplyr::mutate(
          dplyr::across(tidyselect::all_of(charge_columns),
                        ~ shinyInput(checkboxInput,
                                     len = nrow(table),
                                     id = paste0("checkbox", dplyr::cur_column()),
                                     values = dplyr::if_else(.x == "Yes", TRUE, FALSE)),
                        .names = "Include {col} in further analysis")
        ) %>% 
        dplyr::select(analyte,
                      tidyselect::contains(charge_columns))
      return(table_with_checkboxes)
    })
      
    output$table <- DT::renderDT(server = FALSE, expr = {
      req(info_table()) 
      
      DT::datatable(
        info_table(),
        escape = FALSE,
        selection = "none",
        options = list(searching = FALSE,
                       paging = FALSE,
                       preDrawCallback = DT::JS('function() {
Shiny.unbindAll(this.api().table().node()); }'),
drawCallback = DT::JS('function() {
Shiny.bindAll(this.api().table().node()); } ')
        )
      ) %>%
        DT::formatStyle(columns = 2:ncol(info_table()),
                        color = DT::styleEqual(levels = c("Yes", 
                                                          "No"), 
                                               values = c("#3498DB", 
                                                          "#E74C3C")))
      
    })
    
    analytes_to_include <- reactive({
      req(info_table())
      
      charge_columns <- stringr::str_subset(colnames(info_table())[-1],
                                            "Include",
                                            negate = TRUE)
      
      analytes_to_include_per_charge <- rlang::set_names(charge_columns) %>% 
        purrr::map_dfc(.,
                   function(charge_column) {
                     checkbox_values <- shinyValue(paste0("checkbox", charge_column),
                                                   nrow(info_table()))
                     ifelse(checkbox_values,
                            info_table()$analyte,
                            NA)
                   }) %>% 
        tidyr::pivot_longer(
          tidyselect::everything(),
          names_to = "charge",
          values_to = "analyte"
        ) %>% 
        dplyr::filter(!is.na(analyte)) %>% 
        dplyr::mutate(dplyr::across(analyte, as.character))
      
      return(analytes_to_include_per_charge)
    })
    
    observe({
      req(analytes_to_include())
      print("analytes_to_include() looks like this:")
      print(analytes_to_include())
    })
    
    return(list(plot = info_plot,
                table = info_table,
                analytes_to_include = analytes_to_include))
    
  })
}
    
## To be copied in the UI
# mod_information_box_ui("information_box_ui_1")
    
## To be copied in the server
# mod_information_box_server("information_box_ui_1")
