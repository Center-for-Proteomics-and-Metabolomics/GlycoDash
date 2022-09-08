#' data_import UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_data_import_ui <- function(id){
  ns <- NS(id)
  tagList(
    shinyFeedback::useShinyFeedback(),
    bsplus::use_bs_popover(),
    fluidPage(
      fluidRow(
        h1("Data Import")
      ),
      fluidRow(
        column(
          width = 6,
          mod_read_lacytools_ui(ns("read_lacytools_ui_1")),
          mod_add_sample_ids_ui(ns("add_sample_ids_ui_1")),
          mod_add_sample_types_ui(ns("add_sample_types_ui_1")),
          mod_clusters_ui(ns("clusters_ui_1")),
          mod_add_metadata_ui(ns("add_metadata_ui_1"))
        ),
        column(
          width = 6,
          shinydashboard::box(
            title = "View the converted data",
            width = NULL,
            solidHeader = TRUE,
            status = "primary",
            DT::DTOutput(ns("data_table"))
          ),
          shinydashboard::box(
            title = "Export results",
            width = NULL,
            solidHeader = TRUE,
            status = "primary",
            radioButtons(ns("download_format"),
                         "Choose a file format:",
                         choices = c("Excel file", "R object")),
            downloadButton(ns("download"), 
                           "Download data")
          )
        )
      )
    )
  )
}
    
#' data_import Server Functions
#'
#' @noRd 
mod_data_import_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    summary <- mod_read_lacytools_server("read_lacytools_ui_1")
    
    data_incl_sample_ids <- mod_add_sample_ids_server("add_sample_ids_ui_1",
                                                      keyword_specific = summary$keyword_specific,
                                                      keyword_total = summary$keyword_total,
                                                      Ig_data = summary$Ig_data,
                                                      summary = summary$data,
                                                      lacytools_fileInput = summary$lacytools_fileInput,
                                                      read_lacytools_button = summary$button)
    
    data_incl_sample_types <- mod_add_sample_types_server("add_sample_types_ui_1",
                                                          summary = data_incl_sample_ids$data,
                                                          read_lacytools_button = summary$button,
                                                          sample_ids_button = data_incl_sample_ids$button)
    
    data_incl_clusters <- mod_clusters_server("clusters_ui_1",
                                              summary = data_incl_sample_types$data)
    
    data_incl_metadata <- mod_add_metadata_server("add_metadata_ui_1",
                                                  summary = data_incl_clusters$data)
    
    
    show_in_table <- reactive({
      req(summary$data())
      
      if (is_truthy(data_incl_metadata$data())) {
        show_in_table <- data_incl_metadata$data()
      } else {
        if (is_truthy(data_incl_clusters$data())) {
          show_in_table <- data_incl_clusters$data()
          showNotification("The clusters are being added to the data. This may take a while",
                           type = "message")
        } else {
          if (is_truthy(data_incl_sample_types$data())) {
            show_in_table <- data_incl_sample_types$data()
            showNotification("The sample types were added to the data",
                             type = "message")
          } else { 
            if (is_truthy(data_incl_sample_ids$data())) {
              show_in_table <- data_incl_sample_ids$data()
              showNotification("The sample ID's were added to the data",
                               type = "message")
            } else {
              if (is_truthy(summary$data())) {
                show_in_table <- summary$data()
                showNotification("The LacyTools summary has been loaded.",
                                 type = "message")
              }
            } 
          }
        }
      }
      return(show_in_table)
    }) %>% bindEvent(summary$button(), 
                    data_incl_sample_ids$button(),
                    data_incl_sample_types$button(),
                    data_incl_clusters$button(),
                    data_incl_metadata$button())
    
    # When the lacytools summary has been read in, the converted data is shown
    # in the data table
    output$data_table <- DT::renderDT({
      req(show_in_table())
      
      DT::datatable(show_in_table(),
                    options = list(scrollX = TRUE),
                    filter = "top")
    })
    
    to_return <- reactive({
      if (is_truthy(data_incl_metadata$data())) {
        data_incl_metadata$data()
      } else {
        if (is_truthy(data_incl_clusters$data())) {
          data_incl_clusters$data()
        } else { 
          NULL
        }
      }
    })
    
    # The download button is disabled until data has been loaded:
    observe({
      shinyjs::toggleState("download",
                           condition = is_truthy(show_in_table()))
    })
    
    output$download <- downloadHandler(
      filename = function() {
        todays_date <- paste0(stringr::str_replace_all(Sys.Date(),
                                                       pattern = "-",
                                                       replacement = ""))
        switch(input$download_format,
               "R object" = paste0(todays_date, "_data.rds"),
               "Excel file" = paste0(todays_date, "_data.xlsx"))
      },
      content = function(file) {
        data_to_download <- show_in_table()
        
        switch(input$download_format,
               "R object" = save(data_to_download, 
                                 file = file),
               "Excel file" = writexl::write_xlsx(data_to_download, 
                                                  path = file))
      }
    )
    
    return(list(
      summary = to_return,
      Ig_data = summary$Ig_data,
      keyword_specific = summary$keyword_specific,
      keyword_total = summary$keyword_total,
      filename_summary = reactive({ summary$lacytools_fileInput()$name }),
      filenames_plate_design = data_incl_sample_ids$filenames_plate_design,
      filename_sample_list = data_incl_sample_ids$filename_sample_list,
      filenames_metadata = data_incl_metadata$filenames_metadata,
      sample_types_method = data_incl_sample_types$method,
      filename_sample_types = data_incl_sample_types$filename_sample_types
    ))
    
  })
}

## To be copied in the UI
# mod_data_import_ui("data_import_ui_1")

## To be copied in the server
# mod_data_import_server("data_import_ui_1")
