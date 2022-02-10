#' analyte_curation UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_analyte_curation_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidPage(
      fluidRow(
        h1("Analyte curation")
      ),
      fluidRow(
        column(
          width = 6,
          shinydashboard::box(
            title = "Method for analyte curation",
            width = NULL,
            solidHeader = TRUE,
            status = "primary",
            selectInput(ns("method"), 
                        "Choose method for analyte curation:",
                        choices = c("Supply an analyte list", 
                                    "Curate analytes based on data")),
            fileInput(ns("analyte_list"), "Upload an Excel file or R object with an analyte list"),
            div(
              id = ns("curation_based_on_data"),
              selectizeInput(ns("ignore_samples"),
                          "Sample types to ignore regarding analyte curation:",
                          choices = c("Total", "Blanks", "Negative controls"),
                          multiple = TRUE),
              numericInput(ns("cut_off"), "Cut-off (%)", value = 25)
              ),
            actionButton(ns("curate_analytes"), 
                         "Perform analyte curation")
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
                           "Download analyte-curated data")
          )
        ),
        column(
          width = 6,
          shinydashboard::box(
            title = "Information on analyte curation",
            width = NULL,
            solidHeader = TRUE,
            status = "primary",
            tableOutput(ns("info_table"))
          )
        )
      )
    )
  )
}
    
#' analyte_curation Server Functions
#'
#' @noRd 
mod_analyte_curation_server <- function(id, results_spectra_curation){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    x <- reactiveValues()
    
    observe({
      req(results_spectra_curation$curated_spectra())
      x$data <- results_spectra_curation$curated_spectra()
      print(x$data)
    })
    
    observe({
      shinyjs::toggle("analyte_list", 
                      condition = input$method == "Supply an analyte list")
      shinyjs::toggle("curation_based_on_data", 
                      condition = input$method == "Curate analytes based on data")
      shinyjs::toggleState("curate_analytes", 
                           condition = 
                             (input$method == "Supply an analyte list" & 
                             !is.null(input$analyte_list)) | 
                             (input$method == "Curate analytes based on data") &
                             !is.null(input$ignore_samples))
    })
    
    output$info_table <- renderTable({shinipsum::random_table(3, 3)})
    
    # The selection menu for input$ignore_samples is updated so that the choices
    # are sample_types and groups that are present in the data.
    observeEvent(x$data, {
      options <- c(paste(unique(x$data$sample_type), "samples"), 
                   paste(unique(x$data$group), "samples"))
      updateSelectizeInput(inputId = "ignore_samples",
                           choices = c("", options))
    })
    
    ext_analyte_list <- reactive({
      req(input$analyte_list)
      ext <- tools::file_ext(input$analyte_list$name)
      return(ext)
    })
    
    # Read in the analyte list when it is uploaded, or show a feedbackWarning if
    # it's the wrong filetype:
    observeEvent(input$analyte_list, {
      req(ext_analyte_list())
      if (ext_analyte_list() == "rds") {
        x$analyte_list <- load_and_assign(input$analyte_list$datapath)
      } else { if (ext_analyte_list() %in% c("xlsx", "xls")) {
        x$analyte_list <- readxl::read_excel(input$analyte_list$datapath,
                                             col_names = FALSE)
      } 
      }
      
      if (is.data.frame(x$analyte_list)) {
        x$analyte_list <- x$analyte_list$...1
      }
      
      shinyFeedback::feedbackWarning(inputId = "analyte_list", 
                                     show = !(ext_analyte_list() %in% c("rds", "xlsx", "xls")),
                                     text = "Please upload a .xlsx, .xls or .rds file.")
    })
    
    observeEvent(input$curate_analytes, {
      
      if (input$method == "Curate analytes based on data") {
        groups_to_ignore <- stringr::str_extract(string = input$ignore_samples,
                                                 pattern = paste0(unique(x$data$group),
                                                                  collapse = "|"))
        
        sample_types_to_ignore <- stringr::str_extract(string = input$ignore_samples,
                                                       pattern = paste0(unique(x$data$sample_type),
                                                                        collapse = "|"))
        
        # Perform analyte curation:
        curated_analytes <- curate_analytes(data = x$data,
                                            groups_to_ignore = groups_to_ignore,
                                            sample_types_to_ignore = sample_types_to_ignore,
                                            cut_off_percentage = input$cut_off)
        
        x$analyte_curated_data <- dplyr::left_join(curated_analytes,
                                                   x$data)
      } else { if (input$method == "Supply an analyte list") {
        
        analytes_to_include <- purrr::map(x$analyte_list,
                                          function(analyte) {
                                            stringr::str_subset(string = unique(x$data$analyte),
                                                                pattern = paste0(analyte, 
                                                                                 "$"))
                                          }) %>% 
          unlist(.)
        
        x$analyte_curated_data <- x$data %>% 
          dplyr::filter(analyte %in% analytes_to_include)
      }
      }
      
    })
    
    # Make downloading analyte_curated_data possible:
    output$download <- downloadHandler(
      filename = function() {
        todays_date <- paste0(stringr::str_replace_all(Sys.Date(),
                                                       pattern = "-",
                                                       replacement = ""))
        switch(input$download_format,
               "R object" = paste0(todays_date, "_curated_analytes.rds"),
               "Excel file" = paste0(todays_date, "_curated_analytes.xlsx"))
      },
      content = function(file) {
        data_to_download <- x$analyte_curated_data
        switch(input$download_format,
               "R object" = save(data_to_download, 
                                 file = file),
               "Excel file" = writexl::write_xlsx(data_to_download, 
                                                  path = file))
      }
    )
 
  })
}
    
## To be copied in the UI
# mod_analyte_curation_ui("analyte_curation_ui_1")
    
## To be copied in the server
# mod_analyte_curation_server("analyte_curation_ui_1")
