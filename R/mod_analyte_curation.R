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
          
        )
      ),
      fluidRow(
        column(
          width = 12,
          uiOutput(ns("information"))
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
    
    clusters <- reactive({
      unique(x$data$cluster)
    })
    
    info <- list(
      curated_analytes = reactive(x$curated_analytes),
      cut_off = reactive(input$cut_off),
      analyte_curated_data = reactive(x$analyte_curated_data)
    )
    
    output$information <- renderUI({
      # req(all(purrr::map_lgl(info,
      #                        ~ isTruthy(.x()))))
      
      mod_information_box_ui(ns("information_box_ui_1"))
        
    })
    
    observe({
      # req(all(purrr::map_lgl(info,
      #                        ~ isTruthy(.x()))))
      
      mod_information_box_server("information_box_ui_1",
                                 info = info,
                                 clusters = clusters)
    })
    
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
      # x$curated_analytes <- NULL
      # x$analyte_curated_data <- NULL
      if (input$method == "Curate analytes based on data") {
        group_to_ignore <- stringr::str_extract(string = input$ignore_samples,
                                                 pattern = paste0(unique(x$data$group),
                                                                  collapse = "|")) %>% 
          na.omit(.)
        
        sample_types_to_ignore <- stringr::str_extract(string = input$ignore_samples,
                                                       pattern = paste0(unique(x$data$sample_type),
                                                                        collapse = "|")) %>% 
          na.omit(.)
        
        # Perform analyte curation:
        x$curated_analytes <- curate_analytes(data = x$data,
                                              group_to_ignore = group_to_ignore,
                                              sample_types_to_ignore = sample_types_to_ignore,
                                              cut_off_percentage = input$cut_off)
        
        passing_analytes <- x$curated_analytes %>% 
          dplyr::filter(passed_curation == TRUE) %>% 
          dplyr::select(-passed_curation)
        
        x$analyte_curated_data <- dplyr::left_join(passing_analytes, 
                                                   x$data)
        
        showNotification("Analyte curation has been performed based on the data.", 
                         type = "message")
        
      } else { if (input$method == "Supply an analyte list") {
        
        analytes_to_include <- purrr::map(x$analyte_list,
                                          function(analyte) {
                                            stringr::str_subset(string = unique(x$data$analyte),
                                                                pattern = paste0("^",
                                                                                 analyte, 
                                                                                 "$"))
                                          }) %>% 
          unlist(.)
        
        x$analyte_curated_data <- x$data %>% 
          dplyr::filter(analyte %in% analytes_to_include) %>% 
          dplyr::select(-passed_curation)
        
        showNotification("Analyte curation has been performed based on the analyte list.", 
                         type = "message")
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
    
    return(list(
      analyte_curated_data = reactive({x$analyte_curated_data})
    ))
 
  })
}
    
## To be copied in the UI
# mod_analyte_curation_ui("analyte_curation_ui_1")
    
## To be copied in the server
# mod_analyte_curation_server("analyte_curation_ui_1")
