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
    fluidPage(
      fluidRow(
        h1("Data Import")
      ),
      fluidRow(
        column(
          width = 6,
          shinydashboard::box(
            title = "Upload your files",
            width = NULL,
            fileInput(ns("lacytools_summary"), "Upload LacyTools summary.txt file:"),
            fileInput(ns("plate_design"), "Upload a plate design Excel file:"),
            fileInput(ns("metadata"), "Upload a metadata Excel file:")
          ),
          shinydashboard::box(
            title = "Read and convert data",
            width = NULL,
            actionButton(ns("read_summary"), "Convert the LacyTools summary file to an R-suitable format"),
            br(),
            br(),
            actionButton(ns("add_plate_design"), "Add sample ID's from the plate design to the data"),
            br(),
            br(),
            div(
              id = ns("metadata_menu"),
              selectizeInput(ns("sample_id_column"),
                          "Which column in the metadata contains the sample ID's?",
                          choices = NULL,
                          options = list(placeholder = "select a column")),
              selectInput(ns("date_columns"),
                          "Which columns in the metadata contain dates?",
                          choices = c(""),
                          multiple = TRUE)
            ),
            actionButton(ns("add_metadata"), "Add the metadata"),
          )
        ),
        column(
          width = 6,
          shinydashboard::box(
            title = "View the converted data",
            width = NULL,
            DT::dataTableOutput(ns("data_table"))
          )
        )
      )
    )
  )
}
    
#' data_import Server Functions
#'
#' @noRd 
mod_data_import_server <- function(id, r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    x <- reactiveValues()
    
    observe({
      shinyjs::toggle("metadata_menu", condition = !is.null(x$metadata))
    })
    
    ext_lacytools_summary <- reactive({
      req(input$lacytools_summary)
      ext <- tools::file_ext(input$lacytools_summary$name)
      return(ext)
    })
    
    ext_plate_design <- reactive({
      req(input$plate_design)
      ext <- tools::file_ext(input$plate_design$name)
      return(ext)
    })
    
    ext_metadata <- reactive({
      req(input$metadata)
      ext <- tools::file_ext(input$metadata$name)
      return(ext)
    })
    
    observe({
      req(input$lacytools_summary)
      shinyFeedback::feedbackWarning("lacytools_summary",
                                     ext_lacytools_summary() != "txt",
                                     text = "Please upload a .txt file.")
    })
    
    observe({
      req(ext_metadata())
      if (ext_metadata() %in% c("xlsx", "xls")) {
        x$metadata <- read_metadata(input$metadata$datapath)
      } else {
        shinyFeedback::feedbackWarning("metadata",
                                     show = TRUE,
                                     text = "Please upload a .xlsx or .xls file.")
      }
    })
    
    observe({
      req(input$plate_design)
      shinyFeedback::feedbackWarning("plate_design",
                                     !(ext_plate_design() %in% c("xlsx", "xls")),
                                     text = "Please upload a .xlsx or .xls file.")
    })
    
    observe({
      shinyjs::toggleState(id = "read_summary", 
                           !is.null(input$lacytools_summary))
      shinyjs::toggleState(id = "read_summary",
                           ext_lacytools_summary() == "txt")
    })
    
    data <- eventReactive(input$read_summary, {
      read_lacytools_summary(input$lacytools_summary$datapath)
    })
    
    observe({
      x$data <- data()
      })
    
    output$data_table <- DT::renderDT({
      req(x$data)
        DT::datatable(x$data, options = list(scrollX = TRUE))
    })
    
    # This observe call ensures that the add_plate_design actionButton is only
    # enabled under the right circumstances
    observe({
      shinyjs::disable(id = "add_plate_design")
      if (all(isTruthy(x$data), isTruthy(input$plate_design))) {
        if (ext_plate_design() %in% c("xlsx", "xls")) {
          shinyjs::enable(id = "add_plate_design")
        }
      }
    })
    
    # This observe call ensures that the add_metadata actionButton is only
    # enabled under the right circumstances
    observe({
      shinyjs::toggleState(id = "add_metadata",
                           condition = all(isTruthy(x$data), 
                                           "sample_id" %in% colnames(x$data),
                                           isTruthy(x$metadata),
                                           isTruthy(input$sample_id_column)))
    })
    
    observeEvent(input$add_plate_design, {
      x$plate_design <- read_and_process_plate_design(input$plate_design$datapath)
      shinyalert::shinyalert(
        html = TRUE,
        text = tagList(
          "Based on the sample IDs the following groups were defined:",
          DT::dataTableOutput(ns("group"))
        ),
        size = "m",
        confirmButtonText = "Accept these groups",
        showCancelButton = TRUE,
        cancelButtonText = "Manually enter groups",
        callbackR = function(y) {
          x$response <- y
        }
      )
    })
    
    output$group <- DT::renderDataTable({
      groups <- data.frame(unique(x$plate_design$sample_type))
      groups_tbl <- DT::datatable(groups,
                                  options = list(
                                    scrollY = "150px",
                                    paging = FALSE,
                                    searching = FALSE,
                                    columnDefs = list(
                                      list(
                                        className = 'dt-center', 
                                        targets = "_all"))),
                                  colnames = "Sample type",
                                  rownames = FALSE
                                  )
      return(groups_tbl)
    })
    
    observeEvent(x$response, {
      if (x$response) {
         x$data <- dplyr::left_join(x$data, x$plate_design)
         print("Data has been updated")
      } else {
        
        shinyjs::delay(381,
                       shinyalert::shinyalert(
                         html = TRUE,
                         text = tagList(
                           tags$b("Upload an Excel file or an R object (.rds) that contains:"),
                           tags$ul(
                             tags$li(tags$span("a column named \"sample_id\" with all sample ID's for the data")),
                             tags$li(tags$span("a column named \"group\" with the corresponding group that the sample belongs to"))
                           ),
                           fileInput(ns("groups"), label = "Upload file:")
                         ),
                         size = "s",
                         confirmButtonText = "Enter groups",
                         showCancelButton = TRUE,
                         cancelButtonText = "Cancel adding sample ID's",
                         callbackR = function(y) {
                           x$response_2 <- y
                         }
                         )
                       )
      }
    })
    
    ext_groups <- reactive({
      req(input$groups)
      ext <- tools::file_ext(input$groups$name)
      return(ext)
    })
    
    observe({
      req(ext_groups())
      if (ext_groups() == "rds") {
        load(file = input$groups$datapath)
        x$groups <- groups
      } else { if (ext_groups() %in% c("xlsx", "xls")) {
        x$groups <- readxl::read_excel(input$groups$datapath)
        } else {
          shinyFeedback::feedbackWarning("groups", 
                                         show = TRUE,
                                         text = "Please upload a .xlsx, .xls or .rds file.")
          }
        }
    })
    
    observeEvent(x$response_2, {
      req(x$groups)
      if (x$response_2) {
        x$plate_design <- x$plate_design %>% 
          dplyr::select(-sample_type)
        x$groups_and_plate_design <- dplyr::full_join(x$plate_design, x$groups)
        x$groups_and_plate_design <- unique(x$groups_and_plate_design)
        x$data <- dplyr::left_join(data(), x$groups_and_plate_design)
        print("Data has been updated")
      }
    })
    
    observeEvent(x$metadata, {
      updateSelectizeInput(inputId = "sample_id_column",
                           choices = unique(colnames(x$metadata)),
                           selected = "",
                           server = TRUE)
      updateSelectInput(inputId = "date_columns",
                        choices = unique(colnames(x$metadata)),
                        selected = stringr::str_subset(colnames(x$metadata),
                                                       pattern = stringr::regex("date",
                                                                                ignore_case = TRUE)))
    })
    
    observeEvent(input$add_metadata, {
      x$metadata <- x$metadata %>% 
        dplyr::mutate(dplyr::across(tidyselect::any_of(input$date_columns), 
                                    date_with_text)) %>% 
        dplyr::rename(sample_id = input$sample_id_column)
      x$data <- dplyr::left_join(x$data, x$metadata)
      })
    
  })
}
    
## To be copied in the UI
# mod_data_import_ui("data_import_ui_1")
    
## To be copied in the server
# mod_data_import_server("data_import_ui_1")
