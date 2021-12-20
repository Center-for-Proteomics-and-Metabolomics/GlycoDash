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
            fileInput(ns("metadata"), "Upload a metadata Excel file:"),
            fileInput(ns("plate_design"), "Upload a plate design Excel file:")
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
            actionButton(ns("add_metadata"), "Add the metadata")
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
    
    r$mod_01 <- reactiveValues()
    x <- reactiveValues()
    
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
    
    observe({
      req(input$lacytools_summary)
      shinyFeedback::feedbackWarning("lacytools_summary",
                                     ext_lacytools_summary() != "txt",
                                     text = "Please upload a .txt file.")
    })
    
    observe({
      req(input$metadata)
      shinyFeedback::feedbackWarning("metadata",
                                     !(tools::file_ext(input$metadata$name) %in% c("xlsx", "xls")),
                                     text = "Please upload a .xlsx or .xls file.")
    })
    
    observe({
      req(input$plate_design)
      shinyFeedback::feedbackWarning("plate_design",
                                     !(tools::file_ext(input$plate_design$name) %in% c("xlsx", "xls")),
                                     text = "Please upload a .xlsx or .xls file.")
    })
    
    observe({
      shinyjs::toggleState(id = "read_summary", 
                           !is.null(input$lacytools_summary))
    })
    
    observe({
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
    
    observe({
      shinyjs::toggleState(id = "add_plate_design", 
                           !is.null(input$plate_design))
    })
    
    observe({
      shinyjs::toggleState(id = "add_plate_design",
                           ext_plate_design() %in% c("xlsx", "xls"))
    })
    
    plate_design <- eventReactive(input$add_plate_design, {
      plate_design <- read_and_process_plate_design(input$plate_design$datapath)
      return(plate_design)
    })
    
    output$group <- DT::renderDataTable({
      groups <- data.frame(unique(plate_design()$sample_type))
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
    
    observeEvent(plate_design(), {
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
    
    empty_data <- data.frame("group" = c("test", "test2", "test3"),
                               "pattern" = c("bla", "", ""))
    
    x$manual_groups <- empty_data
    
    
    output$test <- DT::renderDataTable({
      
      table <- DT::datatable(empty_data,
                    options = list(
                      scrollY = "150px",
                      paging = FALSE,
                      searching = FALSE),
                    rownames = FALSE,
                    editable = "cell")
      return(table)
      
    })
    
    proxy <- DT::dataTableProxy("test")
    
    observeEvent(input$test_cell_edit, {
      info <- input$test_cell_edit
      str(info)
      empty_data <<- DT::editData(empty_data, info, "proxy")
      x$manual_groups <- empty_data
      
    })
    
    observeEvent(x$empty_data, {
      print(x$empty_data)
    })
    
    observeEvent(x$response, {
      if (x$response) {
         x$data <- dplyr::left_join(x$data, plate_design())
         print("Data has been updated")
      } else {
        shinyjs::delay(381,
                       shinyalert::shinyalert(
                         html = TRUE,
                         text = tagList(
                           DT::dataTableOutput(ns("test"))
                         ),
                         size = "m",
                         confirmButtonText = "Enter groups",
                         showCancelButton = TRUE,
                         cancelButtonText = "Cancel adding sample ID's"
                         )
                       )
        
      }
    })
    
    
    
    
    
  })
}
    
## To be copied in the UI
# mod_data_import_ui("data_import_ui_1")
    
## To be copied in the server
# mod_data_import_server("data_import_ui_1")
