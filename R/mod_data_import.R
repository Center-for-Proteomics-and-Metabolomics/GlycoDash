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
    
    output$data_table <- DT::renderDT({
      req(data())
      DT::datatable(data(), options = list(scrollX = TRUE))
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
    
    output$groups <- renderText({
      unique(plate_design()$sample_type)
    })
    
    observeEvent(plate_design(), {
      shinyalert::shinyalert(
        html = TRUE,
        text = tagList(
          "Based on the sample IDs the following groups were defined:",
          textOutput(ns("groups"), inline = TRUE)
        ),
        size = "m",
        confirmButtonText = "Accept these groups",
        showCancelButton = TRUE,
        cancelButtonText = "Manually enter groups"
      )
    })
    
  })
}
    
## To be copied in the UI
# mod_data_import_ui("data_import_ui_1")
    
## To be copied in the server
# mod_data_import_server("data_import_ui_1")
