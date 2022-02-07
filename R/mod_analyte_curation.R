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
            fileInput(ns("analyte_list"), "Upload file with analyte list"),
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
            radioButtons(ns("output_format"),
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
    # are all combinations of sample_types and groups that are present in the
    # data.
    observeEvent(x$data, {
      combinations <- expand.grid(sample_type = unique(x$data$sample_type),
                                  group = unique(x$data$group))
      combination_strings <- purrr::pmap_chr(combinations,
                                             function(sample_type, group) {
                                               paste(group,
                                                     sample_type,
                                                     "samples")
                                             })
      options <- c(paste("all", unique(x$data$sample_type), "samples"), 
                   paste("all", unique(x$data$group), "samples"))
      updateSelectizeInput(inputId = "ignore_samples",
                           choices = c("", options))
    })
 
  })
}
    
## To be copied in the UI
# mod_analyte_curation_ui("analyte_curation_ui_1")
    
## To be copied in the server
# mod_analyte_curation_server("analyte_curation_ui_1")
