#' spectra_curation UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_spectra_curation_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidPage(
      fluidRow(
        h1("Spectra curation") 
      ),
      fluidRow(
        column(
          width = 6,
          shinydashboard::box(
            title = "Spectra curation",
            width = NULL,
            solidHeader = TRUE,
            status = "primary",
            numericInput(ns("n_clusters"), 
                         "How many clusters does your data contain?",
                         value = 1),
            uiOutput(ns("clusters")),
            shinydashboard::box(
              title = "Quality criteria",
              width = NULL,
              status = "primary",
              solidHeader = TRUE,
              sliderInput(ns("mass_accuracy"), 
                          "Acceptable mass accuracy range:",
                          min = -50,
                          max = 50,
                          value = c(-20, 20)
              ),
              numericInput(ns("IPQ"),
                           "Max. IPQ value:",
                           value = 0.2,
                           step = 0.1),
              numericInput(ns("sn"),
                           "Min. S/N ratio:",
                           value = 9)
            ),
            selectInput(ns("cut_off_basis"),
                        "Base the spectra curation cut-off on:",
                        choices = c("Negative controls; specific",
                                    "Blanks")
            ),
            actionButton(ns("curate_spectra"),
                         "Perform spectra curation")
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
                         "Download curated spectra")
          )
        ),
        column(
          width = 6,
          shinydashboard::box(
            title = "Information on spectra curation",
            width = NULL,
            solidHeader = TRUE,
            status = "primary",
            tableOutput(ns("p")),
            tableOutput(ns("fail_table"))
          )
        )
      )
    )
  )
}
    
#' spectra_curation Server Functions
#'
#' @noRd 
mod_spectra_curation_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    # Create inputIds for the cluster textInputs based on the 
    # value of input$n_clusters:
    cluster_inputIds <- reactive({
      req(input$n_clusters)
      cluster_inputIds <- purrr::map(seq_len(input$n_clusters),
                                       ~ paste0("cluster", .x))
      return(cluster_inputIds)
    })
    
    output$clusters <- renderUI({
      req(cluster_inputIds())
        purrr::imap(cluster_inputIds(),
                    function(inputId, i) textInput(
                      ns(inputId),
                      label = paste("By what words/letters in the analyte name can the analytes belonging to cluster",
                                    i,
                                    "be recognized?")
                      ))
    })
    
    output$p <- renderTable({shinipsum::random_table(3, 3)})
    output$fail_table <- renderTable({shinipsum::random_table(3, 3)})
  })
}
    
## To be copied in the UI
# mod_spectra_curation_ui("spectra_curation_ui_1")
    
## To be copied in the server
# mod_spectra_curation_server("spectra_curation_ui_1")
