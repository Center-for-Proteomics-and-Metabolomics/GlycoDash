#' add_sample_types UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_add_sample_types_ui <- function(id){
  ns <- NS(id)
  shinydashboardPlus::box(
    title = "Add sample types",
    width = NULL,
    solidHeader = TRUE,
    status = "primary",
    selectInput(ns("method"),
                "Choose a method to add sample types to your data:",
                choices = c("Automatically determine sample types based on sample ID's",
                            "Upload a list with sample ID's and corresponding sample types")) %>% 
      bsplus::bs_embed_popover(
        title = "Method to add sample types",
        content = HTML(paste0(
          tags$b("Automatically:"),
          tags$p(paste(
            "For each sample the first string of letters within the sample ID is", 
            "assumed to be the sample type."
          )),
          tags$p(paste(
            "For example, if the sample ID is",
            "\"36_patient_67b\", then the automatically determined sample type",
            "will be \"patient\"."
          )),
          tags$p(tags$b("Upload a list:"),
                 br(),
                 paste(
                   "If your sample ID's are not suitable for automatic sample type",
                   "determination, use this method instead."
                 ))
        )),
        html = "true",
        trigger = "hover",
        placement = "right"
      ),
    div(id = ns("upload_div"),
        fileInput(ns("file"),
                  "Upload an Excel file with your sample types:")
        ),
    actionButton(ns("button"),
                 "Determine the sample types")
  )
}
    
#' add_sample_types Server Functions
#'
#' @noRd 
mod_add_sample_types_server <- function(id, summary){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    observe({
      shinyjs::toggle(id = "upload_div",
                      condition = input$method == "Upload a list with sample ID's and corresponding sample types")
    })
    
    with_auto_sample_types <- reactive({
      req(summary(),
          input$method == "Automatically determine sample types based on sample ID's")
      
      summary() %>% 
        tidyr::extract(col = sample_id,
                       into = c("sample_type"),
                       regex = "([[:alpha:]]+)",
                       remove = FALSE)
    })
    
    extension <- reactive({
      req(input$file)
      tools::file_ext(input$file$name)
    })
    
    allowed <- c("rds", "xlsx", "xls")
    
    wrong_extension_warning <- paste("Please upload a",
                                     comma_or(paste0(".", allowed)),
                                     "file.")
    
    observe({
      req(extension())
      shinyFeedback::feedbackDanger("file",
                                    !(extension() %in% allowed),
                                    text = wrong_extension_warning)
    })
    
    manual_sample_types <- reactive({
      req(extension())
      
      if (extension() == "rds") {
        load_and_assign(input$file$datapath)
      } else { if (extension() %in% c("xlsx", "xls")) {
        readxl::read_excel(input$file$datapath, 
                           col_names = TRUE)
      }
      }
    })
    
    observe({
      # rewrite this into an add_sample_types function that throws an
      # error when columns are missing and adds the sample types to the data.
      # put into a reactive with tryCatch
      req(manual_sample_types())
      
      required_columns <- c("sample_id", "sample_type")
      columns_are_missing <- any(!(required_columns %in% colnames(manual_sample_types())))
      
      shinyFeedback::feedbackDanger("file",
                                    show = columns_are_missing,
                                    text = "Incorrect column names.")
      if (columns_are_missing) {
        showNotification(
          paste(
            "Please check that your file with sample types is formatted correctly.",
            "Click on the information icon to find the required format."
          ),
          type = "error",
          duration = NULL
        )
      }
    })
    
    # Show popup with automatically determined sample types if automatic method
    # is chosen and button is clicked:
    observeEvent(input$button, {
      req(with_auto_sample_types())
      
      shinyalert::shinyalert(
        inputId = "popup",
        html = TRUE,
        text = tagList(
          paste("Based on the sample IDs the following",
                length(unique(with_auto_sample_types()$sample_type)),
                "sample types were defined:"),
          DT::dataTableOutput(ns("popup_table"))
        ),
        size = "m",
        confirmButtonText = "Accept these sample types",
        showCancelButton = TRUE,
        cancelButtonText = "Cancel",
        confirmButtonCol = "#3c8dbc"
      )
    })
    
    # This datatable with the automatically determined sample_types is shown in
    # the pop-up:
    output$popup_table <- DT::renderDataTable({
      sample_types <- data.frame(unique(isolate(with_auto_sample_types()$sample_type)))
      DT::datatable(sample_types,
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
    })
    
    observe({
      req(!is.null(input$popup))
      if(!is_truthy(input$popup)) {
        updateSelectInput("method",
                          session = session,
                          selected = "Upload a list with sample ID's and corresponding sample types")
      }
    })
    
    to_return <- reactive({
      if (input$method == "Automatically determine sample types based on sample ID's") {
        req(input$popup)
        with_auto_sample_types()
        
      } else {
        req(with_manual_sample_types())
        with_manual_sample_types()
      }
    }) %>% bindEvent(input$button)
    
    observe({
      req(to_return())
      showNotification("The sample types were added to the data",
                       type = "message")
    }) %>% bindEvent(to_return())
    
    observe({
      req(to_return())
      print(to_return())
    })
    
  })
}
    
## To be copied in the UI
# mod_add_sample_types_ui("add_sample_types_ui_1")
    
## To be copied in the server
# mod_add_sample_types_server("add_sample_types_ui_1")
