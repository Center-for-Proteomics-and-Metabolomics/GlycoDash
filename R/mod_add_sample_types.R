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
        mod_process_sample_type_file_ui(
          ns("process_sample_type_file_ui_1"),
          fileInput_label = "Upload an Excel file with your sample types:",
          popover_width = "400px",
          popover_title = "Format of sample type list",
          popover_content_html = ""
        )
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
    
    observe({
      shinyjs::toggleState(id = "button",
                           condition = any(
                             input$method == "Automatically determine sample types based on sample ID's",
                             all(
                               input$method == "Upload a list with sample ID's and corresponding sample types",
                               is_truthy(manual_sample_types())
                             )
                           ))
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
    
    manual_sample_types <- mod_process_sample_type_file_server("process_sample_type_file_ui_1",
                                                               allowed = c("rds", "xlsx", "xls"))
    
    with_manual_sample_types <- reactive({
      req(manual_sample_types(),
          summary(),
          input$method == "Upload a list with sample ID's and corresponding sample types")
      
      dplyr::left_join(summary(),
                       manual_sample_types())
    })
    
    # Show popup with automatically determined sample types if automatic method
    # is chosen and button is clicked:
    observe({
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
    }) %>% bindEvent(input$button)
    
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
    
    return(to_return)
    
  })
}
    
## To be copied in the UI
# mod_add_sample_types_ui("add_sample_types_ui_1")
    
## To be copied in the server
# mod_add_sample_types_server("add_sample_types_ui_1")
