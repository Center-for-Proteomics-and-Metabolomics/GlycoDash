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
  tagList(
    tags$style(HTML(paste0(
      "#",
      ns("box_header"),
      " .awesome-checkbox {padding-top: 7px}",
      "#",
      ns("box_header"),
      " .popover {max-width: 400px !important; color: #333}",
      "#",
      ns("box"),
      " .box-title {width: 100%}",
      "#",
      ns("box_header"),
      " .fa {float: right; margin-right: 5px; font-size: 18px}",
      "#",
      ns("box_header"),
      " .direct-chat-contacts {right: 0; background: #222d32!important}",
      "#",
      ns("box_header"),
      " .btn {float: right; border-width: 0px; margin-right: 10px}",
      "#",
      ns("box"),
      " .dropdown {display: inline-block; float: right; width: 330px}",
      "#",
      ns("box_header"),
      " .dropdown-menu {background: #333; right: -30px; left: auto; top: 28px;}"
    ))
    ),
    shinydashboardPlus::box(
      id = ns("box"),
      title = div(
        id = ns("box_header"),
        "Add sample types",
        icon("info-circle",
             class = "ml") %>% 
          bsplus::bs_embed_popover(
            title = "Explanation",
            content = HTML(paste0(
              tags$p(paste(
                "Your samples need to be divided into categories (sample types)",
                "like blanks, negative controls, patients, standards etc.")),
              tags$p("This step cannot be skipped, because sample types are needed in later steps.")
            )),
            # Don't use body = container here, because then the custom CSS
            # styling for .popover won't be applied
            trigger = "hover", # if trigger = "focus" use tabindex: 0 on icon
            placement = "right",
            html = "true"),
        shinyWidgets::dropdownButton(
          tags$style(HTML(paste0(
            "#",
            ns("dropdown_content"),
            " .fa {float: left}",
            "#",
            ns("dropdown_content"),
            " .btn {float: none; border-width: 1px; width: 280px; margin: 10px}"
          ))),
          div(id = ns("dropdown_content"),
              downloadButton(ns("download_ex_sample_types"),
                             "Download a sample types example file")),
          icon = icon("paperclip",
                      class = "ml"),
          tooltip = shinyWidgets::tooltipOptions(placement = "top",
                                                 title = "Examples"),
          width = "330px",
          size = "xs"
        )),
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
            popover_content_html = HTML(paste0(
              tags$p(paste(
                "The Excel file should contain only one sheet.",
                "This sheet should contain one column named \"sample_id\"",
                "and one column named \"sample_type\". The \"sample_id\" column should",
                "contain all your sample ID's including blanks and standards.",
                "The \"sample_type\" column should contain the corresponding sample",
                "type of each sample."
              )),
              tags$p("For an example, click on the paperclip icon.")
            ))
          )
      ),
      actionButton(ns("button"),
                   "Determine the sample types")
    )
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
                           condition = all(
                             any(
                               input$method == "Automatically determine sample types based on sample ID's",
                               all(
                                 input$method == "Upload a list with sample ID's and corresponding sample types",
                                 is_truthy(manual_sample_types())
                               )
                             ),
                             is_truthy(summary())
                           ))
    })
    
    r <- reactiveValues()
    
    observe({
      r$with_auto_sample_types <- NULL
      shinyjs::reset("popup")
      print("r$with_auto_sample_types was reset")
    }) %>% bindEvent(!is_truthy(summary()))
    
    
    observe({
      req(summary(),
          input$method == "Automatically determine sample types based on sample ID's")
      
      r$with_auto_sample_types <- summary() %>% 
        tidyr::extract(col = sample_id,
                       into = c("sample_type"),
                       regex = "([[:alpha:]]+)",
                       remove = FALSE)
    })
    
    # with_auto_sample_types <- reactive({
    #   req(summary(),
    #       input$method == "Automatically determine sample types based on sample ID's")
    #   
    #   summary() %>% 
    #     tidyr::extract(col = sample_id,
    #                    into = c("sample_type"),
    #                    regex = "([[:alpha:]]+)",
    #                    remove = FALSE)
    # })
    
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
      req(r$with_auto_sample_types)
      
      shinyalert::shinyalert(
        inputId = "popup",
        html = TRUE,
        text = tagList(
          paste("Based on the sample IDs the following",
                length(unique(r$with_auto_sample_types$sample_type)),
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
      sample_types <- data.frame(unique(isolate(r$with_auto_sample_types$sample_type)))
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
        r$with_auto_sample_types
        
      } else {
        req(with_manual_sample_types())
        with_manual_sample_types()
      }
    }) #%>% bindEvent(input$button,
    #                  input$popup)
    
    observe({
      print("to_return():")
      print(is_truthy(to_return()))
      print(req(to_return()))
    })
    
    
    output$download_ex_sample_types <- downloadHandler(
      filename = "Example sample types file.xlsx",
      content = function(file) {
        example_file <- system.file("app",
                                    "www",
                                    "Example sample types file.xlsx",
                                    package = "glycodash")
        file.copy(example_file, file)
      }
    )
    
    return(list(
      data = to_return,
      button = reactive({input$button}),
      popup = reactive({input$popup})
      ))
    
  })
}
    
## To be copied in the UI
# mod_add_sample_types_ui("add_sample_types_ui_1")
    
## To be copied in the server
# mod_add_sample_types_server("add_sample_types_ui_1")
