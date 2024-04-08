#' add_sample_types UI Function
#'
#' @description A shiny Module to add sample types to your data either 
#' automatically based on the sample ID's or by uploading a sample types Excel 
#' file.
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
      "#", ns("box_header"), " .awesome-checkbox {padding-top: 7px}",
      "#", ns("box_header"), " .popover {max-width: 400px !important; color: #333}",
      "#", ns("box"), " .box-title {width: 100%}",
      "#", ns("box_header"), " .fas {float: right; margin-right: 5px; font-size: 18px}",
      "#", ns("box_header"), " .direct-chat-contacts {right: 0; background: #222d32!important}",
      "#", ns("box_header"), " .btn {float: right; border-width: 0px; margin-right: 10px}",
      "#", ns("box"), " .dropdown {display: inline-block; float: right; width: 330px}",
      "#", ns("box_header"), " .dropdown-menu {background: #333; right: -30px; left: auto; top: 28px;}"
    ))),
    shinydashboardPlus::box(
      id = ns("box"),
      title = div(
        id = ns("box_header"),
        "Add sample types",
        icon("info-circle",
             class = "ml") %>% 
          bsplus::bs_embed_popover(
            title = "Explanation",
            content = HTML(
              "
              Your samples need to be divided into categories (sample types),
              such as blanks, negative controls, patients, standards, etc.
              
              "
            ),
            # Don't use body = container here, because then the custom CSS
            # styling for .popover won't be applied
            trigger = "hover", # if trigger = "focus" use tabindex: 0 on icon
            placement = "right",
            html = "true"),
        shinyWidgets::dropdownButton(
          tags$style(HTML(paste0(
            "#",
            ns("dropdown_content"),
            " .fas {float: left}",
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
          content = HTML(
            "
            <b> Automatically </b>
            <br>
            For each sample, the first substring of letters within the sample ID is
            assumed to be the sample type. For example, if the sample ID is
            \"<i>36_patient_67b</i>\", then the automatically determined sample type 
            will be \"patient\". Sample ID's that don't contain any letters will be assigned
            \"undetermined\".
            <br> <br>
            <b> Upload a list </b>
            <br>
            If your sample ID's are not suitable for automatically determine sample types,
            use this method instead. Your list should be an Excel file that contains 
            one column called \"sample_id\", and one column called \"sample_type\". 
            Each sample ID should be present once in your file. For an example file,
            click the paperclip button.
            "
          ),
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
            popover_content_html = HTML(
              "
              The Excel file should contain only one sheet. This sheet should
              contain one column named \"sample_id\" and one column named \"sample_type\".
              The \"sample_id\" column should contain all your sample ID's, including blanks
              and standards. The \"sample_type\" column should contain the corresponding sample
              type of each sample.
              <br> <br>
              Each sample ID should be present only once in your file, even if it is present
              multiple times in your plate design.
              <br> <br>
              For an example file, click on the paperclip icon.
              "
            )
          )
      ),
      actionButton(ns("button"), "Determine the sample types")
    )
  )
}
    
#' add_sample_types Server Functions
#'
#' @noRd 
mod_add_sample_types_server <- function(id, LaCyTools_summary){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    observe({
      # Show file upload when user chooses sample type list option, and hide the button.
      if (input$method == "Upload a list with sample ID's and corresponding sample types") {
        shinyjs::show("upload_div")
        shinyjs::hide("button")
      } else {
        shinyjs::hide("upload_div")
        shinyjs::show("button")
      }
      # Toggle state of the button
      shinyjs::toggleState(
        "button", condition = all(
          input$method == "Automatically determine sample types based on sample ID's",
          is_truthy(LaCyTools_summary())
        )
      )
    })
    
    r <- reactiveValues()
    
    observe({
      if (is_truthy(r$with_auto_sample_types)) {
        r$with_auto_sample_types <- NULL
        r$response <- NULL
        r$show_reset_warning <- TRUE
      }
    }) %>% bindEvent(LaCyTools_summary())
    
    observe({
      if (is_truthy(r$show_reset_warning)) {
        showNotification("Please re-add the sample types to your data.",
                         type = "warning", duration = 10)
        r$show_reset_warning <- FALSE
      }
    }) %>% bindEvent(LaCyTools_summary())
    
    observe({
      req(LaCyTools_summary(),
          input$method == "Automatically determine sample types based on sample ID's")

      
      #TODO: convert this to a function:
      r$with_auto_sample_types <- LaCyTools_summary() %>% 
        tidyr::extract(col = sample_id,
                       into = c("sample_type"),
                       regex = "([[:alpha:]]+)",
                       remove = FALSE) %>% 
        dplyr::mutate(sample_type = ifelse(sample_id == "empty cell in plate design",
                                           "unknown",
                                           sample_type),
                      sample_type = ifelse(is.na(sample_type),
                                           "undetermined",
                                           sample_type),
                      sample_type = as.factor(sample_type))
    })
    
    manual_sample_types <- mod_process_sample_type_file_server("process_sample_type_file_ui_1",
                                                               allowed = c("rds", "xlsx", "xls"))
    
    
    
    # In the case of manual sample types: make sure that the sample IDs are unique
    duplicate_sample_ids <- reactive({
      req(manual_sample_types$list())
      sample_ids <- manual_sample_types$list()$sample_id
      all_unique <- length(sample_ids) == length(unique(sample_ids))
      if (all_unique) {
        return(NULL)
      } else {
        non_unique_ids <- sample_ids[duplicated(sample_ids)]
        return(non_unique_ids)
      }
    })
    
    observe({
      req(duplicate_sample_ids())
      shinyalert::shinyalert(
        inputId = "popup_duplicates",
        html = TRUE,
        text = paste(
          "The following sample ID's are present more than once in your file:",
          shinycssloaders::withSpinner(DT::dataTableOutput(ns("popup_table_duplicates"))),
          "<br>Please change your file such that each sample ID is present only once, and try again."
        ),
        size = "m",
        confirmButtonText = "OK",
        confirmButtonCol = "tomato",
        showCancelButton = FALSE,
        showConfirmButton = TRUE
      )
    })
    
    output$popup_table_duplicates <- render_my_datatable(
      data.frame(duplicate_sample_ids()), "Sample ID"
    )
    
    
        
    # In the case of manual sample types: check for unmatched ID's
    unmatched_sample_ids <- reactive({
      req(!is_truthy(duplicate_sample_ids()), LaCyTools_summary(), manual_sample_types$list())
      unmatched <- setdiff(LaCyTools_summary()$sample_id, manual_sample_types$list()$sample_id)
      if (rlang::is_empty(unmatched)) {
        return(NULL)
      } else return(unmatched)
    })
    
    observe({
      req(unmatched_sample_ids())
      shinyalert::shinyalert(
        inputId = "popup_unmatched",
        html = TRUE,
        text = paste(
          "The following sample ID's from your data are not present in your sample type list:",
          shinycssloaders::withSpinner(DT::dataTableOutput(ns("popup_table_unmatched"))),
          "<br>Please add these sample ID's to your file, and try again."
        ),
        size = "m",
        confirmButtonText = "OK",
        confirmButtonCol = "tomato",
        showCancelButton = FALSE,
        showConfirmButton = TRUE
      )
    })
    
    output$popup_table_unmatched <- render_my_datatable(
      data.frame(unmatched_sample_ids()), "Sample ID"
    )
    
    
    
    # Combine manual sample types with data
    with_manual_sample_types <- reactive({
      req(LaCyTools_summary(),
          input$method == "Upload a list with sample ID's and corresponding sample types",
          manual_sample_types$list(),
          !is_truthy(unmatched_sample_ids()),
          !is_truthy(duplicate_sample_ids())
          )

      sample_types_as_factor <- manual_sample_types$list() %>%
        dplyr::mutate(sample_type = as.factor(sample_type)) %>%
        dplyr::distinct()

      dplyr::left_join(LaCyTools_summary(), sample_types_as_factor) %>%
        dplyr::relocate(sample_type, .after = sample_id)
    })
    
    
    # Show popup with automatically determined sample types if automatic method
    # is chosen and button is clicked:
    observe({
      req(r$with_auto_sample_types,
          input$method == "Automatically determine sample types based on sample ID's")
      shinyalert::shinyalert(
        inputId = "popup_sampletypes",
        html = TRUE,
        text = paste("Based on the sample ID's the following",
                     length(unique(r$with_auto_sample_types$sample_type)),
                     "sample types were defined:",
                     shinycssloaders::withSpinner(DT::dataTableOutput(ns("popup_table_sampletypes")))),
        size = "m",
        confirmButtonText = "Accept these sample types",
        showCancelButton = TRUE,
        cancelButtonText = "Cancel",
        confirmButtonCol = "#3c8dbc",
        callbackR = function(response) {
          r$response <- response
        }
      )
    }) %>% bindEvent(input$button)
    
    # This datatable with the automatically determined sample_types is shown in
    # the pop-up:
    output$popup_table_sampletypes <- render_my_datatable(
      data.frame(unique(r$with_auto_sample_types$sample_type)), "Sample type"
    )
    
    
    observe({
      req(!is.null(r$response))
      if(!is_truthy(r$response)) {
        updateSelectInput("method",
                          session = session,
                          selected = "Upload a list with sample ID's and corresponding sample types")
      }
    })
    
    to_return <- reactive({
      if (input$method == "Automatically determine sample types based on sample ID's") {
        req(r$response)
        r$with_auto_sample_types
        
      } else {
        req(with_manual_sample_types())
        with_manual_sample_types()
      }
    })
    
    output$download_ex_sample_types <- downloadHandler(
      filename = "Example sample types file.xlsx",
      content = function(file) {
        example_file <- system.file("app",
                                    "www",
                                    "Example sample types file.xlsx",
                                    package = "GlycoDash")
        file.copy(example_file, file)
      }
    )
    
    return(list(
      data = to_return,
      popup = reactive({ r$response }),
      method = reactive({ input$method }),
      filename_sample_types = manual_sample_types$filename
      ))
    
  })
}
    
## To be copied in the UI
# mod_add_sample_types_ui("add_sample_types_ui_1")
    
## To be copied in the server
# mod_add_sample_types_server("add_sample_types_ui_1")
