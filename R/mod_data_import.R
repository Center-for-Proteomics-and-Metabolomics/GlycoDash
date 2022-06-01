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
    bsplus::use_bs_popover(),
    fluidPage(
      fluidRow(
        h1("Data Import")
      ),
      fluidRow(
        column(
          width = 6,
          mod_read_lacytools_ui(ns("read_lacytools_ui_1")),
          mod_add_sample_ids_ui(ns("add_sample_ids_ui_1")),
          mod_add_sample_types_ui(ns("add_sample_types_ui_1")),
          shinydashboard::box(
            title = "Upload your metadata",
            width = NULL,
            solidHeader = TRUE,
            status = "primary",
            fileInput(ns("metadata"), 
                      "Upload one or more metadata Excel file(s) or R object(s):",
                      multiple = TRUE),
            div(
              id = ns("metadata_menu"),
              uiOutput(ns("sample_id")),
              uiOutput(ns("date"))
            ),
            actionButton(ns("add_metadata"), "Add the metadata")
          ),
        ),
        column(
          width = 6,
          shinydashboard::box(
            title = "View the converted data",
            width = NULL,
            solidHeader = TRUE,
            status = "primary",
            DT::DTOutput(ns("data_table"))
          )
        )
      )
    )
  )
}
    
#' data_import Server Functions
#'
#' @noRd 
mod_data_import_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    # Creating a reactiveValues object in which reactiveVals from this module can be saved:
    # (reactiveVals are often easier to work with than reactive expressions for some reason)
    x <- reactiveValues()
    
    ext_metadata <- reactive({
      req(input$metadata)
      ext <- tools::file_ext(input$metadata$name)
      return(ext)
    })
    
    summary <- mod_read_lacytools_server("read_lacytools_ui_1")
    
    data_incl_sample_ids <- mod_add_sample_ids_server("add_sample_ids_ui_1",
                                                      keyword_specific = summary$keyword_specific,
                                                      keyword_total = summary$keyword_total,
                                                      Ig_data = summary$Ig_data,
                                                      summary = summary$data)
    
    data_incl_sample_types <- mod_add_sample_types_server("add_sample_types_ui_1",
                                                          summary = data_incl_sample_ids)
    
    # When the lacytools summary has been read in, the converted data is shown
    # in the data table
    output$data_table <- DT::renderDT({
      req(summary$data())
      
      if (is_truthy(data_incl_sample_types())) {
        show_in_table <- data_incl_sample_types()
      } else { if (is_truthy(data_incl_sample_ids())) {
        show_in_table <- data_incl_sample_ids()
      } else {
        show_in_table <- summary$data()
      } 
      }
      
      DT::datatable(show_in_table,
                    options = list(scrollX = TRUE),
                    filter = "top")
    })
    
    # Hide the metadata menu until metadata is uploaded:
    observe({
      shinyjs::toggle("metadata_menu", condition = !is.null(x$metadata))
    })
    
    # Metadata ----------------------------------------------------------------
    
    # This observe call ensures that the add_metadata actionButton is only
    # enabled under the right circumstances
    observe({
      shinyjs::disable(id = "add_metadata")
      print(is_truthy(data_incl_sample_types()))
      if (all(is_truthy(data_incl_sample_types()), 
              isTruthy(x$metadata))) {
        # Check if all sample_id_column inputs in the metadata menu are filled in: 
        if (all(purrr::map_lgl(sample_id_inputIds(),
                               ~ isTruthy(input[[.x]])))) {
          shinyjs::enable(id = "add_metadata")
        }
      }
    })
    
    # Create inputIds for the sample_id_column selectizeInputs based on the 
    # number of metadata files that were uploaded:
    sample_id_inputIds <- reactive({
      req(x$metadata)
      sample_id_inputIds <- purrr::map(seq_len(length(x$metadata)),
                                       ~ paste0("sample_id_column", .x))
      return(sample_id_inputIds)
    })
    
    # Read in the metadata files when they are uploaded, or show a warning when
    # any of the uploaded files are of the wrong type:
    observe({
      req(input$metadata)
      metadata_list <- list()
      i <- 1
      for (ext in ext_metadata()) {
        if (ext %in% c("xlsx", "xls")) {
          metadata_list[[i]] <- read_metadata(input$metadata$datapath[i])
        } else { if (ext == "rds") {
          metadata_list[[i]] <- load_and_assign(input$metadata$datapath[i]) %>%
            dplyr::rename_with(.cols = tidyselect::everything(),
                               .fn = snakecase::to_snake_case)
        } else {
        }
        }
        i <- i + 1
      }
      
      shinyFeedback::feedbackWarning("metadata",
                                     show = any(!(ext_metadata() %in% c("xlsx", "xls", "rds"))),
                                     text = "Please upload only .xlsx, .xls or .rds files.")
      
      if (!rlang::is_empty(metadata_list)) {
        names(metadata_list) <- input$metadata$name
        # Saving the metadata_list in the reactiveVals object x:
        x$metadata <- metadata_list
      }
    })
    
    # Create selectizeInputs for the sample_id_columns. The number of inputs 
    # created is the same as the number of metadata files that were uploaded.
    output$sample_id <- renderUI({
      req(sample_id_inputIds())
      purrr::pmap(list(sample_id_inputIds(),
                       x$metadata,
                       names(x$metadata)),
                  function(inputId, metadata, metadata_name) selectizeInput(
                    ns(inputId),
                    label = paste("Which column in", 
                                  metadata_name, 
                                  "contains the sample ID's?"),
                    # The choices for each input correspond to the names of the 
                    # columns in the metadata file:
                    choices = c("", unique(colnames(metadata))),
                    selected = NULL,
                    multiple = FALSE,
                    options = list(placeholder = "select a column")))
    })
    
    # Create inputIds for the date_column selectizeInputs based on the 
    # number of metadata files that were uploaded:
    date_column_inputIds <- reactive({
      req(x$metadata)
      date_column_inputIds <- purrr::map(seq_len(length(x$metadata)),
                                         ~ paste0("date_column", .x))
      return(date_column_inputIds)
    })
    
    # Create selectizeInputs for the date_columns. The number of inputs 
    # created is the same as the number of metadata files that were uploaded.
    output$date <- renderUI({
      req(date_column_inputIds())
      purrr::pmap(list(date_column_inputIds(),
                       x$metadata,
                       names(x$metadata)),
                  function(inputIds, metadata, metadata_name) selectizeInput(
                    ns(inputIds),
                    label = paste("Which columns in", 
                                  metadata_name, 
                                  "contain dates?"),
                    # The choices for each input correspond to the names of the 
                    # columns in the metadata file:
                    choices = unique(colnames(metadata)),
                    # By default all columns with "date" in their name are selected:
                    selected = stringr::str_subset(
                      colnames(metadata),
                      pattern = stringr::regex("date", ignore_case = TRUE)),
                    multiple = TRUE))
    })
    
    # When the add_metadata actionButton is clicked the process of adding the 
    # metadata to the data is started:
    observeEvent(input$add_metadata, {
      # Reset x$merged_metadata:
      x$merged_metadata <- NULL
      # For all metadata files in the metadata_list:
      # Convert all date columns to date format (or if it's a mixed date and text format,
      # to character) and rename the column with sample ID's to "sample_id":
      metadata_list <- purrr::pmap(
        list(x$metadata,
             sample_id_inputIds(),
             date_column_inputIds()),
        function(metadata, 
                 sample_id_inputId,
                 date_column_inputId) {
          # Check whether the metadata contains a column named "sample_id" that
          # is not chosen as the sample ID column. If this is the case, the
          # column needs to be renamed to prevent duplicated names (which would
          # cause the app to crash)
          conflict <- "sample_id" %in% colnames(metadata) & input[[sample_id_inputId]] != "sample_id"
          if (conflict == TRUE) {
            metadata <- metadata %>% 
              dplyr::rename(sample_id_original = sample_id)
          }
          metadata <- metadata %>% 
            dplyr::mutate(dplyr::across(tidyselect::any_of(input[[date_column_inputId]]), 
                                        date_with_text)) %>% 
            dplyr::rename(sample_id = input[[sample_id_inputId]])
          return(metadata)
        })
      # Merge all metadata files in metadata_list together (key = sample_id):
      x$merged_metadata <- purrr::reduce(metadata_list, 
                                         dplyr::full_join, by = "sample_id")
      # Check for unmatched sample ID's in the data:
      unmatched <- setdiff(x$data_incl_plate_design$sample_id,
                           x$merged_metadata$sample_id)
      if(rlang::is_empty(unmatched)) {
        x$data_incl_metadata <- dplyr::left_join(x$data_incl_plate_design,
                                                 x$merged_metadata)
        showNotification("The metadata was added to the data", type = "message")
      } else {
        # If there are unmatched sample ID's a pop-up is shown.
        # Reset the response to the pop-up in case it has been shown before:
        x$response_metadata <- NULL
        
        shinyalert::shinyalert(
          html = TRUE,
          text = tagList(
            paste(length(unmatched),
                  "sample ID's in the data had no match in the metadata:"),
            DT::dataTableOutput(ns("unmatched_ids")),
            br(),
            "Please check: 1) Does the spelling of sample IDs in your metadata corresponds to the spelling in your plate design?",
            "and 2) Have you selected the correct sample ID columns?"
          ),
          size = "m",
          confirmButtonText = "Add the metadata despite the unmatched ID's",
          confirmButtonCol = "#3c8dbc",
          showCancelButton = TRUE,
          cancelButtonText = "Don't add the metadata now",
          type = ifelse(length(unmatched) > 20, "warning", ""),
          callbackR = function(response) {
            x$response_metadata <- response
      })
      }
    })
    
    # This is the datatable containing the unmatched sample ID's that is shown 
    # in the pop-up:
    output$unmatched_ids <- DT::renderDataTable({
      unmatched <- setdiff(x$data_incl_plate_design$sample_id,
                           x$merged_metadata$sample_id)
      
      unmatched <- as.data.frame(unmatched)
      table <- DT::datatable(unmatched,
                             options = list(
                               scrollY = "100px",
                               paging = FALSE,
                               searching = FALSE,
                               columnDefs = list(
                                 list(
                                   className = 'dt-center', 
                                   targets = "_all"))),
                             colnames = "Sample ID",
                             rownames = FALSE)
      return(table)
    })
    
    # If the user wants to add the metadata despite unmatched sample ID's 
    # (x$response_metadata = TRUE), the metadata is joined with the data:
    observeEvent(x$response_metadata, {
      if (x$response_metadata) {
        x$data_incl_metadata <- dplyr::left_join(data_incl_sample_types, x$merged_metadata)
        showNotification("The metadata was added to the data", type = "message")
      }
    })
    
    return(list(
      data_incl_plate_design = reactive({x$data_incl_plate_design}),
      data_incl_metadata = reactive({x$data_incl_metadata}),
      Ig_data = reactive({input$Ig_data}),
      lacytools_summary = reactive({input$lacytools_summary$name}),
      plate_design = list(reactive({input$plate_design$name}),
                          reactive({input$plate_design_specific$name}),
                          reactive({input$plate_design_total$name})),
      metadata = reactive({input$metadata$name}),
      manual_sample_types = reactive({!x$response}),
      sample_types_file = reactive({input$groups_file$name})
    ))
    
  })
}
    
## To be copied in the UI
# mod_data_import_ui("data_import_ui_1")
    
## To be copied in the server
# mod_data_import_server("data_import_ui_1")
