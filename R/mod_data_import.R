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
            title = "Upload your data",
            width = NULL,
            solidHeader = TRUE,
            status = "primary",
            fileInput(ns("lacytools_summary"), "Upload LacyTools summary.txt file:"),
            radioButtons(ns("Ig_data"), "Does your data contain total and specific immunoglobulin samples?",
                         choices = c("Yes", "No"),
                         selected = character(0)),
            div(id = ns("keywords_specific_total"),
                textInput(ns("keyword_specific"), 
                          label = "By what keyword can the specific Ig samples be recognized?"),
                textInput(ns("keyword_total"), 
                          label = "By what keyword can the total Ig samples be recognized?")),
            actionButton(ns("read_summary"), "Convert the LacyTools summary file to an R-suitable format")
          ),
          shinydashboard::box(
            title = "Upload your plate design",
            width = NULL,
            solidHeader = TRUE,
            status = "primary",
            fileInput(ns("plate_design"), "Upload a plate design Excel file:"),
            actionButton(ns("add_plate_design"), "Add sample ID's and sample types to the data based on the plate design"),
            br(),
            br(),
            div(id = ns("manual_sample_types"),
                tags$b("Upload an Excel file or an R object (.rds) that contains:"),
                tags$ul(
                  tags$li(tags$span("a column named \"sample_id\" with the sample ID's for all samples in the data")),
                  tags$li(tags$span("a column named \"sample_type\" with the corresponding sample types"))
                ),
                fileInput(ns("groups_file"), label = NULL))
          ),
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
mod_data_import_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    # Creating a reactiveValues object in which reactiveVals from this module can be saved:
    # (reactiveVals are often easier to work with than reactive expressions for some reason)
    x <- reactiveValues()
    
    # Make reactive expressions containing the file extensions for each file that 
    # can be uploaded:
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
    
    # Data --------------------------------------------------------------------
    
    # Show a warning when the wrong type of file is uploaded as lacytools summary:
    observe({
      req(input$lacytools_summary)
      shinyFeedback::feedbackWarning("lacytools_summary",
                                     ext_lacytools_summary() != "txt",
                                     text = "Please upload a .txt file.")
    })
    
    # Make sure that the read_summary actionButton is only available once the
    # right type of file is uploaded as lacytools summary and once the user has
    # provided all required inputs:
    observe({
      shinyjs::disable(id = "read_summary")
      if (all(isTruthy(input$lacytools_summary), 
              ext_lacytools_summary() == "txt")) {
        if (isTruthy(input$Ig_data)) {
          if (input$Ig_data == "Yes") {
            if (all(isTruthy(input$keyword_total), 
                    isTruthy(input$keyword_specific))) {
              if (all(isTruthy(x$keyword_total_OK),
                      isTruthy(x$keyword_specific_OK))) {
                shinyjs::enable(id = "read_summary")
              }
            }
          } else {
            shinyjs::enable(id = "read_summary")
          }
        }
      }
    })
    
    # If the user indicates (via input$Ig_data) that the data contains total and
    # specific Ig samples, the textInputs for the specific and total keywords
    # are shown.
    observe({
      shinyjs::hide("keywords_specific_total")
      if (!is.null(input$Ig_data)) {
        if (input$Ig_data == "Yes") {
          shinyjs::show("keywords_specific_total")
        }
      }  
    })
    
    # Check whether the keyword given for specific samples has matches with the
    # sample names in the data:
    observeEvent(input$keyword_specific, {
      req(input$lacytools_summary)               
      x$keyword_specific_OK <- TRUE
      shinyFeedback::hideFeedback("keyword_specific")
      data <- read_non_rectangular(input$lacytools_summary$datapath)
      all_blocks <- purrr::map(outputs,
                               function(output) get_block(data = data, 
                                                          variable = output, 
                                                          Ig_data = "No"))
      
      matches <- purrr::map(all_blocks,
                            function(block) stringr::str_detect(block[["sample_name"]],
                                                                pattern = input$keyword_specific))
      
      keyword_is_unmatched <- !(all(purrr::map_lgl(matches,
                                                   any)))
      print(keyword_is_unmatched)
      
      if (keyword_is_unmatched) {
        shinyFeedback::feedbackDanger("keyword_specific",
                                      show = TRUE,
                                      text = "This keyword did not match any sample names in your data. Please choose a different keyword.")
        x$keyword_specific_OK <- FALSE
      }
    })
    
    # Check whether the keyword given for total samples has matches with the
    # sample names in the data:
    observeEvent(input$keyword_total, {
      req(input$lacytools_summary)               
      x$keyword_total_OK <- TRUE
      shinyFeedback::hideFeedback("keyword_total")
      data <- read_non_rectangular(input$lacytools_summary$datapath)
      all_blocks <- purrr::map(outputs,
                               function(output) get_block(data = data, 
                                                          variable = output, 
                                                          Ig_data = "No"))
      
      matches <- purrr::map(all_blocks,
                            function(block) stringr::str_detect(block[["sample_name"]],
                                                                pattern = input$keyword_total))
      
      keyword_is_unmatched <- !(all(purrr::map_lgl(matches,
                                                   any)))
      
      if (keyword_is_unmatched) {
        shinyFeedback::feedbackDanger("keyword_total",
                                      show = TRUE,
                                      text = "This keyword did not match any sample names in your data. Please choose a different keyword.")
        x$keyword_total_OK <- FALSE
      }
    })
    
    # When the read_summary actionButton is clicked, the lacytools summary is read
    # and saved as a reactive expression data_at_read_in()
    data_at_read_in <- eventReactive(input$read_summary, {
      read_lacytools_summary(summary_file = input$lacytools_summary$datapath,
                             Ig_data = input$Ig_data,
                             keyword_total = input$keyword_total,
                             keyword_specific = input$keyword_specific)
    })
    
    observe({
      x$data <- data_at_read_in()
      showNotification("The data has been read in and converted.", 
                       type = "message")
    })
    
    # When the read_summary actionButton is clicked, reset the data reactiveVals 
    # to NULL (so that user can start over even after next steps have been taken).
    # Add some kind of confirmation step here so that users don't accidentally 
    # throw away their progress?
    observeEvent(input$read_summary, {
      if (isTruthy(x$data_incl_plate_design)){
        x$data_incl_plate_design <- NULL
        showNotification("Sample ID's and sample types have to be re-added to the data",
                         type = "warning")
        if (isTruthy(x$data_incl_metadata)) {
          x$data_incl_metadata <- NULL
          showNotification("The metadata has to be re-added to the data",
                           type = "warning")
        }
      }
    })
    
    # When the lacytools summary has been read in, the converted data is shown
    # in the data table
    output$data_table <- DT::renderDT({
      req(x$data)
      if (isTruthy(x$data_incl_metadata)){
        DT::datatable(x$data_incl_metadata, options = list(scrollX = TRUE))
      } else { if (isTruthy(x$data_incl_plate_design)){
        DT::datatable(x$data_incl_plate_design, options = list(scrollX = TRUE))
      } else {
        DT::datatable(x$data, options = list(scrollX = TRUE))
      }
      }
    })
    
    # Hide the metadata menu until metadata is uploaded:
    observe({
      shinyjs::toggle("metadata_menu", condition = !is.null(x$metadata))
    })
    
    # Plate design ------------------------------------------------------------
    
    # Show a warning when the wrong type of file is uploaded as plate design:
    observe({
      req(input$plate_design)
      shinyFeedback::feedbackWarning("plate_design",
                                     !(ext_plate_design() %in% c("xlsx", "xls")),
                                     text = "Please upload a .xlsx or .xls file.")
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
    
    # When the add_plate_design actionButton is clicked, the plate_design file is
    # read in and a pop-up is shown with the automatically determined sample types.
    observeEvent(input$add_plate_design, {
      
      # The x$data_incl_metadata reactiveVal is reset to NULL, so that users can
      # change the plate design file after metadata has already been added:
      if (isTruthy(x$data_incl_metadata)) {
        x$data_incl_metadata <- NULL
        showNotification("The metadata has to be re-added to the data",
                         type = "warning")
      }
      
      x$plate_design <- read_and_process_plate_design(input$plate_design$datapath)
      
      # Reset x$response in case the pop-up has been shown before:
      x$response <- NULL
      
      shinyalert::shinyalert(
        html = TRUE,
        text = tagList(
          paste("Based on the sample IDs the following",
                length(unique(isolate(x$plate_design$sample_type))), 
                "sample types were defined:"),
          DT::dataTableOutput(ns("group"))
        ),
        size = "m",
        confirmButtonText = "Accept these sample types",
        showCancelButton = TRUE,
        # --> Explain further what the user can expect when choosing cancel
        cancelButtonText = "Manually enter sample types",
        callbackR = function(response) {
          x$response <- response
        }
      )
    })
    
    # This datatable with the automatically determined sample_types is shown in
    # the pop-up:
    output$group <- DT::renderDataTable({
      groups <- data.frame(unique(isolate(x$plate_design$sample_type)))
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
    
    # If the automatically determined sample_types shown in the pop-up are 
    # accepted (x$response = TRUE), the plate design is joined with the data.
    # Now the data contains columns with the sample ID, with the sample type and
    # with whether the sample is a duplicate of another sample:
    observeEvent(x$response, {
      if (x$response == TRUE) {
        x$data_incl_plate_design <- dplyr::left_join(x$data, x$plate_design)
      } 
    })
    
    # Hide the fileInput for the manual sample_type addition until the manual option
    # is chosen in the pop-up (x$response = FALSE)
    observe({
      shinyjs::hide("manual_sample_types")
      if (!is.null(x$response)) {
        if (x$response == FALSE) {
          shinyjs::show("manual_sample_types")
          # Reset the input for the file with manual sample types:
          shinyjs::reset("groups_file")
          shinyFeedback::hideFeedback(inputId = "groups_file")
        }
      }
    })
    
    # Make  a reactive expression that contains the file extension for the file 
    # to manually add sample types:
    ext_groups <- reactive({
      req(input$groups_file)
      ext <- tools::file_ext(input$groups_file$name)
      return(ext)
    })
    
    # Read in the file with manual sample types when it is uploaded, or show a 
    # warning when the uploaded file is of the wrong type:
    observeEvent(ext_groups(), {
      req(ext_groups())
      
      if (ext_groups() == "rds") {
        x$groups <- load_and_assign(input$groups_file$datapath)
        # write a check that column names are named correctly
      } else { if (ext_groups() %in% c("xlsx", "xls")) {
        x$groups <- readxl::read_excel(input$groups_file$datapath)
        # write a check that column names are named correctly
      } 
      }
      shinyFeedback::feedbackWarning(inputId = "groups_file", 
                                     show = !(ext_groups() %in% c("rds", "xlsx", "xls")),
                                     text = "Please upload a .xlsx, .xls or .rds file.")
      if (isTruthy(x$groups)) {
        if (all(c("sample_id", "sample_type") %in% colnames(x$groups))) {
          x$correct_column_names <- TRUE
        } else {
          x$correct_column_names <- FALSE
        }
        shinyFeedback::feedbackWarning(inputId = "groups_file",
                                       show = x$correct_column_names == FALSE,
                                       text = "Please name the columns \"sample_id\" and \"sample_type\"")
      }
    })
    
    # When the manual sample types are read in, join them with the plate design 
    # and the data:
    observeEvent(x$correct_column_names, {
      if (isTruthy(x$correct_column_names)){
        x$plate_design <- x$plate_design %>% 
          dplyr::select(-sample_type)
        x$groups_and_plate_design <- dplyr::full_join(x$plate_design, x$groups) %>% 
          dplyr::distinct()
        x$data_incl_plate_design <- dplyr::left_join(data_at_read_in(), x$groups_and_plate_design)
        # choose which of these is better:
        shinyFeedback::feedbackSuccess(inputId = "groups_file", 
                                       show = isTruthy(x$groups_and_plate_design),
                                       color = "#18BC9C",
                                       text = "The sample types were added to the data.")
        showNotification("The sample types were added to the data", type = "message")
        
        # The x$data_incl_metadata reactiveVal is reset to NULL, so that users can
        # change the plate design file after metadata has already been added:
        if (isTruthy(x$data_incl_metadata)) {
          x$data_incl_metadata <- NULL
          showNotification("The metadata has to be re-added to the data",
                           type = "warning")
        }
      }
    })
    
    # Metadata ----------------------------------------------------------------
    
    # This observe call ensures that the add_metadata actionButton is only
    # enabled under the right circumstances
    observe({
      shinyjs::disable(id = "add_metadata")
      if (all(isTruthy(x$data_incl_plate_design), 
              isTruthy(x$metadata), 
              "sample_id" %in% colnames(x$data_incl_plate_design))) {
        # Check if all sample_id_column inputs in the metadata menu are filled in: 
        if (all(purrr::map_lgl(sample_id_inputIds(),
                               ~ isTruthy(input[[.x]])))) {
          shinyjs::enable(id = "add_metadata")
        }
      }
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
    
    # Create inputIds for the sample_id_column selectizeInputs based on the 
    # number of metadata files that were uploaded:
    sample_id_inputIds <- reactive({
      req(x$metadata)
      sample_id_inputIds <- purrr::map(seq_len(length(x$metadata)),
                                       ~ paste0("sample_id_column", .x))
      return(sample_id_inputIds)
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
        x$data_incl_metadata <- dplyr::left_join(x$data_incl_plate_design, x$merged_metadata)
        showNotification("The metadata was added to the data", type = "message")
      }
    })
    
    return(list(
      data_incl_plate_design = reactive({x$data_incl_plate_design}),
      data_incl_metadata = reactive({x$data_incl_metadata})
    ))
    
  })
}
    
## To be copied in the UI
# mod_data_import_ui("data_import_ui_1")
    
## To be copied in the server
# mod_data_import_server("data_import_ui_1")
