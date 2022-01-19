#' data_import_v2 UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_data_import_v2_ui <- function(id){
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
            fileInput(ns("lacytools_summary"), "Upload LacyTools summary.txt file:"),
            actionButton(ns("read_summary"), "Convert the LacyTools summary file to an R-suitable format")
          ),
          shinydashboard::box(
            title = "Upload your plate design",
            width = NULL,
            fileInput(ns("plate_design"), "Upload a plate design Excel file:"),
            actionButton(ns("add_plate_design"), "Add sample ID's from the plate design to the data"),
            br(),
            br(),
            div(id = ns("manual_sample_types"),
                tags$b("Upload an Excel file or an R object (.rds) that contains:"),
                tags$ul(
                  tags$li(tags$span("a column named \"sample_id\" with the sample ID's for all samples in the data")),
                  tags$li(tags$span("a column named \"group\" with the corresponding group that the sample belongs to"))
                ),
                fileInput(ns("groups_file"), label = NULL))
          ),
          shinydashboard::box(
            title = "Upload your metadata",
            width = NULL,
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
            DT::dataTableOutput(ns("data_table"))
          )
        )
      )
    )
  )
}
    
#' data_import_v2 Server Functions
#'
#' @noRd 
mod_data_import_v2_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    # Creating a reactiveValues object in which reactiveVals from this module can be saved:
    x <- reactiveValues()
    
    # Hide the metadata menu until metadata is uploaded:
    observe({
      shinyjs::toggle("metadata_menu", condition = !is.null(x$metadata))
    })
    
    # Hide the fileInput for the manual sample_type addition until the manual option
    # is chosen in the pop-up (x$response = FALSE)
    observe({
      shinyjs::hide("manual_sample_types")
      if (!is.null(x$response)) {
        if (x$response == FALSE) {
          shinyjs::show("manual_sample_types")
        }
      }
    })
    
    # Make reactives containing the file extensions for each file that can be uploaded:
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
    
    # Show a warning when the wrong type of file is uploaded as lacytools summary:
    observe({
      req(input$lacytools_summary)
      shinyFeedback::feedbackWarning("lacytools_summary",
                                     ext_lacytools_summary() != "txt",
                                     text = "Please upload a .txt file.")
    })
    
    # Read in the metadata files when they are uploaded, or show a warning when
    # any of the uploaded files are of the wrong type:
    observe({
      req(input$metadata)
      # --> change this for loop to a map call? (if possible)
      metadata_list <- list()
      i <- 1
      for (ext in ext_metadata()) {
        if (ext %in% c("xlsx", "xls")) {
          metadata_list[[i]] <- read_metadata(input$metadata$datapath[i])
        } else { if (ext %in% c("rds")) {
          metadata_list[[i]] <- load_and_assign(input$metadata$datapath[i]) %>%
            dplyr::rename_with(.cols = tidyselect::everything(),
                               .fn = snakecase::to_snake_case)
        } else {
          shinyFeedback::feedbackWarning("metadata",
                                         show = TRUE,
                                         text = "Please upload only .xlsx, .xls or .rds files.")
        }
        }
        i <- i + 1
      }
      
      names(metadata_list) <- input$metadata$name
      x$metadata <- metadata_list
    })
    
    # Show a warning when the wrong type of file is uploaded as plate design:
    observe({
      req(input$plate_design)
      shinyFeedback::feedbackWarning("plate_design",
                                     !(ext_plate_design() %in% c("xlsx", "xls")),
                                     text = "Please upload a .xlsx or .xls file.")
    })
    
    # Make sure that the read_summary actionButton is only available once the
    # right type of file is uploaded as lacytools summary
    observe({
      shinyjs::toggleState(id = "read_summary", 
                           !is.null(input$lacytools_summary))
      shinyjs::toggleState(id = "read_summary",
                           ext_lacytools_summary() == "txt")
    })
    
    # When the read_summary actionButton is clicked, the lacytools summary is read
    # and saved as a reactive expression data()
    data <- eventReactive(input$read_summary, {
      read_lacytools_summary(input$lacytools_summary$datapath)
    })
    
    # --> combine this with eventReactive call above?
    observe({
      x$data <- data()
    })
    
    # When the lacytools summary has been read in, the converted data is shown
    # in the data table
    output$data_table <- DT::renderDT({
      req(x$data)
      DT::datatable(x$data, options = list(scrollX = TRUE))
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
    
    # This observe call ensures that the add_metadata actionButton is only
    # enabled under the right circumstances
    observe({
      shinyjs::disable(id = "add_metadata")
      if (all(isTruthy(x$data), 
              isTruthy(x$metadata), 
              # --> make sure sample_id is always called sample_id in plate_design!
              "sample_id" %in% colnames(x$data))) {
        # Check if all sample_id_column inputs in the metadata menu are filled in: 
        if (all(purrr::map_lgl(sample_id_inputIds(),
                               ~ isTruthy(input[[.x]])))) {
          shinyjs::enable(id = "add_metadata")
        }
      }
    })
    
    # When the add_plate_design actionButton is clicked, the plate_design file is
    # read in and a pop-up is shown with the automatically determined sample types.
    observeEvent(input$add_plate_design, {
      x$plate_design <- read_and_process_plate_design(input$plate_design$datapath)
      shinyalert::shinyalert(
        html = TRUE,
        text = tagList(
          "Based on the sample IDs the following sample types were defined:",
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
    
    observeEvent(x$response, {
      if (x$response == TRUE) {
        x$data <- dplyr::left_join(x$data, x$plate_design)
        print("Data has been updated")
      } #else {
      
      # shinyjs::delay(381,
      #                shinyalert::shinyalert(
      #                  html = TRUE,
      #                  text = tagList(
      #                    fileInput(ns("groups"), label = NULL,
      #                                  buttonLabel = div("Browse...", style = "font-size:20px;")),
      #                    tags$b("Upload an Excel file or an R object (.rds) that contains:"),
      #                    tags$ul(
      #                      tags$li(tags$span("a column named \"sample_id\" with the sample ID's for all samples in the data")),
      #                      tags$li(tags$span("a column named \"group\" with the corresponding group that the sample belongs to"))
      #                    )
      #                  ),
      #                  size = "m",
      #                  confirmButtonText = "Enter groups",
      #                  showCancelButton = TRUE,
      #                  cancelButtonText = "Cancel adding sample ID's",
      #                  callbackR = function(y) {
      #                    x$response_2 <- y
      #                  }
      #                  )
      #                )
      #}
    })
    
    ext_groups <- reactive({
      req(input$groups_file)
      ext <- tools::file_ext(input$groups_file$name)
      return(ext)
    })
    
    observe({
      req(ext_groups())
      if (ext_groups() == "rds") {
        x$groups <- load_and_assign(input$groups_file$datapath)
        print("groups_file was loaded")
        # write a check that column names are named correctly
      } else { if (ext_groups() %in% c("xlsx", "xls")) {
        x$groups <- readxl::read_excel(input$groups_file$datapath)
        # write a check that column names are named correctly
      } else {
        shinyFeedback::feedbackWarning("groups_file", 
                                       show = TRUE,
                                       text = "Please upload a .xlsx, .xls or .rds file.")
      }
      }
      
    })
    
    observeEvent(x$groups, {
      print(x$plate_design)
      x$plate_design <- x$plate_design %>% 
        dplyr::select(-sample_type)
      x$groups_and_plate_design <- dplyr::full_join(x$plate_design, x$groups) %>% 
        dplyr::distinct()
      x$data <- dplyr::left_join(data(), x$groups_and_plate_design)
      print("Data has been updated")
    })
    
    # observe({
    #   req(x$groups)
    #   if (x$response_2) {
    #     x$plate_design <- x$plate_design %>% 
    #       dplyr::select(-sample_type)
    #     x$groups_and_plate_design <- dplyr::full_join(x$plate_design, x$groups) %>% 
    #       dplyr::distinct()
    #     x$data <- dplyr::left_join(data(), x$groups_and_plate_design)
    #     print("Data has been updated")
    #   }
    # })
    
    sample_id_inputIds <- reactive({
      req(x$metadata)
      sample_id_inputIds <- purrr::map(seq_len(length(x$metadata)),
                                       ~ paste0("sample_id_column", .x))
      return(sample_id_inputIds)
    })
    
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
                    choices = c("", unique(colnames(metadata))),
                    selected = NULL,
                    multiple = FALSE,
                    options = list(placeholder = "select a column")))
    })
    
    date_column_inputIds <- reactive({
      req(x$metadata)
      date_column_inputIds <- purrr::map(seq_len(length(x$metadata)),
                                         ~ paste0("date_column", .x))
      return(date_column_inputIds)
    })
    
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
                    choices = unique(colnames(metadata)),
                    selected = stringr::str_subset(
                      colnames(metadata),
                      pattern = stringr::regex("date", ignore_case = TRUE)),
                    multiple = TRUE))
    })
    
    observeEvent(input$add_metadata, {
      # Convert all date columns to date format (or if it's a mixed date and text format,
      # to character) and rename the column with sample_ids
      # --> make this map for all metadata's in x$metadata
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
      # --> merge the metadata's together (key = sample_id)
      x$merged_metadata <- purrr::reduce(metadata_list, dplyr::full_join, by = "sample_id")
      print(x$merged_metadata)
      # check for unmatched sample ids in the data
      tryCatch(expr = {
        check_sample_id_matches(plate_design_ids = x$data$sample_id,
                                metadata_ids = x$merged_metadata$sample_id)
        x$data <- dplyr::left_join(x$data,
                                   x$merged_metadata)
      },
      warning = function(w) {
        unmatched_ids <- suppressWarnings(check_sample_id_matches(plate_design = x$data$sample_id,
                                                                  metadata = x$merged_metadata$sample_id))
        shinyalert::shinyalert(
          html = TRUE,
          text = tagList(
            paste(length(unmatched_ids),
                  "sample ID's in the data had no match in the metadata:"),
            DT::dataTableOutput(ns("unmatched_ids")),
            "Please check if the spelling of sample IDs in your metadata corresponds to the spelling in your plate design."
          ),
          #type = "warning",
          size = "m",
          confirmButtonText = "Continue with adding metadata despite unmatched sample ID's",
          showCancelButton = TRUE,
          cancelButtonText = "Cancel adding the metadata",
          callbackR = function(y) {
            x$response_3 <- y
          }
        )
      }
      )
    })
    
    output$unmatched_ids <- DT::renderDataTable({
      unmatched_ids <- suppressWarnings(check_sample_id_matches(plate_design = x$data$sample_id,
                                                                # CREATE MERGED metadata reactiveVal
                                                                metadata = x$merged_metadata$sample_id))
      unmatched_ids <- as.data.frame(unmatched_ids)
      table <- DT::datatable(unmatched_ids,
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
    
    observeEvent(x$response_3, {
      if (x$response_3) {
        x$data <- dplyr::left_join(x$data, x$merged_metadata)
        print(x$merged_metadata)
      }
    })
    
  })
}
    
## To be copied in the UI
# mod_data_import_v2_ui("data_import_v2_ui_1")
    
## To be copied in the server
# mod_data_import_v2_server("data_import_v2_ui_1")
