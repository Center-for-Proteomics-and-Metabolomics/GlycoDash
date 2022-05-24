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
          # tags$style(HTML(paste0(
          #   "#",
          #   ns("box_header"),
          #   " .awesome-checkbox {padding-top: 7px}",
          #   "#",
          #   ns("box_header"),
          #   " .popover {max-width: 400px !important; color: #333}",
          #   "#",
          #   ns("sample_id_box"),
          #   " .box-title {width: 100%}",
          #   "#",
          #   ns("box_header"),
          #   " .fa {float: right; margin-right: 5px; font-size: 18px}",
          #   "#",
          #   ns("box_header"),
          #   " .direct-chat-contacts {right: 0; background: #222d32!important}",
          #   "#",
          #   ns("box_header"),
          #   " .btn {float: right; border-width: 0px; margin-right: 10px}",
          #   "#",
          #   ns("sample_id_box"),
          #   " .dropdown {display: inline-block; float: right; width: 330px}",
          #   "#",
          #   ns("box_header"),
          #   " .dropdown-menu {background: #333; right: -30px; left: auto; top: 28px;}"
          # ))
          # ),
          # shinydashboardPlus::box(
          #   id = ns("sample_id_box"),
          #   title = div(
          #     id = ns("box_header"),
          #     "Add sample ID's",
          #     icon("info-circle",
          #          class = "ml",
          #          tabindex = "0") %>% 
          #       bsplus::bs_embed_popover(
          #         title = "Explanation",
          #         content = HTML(paste0(
          #           tags$p(paste(
          #             "Adding sample ID's to your data allows you to see", 
          #             "which measurement corresponds to which sample.")),
          #           "Sample ID's will in later steps be used to:",
          #           tags$ul(tags$li(paste(
          #             "determine the sample type (e.g. blank, standard,", 
          #             "negative control, patient, etc.)"
          #           )), 
          #           tags$li(paste(
          #             "link metadata to your data (e.g. age and", 
          #             "gender of the study subjects)"
          #           ))
          #           ))),
          #         # Don't use body = container here, because then the custom CSS
          #         # styling for .popover won't be applied
          #         trigger = "hover",
          #         placement = "right",
          #         html = "true"),
          #     shinyWidgets::dropdownButton(
          #       tags$style(HTML(paste0(
          #         "#",
          #         ns("dropdown_content"),
          #         " .fa {float: left}",
          #         "#",
          #         ns("dropdown_content"),
          #         " .btn {float: none; border-width: 1px; width: 280px; margin: 10px}"
          #       ))),
          #       div(id = ns("dropdown_content"),
          #           downloadButton(ns("download_ex_plate_design"),
          #                          "Download a plate design example file"),
          #           downloadButton(ns("download_ex_sample_list"),
          #                          "Download a sample list example file")),
          #       icon = icon("paperclip",
          #                   class = "ml"),
          #       tooltip = shinyWidgets::tooltipOptions(placement = "top",
          #                                              title = "Examples"),
          #       width = "330px",
          #       size = "xs"
          #     )),
          #   width = NULL,
          #   solidHeader = TRUE,
          #   status = "primary",
          #   selectInput(ns("sample_id_method"),
          #               "Choose a method to add sample ID's to your data:",
          #               choices = c("Upload a plate design",
          #                           "Upload a sample list")) %>% 
          #     bsplus::bs_embed_popover(
          #       title = "Method to add sample ID's",
          #       content = HTML(paste0(
          #         tags$b("Plate design:"),
          #         tags$p(paste(
          #           "You can only use this method when your sample names contain",
          #           "information on the plate and well position of the sample,", 
          #           "in the correct format:"
          #         )),
          #         "<p>plate<i>[insert plate number here]</i>_<i>[insert well position here]</i></p>",
          #         "An example of a valid sample name is:",
          #         "<p>\"38160_38161_IM5_<b>plate1_A8</b>_01_Spike 20210409_000237.raw\"</P",
          #         br(),
          #         br(),
          #         tags$p(tags$b("Sample list:"),
          #                br(),
          #                paste(
          #                  "Use this method when your samples were not measured on plates",
          #                  "or when your sample names don't meet the requirements described above."
          #                ))
          #       )),
          #       html = "true",
          #       trigger = "hover",
          #       placement = "right"
          #     ),
          #   shinyWidgets::materialSwitch(ns("switch_two_plate_designs"),
          #                                "Add separate plate design files for specific and and for total Ig samples.",
          #                                status = "primary",
          #                                right = TRUE),
          #   div(id = ns("one_plate_design"),
          #       mod_fileInput_with_info_ui(
          #         id = ns("plate_design"),
          #         fileInput_label = "Upload a plate design Excel file:",
          #         popover_width = "400px",
          #         popover_title = "Plate design format:",
          #         popover_content_html = HTML(paste0(
          #           tags$p(paste(
          #             "The top-left cell of the Excel sheet should contain",
          #             "the plate number (e.g. \"Plate 1\"). The cells to the",
          #             "right of the top-left cell need to be labelled 1-12,", 
          #             "while the cells below the top-left cell need to be",
          #             "labelled A-H (for a 96-well plate). The cells",
          #             "within the plate should contain the sample ID's.",
          #             "Sample ID's must not contain commas (,) or line breaks."
          #           )),
          #           tags$p(paste("At the bottom of the plate, leave one row", 
          #                        "blank and then add the next plate in the", 
          #                        "same format.")),
          #           tags$p("For an example, click on the paperclip icon.")
          #         ))
          #       )
          #   ),
          #   div(id = ns("two_plate_designs"),
          #       mod_fileInput_with_info_ui(
          #         id = ns("plate_design_specific"),
          #         fileInput_label = "Upload a plate design Excel file for the specific Ig samples:",
          #         popover_width = "400px",
          #         popover_title = "Plate design format:",
          #         popover_content_html = HTML(paste0(
          #           tags$p(paste(
          #             "The top-left cell of the Excel sheet should contain",
          #             "the plate number (e.g. \"Plate 1\"). The cells to the",
          #             "right of the top-left cell need to be labelled 1-12,", 
          #             "while the cells below the top-left cell need to be",
          #             "labelled A-H (for a 96-well plate). The cells",
          #             "within the plate should contain the sample ID's.",
          #             "Sample ID's must not contain commas (,) or line breaks."
          #           )),
          #           tags$p(paste("At the bottom of the plate, leave one row", 
          #                        "blank and then add the next plate in the", 
          #                        "same format.")),
          #           tags$p("For an example, click on the paperclip icon.")
          #         ))
          #       ),
          #       fluidRow(
          #         column(width = 11,
          #                fileInput(ns("plate_design_total"), 
          #                          "Upload a plate design Excel file for the total Ig samples:")))
          #   ),
          #   div(id = ns("sample_list_ui"),
          #       mod_fileInput_with_info_ui(
          #         id = ns("sample_list"),
          #         fileInput_label = "Upload an Excel file with your sample list:",
          #         popover_width = "400px",
          #         popover_title = "Sample list format:",
          #         popover_content_html = HTML(paste0(
          #           tags$p(paste(
          #             "The Excel file should contain only one sheet.",
          #             "This sheet should contain one column named \"sample_name\"",
          #             "and one column named \"sample_id\"."
          #           )),
          #           tags$p("For an example, click on the paperclip icon.")
          #         ))
          #       )
          #   ),
          #   fluidRow(
          #     column(
          #       width = 12,
          #       actionButton(ns("add_plate_design"), 
          #                    "Add sample ID's and sample types to the data based on the plate design"),
          #       br(),
          #       br(),
          #       div(id = ns("manual_sample_types"),
          #           tags$b("Upload an Excel file or an R object (.rds) that contains:"),
          #           tags$ul(
          #             tags$li(tags$span("a column named \"sample_id\" with the sample ID's for all samples in the data")),
          #             tags$li(tags$span("a column named \"sample_type\" with the corresponding sample types"))
          #           ),
          #           fileInput(ns("groups_file"), label = NULL))
          #     ))),
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
    
    # When the lacytools summary has been read in, the converted data is shown
    # in the data table
    output$data_table <- DT::renderDT({
      req(summary$data())
      
      DT::datatable(summary$data(),
                    options = list(scrollX = TRUE),
                    filter = "top")
      # if (isTruthy(x$data_incl_metadata)){
      #   DT::datatable(x$data_incl_metadata,
      #                 options = list(scrollX = TRUE),
      #                 filter = "top")
      # } else { if (isTruthy(x$data_incl_plate_design)){
      #   DT::datatable(x$data_incl_plate_design,
      #                 options = list(scrollX = TRUE),
      #                 filter = "top")
      # } else {
      #   DT::datatable(x$data,
      #                 options = list(scrollX = TRUE),
      #                 filter = "top")
      # }
      # }
    })
    
    # Hide the metadata menu until metadata is uploaded:
    observe({
      shinyjs::toggle("metadata_menu", condition = !is.null(x$metadata))
    })
    
    # Plate design ------------------------------------------------------------
    
    # observe({
    #   shinyjs::toggle("sample_list_ui", 
    #                   condition = input$sample_id_method == "Upload a sample list")
    #   shinyjs::toggle("one_plate_design",
    #                   condition = all(
    #                     input$sample_id_method == "Upload a plate design",
    #                     !isTruthy(input$switch_two_plate_designs)
    #                   ))
    #   shinyjs::toggle("two_plate_designs",
    #                   condition = all(
    #                     input$sample_id_method == "Upload a plate design",
    #                     isTruthy(input$switch_two_plate_designs)
    #                   ))
    #   shinyjs::toggle("switch_two_plate_designs",
    #                   condition = all(
    #                     input$Ig_data == "Yes",
    #                     input$sample_id_method == "Upload a plate design"
    #                   ))
    # })
    # 
    # plate_design_file <- mod_fileInput_with_info_server(
    #   id = "plate_design",
    #   allowed = c("xlsx", "xls")
    # )
    # 
    # plate_design_specific_file <- mod_fileInput_with_info_server(
    #   id = "plate_design_specific",
    #   allowed = c("xlsx", "xls")
    # )
    # 
    # sample_list_file <- mod_fileInput_with_info_server(
    #   id = "sample_list",
    #   allowed = c("xlsx", "xls")
    # )
    # 
    # observe({
    #   req(input$plate_design_total)
    #   shinyFeedback::feedbackWarning("plate_design_total",
    #                                  !(ext_plate_design_total() %in% c("xlsx", "xls")),
    #                                  text = "Please upload a .xlsx or .xls file.")
    # })
    # 
    # # This observe call ensures that the add_plate_design actionButton is only
    # # enabled under the right circumstances
    # observe({
    #   shinyjs::disable(id = "add_plate_design")
    #   if (all(isTruthy(x$data),
    #           isTruthy(input$plate_design))) {
    #     if (ext_plate_design() %in% c("xlsx", "xls")) {
    #       shinyjs::enable(id = "add_plate_design")
    #     } 
    #   } 
    #   if (all(isTruthy(x$data),
    #           isTruthy(input$plate_design_specific),
    #           isTruthy(input$plate_design_total))) {
    #     if (all(ext_plate_design_specific() %in% c("xlsx", "xls"),
    #             ext_plate_design_total() %in% c("xlsx", "xls"))) {
    #       shinyjs::enable(id = "add_plate_design")
    #     }
    #   }
    # })
    # 
    # # When the add_plate_design actionButton is clicked, the plate_design file is
    # # read in and a pop-up is shown with the automatically determined sample types.
    # observeEvent(input$add_plate_design, {
    #   
    #   # The x$data_incl_metadata reactiveVal is reset to NULL, so that users can
    #   # change the plate design file after metadata has already been added:
    #   if (isTruthy(x$data_incl_metadata)) {
    #     x$data_incl_metadata <- NULL
    #     showNotification("The metadata has to be re-added to the data",
    #                      type = "warning")
    #   }
    #   
    #   if (!isTruthy(input$switch_two_plate_designs)) {
    #     tryCatch(
    #       expr = {
    #         x$plate_design <- read_and_process_plate_design(input$plate_design$datapath)
    #       },
    #       error = function(e) {
    #         showNotification(
    #           paste(
    #             "Please check that your plate design file is formatted correctly.",
    #             "Click on the information icon to find the required format."
    #           ), 
    #           type = "error",
    #           duration = NULL
    #         )
    #         print(e$class)
    #       },
    #       warning = function(w) {
    #         showNotification(
    #           w$message,
    #           type = "error",
    #           duration = NULL
    #         )
    #         
    #         # Continue reading plate design despite the warning:
    #         x$plate_design <- read_and_process_plate_design(input$plate_design$datapath)
    #       }
    #     )
    #   } else {
    #     tryCatch(
    #       expr = {
    #         plate_design_specific <- read_and_process_plate_design(input$plate_design_specific$datapath) %>% 
    #           dplyr::mutate(group = input$keyword_total)
    #         
    #         plate_design_total <- read_and_process_plate_design(input$plate_design_total$datapath) %>% 
    #           dplyr::mutate(group = input$keyword_specific)
    #         
    #         x$plate_design <- dplyr::full_join(plate_design_specific,
    #                                            plate_design_total)
    #       },
    #       error = function(e) {
    #         showNotification(
    #           paste(
    #             "Please check that your plate design file is formatted correctly.",
    #             "Click on the information icon to find the required format."
    #           ), 
    #           type = "error",
    #           duration = NULL
    #         )
    #       },
    #       warning = function(w) {
    #         showNotification(
    #           w$message,
    #           type = "error",
    #           duration = NULL
    #         )
    #         
    #         # Continue reading plate design despite the warning:
    #         plate_design_specific <- read_and_process_plate_design(input$plate_design_specific$datapath) %>% 
    #           dplyr::mutate(group = input$keyword_total)
    #         
    #         plate_design_total <- read_and_process_plate_design(input$plate_design_total$datapath) %>% 
    #           dplyr::mutate(group = input$keyword_specific)
    #         
    #         x$plate_design <- dplyr::full_join(plate_design_specific,
    #                                            plate_design_total)
    #       }
    #     )
    #   }
      
      # Reset x$response in case the pop-up has been shown before:
      x$response <- NULL
      
      # Don't show pop-up if reading in the plate design has failed:
      req(x$plate_design)
      
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
        confirmButtonCol = "#3c8dbc",
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
        x$data_incl_plate_design <- dplyr::left_join(x$data, 
                                                     x$plate_design)
        showNotification("The sample types were added to the data", 
                         type = "message")
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
        x$data_incl_metadata <- dplyr::left_join(x$data_incl_plate_design, x$merged_metadata)
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
