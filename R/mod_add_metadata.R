#' add_metadata UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_add_metadata_ui <- function(id){
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
      " .fas {float: right; margin-right: 5px; font-size: 18px}",
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
    ))),
  
    shinydashboardPlus::box(
      id = ns("box"),
      title = div(
        id = ns("box_header"),
        "Upload your metadata (optional)",
        icon("info-circle", class = "ml") %>% 
          bsplus::bs_embed_popover(
            title = "Explanation",
            content = HTML(
              "Your metadata Excel file should contain a column with the sample ID's, and one or more columns with metadata.",
              "<br><br>",
              "<strong>Each sample ID should be present only once in your Excel file</strong>",
              "(even if it is present multiple times in your plate design)."
            ),
            trigger = "hover", 
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
              downloadButton(ns("download_example_metadata"),
                             "Download a metadata example file")),
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
      fileInput(ns("file"), 
                "Upload one or more metadata Excel file(s) or R object(s):",
                multiple = TRUE),
      div(
        id = ns("metadata_menu"),
        uiOutput(ns("sample_id"))
      ),
      actionButton(ns("button"), "Add the metadata")
    )
  )
}
    
#' add_metadata Server Functions
#'
#' @noRd 
mod_add_metadata_server <- function(id, LaCyTools_summary){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    # Read in the metadata files when they are uploaded, or show a warning when
    # any of the uploaded files are of the wrong type:
    metadata_list <- reactive({
      req(input$file)
      
      shinyFeedback::hideFeedback("file")
      
      metadata_list <- tryCatch(
        expr = {
          read_metadata(filepaths = input$file$datapath,
                        filenames = input$file$name)
        },
        wrong_extension = function(c) {
          shinyFeedback::feedbackDanger("file",
                                        show = TRUE,
                                        text = c$message)
          
          NULL
        })
      
      return(metadata_list)
    })
    
    
    filenames_metadata <- reactive({
      req(input$file)
      comma_and(input$file$name)
    })
    
    # Create inputIds for the sample_id_column selectizeInputs based on the 
    # number of metadata files that were uploaded:
    sample_id_inputIds <- reactive({
      req(metadata_list())
      purrr::map(seq_len(length(metadata_list())),
                 ~ paste0("sample_id_column", .x))
    })
    
    
    # Create selectizeInputs for the sample_id_columns. The number of inputs 
    # created is the same as the number of metadata files that were uploaded.
    output$sample_id <- renderUI({
      req(sample_id_inputIds())
      purrr::pmap(list(sample_id_inputIds(),
                       metadata_list(),
                       names(metadata_list())),
                  function(inputId, metadata, metadata_name) { 
                    selectizeInput(
                      ns(inputId),
                      label = paste("Which column in", 
                                    metadata_name, 
                                    "contains the sample ID's?"),
                      # The choices for each input correspond to the names of the 
                      # columns in the metadata file:
                      choices = c("", unique(colnames(metadata))),
                      selected = NULL,
                      multiple = FALSE,
                      options = list(placeholder = "select a column"))
                  })
    })
    
    # Hide the metadata menu until metadata is uploaded:
    observe({
      shinyjs::toggle("metadata_menu", 
                      condition = is_truthy(metadata_list()))
    })
    
    # Check if the inputs for the sample ID columns have been filled in by the
    # user.
    sample_id_inputs_completed <- reactive({
      if (is_truthy(sample_id_inputIds())) {
        all(purrr::map_lgl(sample_id_inputIds(),
                           ~ is_truthy(input[[.x]])))
      } else {
        TRUE
      }
    })
    
    
    # This observe call ensures that the actionButton is only enabled under the
    # right circumstances
    observe({
      shinyjs::toggleState("button",
                           condition = all(
                             is_truthy(metadata_list()),
                             is_truthy(LaCyTools_summary()),
                             sample_id_inputs_completed()
                           ))
    })
    
    
    # Merge the metadata dataframes in metadata_list() together:
    merged_metadata <- reactive({
      req(
        metadata_list(),
        all(purrr::map_lgl(sample_id_inputIds(),
                           ~ isTruthy(input[[.x]])))
      )
      # For all metadata files in the metadata_list: Rename the column that the
      # user indicated as the sample ID column to "sample_id" so that the
      # metadata can later be joined with the data using the sample_id column as
      # the key:
      prepped_metadata <- purrr::pmap(
        list(metadata_list(),
             sample_id_inputIds()),
        function(metadata, 
                 sample_id_inputId) {
          sample_id_column <- input[[sample_id_inputId]]
          
          renamed <- tryCatch(
            expr = {
              rename_sample_id_column(metadata = metadata,
                                      sample_id_column = sample_id_column)
          },
          sample_id_conflict = function(c) {
            showNotification(c$message,
                             type = "warning",
                             duration = NULL)
            
            rename_sample_id_column(metadata = metadata,
                                    sample_id_column = sample_id_column)
          })
          
          return(renamed)
        })

      # Join all the metadata together (in case the user uploaded more than one
      # metadata file):
      merged_metadata <- purrr::reduce(prepped_metadata,
                                       dplyr::full_join,
                                       by = "sample_id")

      
      # Check for column names that are not allowed.
      # Vector will be length 0 if there are no forbidden column names.
      
      forbidden_colnames <- check_column_names(merged_metadata)

      if (length(forbidden_colnames) != 0) {
        
        forbidden_colnames_table <- DT::datatable(data.frame(forbidden_colnames),
                                                  options = list(
                                                    scrollY = "100px",
                                                    paging = FALSE,
                                                    searching = FALSE,
                                                    columnDefs = list(
                                                      list(
                                                        className = 'dt-center',
                                                        targets = "_all"))),
                                                  colnames = "",
                                                  rownames = FALSE)
        
        shinyalert::shinyalert(
          html = TRUE,
          text = tagList(
            "The following column names in your metadata are not allowed:",
            forbidden_colnames_table,
            br(),
            "Please rename (or remove) these columns, and try again."
          ),
          size = "m",
          confirmButtonText = "OK",
          confirmButtonCol = "tomato",
          showCancelButton = FALSE,
          showConfirmButton = TRUE
        )

        shinybusy::remove_modal_spinner()

        return(NULL)
      } else return(merged_metadata)
    }) %>% bindEvent(input$button)
    
    
    # Check for duplicate sample IDs
    unique_sample_ids <- reactive({
      req(merged_metadata())
      sample_ids <- merged_metadata()$sample_id
      all_unique <- length(sample_ids) == length(unique(sample_ids))
      if (all_unique) {
        return(TRUE)
      } else {
        # Show table with duplicate sample IDs
        non_unique_ids <- sample_ids[duplicated(sample_ids)]
        duplicates_table <- DT::datatable(data.frame(non_unique_ids),
                                          options = list(
                                            scrollY = "150px",
                                            paging = FALSE,
                                            searching = FALSE,
                                            columnDefs = list(
                                              list(
                                                className = 'dt-center',
                                                targets = "_all"))),
                                          colnames = "sample_id",
                                          rownames = FALSE)

        shinyalert::shinyalert(
          html = TRUE,
          text = tagList(
            "The following sample ID's are present more than once in your file:",
            duplicates_table,
            br(),
            "Please change your file such that each sample ID is present only once, and try again."
          ),
          size = "m",
          confirmButtonText = "OK",
          confirmButtonCol = "tomato",
          showCancelButton = FALSE,
          showConfirmButton = TRUE
        )
        
        # Remove the spinner and return FALSE
        shinybusy::remove_modal_spinner()
        return(FALSE)
      }
    })
    
    
    # Check for unmatched id's
    unmatched_ids <- reactive({
      req(merged_metadata(),
          unique_sample_ids() == TRUE,
          LaCyTools_summary())
      unmatched <- setdiff(LaCyTools_summary()$sample_id,
                           merged_metadata()$sample_id)
      
      if (rlang::is_empty(unmatched)) {
        return("none")
      } else {
        return(unmatched)
      }
    })
    
    # If there are unmatched sample ID's a pop-up is shown.
    observe({
      req(!isTRUE(all.equal(unmatched_ids(), "none")))
      shinyalert::shinyalert(
        inputId = "popup",
        html = TRUE,
        text = tagList(
          paste(length(unmatched_ids()),
                "sample ID's in the data had no match in the metadata:"),
          DT::dataTableOutput(ns("unmatched_ids_table")),
          br(),
          "Please check: 1) Does the spelling of sample IDs in your metadata correspond to the spelling in your plate design?",
          "and 2) Have you selected the correct sample ID columns?"
        ),
        size = "m",
        confirmButtonText = "Add the metadata despite the unmatched ID's",
        confirmButtonCol = "#3c8dbc",
        showCancelButton = TRUE,
        cancelButtonText = "Don't add the metadata now",
        type = ifelse(length(unmatched_ids()) > 20, "warning", "")
      )
    })
    
    
    r <- reactiveValues(master_button = 0)
    
    observe({
      if (!isTRUE(all.equal(unmatched_ids(), "none")) & is_truthy(input$popup)) {
        r$master_button <- isolate(r$master_button) + 1
      } else {
        if (isTRUE(all.equal(unmatched_ids(), "none"))) {
          r$master_button <- isolate(r$master_button) + 1
        }
      }
    }, priority = 5) %>% bindEvent(input$popup, input$button)
    
    with_metadata <- reactive({
      req(unmatched_ids(), !is.null(merged_metadata()), unique_sample_ids() == TRUE)
      if(any(
        isTRUE(all.equal(unmatched_ids(), "none")),
        is_truthy(input$popup)
      )) {
        dplyr::left_join(LaCyTools_summary(),
                         merged_metadata(),
                         by = "sample_id") %>% 
          dplyr::relocate(colnames(merged_metadata())[-1], .after = sample_id)
      } else {
        NULL
      }
    })

    
    # This is the datatable containing the unmatched sample ID's that is shown 
    # in the pop-up:
    output$unmatched_ids_table <- DT::renderDataTable({
      req(!isTRUE(all.equal(unmatched_ids(), "none")))
      unmatched <- matrix(unmatched_ids())
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
    },
    server = FALSE)
    
    
    
    # Show a spinner while the metadata is being processed.
    observeEvent(input$button, {
      shinybusy::show_modal_spinner(
        spin = "cube-grid", color = "#0275D8", 
        text = HTML("<br/><strong>Processing metadata...")
      )
    }, priority = 50)

    # Remove the spinner when metadata is added.
    observeEvent(with_metadata(), {
      shinybusy::remove_modal_spinner()
    })
    
    # Remove spinner when user cancels after warning popup
    # This takes too long
    observeEvent(input$popup, {
      if (input$popup == FALSE) {
        shinybusy::remove_modal_spinner()
      }
    }, priority = 20)
    
    
    # Download example metadata file
    output$download_example_metadata <- downloadHandler(
      filename = "metadata_example.xlsx",
      content = function(file) {
        example_file <- system.file("app",
                                    "www",
                                    "metadata_example.xlsx",
                                    package = "GlycoDash")
        file.copy(example_file, file)
      }
    )
    
    # Only return merged_metadata if sample IDs are unique.
    merged_metadata_to_return <- reactive({
      req(merged_metadata(), unique_sample_ids() == TRUE)
      merged_metadata()
    })
    
    return(list(
      data = with_metadata,
      button = reactive({r$master_button}),
      filenames_metadata = filenames_metadata, # pass the filenames along for the report
      merged_metadata = merged_metadata_to_return  # for combining with normalized data
      ))
    
  })
}
