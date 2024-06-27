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
        "Upload your metadata (optional)",
        icon("info-circle", class = "ml") %>% 
          bsplus::bs_embed_popover(
            title = "Explanation",
            content = HTML(
              "
              Your metadata Excel file should contain a named column that contains the sample ID's,
              and one or more named columns with metadata (e.g. \"age\", \"sex\", \"disease\").
              <br> <br>
              Each sample ID should be present only once in your file, even if it is present
              multiple times in your plate design.
              <br> <br>
              For an example file, click the paperclip button.
              "
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
      )
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
      
      metadata_list <- tryCatch({
        read_metadata(input$file$datapath, input$file$name)
      }, error = function(e) {
        shinyFeedback::feedbackDanger("file", show = TRUE, text = "Please upload a .xlsx, .xls or .rds file")
        return(NULL)
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
    
    
    
    rv <- reactiveValues() 
    
    
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
        
        # Pass to reactiveValues vector
        rv$forbidden_colnames <- forbidden_colnames
        
        shinyalert::shinyalert(
          html = TRUE,
          text = paste(
            "The following column names in your metadata are not allowed:",
            shinycssloaders::withSpinner(DT::dataTableOutput(ns("popup_table"))),
            "<br>Please rename these columns and re-upload your metadata file."
          ),
          size = "m",
          confirmButtonText = "OK",
          confirmButtonCol = "tomato",
          showCancelButton = FALSE,
          showConfirmButton = TRUE
        )
        
        shinyFeedback::feedbackDanger("file", show = TRUE, text = "Please adjust your metadata file.")

        return(NULL)
      } else {
        rv$forbidden_colnames <- NULL
        return(merged_metadata)
      }
    })
    
    output$popup_table <- DT::renderDataTable({
      DT::datatable(
        data.frame(rv$forbidden_colnames),
        options = list(
          scrollY = "150px",
          paging = FALSE,
          searching = FALSE,
          columnDefs = list(
            list(
              className = 'dt-center',
              targets = "_all"))),
        colnames = "Column name",
        rownames = FALSE
      )
    }) 
    
    
    
    # Check for duplicate sample IDs
    unique_sample_ids <- reactive({
      req(merged_metadata())
      sample_ids <- merged_metadata()$sample_id
      all_unique <- length(sample_ids) == length(unique(sample_ids))
      if (all_unique) {
        rv$non_unique_ids <- NULL
        return(TRUE)
      } else {
        # Show table with duplicate sample IDs
        rv$non_unique_ids <- sample_ids[duplicated(sample_ids)]
        shinyalert::shinyalert(
          html = TRUE,
          text = paste(
            "The following sample ID's are present more than once in your file:",
            shinycssloaders::withSpinner(DT::dataTableOutput(ns("popup_table_duplicates")))
          ),
          size = "m",
          confirmButtonText = "OK",
          confirmButtonCol = "tomato",
          showCancelButton = FALSE,
          showConfirmButton = TRUE
        )
        
        shinyFeedback::feedbackDanger("file", show = TRUE, text = "Please adjust your metadata file.")
        
        return(FALSE)
      }
    })
    
    output$popup_table_duplicates <- DT::renderDataTable({
      DT::datatable(
        data.frame(rv$non_unique_ids),
        options = list(
          scrollY = "150px",
          paging = FALSE,
          searching = FALSE,
          columnDefs = list(
            list(
              className = 'dt-center',
              targets = "_all"))),
        colnames = "sample_id",
        rownames = FALSE
      )
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
        text = paste(
          length(unmatched_ids()),
          "sample ID's in the data had no match in the metadata:",
          shinycssloaders::withSpinner(DT::dataTableOutput(ns("popup_table_unmatched"))),
          "<br>Please check: 1) Does the spelling of sample IDs in your metadata correspond to the spelling in your plate design?",
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
    
    output$popup_table_unmatched <- DT::renderDataTable({
      req(!isTRUE(all.equal(unmatched_ids(), "none")))
      unmatched <- matrix(unmatched_ids())
      table <- DT::datatable(unmatched,
                             options = list(
                               scrollY = "150px",
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
      filenames_metadata = filenames_metadata, # pass the filenames along for the report
      merged_metadata = merged_metadata_to_return  # for combining with normalized data
      ))
    
  })
}
