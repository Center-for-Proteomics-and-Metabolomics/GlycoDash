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
  shinydashboard::box(
    title = "Upload your metadata",
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
}
    
#' add_metadata Server Functions
#'
#' @noRd 
mod_add_metadata_server <- function(id, LacyTools_summary){
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
                             is_truthy(LacyTools_summary()),
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
      
      return(merged_metadata)
    }) %>% bindEvent(input$button)
    
    unmatched_ids <- reactive({
      req(merged_metadata(),
          LacyTools_summary())
      
      unmatched <- setdiff(LacyTools_summary()$sample_id,
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
          "Please check: 1) Does the spelling of sample IDs in your metadata corresponds to the spelling in your plate design?",
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
    }) %>% bindEvent(input$popup, input$button)
    
    with_metadata <- reactive({
      req(unmatched_ids())
      if(any(
        isTRUE(all.equal(unmatched_ids(), "none")),
        is_truthy(input$popup)
      )) {
        dplyr::left_join(summary(),
                         merged_metadata(),
                         by = "sample_id")
      } else {
        NULL
      }
      
    })
    
    observe({
      showNotification("The metadata is being added to the data. This may take a while",
                       type = "message")
    }) %>% bindEvent(with_metadata())
    
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
    
    return(list(
      data = with_metadata,
      button = reactive({r$master_button}),
      filenames_metadata = filenames_metadata # pass the filenames along for the report
      ))
    
  })
}
