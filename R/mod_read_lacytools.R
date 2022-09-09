#' read_lacytools UI Function
#'
#' @description A shiny Module to upload and read a LacyTools summary file.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_read_lacytools_ui <- function(id){
  ns <- NS(id)
  
  shinydashboard::box(
    title = "Upload your data",
    width = NULL,
    solidHeader = TRUE,
    status = "primary",
    fileInput(ns("lacytools_summary"), 
              "Upload LacyTools summary.txt file:"),
    shinyWidgets::awesomeRadio(ns("Ig_data"),
                               "Does your data contain total and specific immunoglobulin samples?",
                               choices = c("Yes", "No"),
                               selected = "No"),
    div(id = ns("keywords_specific_total"),
        # Set the width of popovers in this div to 200px:
        tags$style(HTML(paste0("#",
                               ns("keywords_specific_total"),
                               " .popover{width: 200px !important;}"))),
        textInput(ns("keyword_specific"), 
                  label = "By what keyword can the specific Ig samples be recognized?") %>% 
          bsplus::bs_embed_popover(
            title = "Explanation",
            content = paste("All specific Ig samples should have sample names",
                            "that contain this keyword. The keyword is case-sensitive."),
            trigger = "hover",
            placement = "right"),
        textInput(ns("keyword_total"), 
                  label = "By what keyword can the total Ig samples be recognized?") %>% 
          bsplus::bs_embed_popover(
            title = "Explanation",
            content = paste("All total Ig samples should have sample names",
                            "that contain this keyword. The keyword is case-sensitive."),
            trigger = "hover",
            placement = "right")
    ),
    actionButton(ns("button"),
                 "Load the LacyTools summary file")
  )
}
    
#' read_lacytools Server Functions
#'
#' @noRd 
mod_read_lacytools_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    # Make a reactive expression containing the file extension of uploaded file:
    extension <- reactive({
      req(input$lacytools_summary)
      tools::file_ext(input$lacytools_summary$name)
    })
    
    # Show a warning when the wrong type of file is uploaded as lacytools summary:
    observe({
      req(extension())
      shinyFeedback::feedbackWarning("lacytools_summary",
                                     extension() != "txt",
                                     text = "Please upload a .txt file.")
    })
    
    # Make sure that the read_summary actionButton is only available once the
    # right type of file is uploaded as lacytools summary and once the user has
    # provided all required inputs:
    observe({
      shinyjs::toggleState(
        id = "button",
        condition = any(
          all(
            input$Ig_data == "No",
            is_truthy(lacytools_summary())
          ),
          all(
            input$Ig_data == "Yes",
            is_truthy(lacytools_summary_Ig_data())
          )
        )
      )
    })
    
    # If the user indicates (via input$Ig_data) that the data contains total and
    # specific Ig samples, the textInputs for the specific and total keywords
    # are shown.
    observe({
      shinyjs::toggle("keywords_specific_total",
                      condition = input$Ig_data == "Yes")
    })
    
    raw_lacytools_summary <- reactive({
      req(input$lacytools_summary$datapath)
      read_non_rectangular(input$lacytools_summary$datapath)
    })
    
    warn_duplicated_analytes <- reactive({
      req(raw_lacytools_summary())
      # Create the object 'warning' to save the result of for-loop in:
      warning <- NULL
      
      # Try to get_block() with each output, until either a block is found
      # without errors/warnings, or until the 'duplicated_analytes' warning
      # occurs:
      for (output in outputs) {
        result <- tryCatch(
          expr = {
            get_block(data = raw_lacytools_summary(),
                      variable = output)
            # If get_block has run without errors/warnings, return NULL
            NULL
          },
          lacytools_output_not_found = function(c) {
            # If this error occurs, return 'block_not_found"
            "block_not_found"
          },
          duplicated_analytes = function(c) {
            # If this warning occurs, return warning message
            c$message
          })
        
        # Save the result of this round in warning
        warning <- result
        
        # Unless result of this round was "block_not_found", exit the loop
        if (result != "block_not_found") {
          break
        }
      }
      
      # In case we have looped through all outputs and no block was found,
      # return 'warning' as NULL
      if (warning == "block_not_found") {
        warning <- NULL
      }
      
      return(warning)
    })
    
    observeEvent(input$read_summary, {
      if (is_truthy(warn_duplicated_analytes())) {
        showNotification(warn_duplicated_analytes(),
                         type = "warning",
                         duration = NULL)
      }
    })
    
    lacytools_summary <- reactive({
      req(raw_lacytools_summary())
      tidy_summary <- tryCatch(
        expr = {
          convert_lacytools_summary(data = raw_lacytools_summary())
        },
        no_outputs_present = function(c) {
          # Show feedback that there are no outputs found in the LacyTools file:
          shinyFeedback::feedbackDanger("lacytools_summary",
                                        show = TRUE,
                                        text = c$message)
          # Return NULL to the reactive lacytools_summary()
          NULL
        })
      return(tidy_summary)
    })
    
    lacytools_summary_Ig_data <- reactive({
      
      shinyFeedback::hideFeedback("keyword_specific")
      shinyFeedback::hideFeedback("keyword_total")
      
      # Pause here until lacytools_summary() is Truthy, and until the inputs for
      # the keywords are not empty:
      req(
        lacytools_summary(),
        input$keyword_specific,
        input$keyword_total
      )
      
      summary <- tryCatch(
        expr = {
          # Detect based on sample names which samples are Total Ig and which are
          # Specific Ig samples
          detect_group(data = lacytools_summary(),
                       keyword_specific = input$keyword_specific,
                       keyword_total = input$keyword_total)
        },
        unmatched_keyword_specific = function(c) {
          shinyFeedback::feedbackDanger(
            inputId = "keyword_specific",
            show = TRUE,
            text = paste("This keyword did not match any sample names in your data.", 
                         "Please choose a different keyword.")
          )
          NULL
        },
        unmatched_keyword_total = function(c) {
          shinyFeedback::feedbackDanger(
            inputId = "keyword_total",
            show = TRUE,
            text = paste("This keyword did not match any sample names in your data.", 
                         "Please choose a different keyword.")
          )
          NULL
        })
      
      return(summary)
    })
    
    to_return <- reactive({
      req(lacytools_summary())
      summary <- tryCatch(lacytools_summary_Ig_data(),
                          error = function(e) {
                            lacytools_summary()
                          })
      return(summary)
    }) # can't use bindEvent(input$button), because then re-adding sample ID's 
    # to a newly uploaded summary is faster then resetting the plate_design to NULL,
    # which results in an empty sample_id column added to the new summary
    
    return(list(
      data = to_return,
      button = reactive({input$button}),
      keyword_specific = reactive({input$keyword_specific}),
      keyword_total = reactive({input$keyword_total}),
      Ig_data = reactive({input$Ig_data}),
      lacytools_fileInput = reactive({input$lacytools_summary})
    ))
    
  })
}
    
## To be copied in the UI
# mod_read_lacytools_ui("read_lacytools_ui_1")
    
## To be copied in the server
# mod_read_lacytools_server("read_lacytools_ui_1")
