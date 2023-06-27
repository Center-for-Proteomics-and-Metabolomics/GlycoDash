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
    numericInput(
      ns("n_summaries"),
      "How many LaCyTools summary.txt files do you want to upload?",
      value = 1, min = 1, max = 10
    ),
    uiOutput(ns("summaries")),
    fileInput(ns("lacytools_summary"),
              "Upload LaCyTools summary.txt file:"),
    shinyWidgets::awesomeRadio(ns("contains_total_and_specific_samples"),
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
                 "Load the LaCyTools summary files")
  )
}
    
#' read_lacytools Server Functions
#'
#' @noRd 
mod_read_lacytools_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    # Create inputIds for the LaCyTools summaries
    summary_inputIds <- reactive({
      purrr::map(
        seq_len(input$n_summaries),
        ~ paste0("summary", .x)
      )
    })
    
    # Create fileInputs for the summary.txt files.
    output$summaries <- renderUI({
      purrr::imap(
        summary_inputIds(),
        function(inputId, i) fileInput(
          ns(inputId), # This creates input$summary1, input$summary2 etc.
          label = paste("Upload your", getOrdinalSuffix(i), "LaCyTools summary.txt file:")
        )
      )
    })
    
    
    
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
    
    
    
    # Create a named list with file extensions.
    # Names: "summary1", "summary2", etc.
    extensions <- reactive({
      extensions <- purrr::map(summary_inputIds(), 
                               ~ tools::file_ext(input[[.x]]$name))
      names(extensions) <- summary_inputIds()
      return(extensions)
    })
    
    # Show a warning in case of wrong file extension
    observe({
      purrr::map(summary_inputIds(), ~ shinyFeedback::hideFeedback(.x))
      
      has_non_txt_character <- purrr::map(extensions(), ~ is.character(.x) && .x != "txt")
      
      if (any(unlist(has_non_txt_character))) {
        purrr::imap(
          extensions(), function(extension, inputId) {
            shinyFeedback::feedbackWarning(
              inputId,
              show = (extension != "txt" && typeof(extension) == "character"),
              text = "Please upload a .txt file."
            )}
        )}
    })
    
    
    # Make sure that the read_summary actionButton is only available once the
    # right type of file is uploaded as LaCyTools summary and once the user has
    # provided all required inputs:
    observe({
      shinyjs::toggleState(
        id = "button",
        condition = any(
          all(
            input$contains_total_and_specific_samples == "No",
            # is_truthy(lacytools_summary())
            all(extensions() == "txt")
          ),
          all(
            input$contains_total_and_specific_samples == "Yes",
            is_truthy(lacytools_summary_total_and_specific())
          )
        )
      )
    })

    
    
    # If the user indicates (via input$contains_total_and_specific_samples) that the data contains total and
    # specific Ig samples, the textInputs for the specific and total keywords
    # are shown.
    observe({
      shinyjs::toggle("keywords_specific_total",
                      condition = input$contains_total_and_specific_samples == "Yes")
    })
    
    # If the user changes input$contains_total_and_specific_samples to "No"  the
    # textInputs for the keywords are reset to empty strings "". This is needed
    # in case the user first fills in keywords but then changes their mind.
    observe({
      updateTextInput("keyword_specific",
                      value = "",
                      session = session)
      updateTextInput("keyword_total",
                      value = "",
                      session = session)
    }) %>% bindEvent(input$contains_total_and_specific_samples == "No")
    
    
    # Read the raw LaCyTools summary file
    raw_lacytools_summary <- reactive({
      req(input$lacytools_summary$datapath)
      tryCatch(
        expr = {
          read_non_rectangular(input$lacytools_summary$datapath)
        },
        embedded_null = function(c) {
          shinyFeedback::feedbackDanger("lacytools_summary",
                                        show = TRUE,
                                        text = c$message)
          NULL
        },
        empty_file = function(c) {
          shinyFeedback::feedbackDanger("lacytools_summary",
                                        show = TRUE,
                                        text = c$message)
          NULL
        },
        wrong_delim = function(c) {
          shinyFeedback::feedbackDanger("lacytools_summary",
                                        show = TRUE,
                                        text = c$message)
          NULL
        }
      )
    })
    
    
    
    # Read the raw LaCyTools summary files
    raw_lacytools_summaries <- reactive({
      
      purrr::map(summary_inputIds(), function(inputId) {
          if (!is.null(input[[inputId]]$datapath)) {
            tryCatch(
              expr = {
                read_non_rectangular(input[[inputId]]$datapath)
              },
              embedded_null = function(c) {
                shinyFeedback::feedbackDanger(inputId,
                                              show = TRUE,
                                              text = c$message)
                NULL
              },
              empty_file = function(c) {
                shinyFeedback::feedbackDanger(inputId,
                                              show = TRUE,
                                              text = c$message)
                NULL
              },
              wrong_delim = function(c) {
                shinyFeedback::feedbackDanger(inputId,
                                              show = TRUE,
                                              text = c$message)
                NULL
              }
            )
          } else NULL
        })
    })
    # For some reason I need this observer to make the warnings show up...
    observe(req(raw_lacytools_summaries()))
    
    
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
    
    observe({
      if (is_truthy(warn_duplicated_analytes())) {
        showNotification(warn_duplicated_analytes(),
                         type = "warning",
                         duration = NULL)
      }
    }) %>% bindEvent(input$read_summary)
    
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
          
          showNotification(c$message,
                           type = "error",
                           duration = NULL)
          
          # Return NULL to the reactive lacytools_summary()
          NULL
        })
      
      return(tidy_summary)
    })
    
    lacytools_summary_total_and_specific <- reactive({
      
      shinyFeedback::hideFeedback("keyword_specific")
      shinyFeedback::hideFeedback("keyword_total")
      
      # Pause here until lacytools_summary() is Truthy, and until the inputs for
      # the keywords are not empty:
      req(lacytools_summary(),
          input$keyword_specific,
          input$keyword_total)
      
      lacytools_summary <- tryCatch(
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
        },
        NAs = function(c) {
          showNotification(c$message,
                           type = "error",
                           duration = NULL)
          NULL
        })
      
      return(lacytools_summary)
    })
    
    to_return <- reactive({
      req(lacytools_summary())
      summary_to_return <- tryCatch(lacytools_summary_total_and_specific(),
                          error = function(e) {
                            lacytools_summary()
                          })
      return(summary_to_return)
    }) # can't use bindEvent(input$button), because then re-adding sample ID's 
    # to a newly uploaded summary is faster then resetting the plate_design to NULL,
    # which results in an empty sample_id column added to the new summary
    
    return(list(
      data = to_return,
      button = reactive({input$button}),
      keyword_specific = reactive({input$keyword_specific}),
      keyword_total = reactive({input$keyword_total}),
      contains_total_and_specific_samples = reactive({input$contains_total_and_specific_samples}),
      lacytools_fileInput = reactive({input$lacytools_summary})
    ))
    
  })
}
    
## To be copied in the UI
# mod_read_lacytools_ui("read_lacytools_ui_1")
    
## To be copied in the server
# mod_read_lacytools_server("read_lacytools_ui_1")
