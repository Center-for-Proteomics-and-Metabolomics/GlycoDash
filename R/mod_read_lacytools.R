#' read_lacytools UI Function
#'
#' @description A shiny Module to upload and read a LaCyTools summary file.
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
      value = 1, min = 1, max = 50
    ),
    uiOutput(ns("summaries")),
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
    
    
    # Create a list with names of the uploaded summary files.
    summary_filenames <- reactive({
      req(summary_inputIds())
      purrr::map(summary_inputIds(), function(inputId) input[[inputId]]$name)
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
    
    
    
    # Create a vector that contains the raw LaCyTools summary files.
    raw_lacytools_summaries <- reactive({
      req(summary_inputIds())
      
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
    # For some reason I need this observer to make the warnings show up.
    observe(req(raw_lacytools_summaries()))

  
    
    # Check for duplicate analytes in each uploaded summary file.
    warning_duplicate_analytes <- reactive({
      req(!all(sapply(raw_lacytools_summaries(), is.null)))  # Don't do anything if no summary was yet uploaded.
      
      purrr::map(raw_lacytools_summaries(), function(summary) {
        
        if (is.null(summary)) {
          return(NULL)
        }
        else {
          # Create the object 'warning' to save the result of for-loop in:
          warning <- NULL
          
          # Try to get_block() with each output, until either a block is found
          # without errors/warnings, or until the 'duplicated_analytes' warning
          # occurs:
          for (output in outputs) {
            result <- tryCatch(
              expr = {
                get_block(data = summary,
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
            if (list(result) != "block_not_found") {
              break
            }
          }
          
          # In case we have looped through all outputs and no block was found,
          # return 'warning' as NULL
          if (list(warning) == "block_not_found") {
            warning <- NULL
          }
          
          return(warning)
        }
      })
    })
    
    
    # Show warning for duplicate analytes when applicable.
    observe({
      req(!all(sapply(warning_duplicate_analytes(), is.null)))
      for (i in seq_along(warning_duplicate_analytes())) {
        warning <- warning_duplicate_analytes()[[i]]
        if (!is.null(warning)) {
          showNotification(
            paste0("In LaCyTools summary file number ", i, ", ", warning),
            type = "warning",
            duration = NULL
          )}
        }
      }) %>% bindEvent(input$button)
    
    
    # Show spinner while processing LaCyTools summaries
    observe({
      req(!any(sapply(raw_lacytools_summaries(), is.null)))
      shinybusy::show_modal_spinner(
        spin = "cube-grid", color = "#0275D8",
        text = HTML("<br/><strong>Processing LaCyTools summaries...")
      )
    }, priority = 5)
    
    # Create a list with tidy LaCyTools summaries
    lacytools_summaries <- reactive({
      req(!any(sapply(raw_lacytools_summaries(), is.null))) 
      
      tidy_summaries <- purrr::imap(raw_lacytools_summaries(), function(summary, i) {
        tryCatch(
          expr = {
            convert_lacytools_summary(data = raw_lacytools_summaries()[[i]])
          },
          no_outputs_present = function(c) {
            # Show feedback that there are no outputs found in the LaCyTools file:
            shinyFeedback::feedbackDanger("lacytools_message",
                                          show = TRUE,
                                          text = paste("In summary file", i, c$message))
            
            showNotification(paste0("In summary file ", i, c$message),
                             type = "error",
                             duration = NULL)
            
            # Return NULL
            NULL
          })
      })
      
      return(tidy_summaries)
    })
    
    
    # Combine the LaCyTools_summaries using dplyr::bind_rows
    lacytools_summaries_combined <- reactive({
      req(lacytools_summaries())
      do.call(dplyr::bind_rows, lacytools_summaries())
    })
    
    # Hide spinner
    observeEvent(lacytools_summaries_combined(), {
      shinybusy::remove_modal_spinner()
    })
    
    # Detect total and specific samples if applicable.
    lacytools_summaries_total_and_specific <- reactive({
      
      shinyFeedback::hideFeedback("keyword_specific")
      shinyFeedback::hideFeedback("keyword_total")
      
      # Pause here until lacytools_summaries_combined() is Truthy, and until the inputs for
      # the keywords are not empty:
      req(lacytools_summaries_combined(),
          input$keyword_specific,
          input$keyword_total)
      
      summary <- tryCatch(
        expr = {
          # Detect based on sample names which samples are Total Ig and which are
          # Specific Ig samples
          detect_group(data = lacytools_summaries_combined(),
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
      
      return(summary)
    })
    
    
    
    # Return combined lacytools summaries.
    to_return <- reactive({
      req(lacytools_summaries_combined())
      tryCatch(
        lacytools_summaries_total_and_specific(),
        error = function(e) {
          lacytools_summaries_combined()
        }
      )
    })  # can't use bindEvent(input$button), because then re-adding sample ID's 
    # # to a newly uploaded summary is faster then resetting the plate_design to NULL,
    # # which results in an empty sample_id column added to the new summary
    
    
    
    # Control the state of the "Load" actionButton
    observe({
      shinyjs::toggleState(
        id = "button",
        condition = any(
          all(
            input$contains_total_and_specific_samples == "No",
            is_truthy(lacytools_summaries_combined())
          ),
          all(
            input$contains_total_and_specific_samples == "Yes",
            is_truthy(lacytools_summaries_total_and_specific())
          )))
    })
    
    
    return(list(
      data = to_return,
      button = reactive({input$button}),
      keyword_specific = reactive({input$keyword_specific}),
      keyword_total = reactive({input$keyword_total}),
      contains_total_and_specific_samples = reactive({input$contains_total_and_specific_samples}),
      summary_filenames = summary_filenames
    ))
    
  })
}
    
## To be copied in the UI
# mod_read_lacytools_ui("read_lacytools_ui_1")
    
## To be copied in the server
# mod_read_lacytools_server("read_lacytools_ui_1")
