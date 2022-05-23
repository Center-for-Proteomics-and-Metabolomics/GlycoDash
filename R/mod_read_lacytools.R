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
    actionButton(ns("read_summary"),
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
        id = "read_summary",
        condition = all(
          extension() == "txt",
          any(
            input$Ig_data == "No",
            all(
              input$Ig_data == "Yes",
              isTruthy(lacytools_summary_Ig_data())
            )
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
      read_non_rectangular(input$lacytools_summary$datapath)
    })
    
    warn_duplicated_analytes <- reactive({
      for (output in outputs) {
        tryCatch(expr = {
          get_block(data = raw_lacytools_summary(),
                    variable = output)
          # If get_block has run without errors/warnings, exit the loop and
          # return FALSE
          return(FALSE)
        },
        lacytools_output_not_found = function(c) {
          # If this error occurs, go through loop again with next output
        },
        duplicated_analytes = function(c) {
          # If this warning occurs, exit loop and return TRUE
          return(TRUE)
        })
      }
    })
    
    lacytools_summary <- reactive({
      tryCatch(expr = {
        read_lacytools_summary(data = raw_lacytools_summary())
      },
      no_outputs_present = function(c) {
        # Show feedback that there are no outputs found in the LacyTools file:
        shinyFeedback::feedbackDanger("lacytools_summary",
                                      show = TRUE,
                                      text = c$message)
        # Return NULL to the reactive lacytools_summary()
        NULL
      })
    })
    
    lacytools_summary_Ig_data <- reactive({
      # Pause here until lacytools_summary() is Truthy, and until the inputs for
      # the keywords are not empty:
      req(
        lacytools_summary(),
        input$keyword_specific,
        input$keyword_total
      )
      
      shinyFeedback::hideFeedback("keyword_specific")
      shinyFeedback::hideFeedback("keyword_total")
      
      tryCatch(expr = {
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
    })
    
    #     # Check whether the keyword given for specific samples has matches with the
    # # sample names in the data:
    # observeEvent(input$keyword_specific, {
    #   req(input$lacytools_summary)               
    #   x$keyword_specific_OK <- TRUE
    #   shinyFeedback::hideFeedback("keyword_specific")
    #   shinyFeedback::hideFeedback("lacytools_summary")
    #   # Read in the data so that the keywords can be compared to it:
    #   data <- read_non_rectangular(input$lacytools_summary$datapath)
    #   
    #   all_blocks <- purrr::map(outputs,
    #                            function(output) {
    #                              tryCatch(expr = {
    #                                get_block(data = data, 
    #                                          variable = output, 
    #                                          Ig_data = "No")
    #                              },
    #                              error = function(e) { })
    #                            })
    #   
    #   all_blocks <- all_blocks[!sapply(all_blocks, is.null)]
    #   
    #   if (rlang::is_empty(all_blocks)) {
    #     shinyFeedback::feedbackDanger(
    #       "lacytools_summary",
    #       show = TRUE,
    #       text = paste("No LacyTools output variables could be found in this file."))
    #   } else {
    #     # stringr::str_detect() throws an error when pattern is an empty string,
    #     # so don't proceed if input$keyword_specific is empty:
    #     req(input$keyword_specific != "")
    #     
    #     matches <- purrr::map(all_blocks,
    #                           function(block) {
    #                             stringr::str_detect(
    #                               block[["sample_name"]],
    #                               pattern = stringr::fixed(input$keyword_specific))
    #                           })
    #     
    #     keyword_is_unmatched <- !(all(purrr::map_lgl(matches,
    #                                                  any)))
    #     
    #     if (keyword_is_unmatched) {
    #       shinyFeedback::feedbackDanger("keyword_specific",
    #                                     show = TRUE,
    #                                     text = "This keyword did not match any sample names in your data. Please choose a different keyword.")
    #       x$keyword_specific_OK <- FALSE
    #     }
    #   }
    #   
    # })
    # 
    # # Check whether the keyword given for total samples has matches with the
    # # sample names in the data:
    # observeEvent(input$keyword_total, {
    #   req(input$lacytools_summary)               
    #   x$keyword_total_OK <- TRUE
    #   shinyFeedback::hideFeedback("keyword_total")
    #   shinyFeedback::hideFeedback("lacytools_summary")
    #   data <- read_non_rectangular(input$lacytools_summary$datapath)
    #   all_blocks <- purrr::map(outputs,
    #                            function(output) {
    #                              tryCatch(expr = {
    #                                get_block(data = data, 
    #                                          variable = output, 
    #                                          Ig_data = "No")
    #                              },
    #                              error = function(e) { })
    #                            })
    #   
    #   all_blocks <- all_blocks[which(purrr::map_lgl(all_blocks, is.data.frame))]
    #   
    #   if (rlang::is_empty(all_blocks)) {
    #     shinyFeedback::feedbackDanger(
    #       "lacytools_summary",
    #       show = TRUE,
    #       text = paste("No LacyTools output variables could be found in this file."))
    #   } else {
    #     # stringr::str_detect() throws an error when pattern is an empty string,
    #     # so don't proceed if input$keyword_total is empty:
    #     req(input$keyword_total != "")
    #     
    #     matches <- purrr::map(all_blocks,
    #                           function(block) stringr::str_detect(
    #                             block[["sample_name"]],
    #                             pattern = stringr::fixed(input$keyword_total)))
    #     
    #     # In each block there needs to be at least one match, if not then the
    #     # keyword is unmatched:
    #     keyword_is_unmatched <- !(all(purrr::map_lgl(matches,
    #                                                  any)))
    #     
    #     if (keyword_is_unmatched) {
    #       shinyFeedback::feedbackDanger("keyword_total",
    #                                     show = TRUE,
    #                                     text = "This keyword did not match any sample names in your data. Please choose a different keyword.")
    #       x$keyword_total_OK <- FALSE
    #     }
    #   }
    # })
    
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
        DT::datatable(x$data_incl_metadata, 
                      options = list(scrollX = TRUE),
                      filter = "top")
      } else { if (isTruthy(x$data_incl_plate_design)){
        DT::datatable(x$data_incl_plate_design, 
                      options = list(scrollX = TRUE),
                      filter = "top")
      } else {
        DT::datatable(x$data, 
                      options = list(scrollX = TRUE),
                      filter = "top")
      }
      }
    })
    
    
  })
}
    
## To be copied in the UI
# mod_read_lacytools_ui("read_lacytools_ui_1")
    
## To be copied in the server
# mod_read_lacytools_server("read_lacytools_ui_1")
