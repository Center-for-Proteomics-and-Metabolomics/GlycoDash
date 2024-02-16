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
    fileInput(
      ns("summaries_input"),
      "Upload one or more LaCyTools summary text files:",
      multiple = TRUE
    ),
    tableOutput(ns("uploaded_files")),
    shinyWidgets::materialSwitch(
      ns("contains_total_and_specific_samples"),
      HTML("<i> <strong> Optional: </strong> Specify total and specific immunoglobulin samples </i>"),
      status = "success",
      right = TRUE
    ),
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
    
    # Show the uploaded LaCyTools files in the table
    output$uploaded_files <- renderTable({
      uploaded_files <- input$summaries_input
      uploaded_files$datapath <- NULL  # Get rid of the "datapath" column
      uploaded_files
    }, striped = TRUE, bordered = TRUE, rownames = TRUE, align = "c")
    
    # Check the file extensions of the uploaded summaries.
    all_txt_files <- reactive({
      req(input$summaries_input)
      non_txt_files <- subset(input$summaries_input, !grepl("\\.txt$", name, ignore.case = TRUE))
      if (nrow(non_txt_files) > 0) {
        FALSE
      } else {
        TRUE
      }
    })
    
    # Show a warning when non-text files are uploaded
    observe({
      req(input$summaries_input)
      shinyFeedback::feedbackDanger(
        inputId = "summaries_input",
        show = !is_truthy(all_txt_files()),
        text = "Please only upload text files."
      )
    })
  
    
    # If the user indicates (via input$contains_total_and_specific_samples) that the data contains total and
    # specific Ig samples, the textInputs for the specific and total keywords
    # are shown.
    observe({
      shinyjs::toggle("keywords_specific_total",
                      condition = input$contains_total_and_specific_samples == TRUE)
    })
    
    
    # If the user changes input$contains_total_and_specific_samples to FALSE  the
    # textInputs for the keywords are reset to empty strings "". This is needed
    # in case the user first fills in keywords but then changes their mind.
    observe({
      updateTextInput("keyword_specific",
                      value = "",
                      session = session)
      updateTextInput("keyword_total",
                      value = "",
                      session = session)
    }) %>% bindEvent(input$contains_total_and_specific_samples == FALSE)
    
    
    
    # Create a vector that contains the raw LaCyTools summary files
    raw_lacytools_summaries <- reactive({
      req(all_txt_files())
      summaries <- vector("list", length = nrow(input$summaries_input))
      for (i in seq(nrow(input$summaries_input))) {
        summaries[[i]] <- tryCatch(
          expr = read_non_rectangular(input$summaries_input$datapath[i]),
          embedded_null = function(c) {
            shinyFeedback::feedbackDanger("summaries_input", TRUE, c$message)
            NULL
          },
          empty_file = function(c) {
            shinyFeedback::feedbackDanger("summaries_input", TRUE, c$message)
            NULL
          },
          wrong_delim = function(c) {
            shinyFeedback::feedbackDanger("summaries_input", TRUE, c$message)
            NULL
          }
        )
      }
      return(summaries)
    })
    
    
    # Show spinner while processing LaCyTools summaries
    observe({
      req(raw_lacytools_summaries())
      shinybusy::show_modal_spinner(
        spin = "cube-grid", color = "#0275D8",
        text = HTML("<br/><strong>Processing LaCyTools summaries...")
      )
    }, priority = 5)
    
    
    # Create a list with tidy LaCyTools summaries
    lacytools_summaries <- reactive({
      req(raw_lacytools_summaries())
      tidy_summaries <- purrr::imap(raw_lacytools_summaries(), function(summary, i) {
        tryCatch(
          expr = convert_lacytools_summary(data = raw_lacytools_summaries()[[i]]),
          no_outputs_present = function(c) {
            showNotification(paste("In summary file", i, c$message), type = "error", duration = NULL)
            NULL
          }
        )
      })
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
        # to a newly uploaded summary is faster then resetting the plate_design to NULL,
        # which results in an empty sample_id column added to the new summary
    
    
 
    # Control the state of the "Load" actionButton
    observe({
      shinyjs::toggleState(
        id = "button",
        condition = any(
          all(
            input$contains_total_and_specific_samples == FALSE,
            is_truthy(lacytools_summaries_combined())
          ),
          all(
            input$contains_total_and_specific_samples == TRUE,
            is_truthy(lacytools_summaries_total_and_specific())
          )))
    })
    
    
    return(list(
      data = to_return,
      button = reactive({input$button}),
      keyword_specific = reactive({input$keyword_specific}),
      keyword_total = reactive({input$keyword_total}),
      contains_total_and_specific_samples = reactive({input$contains_total_and_specific_samples}),
      summary_filenames = reactive({input$summaries_input$name})
    ))
    
  })
}
    

