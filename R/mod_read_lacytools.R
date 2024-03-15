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
    selectInput(
      ns("data_type"),
      "Choose which type of data you want to upload:",
      choices = c("LaCyTools data", "Skyline data"),
      selected = "LaCyTools data"
    ),
    fileInput(
      ns("lacytools_input"),
      "Upload one or more LaCyTools summary text files:",
      multiple = TRUE
    ),
    fileInput(
      ns("skyline_input"),
      "Upload your Skyline CSV output file:"
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
  )
}
  

  
#' read_lacytools Server Functions
#'
#' @noRd 
mod_read_lacytools_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    # Visibility of fileInputs
    observe({
      if (input$data_type == "LaCyTools data") {
        shinyjs::show("lacytools_input")
        shinyjs::hide("skyline_input")
      } else if (input$data_type == "Skyline data") {
        shinyjs::hide("lacytools_input")
        shinyjs::show("skyline_input")
      }
    })
    
    
    # Check the file extensions of the uploaded summaries.
    correct_file_ext <- reactive({
      if (input$data_type == "LaCyTools data") {
        # LaCyTools --> require text file
        req(input$lacytools_input)
        wrong_file_ext <- subset(input$lacytools_input, !grepl("\\.txt$", name, ignore.case = TRUE))
      } else if (input$data_type == "Skyline data") {
        # Skyline --> require CSV file
        req(input$skyline_input)
        wrong_file_ext <- subset(input$skyline_input, !grepl("\\.csv$", name, ignore.case = TRUE))
      }
      if (nrow(wrong_file_ext) > 0) {
        FALSE
      } else {
        TRUE
      }
    })
    
    
    # Show a warning when files with wrong extension are uploaded
    observe({
      if (input$data_type == "LaCyTools data") {
        req(input$lacytools_input)
        shinyFeedback::feedbackDanger(
          inputId = "lacytools_input",
          show = !is_truthy(correct_file_ext()),
          text = "Please upload text files."
        )
      } else if (input$data_type == "Skyline data") {
        req(input$skyline_input)
        shinyFeedback::feedbackDanger(
          inputId = "skyline_input",
          show = !is_truthy(correct_file_ext()),
          text = "Please upload a CSV file."
        )
      }
    })
  
    
    # Show the uploaded files in the table
    output$uploaded_files <- renderTable({
      req(correct_file_ext())
      if (input$data_type == "LaCyTools data") {
        uploaded_files <- input$lacytools_input
      } else if (input$data_type == "Skyline data") {
        uploaded_files <- input$skyline_input
      }
      uploaded_files$datapath <- NULL  # Get rid of the "datapath" column
      uploaded_files
    }, striped = TRUE, bordered = TRUE, rownames = TRUE, align = "c")
    
    
    
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
    
    
    
    #########################################################################
    #################### LaCyTools ##########################################
    #########################################################################
    
    # Create a vector that contains the raw LaCyTools summary files
    raw_lacytools_summaries <- reactive({
      req(correct_file_ext(), input$data_type == "LaCyTools data", input$lacytools_input)
      summaries <- vector("list", length = nrow(input$lacytools_input))
      for (i in seq(nrow(input$lacytools_input))) {
        summaries[[i]] <- tryCatch(
          expr = read_non_rectangular(input$lacytools_input$datapath[i]),
          embedded_null = function(c) {
            shinyFeedback::feedbackDanger("lacytools_input", TRUE, c$message)
            NULL
          },
          empty_file = function(c) {
            shinyFeedback::feedbackDanger("lacytools_input", TRUE, c$message)
            NULL
          },
          wrong_delim = function(c) {
            shinyFeedback::feedbackDanger("lacytools_input", TRUE, c$message)
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
    
    
    
    #########################################################################
    ####################  Skyline  ##########################################
    #########################################################################
    
    # TODO: implement warning when the required variables are not present
    # in the CSV file.
    
    raw_skyline_data <- reactive({
      req(correct_file_ext(), input$data_type == "Skyline data", input$skyline_input)
      read_skyline_csv(input$skyline_input$datapath)
    })
    
    skyline_data <- reactive({
      req(raw_skyline_data())
      transform_skyline_data(raw_skyline_data())
    })
  
    
    
    # Detect total and specific samples if applicable.
    data_total_and_specific <- reactive({
      
      shinyFeedback::hideFeedback("keyword_specific")
      shinyFeedback::hideFeedback("keyword_total")
      
      # Pause here until lacytools_summaries_combined() is Truthy, and until the inputs for
      # the keywords are not empty:
      req(any(lacytools_summaries_combined(), skyline_data()),
          input$keyword_specific,
          input$keyword_total)
      
      if (is_truthy(lacytools_summaries_combined())) {
        data_to_check <- lacytools_summaries_combined()
      } else if (is_truthy(skyline_data())) {
        data_to_check <- skyline_data()
      }
      
      summary <- tryCatch(
        expr = {
          # Detect based on sample names which samples are Total Ig and which are
          # Specific Ig samples
          detect_group(data = data_to_check,
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
    
    
    # Toggle UI elements
    observeEvent(input$contains_total_and_specific_samples, {
      # I use show/hide because toggle causes problems
      if (input$contains_total_and_specific_samples == TRUE) {
        shinyjs::show("keywords_specific_total")
      } else {
        shinyjs::hide("keywords_specific_total")
      }
    })
  
    
    # Return combined lacytools summaries or skyline data
    to_return <- reactive({
      req(any(
        is_truthy(lacytools_summaries_combined()),
        is_truthy(skyline_data())
      ))
      tryCatch(
        data_total_and_specific(),
        error = function(e) {
          if (is_truthy(lacytools_summaries_combined())) {
            lacytools_summaries_combined()
          } else if (is_truthy(skyline_data())) {
            skyline_data()
          }
        }
      )
    })
    
    
    return(list(
      data = to_return,
      keyword_specific = reactive({input$keyword_specific}),
      keyword_total = reactive({input$keyword_total}),
      contains_total_and_specific_samples = reactive({input$contains_total_and_specific_samples}),
      summary_filenames = reactive({input$lacytools_input$name})
    ))
    
  })
}
    

