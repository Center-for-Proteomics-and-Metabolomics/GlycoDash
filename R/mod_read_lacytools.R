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
    fluidRow(
      column(
        width = 11,
        fileInput(
          ns("lacytools_input"),
          "Upload one or more LaCyTools summary text files:",
          multiple = TRUE
        ),
        fileInput(
          ns("skyline_input"),
          "Upload one or more Skyline CSV output files:",
          multiple = TRUE
        ),
      ),
      column(
        width = 1,
        tags$style(
          HTML(paste0(
            "#",
            ns("info_icon_div1"),
            " .fas {margin-top:28px; color: #3c8dbc;}",
            " .popover {width: 400px}",
            " .col-sm-1 {padding-left: 0px}",
            "#",
            ns("info_icon_div2"),
            " .fas {margin-top:28px; color: #3c8dbc;}",
            " .popover {width: 400px}",
            " .col-sm-1 {padding-left: 0px}"
          ))
      ),
        div(
          id = ns("info_icon_div1"),
          icon("info-circle", class = "fa-2x") %>% 
            bsplus::bs_embed_popover(
              id = ns("popover"),
              title = "LaCyTools data",
              content = HTML(
                "
                You can upload one or more LaCyTools summary text files. The following
                outputs should at least be present in your files for each analyte
                (per charge state):
                <ul>
                    <li> Absolute Intensity (Background Subtracted) </li>
                    <li> Mass Accuracy [ppm] </li>
                    <li> Isotopic Pattern Quality </li>
                    <li> S/N </li>
                </ul>
                "
              ),
              trigger = "hover",
              placement = "right",
              html = "true",
              container = "body"
            )
        ),
      div(
        id = ns("info_icon_div2"),
        icon("info-circle", class = "fa-2x") %>% 
          bsplus::bs_embed_popover(
            id = ns("popover"),
            title = "Skyline data",
            content = HTML(
              "
              You can upload one Skyline output CSV file. The file should contain
              the following columns:
              <ul>
                  <li>
                  <i> Protein Name </i> <br>
                  The entries in this column should consist only of letters. They must
                  specify the peptide to which the glycan is attached (see below).
                  </li>
                  <li>
                  <i> Peptide </i> <br>
                  This column should contain the <b> glycan compositions </b> attached to the peptides
                  specified in <i>Protein Name</i>, the same way they would be specified in LaCyTools 
                  (e.g. \"H5N2\", \"H4N4F1S1\").
                  </li>
                  <li>
                  <i> Precursor </i> <br>
                  A number specifying the charge state of the glycopeptide.
                  </li>
              </ul>
              Additionally, it should contain columns with \"<i>Total Area MS1</i>\",
              \"<i>Isotope Dot Product</i>\" and \"<i>Average Mass Error PPM</i>\" 
              for each sample name.
              "
            ),
            trigger = "hover",
            placement = "right",
            html = "true",
            container = "body"
          )
        )
      )
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
        shinyjs::show("info_icon_div1")
        shinyjs::hide("skyline_input")
        shinyjs::hide("info_icon_div2")
      } else if (input$data_type == "Skyline data") {
        shinyjs::hide("lacytools_input")
        shinyjs::hide("info_icon_div1")
        shinyjs::show("skyline_input")
        shinyjs::show("info_icon_div2")
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
          text = "Please upload CSV files."
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
    
    raw_skyline_data <- reactive({
      req(correct_file_ext(), input$data_type == "Skyline data", input$skyline_input)
      data <- vector("list", length = nrow(input$skyline_input))
      for (i in seq(nrow(input$skyline_input))) {
        # TODO: implement checks and use tryCatch()
        data[[i]] <- read_skyline_csv(input$skyline_input$datapath[[i]])
      }
      return(data)
    })
    
    
    observe({
      req(raw_skyline_data())
      shinybusy::show_modal_spinner(
        spin = "cube-grid", color = "#0275D8",
        text = HTML("<br/><strong>Processing Skyline data...")
      )
    }, priority = 5)
    
    
    skyline_data <- reactive({
      req(raw_skyline_data())
      data <- vector("list", length = length(raw_skyline_data()))
      for (i in seq(length(raw_skyline_data()))) {
        # TODO: implement checks and use tryCatch()
        data[[i]] <- transform_skyline_data(raw_skyline_data()[[i]])
      }
      combined <- do.call(dplyr::bind_rows, data)
      return(combined)
    })
  
    
    observeEvent(skyline_data(), {
      shinybusy::remove_modal_spinner()
    })
    
    
    
    
    # Detect total and specific samples if applicable.
    data_total_and_specific <- reactive({
      
      shinyFeedback::hideFeedback("keyword_specific")
      shinyFeedback::hideFeedback("keyword_total")
      
      # Require data and non-empty keywords
      req(
        any(is_truthy(lacytools_summaries_combined()), is_truthy(skyline_data())),
        input$keyword_specific,
        input$keyword_total
      )
      
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
  
    
    # filenames for report
    filenames <- reactive({
      req(any(
        is_truthy(lacytools_summaries_combined()),
        is_truthy(skyline_data())
      ))
      if (is_truthy(lacytools_summaries_combined())) {
        input$lacytools_input$name
      } else if (is_truthy(skyline_data())) {
        input$skyline_input$name
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
      data_type = reactive(input$data_type),
      keyword_specific = reactive({input$keyword_specific}),
      keyword_total = reactive({input$keyword_total}),
      contains_total_and_specific_samples = reactive({input$contains_total_and_specific_samples}),
      filenames = filenames
    ))
    
  })
}
    

