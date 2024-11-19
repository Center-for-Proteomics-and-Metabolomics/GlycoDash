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
        width = 12,
        shinyjs::hidden(div(
          id = ns("uploaded_lacytools"),
          strong("You uploaded LaCyTools data. 
                 To upload a different data type, reload the dashboard."),
          br(),
          br(),
          style = "color:#0021B8; font-size: 15px"
        )),
        shinyjs::hidden(div(
          id = ns("uploaded_skyline"),
          strong("You uploaded Skyline data. 
                 To upload a different data type, reload the dashboard."),
          br(),
          br(),
          style = "color:#0021B8; font-size: 15px"
        )),
      ),
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
              You can upload one or more Skyline output CSV file. 
              The file should contain the following columns:
              <ul>
                  <li>
                  <i> Protein Name </i> <br>
                  The entries in this column should consist only of letters
                  <b> (no numbers or spaces). </b>
                  They must specify the peptide to which the glycan is attached (see below).
                  </li>
                  <li>
                  <i> Peptide </i> <br>
                  This column should contain the <b> glycan compositions </b> attached to the peptides
                  specified in <i>Protein Name</i>, the same way they would be specified in LaCyTools 
                  (e.g. \"H5N2\", \"H4N4F1S1\").
                  </li>
                  <li>
                  <i> Precursor Charge </i> <br>
                  A number specifying the charge state of the glycopeptide.
                  </li>
              </ul>
              Additionally, it should contain columns with \"<i>Total Area MS1</i>\",
              \"<i>Isotope Dot Product</i>\" and \"<i>Average Mass Error PPM</i>\" 
              for each sample name.
              <br> <br>
              <b>Sample names should not start with a number, and should not contain any spaces.</b>
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
      HTML("<i style='font-size:15px;'> Specify total and specific immunoglobulin samples </i>"),
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
      purrr::imap(input$lacytools_input$datapath, function(datapath, i) {
        tryCatch(
          expr = read_non_rectangular(datapath),
          embedded_null = function(c) {
            showNotification(paste("Summary file", i, ":", c$message), type = "error", duration = NULL)
            NULL
          },
          empty_file = function(c) {
            showNotification(paste("Summary file", i, ":", c$message), type = "error", duration = NULL)
            NULL
          },
          wrong_delim = function(c) {
            showNotification(paste("Summary file", i, ":", c$message), type = "error", duration = NULL)
            NULL
          }
        )
      })
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
      purrr::imap(raw_lacytools_summaries(), function(summary, i) {
        tryCatch(
          expr = convert_lacytools_summary(data = summary),
          no_outputs_present = function(c) {
            showNotification(paste("In summary file", i, c$message), type = "error", duration = NULL)
            shinybusy::remove_modal_spinner()
            return(NULL)
          }
        )
      })
    })
    
    # Combine the LaCyTools_summaries using dplyr::bind_rows
    lacytools_summaries_combined <- reactive({
      req(lacytools_summaries(), !any(sapply(lacytools_summaries(), is.null)))
      do.call(dplyr::bind_rows, lacytools_summaries())
    })
    
    observeEvent(lacytools_summaries_combined(), {
      shinybusy::remove_modal_spinner()
    })
    
    # Check if required data is missing
    observe({
      req(lacytools_summaries_combined())
      required <- c(
        "absolute_intensity_background_subtracted",
        "mass_accuracy_ppm",
        "isotopic_pattern_quality",
        "sn"
      )
      missing <- required[!required %in% colnames(lacytools_summaries_combined())]
      if (length(missing) > 0) {
        showNotification(
          paste(
            "The following required variables are missing in your data:",
            paste0(missing, collapse = ", ")
          ),
          type = "error",
          duration = NULL
        )
      }
    })
    
    
    #########################################################################
    ####################  Skyline  ##########################################
    #########################################################################
    
    # Read raw Skyline data from CSV files.
    # Isomers are renamed.
    raw_skyline_data <- reactive({
      req(correct_file_ext(), input$data_type == "Skyline data", input$skyline_input)
      purrr::map(input$skyline_input$datapath, function(datapath) {
        read_skyline_csv(datapath)
      })
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
      purrr::imap(raw_skyline_data(), function(data, i) {
        tryCatch(
          expr = transform_skyline_data(data, i),
          missing_columns = function(c) {
            showNotification(paste("In CSV file", i, c$message), type = "error", duration = NULL)
            shinybusy::remove_modal_spinner()
            NULL
          },
          missing_variables = function(c) {
            showNotification(paste("In CSV file", i, c$message), type = "error", duration = NULL)
            shinybusy::remove_modal_spinner()
            NULL
          },
          numbers_or_spaces = function(c) {
            showNotification(paste("In CSV file", i, c$message), type = "error", duration = NULL)
            shinybusy::remove_modal_spinner()
            NULL
          }
        )
      })
    })
    
    skyline_data_combined <- reactive({
      req(skyline_data(), !any(sapply(skyline_data(), is.null)))
      do.call(dplyr::bind_rows, skyline_data())
    })
  
    observeEvent(skyline_data_combined(), {
      shinybusy::remove_modal_spinner()
    })
    
    
    # Detect total and specific samples if applicable.
    data_total_and_specific <- reactive({
      
      shinyFeedback::hideFeedback("keyword_specific")
      shinyFeedback::hideFeedback("keyword_total")
      
      # Require data and non-empty keywords
      req(
        any(is_truthy(lacytools_summaries_combined()), is_truthy(skyline_data_combined())),
        input$keyword_specific,
        input$keyword_total
      )
      
      if (is_truthy(lacytools_summaries_combined())) {
        data_to_check <- lacytools_summaries_combined()
      } else if (is_truthy(skyline_data_combined())) {
        data_to_check <- skyline_data_combined()
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
        is_truthy(skyline_data_combined())
      ))
      if (is_truthy(lacytools_summaries_combined())) {
        input$lacytools_input$name
      } else if (is_truthy(skyline_data_combined())) {
        input$skyline_input$name
      }
    })
    
    
    # Return combined LaCytools summaries or skyline data
    to_return <- reactive({
      req(any(
        is_truthy(lacytools_summaries_combined()),
        is_truthy(skyline_data_combined())
      ))
      tryCatch(
        data_total_and_specific(),
        error = function(e) {
          if (is_truthy(lacytools_summaries_combined())) {
            lacytools_summaries_combined()
          } else if (is_truthy(skyline_data_combined())) {
            skyline_data_combined()
          }
        }
      )
    })
    
    # Prevent people from changing input$data_type after uploading data
    observeEvent(to_return(), {
      shinyjs::hide("data_type")
      if (input$data_type == "LaCyTools data") {
        shinyjs::show("uploaded_lacytools")
      } else if (input$data_type == "Skyline data") {
        shinyjs::show("uploaded_skyline")
      }
    })
    
    
    return(list(
      data = to_return,
      data_type = reactive(input$data_type),
      keyword_specific = reactive({input$keyword_specific}),
      keyword_total = reactive({input$keyword_total}),
      contains_total_and_specific_samples = reactive({input$contains_total_and_specific_samples}),
      summary_filenames = filenames
    ))
    
  })
}
    

