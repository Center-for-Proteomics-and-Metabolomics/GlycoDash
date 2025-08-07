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
      choices = c("LaCyTools data", "Skyline data (wide format)"),
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
          id = ns("uploaded_skyline_wide"),
          strong("You uploaded Skyline data in wide format. 
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
          ns("skyline_input_wide"),
          "Upload one Skyline CSV output file in wide format:",
          multiple = FALSE
        ),
        shinyWidgets::awesomeRadio(
          ns("skyline_analyte_format"),
          "Select how analytes are specified:",
          choices = c(
            "One column with peptide sequences and modifications",
            "Two columns: one with glycosylation sites and one with glycans"
          )
        ),
        selectizeInput(
          ns("skyline_analyte_column"),
          "Select column with analytes:",
          choices = c()
        ),
        selectizeInput(
          ns("skyline_cluster_column"),
          "Select column with glycosylation sites:",
          choices = c()
        ),
        selectizeInput(
          ns("skyline_glycan_column"),
          "Select column with glycan compositions:",
          choices = c()
        ),
        selectizeInput(
          ns("skyline_charge_column"),
          "Select column with charge states:",
          choices = c()
        ),
        shinyWidgets::awesomeCheckbox(
          ns("skyline_rename_isomers"),
          label = HTML("<i style='font-size:15px;'> Automatically detect and rename glycan isomers </i>"),
          value = TRUE
        ),
        shinyjs::hidden(div(
          id = ns("button_div"),
          actionButton(ns("button"), "Process Skyline data"),
          br(),
          br()
        ))
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
            title = "Skyline data (wide format)",
            content = HTML(
              "
              You can upload one or more Skyline output CSV files. 
              The file should contain one column specifying the charge states of the
              analytes. The analytes themselves can be specified in one of two ways:
              <ul>
                <li>
                <i>Two separate columns:</i> one with glycosylation sites and one 
                with glycan compositions.
                </li>
                <li>
                <i>One column</i> where each entry contains both a peptide sequence
                and a glycan composition (e.g. \"EEQYN[H3N4F1]STYR\").
                A sequence may also contain methionine oxidation and cysteine
                carbamidomethyl (CAM) modifications.
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
    
    # Visibility of UI elements
    observe({
      if (input$data_type == "LaCyTools data") {
        shinyjs::hide("button_div")
        shinyjs::show("lacytools_input")
        shinyjs::show("info_icon_div1")
        shinyjs::hide("skyline_input_wide")
        shinyjs::hide("info_icon_div2")
        shinyjs::hide("skyline_analyte_format")
        shinyjs::hide("skyline_analyte_column")
        shinyjs::hide("skyline_cluster_column")
        shinyjs::hide("skyline_glycan_column")
        shinyjs::hide("skyline_charge_column")
        shinyjs::hide("skyline_rename_isomers")
      } else if (input$data_type == "Skyline data (wide format)") {
        shinyjs::show("button_div")
        shinyjs::hide("lacytools_input")
        shinyjs::hide("info_icon_div1")
        shinyjs::show("skyline_input_wide")
        shinyjs::show("info_icon_div2")
        shinyjs::show("skyline_analyte_format")
        shinyjs::show("skyline_charge_column")
        shinyjs::show("skyline_rename_isomers")
        if (input$skyline_analyte_format == "One column with peptide sequences and modifications") {
          shinyjs::show("skyline_analyte_column")
          shinyjs::hide("skyline_cluster_column")
          shinyjs::hide("skyline_glycan_column")
        } else {
          shinyjs::hide("skyline_analyte_column")
          shinyjs::show("skyline_cluster_column")
          shinyjs::show("skyline_glycan_column")
        }
      }
    })
    
    
    # Check the file extensions of the uploaded summaries.
    correct_file_ext <- reactive({
      if (input$data_type == "LaCyTools data") {
        # LaCyTools --> require text file
        req(input$lacytools_input)
        wrong_file_ext <- subset(input$lacytools_input, !grepl("\\.txt$", name, ignore.case = TRUE))
      } else if (input$data_type == "Skyline data (wide format)") {
        # Skyline --> require CSV file
        req(input$skyline_input_wide)
        wrong_file_ext <- subset(input$skyline_input_wide, !grepl("\\.csv$", name, ignore.case = TRUE))
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
      } else if (input$data_type == "Skyline data (wide format)") {
        req(input$skyline_input_wide)
        shinyFeedback::feedbackDanger(
          inputId = "skyline_input_wide",
          show = !is_truthy(correct_file_ext()),
          text = "Please upload CSV files."
        )
      }
    })
  
    
    # Show the uploaded LaCyTools files in the table
    output$uploaded_files <- renderTable({
      req(correct_file_ext(), input$data_type == "LaCyTools data")
      uploaded_files <- input$lacytools_input
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
    
    # Read raw Skyline data from CSV file.
    raw_skyline_data_wide <- reactive({
      req(correct_file_ext(), input$data_type == "Skyline data (wide format)", input$skyline_input_wide)
      read_skyline_csv(input$skyline_input_wide$datapath)
    })
    
    # Update column selection options
    observe({
      req(raw_skyline_data_wide())
      columns <- raw_skyline_data_wide() %>% 
        dplyr::select(
          -tidyselect::contains("Total.Area.MS1"),
          -tidyselect::contains("Isotope.Dot.Product"),
          -tidyselect::contains("Mass.Error.PPM"),
          -tidyselect::contains("Best.Retention.Time"),
          -tidyselect::contains("Normalized.Area"),
          -tidyselect::contains("Replicate.Name"),
          -tidyselect::contains("Background.MS1")
          ) %>% 
        colnames()
      
      for (id in c("skyline_analyte_column", "skyline_cluster_column",
                  "skyline_glycan_column", "skyline_charge_column")) {
        updateSelectizeInput(inputId = id, choices = columns)
      }
    })
    
    # Require unique column input names for button
    observe({
      # Set requirements
      req_A <- is_truthy(raw_skyline_data_wide())
      req_B <- dplyr::case_when(
        startsWith(input$skyline_analyte_format, "Two") ~ 
          length(unique(c(
            input$skyline_cluster_column, input$skyline_glycan_column, 
            input$skyline_charge_column
          ))) == 3,
        startsWith(input$skyline_analyte_format, "One") ~ 
          length(unique(c(
            input$skyline_analyte_column, input$skyline_charge_column
          ))) == 2
      )
      # Check requirements
      if (req_A & req_B) {
        shinyjs::enable("button")
      } else {
        shinyjs::disable("button")
      }
    })
    
    
    # Show spinner when processing starts
    observeEvent(input$button, {
      req(raw_skyline_data_wide())
      shinybusy::show_modal_spinner(
        spin = "cube-grid", color = "#0275D8",
        text = HTML("<br/><strong>Processing Skyline data...")
      )
    }, priority = 5)
    
    
    # Transform Skyline data
    # Isomers are renamed when cluster and glycan columns are given separately
    skyline_data_wide <- reactive({
      req(raw_skyline_data_wide())
      if (startsWith(input$skyline_analyte_format, "Two")) {
        # Separate cluster and glycan columns
        tryCatch(
          expr = transform_skyline_data_wide(
            raw_skyline_data_wide(),
            cluster_colname = input$skyline_cluster_column,
            glycan_colname = input$skyline_glycan_column,
            charge_colname = input$skyline_charge_column,
            rename_isomers = input$skyline_rename_isomers
          ),
          missing_variables = function(c) {
            showNotification(c$message, type = "error", duration = NULL)
            shinybusy::remove_modal_spinner()
            NULL
          }
        )
      } else {
        # One analyte column
        tryCatch(
          expr = transform_skyline_data_wide(
            raw_skyline_data_wide(),
            analyte_colname = input$skyline_analyte_column,
            charge_colname = input$skyline_charge_column,
            rename_isomers = input$skyline_rename_isomers
          ),
          missing_variables = function(c) {
            showNotification(c$message, type = "error", duration = NULL)
            shinybusy::remove_modal_spinner()
            NULL
          }
        )
      }
    }) %>% bindEvent(input$button)
  
    # Remove spinner
    observeEvent(skyline_data_wide(), {
      shinybusy::remove_modal_spinner()
    })
    
    
    # Detect total and specific samples if applicable.
    data_total_and_specific <- reactive({
      
      shinyFeedback::hideFeedback("keyword_specific")
      shinyFeedback::hideFeedback("keyword_total")
      
      # Require data and non-empty keywords
      req(
        any(is_truthy(lacytools_summaries_combined()), is_truthy(skyline_data_wide())),
        input$keyword_specific,
        input$keyword_total
      )
      
      if (is_truthy(lacytools_summaries_combined())) {
        data_to_check <- lacytools_summaries_combined()
      } else if (is_truthy(skyline_data_wide())) {
        data_to_check <- skyline_data_wide()
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
        is_truthy(skyline_data_wide())
      ))
      if (is_truthy(lacytools_summaries_combined())) {
        input$lacytools_input$name
      } else if (is_truthy(skyline_data_wide())) {
        input$skyline_input_wide$name
      }
    })
    
    
    # Return combined LaCytools summaries or Skyline data
    to_return <- reactive({
      req(any(
        is_truthy(lacytools_summaries_combined()),
        is_truthy(skyline_data_wide())
      ))
      tryCatch(
        data_total_and_specific(),
        error = function(e) {
          if (is_truthy(lacytools_summaries_combined())) {
            lacytools_summaries_combined()
          } else if (is_truthy(skyline_data_wide())) {
            skyline_data_wide()
          }
        }
      )
    })
    
    # Prevent people from changing input$data_type after uploading data
    observeEvent(to_return(), {
      shinyjs::hide("data_type")
      if (input$data_type == "LaCyTools data") {
        shinyjs::show("uploaded_lacytools")
      } else if (input$data_type == "Skyline data (wide format)") {
        shinyjs::show("uploaded_skyline_wide")
      }
    })
    
    # Remove trailing/leading spaces
    to_return_trimmed <- reactive({
      req(to_return())
      to_return() %>% 
        dplyr::mutate(dplyr::across(tidyselect::where(is.character), trimws))
    })
    
    
    # Data type to return
    data_type_to_return <- reactive({
      if (input$data_type == "LaCyTools data") {
        "LaCyTools data"
      } else if (input$data_type == "Skyline data (wide format)") {
        "Skyline data"
      }
    })
    
    
    return(list(
      data = to_return_trimmed,
      data_type = data_type_to_return,
      keyword_specific = reactive({input$keyword_specific}),
      keyword_total = reactive({input$keyword_total}),
      contains_total_and_specific_samples = reactive({input$contains_total_and_specific_samples}),
      summary_filenames = filenames
    ))
    
  })
}
    

