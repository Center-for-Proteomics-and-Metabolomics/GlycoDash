#' read_data UI Function
#'
#' @description A shiny Module to upload and read data.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_read_data_ui <- function(id) {
  ns <- NS(id)
  
  shinydashboard::box(
    title = "Upload your data",
    width = NULL,
    solidHeader = TRUE,
    status = "primary",
    selectInput(
      ns("data_type"),
      "Choose which type of data you want to upload:",
      choices = c(
        "SweetSuite data", "Skyline data (wide format)", "LaCyTools data"
      ),
      selected = "SweetSuite data"
    ),
    fluidRow(
      column(
        width = 12,
        shinyjs::hidden(div(
          id = ns("uploaded_lacytools"),
          strong(
            "You uploaded LaCyTools data. 
            To upload a different data type, reload the dashboard."
          ),
          br(), br(),
          style = "color:#0021B8; font-size: 15px"
        )),
        shinyjs::hidden(div(
          id = ns("uploaded_skyline_wide"),
          strong(
            "You uploaded Skyline data in wide format. 
            To upload a different data type, reload the dashboard."
          ),
          br(), br(),
          style = "color:#0021B8; font-size: 15px"
        )),
      shinyjs::hidden(div(
          id = ns("uploaded_sweetsuite"),
          strong(
            "You uploaded SweetSuite data. 
            To upload a different data type, reload the dashboard."
          ),
          br(), br(),
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
        fileInput(
          ns("sweetsuite_input"),
          "Upload one or more SweetSuite output xlsx files:",
          multiple = TRUE
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
          ns("skyline_protein_column"),
          "Select column with protein names:",
          choices = c()
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
        shinyWidgets::materialSwitch(
          ns("skyline_contains_notes"),
          HTML("<i style='font-size:15px;'> Specify column with analyte notes </i>"),
          status = "success",
          right = TRUE
        ),
        selectizeInput(
          ns("skyline_note_column"),
          "Select column with notes:",
          choices = c()
        ),
        shinyWidgets::materialSwitch(
          ns("skyline_merge_glycounter"),
          tagList(
            tags$i(style = "font-size:15px;", "Merge with GlyCounter fragmentation data "),
            tags$span(class = "label label-warning", style = "font-size:12px; vertical-align:middle;", "Experimental") %>%
              bsplus::bs_embed_popover(
                id = ns("experimental_popover"),
                title = "Experimental feature",
                content = paste(
                  "GlyCounter fragmentation matching has not yet been fully validated",
                  "for all input types. Inspect the results carefully before using",
                  "them for final analysis."
                ),
                trigger = "hover",
                placement = "right",
                container = "body"
              )
          ),
          status = "success",
          right = TRUE
        ),
        shinyjs::hidden(div(
          id = ns("div_glycounter"),
          selectizeInput(
            ns("skyline_molecular_formula_column"),
            "Select column with molecular formulas:",
            choices = c()
          ),
          fileInput(
            ns("glycounter_files"),
            HTML("Upload GlyCounter <i>OxoSignal</i> text files:"),
            accept = ".txt",
            multiple = TRUE
          ),
          numericInput(
            ns("mz_tolerance_ppm"),
            HTML(
              "Tolerance around theoretical <i>m/z</i> values (ppm)
              in Skyline data:"
            ),
            value = 10, min = 1, step = 1
          ),
          numericInput(
            ns("n_isotopic_peaks"),
            "Number of theoretical isotopic peaks to use for matching:",
            value = 2, min = 1, step = 1
          )
        )),
        shinyWidgets::awesomeCheckbox(
          ns("skyline_rename_isomers"),
          label = HTML("<i style='font-size:15px;'> Automatically detect and rename glycan isomers </i>"),
          value = TRUE
        ),
        shinyjs::hidden(div(
          id = ns("button_div"),
          actionButton(ns("button"), "Process Skyline data"),
          br(), br()
        ))
      ),
      column(
        width = 1,
        tags$style(
          HTML(paste0(
            "#",
            ns("info_icon_lacytools"),
            " .fas {margin-top:28px; color: #3c8dbc;}",
            " .popover {width: 400px}",
            " .col-sm-1 {padding-left: 0px}",
            "#",
            ns("info_icon_skyline"),
            " .fas {margin-top:28px; color: #3c8dbc;}",
            " .popover {width: 400px}",
            " .col-sm-1 {padding-left: 0px}",
            "#",
            ns("info_icon_sweetsuite"),
            " .fas {margin-top:28px; color: #3c8dbc;}",
            " .popover {width: 400px}",
            " .col-sm-1 {padding-left: 0px}"
          ))
      ),
        div(
          id = ns("info_icon_lacytools"),
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
        id = ns("info_icon_skyline"),
        icon("info-circle", class = "fa-2x") %>% 
          bsplus::bs_embed_popover(
            id = ns("popover"),
            title = "Skyline data (wide format)",
            content = HTML(
              "
              You can upload one Skyline output CSV file. 
              Analytes must be specified in one of two ways:
              <ul>
                <li>
                <i>One column</i> where each entry contains both a peptide sequence
                and a glycan composition (e.g. \"EEQYN[H3N4F1]STYR\").
                A sequence may also contain methionine oxidation and cysteine
                carbamidomethyl (CAM) modifications, either fully written out
                or using three-letter abbreviations.
                </li>
                <li>
                <i>Two separate columns:</i> one with glycosylation sites and one 
                with glycan compositions.
                </li>
              </ul>
              There should also be one column specifying the charge states.
              Additionally, the file should contain columns with &quot;Total Area MS1&quot;,
              &quot;Isotope Dot Product&quot; and &quot;Average Mass Error PPM&quot; 
              for each sample name.
              "
            ),
            trigger = "hover",
            placement = "right",
            html = "true",
            container = "body"
          )
        ),
        div(
          id = ns("info_icon_sweetsuite"),
          icon("info-circle", class = "fa-2x") %>% 
            bsplus::bs_embed_popover(
              id = ns("popover"),
              title = "Sweetsuite data",
              content = HTML(
                "Upload one or more SweetSuite output xlsx files.
                Each file should contain a 'Data' tab, as created
                by SweetSuite."
              ),
              trigger = "hover",
              placement = "right",
              html = "true",
              container = "body"
            )
        ),
      )
    ),
    tableOutput(ns("uploaded_files")),
    shinyWidgets::materialSwitch(
      ns("contains_total_and_specific_samples"),
      HTML("<i style='font-size:15px;'> Samples contain specific and total immunoglobulin samples </i>"),
      status = "success",
      right = TRUE
    ),
    div(id = ns("keywords_specific_total"),
        # Set the width of popovers in this div to 200px:
        tags$style(HTML(paste0(
          "#", ns("keywords_specific_total"),
          " .popover{width: 200px !important;}"
        ))),
        textInput(
          ns("keyword_specific"), 
          label = "By what keyword can the specific Ig samples be recognized?"
        ) %>% 
          bsplus::bs_embed_popover(
            title = "Explanation",
            content = paste(
              "All specific Ig samples should have sample names",
              "that contain this keyword. The keyword is case-sensitive."
            ),
            trigger = "hover",
            placement = "right"),
        textInput(
          ns("keyword_total"), 
          label = "By what keyword can the total Ig samples be recognized?"
        ) %>% 
          bsplus::bs_embed_popover(
            title = "Explanation",
            content = paste(
              "All total Ig samples should have sample names",
              "that contain this keyword. The keyword is case-sensitive."
            ),
            trigger = "hover",
            placement = "right")
    )
  )
}
  


#' read_data Server Functions
#'
#' @noRd 
mod_read_data_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Visibility of UI elements
    observe({
      is_lacytools <- input$data_type == "LaCyTools data"
      is_sweetsuite <- input$data_type == "SweetSuite data"
      is_skyline <- input$data_type == "Skyline data (wide format)"
      
      is_analyte_format <- (
        input$skyline_analyte_format == 
          "One column with peptide sequences and modifications"
      )
      
      has_notes <- isTRUE(input$skyline_contains_notes)
      
      # LaCyTools
      shinyjs::toggle("info_icon_lacytools", condition = is_lacytools)
      shinyjs::toggle("lacytools_input", condition = is_lacytools)
      
      # SweetSuite
      shinyjs::toggle("info_icon_sweetsuite", condition = is_sweetsuite)
      shinyjs::toggle("sweetsuite_input", condition = is_sweetsuite)
      
      # Skyline (always shown for Skyline)
      shinyjs::toggle("button_div", condition = is_skyline)
      shinyjs::toggle("info_icon_skyline", condition = is_skyline)
      shinyjs::toggle("skyline_analyte_format", condition = is_skyline)
      shinyjs::toggle("skyline_charge_column", condition = is_skyline)
      shinyjs::toggle("skyline_contains_notes", condition = is_skyline)
      shinyjs::toggle("skyline_input_wide", condition = is_skyline)
      shinyjs::toggle("skyline_rename_isomers", condition = is_skyline)
      shinyjs::toggle("skyline_merge_glycounter", condition = is_skyline)
    
      # Skyline analyte format
      shinyjs::toggle(
        "skyline_analyte_column",
        condition = is_skyline && is_analyte_format
      )
      shinyjs::toggle(
        "skyline_protein_column",
        condition = is_skyline && is_analyte_format
      )
      shinyjs::toggle(
        "skyline_cluster_column",
        condition = is_skyline && !is_analyte_format
      )
      shinyjs::toggle(
        "skyline_glycan_column",
        condition = is_skyline && !is_analyte_format
      )
      
      # Optional notes column
      shinyjs::toggle(
        "skyline_note_column",
        condition = is_skyline && has_notes
      )
      
      # GlyCounter
      if (is_skyline && isTRUE(input$skyline_merge_glycounter)) {
        shinyjs::show("div_glycounter")
      }
      else {
        shinyjs::hide("div_glycounter")
      }
    })
    
    
    # Check the file extensions of the uploaded summaries.
    correct_file_ext <- reactive({
      if (input$data_type == "LaCyTools data") {
        req(input$lacytools_input)
        wrong_file_ext <- subset(
          input$lacytools_input, !grepl("\\.txt$", name, ignore.case = TRUE)
        )
      } 
      else if (input$data_type == "Skyline data (wide format)") {
        req(input$skyline_input_wide)
        wrong_file_ext <- subset(
          input$skyline_input_wide, !grepl("\\.csv$", name, ignore.case = TRUE)
        )
      }
      else if (input$data_type == "SweetSuite data") {
        req(input$sweetsuite_input)
        wrong_file_ext <- subset(
          input$sweetsuite_input, !grepl("\\.xlsx$", name, ignore.case = TRUE)
        )
      }
    
      if (nrow(wrong_file_ext) > 0) {
        FALSE
      }
      else TRUE
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
      } 
      else if (input$data_type == "Skyline data (wide format)") {
        req(input$skyline_input_wide)
        shinyFeedback::feedbackDanger(
          inputId = "skyline_input_wide",
          show = !is_truthy(correct_file_ext()),
          text = "Please upload CSV files."
        )
      }
      else if (input$data_type == "SweetSuite data") {
        req(input$sweetsuite_input)
        shinyFeedback::feedbackDanger(
          inputId = "sweetsuite_input",
          show = !is_truthy(correct_file_ext()),
          text = "Please upload an xlsx file."
        )
      }
    })
    
    # Show the uploaded LaCyTools/SweetSuite files in the table
    output$uploaded_files <- renderTable({
      req(correct_file_ext(), input$data_type %in% c("LaCyTools data", "SweetSuite data"))
      if (input$data_type == "SweetSuite data") {
        uploaded_files <- input$sweetsuite_input
      } 
      else {
        uploaded_files <- input$lacytools_input
      }
      uploaded_files$datapath <- NULL  # Get rid of the "datapath" column
      uploaded_files
    }, striped = TRUE, bordered = TRUE, rownames = TRUE, align = "c")
    
  
    # If the user changes input$contains_total_and_specific_samples to FALSE  the
    # textInputs for the keywords are reset to empty strings "". This is needed
    # in case the user first fills in keywords but then changes their mind.
    observe({
      updateTextInput("keyword_specific", value = "", session = session)
      updateTextInput("keyword_total", value = "", session = session)
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
            showNotification(paste(
              "Summary file", i, ":", c$message), type = "error", duration = NULL
            )
            NULL
          },
          empty_file = function(c) {
            showNotification(paste(
              "Summary file", i, ":", c$message), type = "error", duration = NULL
            )
            NULL
          },
          wrong_delim = function(c) {
            showNotification(paste(
              "Summary file", i, ":", c$message), type = "error", duration = NULL
            )
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
            showNotification(paste(
              "In summary file", i, c$message), type = "error", duration = NULL
            )
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

    
    ################################################################
    ####################  SweetSuite  #######################################
    #########################################################################
    
    sweetsuite_data <- reactive({
      req(
        correct_file_ext(), 
        input$data_type == "SweetSuite data", 
        input$sweetsuite_input
      )
      tryCatch(
        expr = read_sweetsuite_data(input$sweetsuite_input$datapath),
        error = function(e) {
          showNotification(e$message, type = "error", duration = NULL)
          NULL
        }
      )
    })
    
    #########################################################################
    ####################  Skyline  ##########################################
    #########################################################################
    
    # Read raw Skyline data from CSV file.
    raw_skyline_data <- reactive({
      req(
        isTRUE(correct_file_ext()), 
        input$data_type == "Skyline data (wide format)", 
        input$skyline_input_wide
      )
      read_skyline_csv(input$skyline_input_wide$datapath)
    })
    
    # Update column selection options
    observe({
      req(raw_skyline_data())
      columns <- raw_skyline_data() %>% 
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
      
      for (id in c(
        "skyline_protein_column",
        "skyline_analyte_column", 
        "skyline_cluster_column",
        "skyline_glycan_column", 
        "skyline_charge_column",
        "skyline_molecular_formula_column",
        "skyline_note_column"
      )) {
        if (grepl("charge", id) && "Precursor.Charge" %in% columns) {
          updateSelectizeInput(
            inputId = id, choices = columns, selected = "Precursor.Charge"
          )
        }
        else if (grepl("formula", id) && "Molecule.Formula" %in% columns) {
          updateSelectizeInput(
            inputId = id, choices = columns, selected = "Molecule.Formula"
          )
        }
        else {
          updateSelectizeInput(inputId = id, choices = columns)
        }
      }
    })

    
    # Validate that active Skyline column selectors have distinct values.
    # Note and molecular formula columns are only considered when their
    # respective toggles are enabled.
    observe({
      req(input$data_type == "Skyline data (wide format)")

      is_analyte_format <- (
        input$skyline_analyte_format ==
          "One column with peptide sequences and modifications"
      )

      # Collect the selectors that are currently active
      active_cols <- c(skyline_charge_column = input$skyline_charge_column)

      if (isTRUE(is_analyte_format)) {
        active_cols <- c(
          active_cols,
          skyline_protein_column = input$skyline_protein_column,
          skyline_analyte_column = input$skyline_analyte_column
        )
      } 
      else {
        active_cols <- c(
          active_cols,
          skyline_cluster_column = input$skyline_cluster_column,
          skyline_glycan_column  = input$skyline_glycan_column
        )
      }

      if (isTRUE(input$skyline_contains_notes)) {
        active_cols <- c(
          active_cols,
          skyline_note_column = input$skyline_note_column
        )
      }

      if (isTRUE(input$skyline_merge_glycounter)) {
        active_cols <- c(
          active_cols,
          skyline_molecular_formula_column = input$skyline_molecular_formula_column
        )
      }

      # Find selector IDs whose chosen column appears in more than one selector
      non_empty <- active_cols[!is.na(active_cols) & active_cols != ""]
      # Flag only the second (and later) selector that picks a duplicate column,
      # leaving the first occurrence without a warning.
      dup_ids <- names(non_empty)[duplicated(non_empty)]

      # Show / clear danger feedback for each active selector
      for (id in names(active_cols)) {
        shinyFeedback::feedbackDanger(
          inputId = id,
          show = id %in% dup_ids,
          text = "This column is already selected for another field."
        )
      }

      # Disable the process button while any duplicate exists, no Skyline file is
      # uploaded, or GlyCounter merge is enabled but no GlyCounter files are uploaded.
      glycounter_ready <- (
        !isTRUE(input$skyline_merge_glycounter) || !is.null(input$glycounter_files)
      )
      shinyjs::toggleState(
        "button", condition = (
          length(dup_ids) == 0 && 
          length(non_empty) == length(active_cols) &&
          !is.null(input$skyline_input_wide) &&
          glycounter_ready
        )
      )
    })
    
    
    # Check structure of raw data
    raw_skyline_data_checked <- reactive({
      req(raw_skyline_data())
      checked <- check_skyline_data(raw_skyline_data())
      if (is.null(checked)) {
        shinybusy::remove_modal_spinner()
      }
      checked
    })

    
    # Show spinner after button.
    observeEvent(input$button, {
      shinybusy::show_modal_spinner(
        spin = "cube-grid", color = "#0275D8",
        text = HTML("<br/><strong>Processing Skyline data...")
      )
    }, priority = 5)
    
    
    
    # Reformat data: select required columns, convert to numeric,
    # and optionally rename glycan isomers.
    skyline_data_reformatted <- reactive({
      req(raw_skyline_data_checked())

      # Optional columns
      if (isTRUE(input$skyline_contains_notes)) {
        notes_column <- input$skyline_note_column
      }
      else {
        notes_column <- NULL
      }
      
      if (isTRUE(input$skyline_merge_glycounter)) {
        formula_column <- input$skyline_molecular_formula_column
      }
      else {
        formula_column <- NULL
      }
      
      # Reformat data
      if (startsWith(input$skyline_analyte_format, "One")) {
        reformatted <- reformat_skyline_analyte_column(
          raw_skyline_data = raw_skyline_data_checked(),
          protein_colname = input$skyline_protein_column,
          analyte_colname = input$skyline_analyte_column,
          charge_colname = input$skyline_charge_column,
          notes_colname = notes_column,
          molecular_formula_colname = formula_column
        )
      }
      else {
        reformatted <- reformat_skyline_data(
          raw_skyline_data = raw_skyline_data_checked(),
          cluster_colname = input$skyline_cluster_column,
          glycan_colname = input$skyline_glycan_column,
          charge_colname = input$skyline_charge_column,
          notes_colname = notes_column,
          molecular_formula_colname = formula_column
        )
      }
      
      # Rename isomers.
      if (isTRUE(input$skyline_rename_isomers)) {
        rename_skyline_isomers(reformatted)
      }
      else {
        reformatted
      }
    }) %>% bindEvent(input$button)
    
    
    # Reshape data: one column for each variable and a column with sample names.
    skyline_data_reshaped <- reactive({
      req(skyline_data_reformatted())
      reshape_skyline_data(skyline_data_reformatted())
    })
    
    
    # Optionally merge with GlyCounter data.
    glycounter_data <- reactive({
      req(
        skyline_data_reshaped(),
        isTRUE(input$skyline_merge_glycounter),
        input$glycounter_files$datapath
      )
      # Extract filenames of OxoSignal files (original and in memory)
      original_names <- input$glycounter_files$name
      oxosignal_indices <- grepl("_OxoSignal\\.txt$", original_names, ignore.case = TRUE)
      oxosignal_files <- input$glycounter_files$datapath[oxosignal_indices]
      oxosignal_names <- original_names[oxosignal_indices]

      # Process the OxoSignal files.
      # Show warning if none were uploaded.
      if (length(oxosignal_names) > 0) {
        load_glycounter_data(
          setNames(as.list(oxosignal_files), oxosignal_names)
        )
      }
      else{
        showNotification(
          ui = paste(
            "No GlyCounter 'OxoSignal' text files were detected!",
            "Upload the correct files and try again."
          ),
          duration = NULL,
          type = "error"
        )
        shinybusy::remove_modal_spinner()
        NULL
      }
    })
    
    
    skyline_data_merged <- reactive({
      req(skyline_data_reshaped())
      
      if (isFALSE(input$skyline_merge_glycounter)) {
        skyline_data_reshaped()
      }
      else {
        req(glycounter_data())
        
        tryCatch(
          expr = {
            isotopic_patterns <- calculate_skyline_isotopic_patterns(
              skyline_data = skyline_data_reshaped()
            )
            
            isotope_mz_candidates <- extract_isotopic_mz_candidates(
              isotopic_patterns = isotopic_patterns,
              n_peaks = as.integer(input$n_isotopic_peaks)
            )
            
            fragment_cols <- extract_fragment_cols(glycounter_data())
            
            skyline_prepped <- prepare_skyline_data(
              skyline_data = skyline_data_reshaped(), 
              ppm_tolerance = input$mz_tolerance_ppm
            )
            
            skyline_isotope_candidates <- expand_skyline_isotope_candidates(
              skyline_prepped = skyline_prepped,
              isotope_mz_candidates = isotope_mz_candidates
            )
            
            glycounter_candidates <- extract_glycounter_candidates(
              skyline_isotope_candidates = skyline_isotope_candidates,
              glycounter_data = glycounter_data()
            )
            
            glycounter_summary <- summarize_glycounter_data(
              glycounter_candidates, fragment_cols
            )
            
            merge_skyline_glycounter(skyline_prepped, glycounter_summary)
          },
          error = function(e) {
            showNotification(e$message, type = "error", duration = NULL)
            shinybusy::remove_modal_spinner()
            NULL
          }
        )
      }
    }) %>% bindEvent(skyline_data_reshaped())
    
    
    # Renaming columns
    skyline_data_final <- reactive({
      req(skyline_data_merged())
      skyline_data_merged() %>% 
        dplyr::rename(
          sample_name = sample,
          total_area = `Total.Area.MS1`,
          isotope_dot_product = `Isotope.Dot.Product`,
          mass_accuracy_ppm = `Average.Mass.Error.PPM`
        ) %>% 
        dplyr::mutate(analyte = paste0(cluster, "1", glycan)) %>% 
        dplyr::select(-cluster, -glycan) %>% 
        dplyr::relocate(sample_name, analyte, charge)
    })
    
    
    # Remove spinner.
    observeEvent(skyline_data_final(), {
      shinybusy::remove_modal_spinner()
    })
    
    
    # Create a table with protein names, peptide sequences and corresponding 
    # glycosylation site abbreviations
    glycosites_table <- reactive({
      req(
        skyline_data_final(), 
        "peptide_sequence" %in% colnames(skyline_data_final())
      )
      skyline_data_final() %>% 
        tidyr::separate(
          analyte, sep = "1", into = c("glycosylation_site", "glycan"), 
          extra = "merge"
        ) %>% 
        dplyr::select(
          glycosylation_site, 
          protein, 
          peptide_sequence, 
          methionine_oxidation
        ) %>% 
        dplyr::distinct() %>% 
        dplyr::arrange(protein, peptide_sequence)
    })
    

    # Detect total and specific samples if applicable.
    data_total_and_specific <- reactive({
      shinyFeedback::hideFeedback("keyword_specific")
      shinyFeedback::hideFeedback("keyword_total")
      
      # Require data and non-empty keywords
      req(
        any(
          is_truthy(lacytools_summaries_combined()),
          is_truthy(skyline_data_final()),
          is_truthy(sweetsuite_data())
        ),
        input$keyword_specific,
        input$keyword_total
      )
      
      if (is_truthy(lacytools_summaries_combined())) {
        data_to_check <- lacytools_summaries_combined()
      } 
      else if (is_truthy(skyline_data_final())) {
        data_to_check <- skyline_data_final()
      }
      else if (is_truthy(sweetsuite_data())) {
        data_to_check <- sweetsuite_data()
      }
      
      tryCatch(
        expr = {
          # Detect based on sample names which samples are Total Ig and which are
          # Specific Ig samples
          detect_group(
            data = data_to_check,
            keyword_specific = input$keyword_specific,
            keyword_total = input$keyword_total
          )
        },
        unmatched_keyword_specific = function(c) {
          shinyFeedback::feedbackDanger(
            inputId = "keyword_specific",
            show = TRUE,
            text = paste(
              "This keyword did not match any sample names in your data.", 
              "Please choose a different keyword."
            )
          )
          NULL
        },
        unmatched_keyword_total = function(c) {
          shinyFeedback::feedbackDanger(
            inputId = "keyword_total",
            show = TRUE,
            text = paste(
              "This keyword did not match any sample names in your data.", 
              "Please choose a different keyword."
            )
          )
          NULL
        },
        NAs = function(c) {
          showNotification(c$message, type = "error", duration = NULL)
          NULL
        })
    })
    
    
    # Toggle UI elements
    observeEvent(input$contains_total_and_specific_samples, {
      if (input$contains_total_and_specific_samples) {
        shinyjs::show("keywords_specific_total")
      } 
      else {
        shinyjs::hide("keywords_specific_total")
      }
    })
  
    
    # filenames for report
    filenames <- reactive({
      req(any(
        is_truthy(lacytools_summaries_combined()),
        is_truthy(skyline_data_final()),
        is_truthy(sweetsuite_data())
      ))
      if (is_truthy(lacytools_summaries_combined())) {
        input$lacytools_input$name
      } 
      else if (is_truthy(skyline_data_final())) {
        input$skyline_input_wide$name
      }
      else if (is_truthy(sweetsuite_data())) {
        input$sweetsuite_input$name
      }
    })
    
    
    # Return the data.
    to_return <- reactive({
      req(any(
        is_truthy(lacytools_summaries_combined()),
        is_truthy(skyline_data_final()),
        is_truthy(sweetsuite_data())
      ))
      tryCatch(
        data_total_and_specific(),
        error = function(e) {
          if (is_truthy(lacytools_summaries_combined())) {
            lacytools_summaries_combined()
          } 
          else if (is_truthy(skyline_data_final())) {
            skyline_data_final()
          }
          else if (is_truthy(sweetsuite_data())) {
            sweetsuite_data()
          }
        }
      )
    })
    
    # Prevent people from changing input$data_type after uploading data
    observeEvent(to_return(), {
      shinyjs::hide("data_type")
      if (input$data_type == "LaCyTools data") {
        shinyjs::show("uploaded_lacytools")
      } 
      else if (input$data_type == "Skyline data (wide format)") {
        shinyjs::show("uploaded_skyline_wide")
      }
      else if (input$data_type == "SweetSuite data") {
        shinyjs::show("uploaded_sweetsuite")
      }
    })
    
    # Remove trailing/leading spaces.
    # Ensure charge is an integer.
    to_return_trimmed <- reactive({
      req(to_return())
      to_return() %>% 
        dplyr::mutate(
          dplyr::across(tidyselect::where(is.character), trimws),
          charge = as.integer(charge)
        )
    })
    
    
    # Data type to return
    data_type_to_return <- reactive({
      if (input$data_type == "Skyline data (wide format)") {
        "Skyline data"
      }
      else input$data_type
    })
    
    
    return(list(
      data = to_return_trimmed,
      data_type = data_type_to_return,
      keyword_specific = reactive(input$keyword_specific),
      keyword_total = reactive(input$keyword_total),
      contains_total_and_specific_samples = reactive(
        input$contains_total_and_specific_samples
      ),
      summary_filenames = filenames,
      glycosites_table = glycosites_table
    ))
    
  })
}
    
