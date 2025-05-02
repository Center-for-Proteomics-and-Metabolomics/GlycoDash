#' quantitation UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_quantitation_ui <- function(id) {
  ns <- NS(id)
  tagList(
    tags$style(HTML(paste0(
      "#", ns("box_header"), " .awesome-checkbox {padding-top: 7px}",
      "#", ns("box_header"), " .popover {max-width: 400px !important; color: #333}",
      "#", ns("box"), " .box-title {width: 100%}",
      "#", ns("box_header"), " .fas {float: right; margin-right: 5px; font-size: 18px}",
      "#", ns("box_header"), " .direct-chat-contacts {right: 0; background: #222d32!important}",
      "#", ns("box_header"), " .btn {float: right; border-width: 0px; margin-right: 5px}",
      "#", ns("box"), " .dropdown {display: inline-block; float: right; width: 135px}",
      "#", ns("box_header"), " .dropdown-menu {background: #333; right: -30px; left: auto; top: 28px;}"
    ))),
    fluidPage(
      fluidRow(
        h1("Protein quantitation")
      ),
      fluidRow(
        # Upload Excel file with proteins
        shinydashboardPlus::box(
          id = ns("box"),
          title = div(
            id = ns("box_header"),
            "Specify proteins",
            icon("info-circle", class = "ml") %>% 
              bsplus::bs_embed_popover(
                title = "Explanation",
                content = HTML(
                  "
                  Upload here an Excel file specifying which peptides should
                  be used to quantify which protein. The file should contain
                  the following columns:
                  <ul>
                  <li> <i> protein </i> - 
                  Custom names of proteins that you want to quantify. If you use more
                  than one peptide to quantify a protein, the corresponding protein
                  name will be present multiple times in this column.
                  </li>
                  <li> <i> natural </i> - 
                  For each protein, the (automatically detected) names of natural 
                  glycosylation sites and/or non-glycosylated peptides 
                  that you want to use for quantitation of each protein.
                  </li>
                  <li> <i> labeled </i> - 
                  For each natural glycosylation site or non-glycosylated peptide:
                  the name of the corresponding stable isotope labeled glycosylation
                  site or peptide.
                  </li>
                  <li> <i> standard_quantity </i> - 
                  The quantity of stable isotope labeled standard added to each sample.
                  Entries in this column should be numbers. GlycoDash agnostic
                  when it comes to the units of the quantities.
                  </li>
                  </ul>
                  <br>
                  For an example file, click the paperclip icon.
                  "
                ),
                trigger = "hover",
                placement = "bottom",
                html = "true"
              ),
            
            shinyWidgets::dropdownButton(
              tags$style(HTML(paste0(
                "#",
                ns("dropdown_content"),
                " .fas {float: left}",
                "#",
                ns("dropdown_content"),
                " .btn {float: none; border-width: 1px; width: 280px; margin: 10px}"
              ))),
              div(id = ns("dropdown_content"),
                  downloadButton(ns("download_example"),
                                 "Download an example Excel file")),
              icon = icon("paperclip", class = "ml"),
              tooltip = shinyWidgets::tooltipOptions(placement = "top",
                                                     title = "Example"),
              width = "330px",
              size = "xs"
            )
          ),
          width = 5,
          solidHeader = TRUE,
          status = "primary",
          fileInput(ns("proteins_file"),
                    "Upload Excel file with protein specifications:"
          ),
          # Option to exclude peptide ions from calculations
          shinyjs::hidden(selectizeInput(
            ns("exclude_peptides"),
            "Non-glycosylated peptide ions to exclude from the calculations:",
            choices = c(""),
            multiple = TRUE
          )
        )),
        # Quality check for non-glycosylated peptides
        shinydashboardPlus::box(
          id = ns("box"),
          title = div(
            id = ns("box_header"),
            "Quality check for non-glycosylated peptides",
            icon("info-circle", class = "ml") %>% 
              bsplus::bs_embed_popover(
                title = "Explanation",
                content = HTML(
                  "
                  For each non-glycosylated peptide ion that is used for quantitation,
                  the percentage of samples in which the ion fulfills three
                  quality criteria is plotted. For S/N and IPQ (in the case 
                  of LaCyTools data), or total area and IDP (in the case 
                  of Skyline data), the same quality criteria that were used
                  for spectral and analyte curation are applied.
                  <br> <br>
                  Because non-glycosylated peptides are often integrated without
                  calibration, you may want to be more lenient when it comes to
                  the acceptable mass error. This value can be set below.
                  "
                ),
                trigger = "hover",
                placement = "bottom",
                html = "true"
              )
          ),
          downloadButton(ns("download"), "Download quality details"),
          br(),
          br(),
          tabsetPanel(id = ns("peptide_tabs")),
          width = 7,
          solidHeader = TRUE,
          status = "primary"
          )
        ),
      fluidRow(
        shinydashboardPlus::box(
          id = ns("box"),
          title = div(
            id = ns("box_header"),
            "Quantitation results"
          ),
          tabsetPanel(id = ns("protein_tabs")),
          width = 12,
          solidHeader = TRUE,
          status = "primary"
        )
      ),
      fluidRow(
        shinydashboard::box(
          title = "View data with protein quantities",
          width = 12,
          solidHeader = TRUE,
          status = "primary",
          DT::dataTableOutput(ns("data_table"))
        )
      )
    )
  )
}
   
 
#' quantitation Server Functions
#'
#' @noRd 
mod_quantitation_server <- function(id,
                                    peptides,
                                    peptides_data,
                                    results_spectra_curation,
                                    results_normalization) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    normalized_data <- results_normalization$normalized_data
    normalized_data_wide <- results_normalization$normalized_data_wide
    
    # Check file extension
    extension <- reactive({
      req(input$proteins_file)
      tools::file_ext(input$proteins_file$name)
    })
    
    observeEvent(extension(), {
      shinyFeedback::hideFeedback("proteins_file") 
      shinyFeedback::feedbackDanger(
        "proteins_file",
        !extension() %in% c("xlsx", "xls"),
        "Please upload a .xlsx or .xls file."
      )
    })
    
    # Read Excel file
    proteins_excel <- reactive({
      req(input$proteins_file, extension(), extension() %in% c("xlsx", "xls"))
      readxl::read_excel(input$proteins_file$datapath, col_names = TRUE, col_types = "text")
    })
    
    # Initiate reactiveValues vector
    r <- reactiveValues(
      correct_formatting = NULL,
      protein_tabs_contents = NULL,
      peptide_tabs_contents = NULL,
      peptide_tabs_names = NULL,
      peptide_tabs_data = NULL
    )
    
    # Check validity of column names and peptide entries
    observeEvent(proteins_excel(), {
      req(normalized_data())
      # Check column names
      if (!all(
        ncol(proteins_excel()) == 4, 
        colnames(proteins_excel())[1] == "protein", 
        colnames(proteins_excel())[2] == "natural", 
        colnames(proteins_excel())[3] == "labeled",
        colnames(proteins_excel())[4] == "standard_quantity"
      )) {
        shinyalert::shinyalert(
          text = "
          Your Excel file should contain four columns: 
          \"protein\", \"natural\", \"labeled\" and \"standard_quantity\"
          Please adjust your file accordingly.
          ",
          confirmButtonCol = "tomato"
        )
        r$correct_formatting <- FALSE
      } else {
        # Colnames are correct --> check peptides validity
        clusters_specified <- c(proteins_excel()$natural, proteins_excel()$labeled)
        clusters_data <- c(unique(normalized_data()$cluster), peptides())
        missing <- clusters_specified[!clusters_specified %in% clusters_data]
        if (length(missing) > 0) {
          shinyalert::shinyalert(
            text = paste0(
              "The following peptides are not present in your curated data: ",
              paste0(missing, collapse = ", "), ". ",
              "Please adjust your Excel file."
            ),
            confirmButtonCol = "tomato"
          )
          r$correct_formatting <- FALSE
        } else {
          r$correct_formatting <- TRUE
        }
      }
    })
    
    
    # Get intensities of glycopeptides
    glycopeptide_intensities <- reactive({
      req(proteins_excel(), normalized_data_wide(), r$correct_formatting == TRUE)
      get_glycopeptide_intensities(proteins_excel(), normalized_data_wide())
    })
    
    # Get intensities of non-glycosylated peptides
    peptide_intensities <- reactive({
      req(proteins_excel(), peptides_data(), r$correct_formatting == TRUE)
      get_peptide_intensities(proteins_excel(), peptides_data(), input$exclude_peptides)
    })
    
    # Combine peptide and glycopeptide intensities (need at least one)
    combined_intensities <- reactive({
      req(is_truthy(glycopeptide_intensities()) || is_truthy(peptide_intensities()))
      if (is_truthy(glycopeptide_intensities()) && is_truthy(peptide_intensities())) {
        dplyr::bind_rows(glycopeptide_intensities(), peptide_intensities())
      } else if (is_truthy(glycopeptide_intensities())) {
        glycopeptide_intensities()
      } else {
        peptide_intensities()
      }
    })
    
    # Prevent trying quantitation when all required peptide ions are excluded
    proteins_checked <- reactive({
      req(combined_intensities(), proteins_excel())
      present <- unique(combined_intensities()$cluster)
      checked <- proteins_excel() %>% 
        dplyr::filter(natural %in% present & labeled %in% present)
      return(checked)
    })
    
    # Warning message
    observeEvent(proteins_checked(), {
      difference <- dplyr::anti_join(proteins_excel(), proteins_checked())
      if (nrow(difference) > 0) {
        for (rownum in nrow(difference)) {
          row <- difference[rownum, ]
          message <- paste0(
            "Protein ", difference$protein, 
            " could not be quantified based on peptides ",
            paste0(row$natural, " / ", row$labeled),
            ", because too many ions required for the calculation were excluded."
          )
          showNotification(message, type = "warning", duration = 30)
        }
      }
    })
    
    # Get calculated quantities based on different peptides
    protein_quantities <- reactive({
      req(combined_intensities())
      get_protein_quantities(combined_intensities(), proteins_checked())
    })
    
    # Calculate median quantity for each protein per sample
    median_quantities <- reactive({
      req(protein_quantities())
      get_median_quantities(protein_quantities())
    })
    
    # Extract protein names
    proteins <- reactive({
      req(proteins_checked(), r$correct_formatting == TRUE)
      unique(proteins_checked()$protein)
    })
    
    
    # Counter used to create unique tab ids when quantitation
    # is performed multiple times
    counter <- reactiveValues(count = 0)
    
    # Generate tabs for each protein
    observeEvent(median_quantities(), {
      # Up the counter by one
      counter$count <- counter$count + 1
      # Remove previously created tabs
      purrr::map(names(r$protein_tabs_contents), function(current_protein) {
        removeTab(inputId = "protein_tabs", target = current_protein)
      })
      purrr::map(r$peptide_tabs_names, function(current_protein) {
        removeTab(inputId = "peptide_tabs", target = current_protein)
      })
      # Reset tab contents in reactiveValues vector
      r$protein_tabs_contents <- NULL
      r$peptide_tabs_contents <- NULL
      r$peptide_tabs_names <- NULL
      r$peptide_tabs_data <- NULL
      # Create new tabs with quantitation results for each protein
      for (current_protein in proteins()) {
        appendTab(
          inputId = "protein_tabs",
          select = TRUE,
          tab = tabPanel(
            title = current_protein,
            mod_tab_quantitation_ui(
              id = ns(paste0(current_protein, "_", counter$count))
            )
          )
        )
      }
      r$protein_tabs_contents <- rlang::set_names(proteins()) %>% 
        purrr::map(., function(current_protein) {
          mod_tab_quantitation_server(
            id = paste0(current_protein, "_", counter$count),
            quantities = median_quantities() %>% 
              dplyr::filter(protein == current_protein),
            protein_data = protein_quantities() %>% 
              dplyr::filter(protein == current_protein)
          )
        })
      # Generate peptide QC tabs
      if (!is.null(peptides_data())) {
        for (current_protein in proteins()) {
          # Check peptide data for current protein
          peptides <- proteins_excel() %>%
            dplyr::filter(protein == current_protein)
          peptides_data_current_protein <- peptides_data() %>%
            dplyr::filter(cluster %in% c(peptides$natural, peptides$labeled))
          if (nrow(peptides_data_current_protein) == 0) {
            peptides_data_current_protein <- NULL
          }
          # If peptide data exists, generate tab
          if (!is.null(peptides_data_current_protein)) {
            r$peptide_tabs_names <- c(r$peptide_tabs_names, current_protein)
            r$peptide_tabs_data[[current_protein]] <- peptides_data_current_protein
            appendTab(
              inputId = "peptide_tabs",
              select = TRUE,
              tab = tabPanel(
                title = current_protein,
                mod_tab_quantitation_peptides_ui(
                  id = ns(paste0(current_protein, "_peptides_", counter$count))
                )
              )
            )
          }
        }
      }
      r$peptide_tabs_contents <- rlang::set_names(r$peptide_tabs_names) %>%
        purrr::map(., function(current_protein) {
          mod_tab_quantitation_peptides_server(
            id = paste0(current_protein, "_peptides_", counter$count),
            peptides_data = r$peptide_tabs_data[[current_protein]],
            results_spectra_curation = results_spectra_curation
          )
        })
    })
    
    
    # Option to exclude peptide ions from calculations
    observe({
      if (is_truthy(peptides_data())) {
        shinyjs::show("exclude_peptides")
      } else {
        shinyjs::hide("exclude_peptides")
      }
    })
    
    peptide_ions <- reactive({
      req(peptides_data(), proteins_excel())
      peptides_data() %>% 
        dplyr::filter(
          cluster %in% c(proteins_excel()$natural, proteins_excel()$labeled)
        ) %>% 
        dplyr::select(cluster, charge) %>% 
        dplyr::distinct() %>% 
        dplyr::arrange(cluster, charge) %>% 
        dplyr::mutate(ion = paste0(cluster, ", ", charge))
    })
    
    observeEvent(peptide_ions(), {
      updateSelectizeInput(
        inputId = "exclude_peptides",
        choices = peptide_ions()$ion
      )
    })
    
    
    # Get protein quantities in wide format
    data_with_quantities <- reactive({
      req(median_quantities())
      quantities_wide <- median_quantities() %>% 
        dplyr::mutate(protein = paste0(protein, "_quantity")) %>% 
        tidyr::pivot_wider(names_from = protein, values_from = quantity)
      
      data_with_quantities <- normalized_data_wide() %>% 
        dplyr::left_join(quantities_wide) %>% 
        dplyr::relocate(tidyselect::contains("_quantity"), 
                        .after = tidyselect::contains("_sum_intensity"))
      
      return(data_with_quantities)
    })
    
    
    # Display data with protein quantities
    output$data_table <- DT::renderDT({
      req(data_with_quantities())
      DT::datatable(data_with_quantities() %>% # Round numbers to 2 decimals
                      dplyr::mutate_if(is.numeric, ~format(round(., 2), nsmall = 2)),
                    options = list(
                      scrollX = TRUE,
                      pageLength = 6,  # Shows 5 rows
                      columnDefs = list(list(className = "dt-center", targets = "_all"))
                    ),
                    filter = "top")
    })
    
    
    
    # Download example Excel file
    output$download_example <- downloadHandler(
      filename = "protein_quantitation_example.xlsx",
      content = function(file) {
        example_file <- system.file("app",
                                    "www",
                                    "protein_quantitation_example.xlsx",
                                    package = "GlycoDash")
        file.copy(example_file, file)
      }
    )
    
    
    # Download peptide QC data
    observe({
      shinyjs::toggleState("download", condition = (
        is_truthy(peptides_data()) & is_truthy(proteins_excel()) & is_truthy(peptide_ions())
      ))
    })
    
    output$download <- downloadHandler(
      filename = function() {
        current_datetime <- paste0(format(Sys.Date(), "%Y%m%d"), "_", format(Sys.time(), "%H%M"))
        paste0(current_datetime, "_quantitation_peptides_quality.xlsx")
      },
      content = function(file) {
        writexl::write_xlsx(
          peptides_data() %>% 
            dplyr::filter(cluster %in% peptide_ions()$cluster),
          path = file
        )
      }
    )
    
    
    return(list(
      data_with_quantities = data_with_quantities,
      protein_tab_contents = reactive(r$protein_tab_contents),
      peptide_tab_contents = reactive(r$peptide_tab_contents)
    ))
  })
}

