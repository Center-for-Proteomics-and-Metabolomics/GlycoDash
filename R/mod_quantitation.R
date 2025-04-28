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
        shinydashboardPlus::box(
          id = ns("box"),
          title = div(
            id = ns("box_header"),
            "Specify proteins",
            
            # Add info for custom traits
            icon("info-circle", class = "ml") %>% 
              bsplus::bs_embed_popover(
                title = "Explanation",
                content = HTML(
                  "
                  Text here...
                  <br> <br>
                  Needed columns: protein, natural, labeled, standard_quantity
                  "
                ),
                trigger = "hover",
                placement = "right",
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
                  downloadButton(ns("download_ex_custom_formulas"),
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
          )
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
      tab_contents = NULL
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
      req(proteins_excel(), normalized_data_wide())
      get_glycopeptide_intensities(proteins_excel(), normalized_data_wide())
    })
    
    # Get intensities of non-glycosylated peptides
    peptide_intensities <- reactive({
      req(proteins_excel(), peptides_data())
      get_peptide_intensities(proteins_excel(), peptides_data())
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
    
    # Get calculated quantities based on different peptides
    protein_quantities <- reactive({
      req(combined_intensities())
      get_protein_quantities(combined_intensities(), proteins_excel())
    })
    
    # Calculate median quantity for each protein per sample
    median_quantities <- reactive({
      req(protein_quantities())
      get_median_quantities(protein_quantities())
    })
    
    # Extract protein names
    proteins <- reactive({
      req(proteins_excel(), r$correct_formatting == TRUE)
      unique(proteins_excel()$protein)
    })
    
    
    # Counter used to create unique tab ids when quantitation
    # is performed multiple times
    counter <- reactiveValues(count = 0)
    
    # Generate tabs for each protein
    observeEvent(median_quantities(), {
      # Up the counter by one
      counter$count <- counter$count + 1
      # Remove previously created tabs
      purrr::map(names(r$tab_contents), function(current_protein) {
        removeTab(inputId = "protein_tabs", target = current_protein)
      })
      # Reset r$tab_contents
      r$tab_contents <- NULL
      # Create new tabs for each protein
      purrr::map(proteins(), function(current_protein) {
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
      })
      # Generate tabs contents
      r$tab_contents <- rlang::set_names(proteins()) %>% 
        purrr::map(., function(current_protein) {
          mod_tab_quantitation_server(
            id = paste0(current_protein, "_", counter$count),
            quantities = median_quantities() %>% 
              dplyr::filter(protein == current_protein),
            protein_data = protein_quantities() %>% 
              dplyr::filter(protein == current_protein)
          )
        })
    })
    
    # Get protein quantities in wide format to display and pass on
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
    
  })
}

