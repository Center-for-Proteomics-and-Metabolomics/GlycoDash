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
                  Needed columns: Protein, Natural, Labeled
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
      )
    )
  )
}
   
 
#' quantitation Server Functions
#'
#' @noRd 
mod_quantitation_server <- function(id,
                                    peptides,
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
    
    # Check validity of column names and peptide entries
    r <- reactiveValues()
    observeEvent(proteins_excel(), {
      req(normalized_data())
      r$correct_formatting <- TRUE
      # Check column names
      if (!all(
        ncol(proteins_excel()) == 3, 
        colnames(proteins_excel())[1] == "Protein", 
        colnames(proteins_excel())[2] == "Natural", 
        colnames(proteins_excel())[3] == "Labeled"
      )) {
        shinyalert::shinyalert(
          text = "
          Your Excel file should contain three columns: 
          \"Protein\", \"Natural\" and \"Labeled\".
          Please adjust your file accordingly.
          ",
          confirmButtonCol = "tomato"
        )
        r$correct_formatting <- FALSE
      } else {
        # Colnames are correct --> check peptides validity
        clusters_specified <- c(proteins_excel()$Natural, proteins_excel()$Labeled)
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
        }
      }
    })
    
    
    
    
    
    
  })
}

