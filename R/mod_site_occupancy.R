#' site_occupancy UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_site_occupancy_ui <- function(id) {
  ns <- NS(id)
  tagList(
    tags$style(HTML(paste0(
      "#", ns("box_header1"), " .awesome-checkbox {padding-top: 7px}",
      "#", ns("box_header1"), " .popover {max-width: 400px !important; color: #333}",
      "#", ns("box_header1"), " .fas {float: right; margin-right: 5px; font-size: 18px}",
      "#", ns("box_header1"), " .direct-chat-contacts {right: 0; background: #222d32!important}",
      "#", ns("box_header1"), " .btn {float: right; border-width: 0px; margin-right: 10px}",
      "#", ns("box_header1"), " .dropdown-menu {background: #333; right: -30px; left: auto; top: 28px;}",
      "#", ns("box1"), " .box-title {width: 100%}",
      "#", ns("box1"), " .dropdown {display: inline-block; float: right; width: 330px}",
      "#", ns("box_header2"), " .awesome-checkbox {padding-top: 7px}",
      "#", ns("box_header2"), " .popover {max-width: 400px !important; color: #333}",
      "#", ns("box_header2"), " .fas {float: right; margin-right: 5px; font-size: 18px}",
      "#", ns("box_header2"), " .direct-chat-contacts {right: 0; background: #222d32!important}",
      "#", ns("box_header2"), " .btn {float: right; border-width: 0px; margin-right: 10px}",
      "#", ns("box_header2"), " .dropdown-menu {background: #333; right: -30px; left: auto; top: 28px;}",
      "#", ns("box2"), " .box-title {width: 100%}",
      "#", ns("box2"), " .dropdown {display: inline-block; float: right; width: 330px}"
    ))),
    fluidPage(
      fluidRow(h1("Site occupancy")),
      fluidRow(
        column(
          width = 5,
          shinydashboardPlus::box(
            id = ns("box1"),
            title = div(
              id = ns("box_header1"),
              "Calculate occupancy of glycosylation sites",
              icon("info-circle", class = "ml") %>% 
                bsplus::bs_embed_popover(
                  title = "Explanation",
                  content = HTML("
                    When a non-glycosylated peptide for a glycosylation site is
                    present in your data, the corresponding site occupancy can be
                    calculated by the diving the sum intensity of the glycopeptides
                    by the intensity of the non-glycosylated peptide.
                    <br><br>
                    You can choose to exclude a non-glycosylated peptide from 
                    the site occupancy calculations (e.g., when the quality 
                    of the data is insufficient.)
                  "),
                  trigger = "hover",
                  placement = "bottom",
                  html = "true"
                )
            ),
            width = NULL,
            solidHeader = TRUE,
            status = "primary",
            # Text for when there are no non-glycosylated peptides
            div(
              id = ns("no_peptides"),
              strong("Your samples do not contain non-glycosylated peptides.\n\n"),
              style = "color:#0021B8; font-size: 16px"
            ),
            # Detected clusters in table
            div(
              id = ns("info_clusters"),
              HTML("
                <strong> 
                The following non-glycosylated peptides were detected: 
                </strong> 
                <br> <br>
              ")
            ),
            tableOutput(ns("peptides_table")),
            # Option to exclude peptides from calculation
            selectizeInput(
              ns("excluded_peptides"),
              "Peptides to exclude from site occupancy calculations:",
              choices = c(""), 
              multiple = TRUE
            )
          )
        )
      )
    )
  )
}
    
#' site_occupancy Server Functions
#'
#' @noRd 
mod_site_occupancy_server <- function(id,
                                      results_normalization,
                                      results_quantitation,
                                      results_derived_traits ){
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    normalized_data_wide <- reactive({
      req(results_normalization$normalized_data_wide())
      results_normalization$normalized_data_wide()
    })
    
    
    # Check for non-glycosylated peptides
    peptides <- reactive({
      req(normalized_data_wide())
      colnames <- colnames(normalized_data_wide())[
        grepl("1_peptide_intensity", colnames(normalized_data_wide()))
      ]
      
      peptides <- gsub("1_peptide_intensity", "", colnames)
      
      if (length(peptides) > 0) {
        return(peptides)
      } else {
        return(NULL)
      }
    })
    
    # Show the peptides in a table
    output$peptides_table <- renderTable({
      data.frame(peptides())
    }, striped = TRUE, bordered = TRUE, rownames = TRUE, colnames = FALSE, align = "l")
    
    
    
    observe({
      if (is_truthy(peptides())) {
        shinyjs::hide("no_peptides")
        shinyjs::show("info_clusters")
        shinyjs::show("peptides_table")
        shinyjs::show("excluded_peptides")
        
        updateSelectizeInput(inputId = "excluded_peptides", 
                             choices = peptides(),
                             options = list(maxItems = length(peptides()) - 1))
        
      } else {
        shinyjs::show("no_peptides")
        shinyjs::hide("info_clusters")
        shinyjs::hide("peptides_table")
        shinyjs::hide("excluded_peptides")
      }
    })
    
  })
}
