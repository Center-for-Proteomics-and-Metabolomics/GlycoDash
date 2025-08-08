#' clusters UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_clusters_ui <- function(id) {
  ns <- NS(id)
  tagList(
    tags$style(HTML(paste0(
      "#", ns("box_header"), " .awesome-checkbox {padding-top: 7px}",
      "#", ns("box_header"), " .popover {max-width: 400px !important; color: #333}",
      "#", ns("box"), " .box-title {width: 100%}",
      "#", ns("box_header"), " .fas {float: right; margin-right: 5px; font-size: 18px}",
      "#", ns("box_header"), " .direct-chat-contacts {right: 0; background: #222d32!important}",
      "#", ns("box_header"), " .btn {float: right; border-width: 0px; margin-right: 10px}",
      "#", ns("box"), " .dropdown {display: inline-block; float: right; width: 330px}",
      "#", ns("box_header"), " .dropdown-menu {background: #333; right: -30px; left: auto; top: 28px;}"
    ))),
    shinydashboardPlus::box(
      id = ns("box"),
      title = div(
        id = ns("box_header"),
        "Glycosylation sites",
        icon("info-circle", class = "ml") %>% 
          bsplus::bs_embed_popover(
            title = "Explanation",
            content = HTML("
            Glycopeptides in your data will be assigned to a glycosylation 
            site based on their names. For example, two analytes named \"IgGI1H3N4F1\" and
            \"IgGI1H4N4F1\" would be assigned to the glycosylation site \"IgGI\".
            <br> <br>
            Non-glycosylated peptides are also automatically detected. When corresponding
            glycopeptides exist, then this peptide can later be uesd to calculate site occupancies.
            In the example above, this would be the case when the analyte \"IgGI1\" is present in
            the data, which can be used to calculate the occupancy of glycosylation site \"IgGI\".
            When no corresponding glycopeptides are present, the peptide is not considered 
            to be a glycosylation site. These peptides can later be used for protein quantitation.
            "),
            trigger = "hover",
            placement = "right",
            html = "true"
          )
      ),
      width = NULL,
      solidHeader = TRUE,
      status = "primary",
      div(
        id = ns("info_detection"),
        HTML("
        <strong> Glycosylation sites in your data will be detected automatically after adding the sample types. </strong>
        <br> <br>
      ")
      ),
      div(
        id = ns("info_clusters"),
        HTML("
        <strong> The following glycosylation sites were detected in your data: </strong>
        <br> <br>
      ")
      ),
      tableOutput(ns("clusters_table")),
      div(
        id = ns("info_peptides"),
        HTML("
        <strong> The following peptides were detected without any
        corresponding glycopeptides: </strong>
        <br> <br>
      ")
      ),
      tableOutput(ns("peptides_table"))
    )
  )
}



#' clusters Server Functions
#'
#' @noRd 
mod_clusters_server <- function(id, LaCyTools_summary) {
  moduleServer( id, function(input, output, session) {
    ns <- session$ns
    
    # Determine the clusters in the data
    glycopeptide_clusters <- reactive({
      req(LaCyTools_summary())
      subset <- LaCyTools_summary() %>% 
        tidyr::separate(analyte, sep = "1", into = c("cluster", "glycan"), extra = "merge") %>% 
        dplyr::filter(glycan != "")
      return(sort(unique(subset$cluster)))
    })
    
    peptides <- reactive({
      req(glycopeptide_clusters())
      subset <- LaCyTools_summary() %>% 
        tidyr::separate(analyte, sep = "1", into = c("cluster", "glycan"), extra = "merge") %>% 
        dplyr::filter(!cluster %in% glycopeptide_clusters())
      return(sort(unique(subset$cluster)))
    })
    
    
    # Show the clusters in a table
    output$clusters_table <- renderTable({
      req(glycopeptide_clusters())
      data.frame(glycopeptide_clusters())
    }, striped = TRUE, bordered = TRUE, rownames = TRUE, colnames = FALSE, align = "l")
    
    output$peptides_table <- renderTable({
      req(peptides())
      data.frame(peptides())
    }, striped = TRUE, bordered = TRUE, rownames = TRUE, colnames = FALSE, align = "l")
    
    
    # Create a dataframe with a cluster column when user pushes the button
    data_with_clusters <- reactive({
      req(LaCyTools_summary())
      LaCyTools_summary() %>% 
        tidyr::separate(analyte, sep = "1", into = c("cluster", "glycan"), extra = "merge",
                        remove = FALSE) %>% 
        dplyr::select(-glycan)
    })
    
    
    
    # Determine visibility of UI elements.
    observe({
      
      shinyjs::toggle("info_detection", condition = !is_truthy(glycopeptide_clusters()))
      shinyjs::toggle("info_clusters", condition = is_truthy(glycopeptide_clusters()))
      shinyjs::toggle("clusters_table", condition = is_truthy(glycopeptide_clusters()))
      
      if (is_truthy(peptides())) {
        shinyjs::show("info_peptides")
        shinyjs::show("peptides_table")
      } else {
        shinyjs::hide("info_peptides")
        shinyjs::hide("peptides_table")
      }
    })
    
    
    return(list(
      data = data_with_clusters,
      peptides = peptides
    ))
    
  })
}
