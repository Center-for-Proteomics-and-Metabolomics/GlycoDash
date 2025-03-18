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
  shinydashboard::box(
    title = "Glycosylation sites",
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
      return(unique(subset$cluster))
    })
    
    peptides <- reactive({
      req(glycopeptide_clusters())
      subset <- LaCyTools_summary() %>% 
        tidyr::separate(analyte, sep = "1", into = c("cluster", "glycan"), extra = "merge") %>% 
        dplyr::filter(!cluster %in% glycopeptide_clusters())
      return(unique(subset$cluster))
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
      data = data_with_clusters
    ))
    
  })
}
