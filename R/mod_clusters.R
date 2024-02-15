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
    title = "Peptides/clusters",
    width = NULL,
    solidHeader = TRUE,
    status = "primary",
    div(
      id = ns("info_detection"),
      HTML("
        <strong> Peptides/clusters in your data will be detected automatically after adding the sample types. </strong>
        <br> <br>
      ")
    ),
    div(
      id = ns("info_clusters"),
      HTML("
        <strong> The following peptides/clusters were detected in your data: </strong>
        <br> <br>
      ")
    ),
    tableOutput(ns("clusters_table")),
    shinyWidgets::materialSwitch(
      ns("contains_silumab"),
      "Samples contain SILuMAb for IgG1 quantitation",
      status = "success",
      right = TRUE
    ),
    selectInput(
      ns("silumab_cluster_glyco"),
      "Which peptide/cluster corresponds to the SILuMAb glycopeptides?",
      choices = c("")
    ),
    selectInput(
      ns("IgG1_cluster_glyco"),
      "Which peptide/cluster corresponds to the natural IgG1 glycopeptides?",
      choices = c("")
    ),
    selectInput(
      ns("silumab_cluster_GPS"),
      "Which peptide/cluster corresponds to the SILuMAb peptide GPSVFPLAPSSK?",
      choices = c("")
    ),
    selectInput(
      ns("IgG1_cluster_GPS"),
      "Which peptide/cluster corresponds to the natural IgG1 peptide GPSVFPLAPSSK?",
      choices = c("")
    ),
    actionButton(ns("button"), "Add clusters to the data")
  )
}



#' clusters Server Functions
#'
#' @noRd 
mod_clusters_server <- function(id, LaCyTools_summary) {
  moduleServer( id, function(input, output, session) {
    ns <- session$ns
    
    # Determine the clusters in the data
    clusters <- reactive({
      req(LaCyTools_summary())
      data_with_clusters <- LaCyTools_summary() %>% 
        tidyr::separate(analyte, sep = "1", into = c("cluster", "glycan"), extra = "merge")
      unique(data_with_clusters$cluster)
    })
    
    # Show the clusters in a table
    output$clusters_table <- renderTable({
      data.frame(clusters())
    }, striped = TRUE, bordered = TRUE, rownames = TRUE, colnames = FALSE, align = "l")
    
    
    # Create a dataframe with a cluster column when user pushes the button
    data_with_clusters <- reactive({
      req(LaCyTools_summary())
      LaCyTools_summary() %>% 
        tidyr::separate(analyte, sep = "1", into = c("cluster", "glycan"), extra = "merge",
                        remove = FALSE) %>% 
        dplyr::select(-glycan)
    }) %>% bindEvent(input$button)
    
    
    # Add clusters as choices to the SILuMAb selectInputs
    observe({
      req(clusters())
      for (i in c("silumab_cluster_glyco", "IgG1_cluster_glyco", "silumab_cluster_GPS", "IgG1_cluster_GPS")) {
        updateSelectInput(inputId = i, choices = clusters())
      }
    })
    
    # Get the cluster names required for IgG1 quantitation, if applicable.
    quantitation_clusters <- reactive({
      req(LaCyTools_summary(), input$contains_silumab == TRUE)
      list(
        "silumab_cluster_glyco" = input$silumab_cluster_glyco,
        "silumab_cluster_GPS" = input$silumab_cluster_GPS,
        "IgG1_cluster_glyco" = input$IgG1_cluster_glyco,
        "IgG1_cluster_GPS" = input$IgG1_cluster_GPS
      )
    })
    
    
    # Toggle the UI elements
    observe({
      shinyjs::toggle("info_detection", condition = !is_truthy(clusters()))
      
      for (i in c("info_clusters", "clusters_table", "contains_silumab")) {
        shinyjs::toggle(i, condition = is_truthy(clusters()))
      }
      
      for (i in c("silumab_cluster_glyco", "IgG1_cluster_glyco", "silumab_cluster_GPS", "IgG1_cluster_GPS")) {
        shinyjs::toggle(i, condition = input$contains_silumab == TRUE)
      }
      
      shinyjs::toggleState(
        id = "button",
        condition = all(
          is_truthy(clusters()),
          if (is_truthy(quantitation_clusters())) {
            length(unlist(quantitation_clusters())) == length(unique(unlist(quantitation_clusters())))
          }
        )
      )
    })
    
    
    return(list(
      data = data_with_clusters,
      quantitation_clusters = quantitation_clusters,
      button = reactive(input$button)
    ))
    
  })
}
