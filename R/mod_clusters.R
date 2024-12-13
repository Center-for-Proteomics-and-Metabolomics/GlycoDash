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
    shinyWidgets::materialSwitch(
      ns("contains_silumab"),
      HTML("<i style='font-size:15px;'> Samples contain SILuMAb for quantitation of antigen-specific IgG1 </i>"),
      status = "success",
      right = TRUE
    ),
    selectInput(
      ns("silumab_peptides"),
      "Choose the peptides you want to use for IgG1 quantitation:",
      choices = c("Glycopeptides and Peptide GPSVFPLAPSSK", "Glycopeptides", "Peptide GPSVFPLAPSSK"),
      selected = "Glycopeptides and Peptide GPSVFPLAPSSK"
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
    })
    
    
    
    # Determine visibility of UI elements.
    observe({
      
      shinyjs::toggle("info_detection", condition = !is_truthy(clusters()))
      shinyjs::toggle("info_clusters", condition = is_truthy(clusters()))
      shinyjs::toggle("clusters_table", condition = is_truthy(clusters()))
      shinyjs::toggle("contains_silumab", condition = is_truthy(clusters()) && length(clusters()) >= 2)
      
      shinyjs::toggle("silumab_peptides", condition = input$contains_silumab == TRUE)
      
      shinyjs::toggle("silumab_cluster_glyco", condition = input$contains_silumab == TRUE && 
                        (input$silumab_peptides == "Glycopeptides and Peptide GPSVFPLAPSSK" ||
                           input$silumab_peptides == "Glycopeptides"))
      
      shinyjs::toggle("IgG1_cluster_glyco", condition = input$contains_silumab == TRUE && 
                        (input$silumab_peptides == "Glycopeptides and Peptide GPSVFPLAPSSK" ||
                           input$silumab_peptides == "Glycopeptides"))
      
      shinyjs::toggle("silumab_cluster_GPS", condition = input$contains_silumab == TRUE && 
                        (input$silumab_peptides == "Glycopeptides and Peptide GPSVFPLAPSSK" ||
                           input$silumab_peptides == "Peptide GPSVFPLAPSSK"))
      
      shinyjs::toggle("IgG1_cluster_GPS", condition = input$contains_silumab == TRUE && 
                        (input$silumab_peptides == "Glycopeptides and Peptide GPSVFPLAPSSK" ||
                           input$silumab_peptides == "Peptide GPSVFPLAPSSK"))

      
    })
    
    
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
      
      if (input$silumab_peptides == "Glycopeptides and Peptide GPSVFPLAPSSK") {
        list(
          "silumab_cluster_glyco" = input$silumab_cluster_glyco,
          "IgG1_cluster_glyco" = input$IgG1_cluster_glyco,
          "silumab_cluster_GPS" = input$silumab_cluster_GPS,
          "IgG1_cluster_GPS" = input$IgG1_cluster_GPS
        )
      } else if (input$silumab_peptides == "Glycopeptides") {
        list(
          "silumab_cluster_glyco" = input$silumab_cluster_glyco,
          "IgG1_cluster_glyco" = input$IgG1_cluster_glyco
        )
      } else if (input$silumab_peptides == "Peptide GPSVFPLAPSSK") {
        list(
          "silumab_cluster_GPS" = input$silumab_cluster_GPS,
          "IgG1_cluster_GPS" = input$IgG1_cluster_GPS
        )
      }
    })
    
    
    return(list(
      data = data_with_clusters,
      quantitation_clusters = quantitation_clusters
    ))
    
  })
}
