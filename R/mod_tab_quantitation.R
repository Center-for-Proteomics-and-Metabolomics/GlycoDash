#' tab_quantitation UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_tab_quantitation_ui <- function(id) {
  ns <- NS(id)
  tagList(
  
  )
}
    
#' tab_quantitation Server Functions
#'
#' @noRd 
mod_tab_quantitation_server <- function(id, 
                                        protein_peptides,
                                        peptides_data,
                                        normalized_data_wide) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Get intensities of glycopeptides
    glycopeptide_intensities <- reactive({
      req(protein_peptides, normalized_data_wide())
      get_glycopeptide_intensities(protein_peptides, normalized_data_wide())
    })
    
    # Get intensities of non-glycosylated peptides
    peptide_intensities <- reactive({
      req(protein_peptides, peptides_data)
      get_peptide_intensities(protein_peptides, peptides_data)
    })
    
    # Get calculated quantities based on different peptides
    protein_quantities <- reactive({
      req(glycopeptide_intensities(), peptide_intensities())
      get_protein_quantities(
        glycopeptide_intensities(), peptide_intensities(), protein_peptides
      )
    })
    
    # Calculate median quantity for each protein per sample
    median_quantities <- reactive({
      req(protein_quantities())
      get_median_quantities(protein_quantities())
    })
    
    observe({
      req(median_quantities())
      # IgG <- median_quantities() %>% 
      #   dplyr::filter(protein == "IgG1")
      # ggplot2::ggplot(IgG, ggplot2::aes(sample_type, quantity)) +
      #   ggplot2::geom_boxplot(outlier.shape = NA) +
      #   ggplot2::geom_point(ggplot2::aes(color = sample_type))
      browser()
    })
    
    

    
    # Temporary code
    df <- reactive({
      req(protein_peptides)
      protein_peptides
    })
    
    return(list(
      df = df
    ))
  })
}
    
