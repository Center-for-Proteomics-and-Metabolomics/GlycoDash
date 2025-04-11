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
      req(protein_peptides, normalized_data_wide)
      normalized_data_wide() %>% 
        tidyr::pivot_longer(
          tidyselect::contains("sum_intensity"),
          names_to = "cluster", values_to = "sum_intensity"
        ) %>% 
        dplyr::mutate(cluster = gsub("_sum_intensity", "", cluster)) %>% 
        dplyr::filter(cluster %in% c(
          protein_peptides$Natural, protein_peptides$Labeled
        )) %>% 
        dplyr::select(
          sample_name, sample_type, sample_id, tidyselect::any_of("group"),
          cluster, sum_intensity
        ) %>% 
        dplyr::filter(!is.na(sum_intensity)) %>% 
        dplyr::distinct()
    })
    
    # Get intensities of non-glycosylated peptides
    # peptides_data contains raw data of all non-glycosylated peptides
    # without corresponding glycopeptides. 
    peptide_intensities <- reactive({
      req(protein_peptides, peptides_data)
      peptides_data %>%
        dplyr::filter(cluster %in% c(
          protein_peptides$Natural, protein_peptides$Labeled
        )) %>% 
        dplyr::mutate(intensity_by_fraction = 
                      absolute_intensity_background_subtracted / fraction) %>% 
        dplyr::group_by(sample_name, cluster) %>% 
        dplyr::mutate(sum_intensity = sum(intensity_by_fraction)) %>% 
        dplyr::select(
          sample_name, sample_type, sample_id, tidyselect::any_of("group"),
          cluster, sum_intensity
        ) %>% 
        dplyr::filter(!is.na(sum_intensity)) %>% 
        dplyr::distinct()
    })
    
    observe({
      req(peptides_data)
      browser()
    })
    
    # Need absolute intensities of non-glycosylated peptides
    
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
    
