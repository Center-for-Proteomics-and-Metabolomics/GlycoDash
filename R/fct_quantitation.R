# This file contains functions that are used in mod_quantitation.R


#' get_IgG1_quantitation_data 
#'
#' @param LaCyTools_summary 
#' LaCyTools summary, as created in data import.
#' @param quantitation_clusters
#' A named list with the cluster names of the peptides that are used
#' for IgG1 quantitation. This list is created in the data import tab.
#' @param analyte_curated_data 
#' The analyte curated data from the analyte curation tab.
#'
#' @return A dataframe with the all non-glycosylated peptide, SILuMAb glycopeptides,
#' and natural IgG1 glycopeptides. For natural IgG1 glycopeptides, only those
#' that passed analyte curation are included.
#' The returned dataframe only contains samples whose IgG1 glycopeptide spectra
#' passed spectra curation.
#' 
get_IgG1_quantitation_data <- function(LaCyTools_summary,
                                      quantitation_clusters,
                                      analyte_curated_data) {
  
  # Get data of passing IgG1 analytes
  passing_IgG1_analytes <- analyte_curated_data %>% 
    dplyr::filter(cluster == quantitation_clusters$IgG1_cluster_glyco)
  
  # Get sample names of passing IgG1 spectra
  passing_IgG1_spectra <- unique(passing_IgG1_analytes$sample_name)
  
  # Get data of SILuMAb glycopeptides and non-glycosylated peptides.
  # Then select only the sample names for which the IgG1 spectra passed.
  # Then combine with metadata 
  quantitation_data <- LaCyTools_summary %>% 
    dplyr::filter(
      cluster %in% quantitation_clusters[names(quantitation_clusters) != "IgG1_cluster_glyco"]
    ) %>% 
    dplyr::filter(sample_name %in% passing_IgG1_spectra) %>% 
    dplyr::bind_rows(passing_IgG1_analytes) %>% 
    # Delete sum_intensity column
    # (For quantitation, will use sum of background subtracted absolute intensities,
    # without dividing by fraction.) ?????
    dplyr::select(-sum_intensity)
  
  return(quantitation_data)
}



