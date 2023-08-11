# This file contains functions that are used in mod_quantitation.R


#' calculate_IgG1_sum_intensities
#'
#' @param LaCyTools_summary 
#' LaCyTools summary, as created in data import.
#' @param quantitation_clusters
#' A named list with the cluster names of the peptides that are used
#' for IgG1 quantitation. This list is created in the data import tab.
#' @param analyte_curated_data 
#' The analyte curated data from the analyte curation tab.
#'
#' @return A dataframe with the sum intensities of each cluster that is required
#' for IgG1 quantitation. 
#' 
calculate_IgG1_sum_intensities <- function(LaCyTools_summary,
                                      quantitation_clusters,
                                      analyte_curated_data) {
  
  # Get data of passing IgG1 analytes
  passing_IgG1_analytes_data <- analyte_curated_data %>% 
    dplyr::filter(cluster == quantitation_clusters$IgG1_cluster_glyco)
  
  # Get sample names of passing IgG1 spectra
  passing_IgG1_spectra <- unique(passing_IgG1_analytes_data$sample_name)
  
  # Get data of SILuMAb glycopeptides and non-glycosylated peptides.
  # Then select only the sample names for which the IgG1 spectra passed.
  # Then combine with natural IgG1 glycopeptide data.
  quantitation_data <- LaCyTools_summary %>% 
    dplyr::filter(
      cluster %in% quantitation_clusters[names(quantitation_clusters) != "IgG1_cluster_glyco"]
    ) %>% 
    dplyr::filter(sample_name %in% passing_IgG1_spectra) %>% 
    dplyr::bind_rows(passing_IgG1_analytes_data) %>% 
    # Use function from normalization to calculate total intensities of analytes
    calculate_total_intensity(.) %>% 
    # Then calculate the sum intensities (code below is part of normalization function)
    dplyr::group_by(cluster,
                    sample_name) %>%
    dplyr::reframe(sum_intensity = sum(total_absolute_intensity),
                   across(everything())) %>% 
    dplyr::select(-total_absolute_intensity) %>% 
    # Select data that's needed
    dplyr::select(sample_name, plate_well, sample_id, sample_type, cluster, sum_intensity) %>% 
    dplyr::distinct()
    
  return(quantitation_data)
}



