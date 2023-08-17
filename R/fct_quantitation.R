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
    dplyr::distinct() %>% 
    tidyr::pivot_wider(names_from = cluster, values_from = sum_intensity)
    
  return(quantitation_data)
}




# Function to calculate ratio between natural and SIL peptides

calculate_IgG1_ratios <- function(IgG1_sum_intensities,
                                  quantitation_clusters) {
  
  sum_intensity_ratios <- IgG1_sum_intensities %>% 
    dplyr::mutate(
      glyco_ratio = .[[quantitation_clusters$IgG1_cluster_glyco]] /
        .[[quantitation_clusters$silumab_cluster_glyco]],
      
      GPS_ratio = .[[quantitation_clusters$IgG1_cluster_GPS]] / 
        .[[quantitation_clusters$silumab_cluster_GPS]],
      
      TTP_ratio = .[[quantitation_clusters$IgG1_cluster_TTP]] / 
        .[[quantitation_clusters$silumab_cluster_GPS]]
    ) %>% 
    # Get rid of sum intensities
    dplyr::select(sample_name:sample_type, tidyselect::contains("ratio"))
  
  
  # Use base R to get median ratio.
  # (Using dplyr::rowwise() is slow)
  ratio_columns <- sum_intensity_ratios[, grepl("ratio", colnames(sum_intensity_ratios))]
  
  sum_intensity_ratios$median_ratio <- apply(ratio_columns, 1, median)
  
  
  # TODO: find a way to get the column name corresponding to the median ratio
  
  
  return(sum_intensity_ratios)
}






