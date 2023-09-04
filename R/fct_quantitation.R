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
  
  # Extract the columns with the ratios.
  ratio_columns <- sum_intensity_ratios[, grepl("ratio", colnames(sum_intensity_ratios))]
  
  # Determine column number with median value for each row
  median_column_numbers <- apply(ratio_columns, 1, function(row) {
    median_value <- median(row)
    which(row == median_value)
  })
  
  # Determine the column name for each median column number
  ratio_colnames <- colnames(ratio_columns)
  median_colnames <- ratio_colnames[median_column_numbers]
  
  # Add column to sum_intensity_ratios
  sum_intensity_ratios$median_colname <- median_colnames
  
  # Create a column with the median values.
  # The code inside apply() apparently extracts the values as character,
  # therefore as.numeric() is placed around it.
  sum_intensity_ratios$median_value <- as.numeric(apply(
    sum_intensity_ratios, 1, function(row) row[row["median_colname"]]
    # row["median_colname"] retrieves the value of the column "median_colname" in the row.
    # row[row["median_colname]] then retrieves the value of that column for the row.
  ))
  
  return(sum_intensity_ratios)
}





# Function to make a quantitation plot.
create_quantitation_plot <- function(IgG1_concentrations) {
  
  n_colors <- length(unique(IgG1_concentrations$sample_type))
  my_palette <- color_palette(n_colors)
  
  plot <- IgG1_concentrations %>%
    ggplot2::ggplot(., ggplot2::aes(
      text = paste0(
        "Sample name: ", sample_name, "\n",
        "Sample ID: ", sample_id, "\n",
        "Plate well: ", plate_well, "\n",
        "Quantitation based on: ", dplyr::case_when(
          median_colname == "glyco_ratio" ~ "glycopeptides",
          median_colname == "GPS_ratio" ~ "peptide GPSVFPLAPSSK",
          median_colname == "TTP_ratio" ~ "peptide TTPVLDSDGSFFLYSK"
        ), "\n",
        "IgG1 concentration: ", paste(IgG1_median_concentration, "ng/mL")
      )
    )) +
    ggplot2::geom_point(ggplot2::aes(
      x = sample_name,
      y = IgG1_median_concentration,
      color = sample_type
    ), size = 1, alpha = 0.7) +
    ggplot2::theme_classic() +
    ggplot2::theme(panel.border = ggplot2::element_rect(colour = "black", fill = NA, size = 0.5),
                   strip.background = ggplot2::element_rect(fill = "#F6F6F8")) +
    ggplot2::scale_color_manual(values = my_palette,
                                name = "Sample type") +
    ggplot2::labs(y = "IgG1 concentration (ng/mL)", x = "") +
    ggplot2::scale_x_discrete(labels = NULL)

  return(plot)
}





