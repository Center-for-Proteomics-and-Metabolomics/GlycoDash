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
    dplyr::group_by(cluster, sample_name) %>%
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
  
  
  # Extract the columns with the ratios, and calculate median ratio for each sample
  ratio_columns <- sum_intensity_ratios[, grepl("ratio", colnames(sum_intensity_ratios))]
  ratio_columns$median_ratio <- apply(ratio_columns, 1, median)
  
  # Add median ratios to sum_intensity_ratios
  sum_intensity_ratios$median_ratio <- ratio_columns$median_ratio
  
  return(sum_intensity_ratios)
}





# Function to make a quantitation plot.
create_quantitation_plot <- function(IgG1_amounts) {
  
  n_colors <- length(unique(IgG1_amounts$sample_type))
  my_palette <- color_palette(n_colors)
  
  plot <- IgG1_amounts %>%
    ggplot2::ggplot(., ggplot2::aes(
      text = paste0(
        "Sample name: ", sample_name, "\n",
        "Sample ID: ", sample_id, "\n",
        "Plate well: ", plate_well, "\n",
        "Amount of IgG1 (ng): ", IgG1_median_amount
      )
    )) +
    ggplot2::geom_boxplot(ggplot2::aes(
      x = sample_type,
      y = IgG1_median_amount
    )) +
    ggplot2::geom_jitter(ggplot2::aes(
      x = sample_type,
      y = IgG1_median_amount,
      color = sample_type
    ), height = 0, width = 0.2, size = 1, alpha = 0.7) +
    ggplot2::theme_classic() +
    ggplot2::theme(panel.border = ggplot2::element_rect(colour = "black", fill = NA, size = 0.5),
                   strip.background = ggplot2::element_rect(fill = "#F6F6F8")) +
    ggplot2::scale_color_manual(values = my_palette,
                                name = "Sample type") +
    ggplot2::labs(y = "Amount of IgG1 (ng)", x = "Sample type") +
    ggplot2::scale_y_continuous(labels = function(x) format(x, scientific = TRUE))

  return(plot)
}





# Function to plot peptide correlations.
plot_peptide_correlation <- function(IgG1_amounts, tab_id, amount) {
  # Determine x and y columns to plot, depending on tab_id
  ycol <- dplyr::case_when(
    tab_id %in% c("glyco_vs_GPS", "glyco_vs_TTP") ~ "glyco_ratio",
    tab_id == "GPS_vs_TTP" ~ "GPS_ratio"
  )
  xcol <- dplyr::case_when(
    tab_id %in% c("glyco_vs_TTP", "GPS_vs_TTP") ~ "TTP_ratio",
    tab_id == "glyco_vs_GPS" ~ "GPS_ratio"
  )
  
  # Calculate Spearman's correlation
  correlation <- stats::cor(IgG1_amounts[[xcol]], IgG1_amounts[[ycol]], 
                            method = "spearman")
  
  # Color palette for plot
  n_colors <- length(unique(IgG1_amounts$sample_type))
  my_palette <- color_palette(n_colors)
  
  # Correlation plot
  ggplot2::ggplot() + 
    ggplot2::ggtitle(paste0(
      "Spearman correlation = ",
      as.character(round(correlation, digits = 2))
    )) +
    ggplot2::geom_point(data = IgG1_amounts, ggplot2::aes(
      x = .data[[xcol]] * amount,
      y = .data[[ycol]] * amount,
      color = sample_type,
      text = paste0(
        "Sample name: ", sample_name, "\n",
        "Sample ID: ", sample_id, "\n",
        "Plate well: ", plate_well
      )
    ), size = 1, alpha = 0.7) +
    ggplot2::xlab(dplyr::case_when(
      xcol == "TTP_ratio" ~ "IgG1 (ng) - Based on TTP",
      xcol == "GPS_ratio" ~ "IgG1 (ng) - Based on GPS"
    )) +
    ggplot2::ylab(dplyr::case_when(
      ycol == "glyco_ratio" ~ "IgG1 (ng) - Based on glycopeptides",
      ycol == "GPS_ratio" ~ "IgG1 (ng) - Based on GPS"
    )) + 
    ggplot2::theme_classic() +
    ggplot2::theme(
      strip.background = ggplot2::element_rect(fill = "#F6F6F8"),
      panel.border = ggplot2::element_rect(colour = "black", fill = NA, size = 0.5),
      plot.title = ggplot2::element_text(size = 12)
    ) +
    ggplot2::scale_color_manual(values = my_palette, name = "Sample type") +
    ggplot2::scale_x_continuous(labels = function(x) format(x, scientific = TRUE)) +
    ggplot2::scale_y_continuous(labels = function(x) format(x, scientific = TRUE))
}

