#' Calculate the total intensity of an analyte in a spectrum
#'
#' The function \code{calculate_total_intensity} calculates the total intensity
#' of an analyte in a spectrum, by combining the intensities from each charge
#' state. For each charge state, the intensity is divided by the fraction. Then
#' these adjusted intensities are summed up to get the total intensity of the
#' analyte.
#'
#' @param data The data in long format (one row for each analyte + charge
#'   combination per spectrum). The data should contain at least the columns
#'   \code{absolute_intensity_background_subtracted}, \code{fraction},
#'   \code{sample_name} and \code{analyte}.
#'   
#' @param data_type "LaCyTools data" or "Skyline data"
#'
#' @return This function returns a tibble with the columns "sample_name",
#'   "analyte", "cluster", "group" (a factor), "sample_type" (a factor),
#'   "sample_id", "plate_well", "sum_intensity" (the total intensity of all
#'   analytes within a spectrum together), "number_of_replicates", "replicates"
#'   and "total_absolute_intensity" (the total intensity of all charge states of
#'   an analyte together).
#' @export
#'
#' @examples
#' # First spectra curation has to be performed:
#' data("example_data")
#'
#' checked_data <- check_analyte_quality_criteria_lacytools(my_data = example_data,
#'                                                min_ppm_deviation = -20,
#'                                                max_ppm_deviation = 20,
#'                                                max_ipq = 0.2,
#'                                                min_sn = 9,
#'                                                criteria_to_consider = c("Mass accuracy",
#'                                                                         "Isotopic pattern quality",
#'                                                                         "S/N"))
#'
#' summarized_checks <- summarize_spectra_checks(checked_data = checked_data)
#'
#' cut_offs_total <- calculate_cut_offs(summarized_checks = summarized_checks,
#'                                      control_sample_types = "PBS",
#'                                      exclude_sample_types = NULL,
#'                                      group_keyword = "Total",
#'                                      percentile = 97,
#'                                      use_mean_SD = FALSE,
#'                                      SD_factor = NULL,
#'                                      uncalibrated_as_NA = TRUE)
#'
#' cut_offs_specific <- calculate_cut_offs(summarized_checks = summarized_checks,
#'                                         control_sample_types = "PBS",
#'                                         exclude_sample_types = NULL,
#'                                         group_keyword = "Spike",
#'                                         percentile = 97,
#'                                         use_mean_SD = FALSE,
#'                                         SD_factor = NULL,
#'                                         uncalibrated_as_NA = TRUE)
#'
#' cut_offs <- dplyr::full_join(cut_offs_total,
#'                              cut_offs_specific)
#'
#' curated_spectra <- curate_spectra(checked_data = checked_data,
#'                                   summarized_checks = summarized_checks,
#'                                   cut_offs = cut_offs)
#'
#' passing_spectra <- kick_out_spectra(curated_spectra = curated_spectra)
#'
#' for_analyte_curation <- remove_unneeded_columns(passing_spectra = passing_spectra)
#'
#' # Then analyte curation is performed:
#' without_samples_to_ignore <- throw_out_samples(
#'    passing_spectra = for_analyte_curation,
#'    samples_to_ignore = c("PBS", "Visucon", "IVIGg", "Total")
#' )
#'
#' checked_analytes <- check_analyte_quality_criteria_lacytools(my_data = without_samples_to_ignore,
#'                                                    min_ppm_deviation = -20,
#'                                                    max_ppm_deviation = 20,
#'                                                    max_ipq = 0.2,
#'                                                    min_sn = 9,
#'                                                    criteria_to_consider = c("Mass accuracy",
#'                                                                             "S/N",
#'                                                                             "Isotopic pattern quality"))
#'
#' curated_analytes <- curate_analytes(checked_analytes = checked_analytes,
#'                                     cut_off_percentage = 25)
#'
#' analyte_curated_data <- dplyr::full_join(curated_analytes,
#'                                          for_analyte_curation) %>%
#'    dplyr::filter(has_passed_analyte_curation) %>%
#'    dplyr::select(-c(has_passed_analyte_curation, passing_percentage))
#'
#' calculate_total_intensity(analyte_curated_data, "LaCyTools_data")
#' 
calculate_total_intensity <- function(data, data_type) {
  
  # Check for missing columns
  if (data_type == "LaCyTools data") {
    required_columns <- c("absolute_intensity_background_subtracted", "fraction", "sample_name", "analyte")
  } else if (data_type == "Skyline data") {
    required_columns <- c("total_area", "sample_name", "analyte")
  }
  
  missing_columns <- required_columns[!(required_columns %in% colnames(data))]
  
  if(!rlang::is_empty(missing_columns)) {
    rlang::abort(class = "missing_columns",
                 message = paste("The required column(s)",
                                 missing_columns,
                                 "are not present in the data."))
  }
  
  # Calculations
  if (data_type == "LaCyTools data") {
    total_intensities <- data %>% 
      dplyr::mutate(intensity_by_fraction = absolute_intensity_background_subtracted / fraction) %>% 
      dplyr::group_by(
        dplyr::across(tidyselect::any_of(
          c("sample_name", "analyte",
            # The remaining grouping variables are only there to ensure they
            # remain in the dataframe after summarize()
            "cluster",
            "group",
            "sample_type",
            "sample_id",
            "plate_well",
            "sum_intensity",
            "number_of_replicates",
            "replicates")
        ))
      ) %>% 
      dplyr::summarize(total_absolute_intensity = sum(intensity_by_fraction, na.rm = TRUE)) %>% 
      dplyr::ungroup()
  } else if (data_type == "Skyline data") {
    # No fraction in case of skyline data
    total_intensities <- data %>% 
      dplyr::group_by(
        dplyr::across(tidyselect::any_of(
          c("sample_name", "analyte", "cluster", "group", "sample_type", 
            "sample_id", "plate_well", "sum_intensity", "number_of_replicates", "replicates")
        ))
      ) %>% 
      dplyr::summarize(total_absolute_intensity = sum(total_area, na.rm = TRUE)) %>% 
      dplyr::ungroup()
  }
  
  return(total_intensities)
}



#' Perform total area normalization
#'
#' This function performs total area normalization per cluster for each
#' spectrum. The total absolute intensity for each analyte per cluster per
#' spectrum (\code{total_absolute_intensity}) is divided by the sum absolute
#' intensity of all analytes in that cluster and spectrum
#' (\code{sum_intensity}). Multiplying this with 100 yields the relative
#' abundance of the analyte in that cluster and spectrum in percent.
#'
#' @param total_intensities The return value of the
#'   \code{\link{calculate_total_intensity}} function.
#'
#' @return The tibble given as the \code{total_intensities} argument, but with
#'   the columns \code{total_absolute_intensity} and \code{sum_intensity}
#'   replaced by the column \code{relative_abundance}.
#' @export
#'
#' @examples
#' # First spectra curation has to be performed:
#' data("example_data")
#'
#' checked_data <- check_analyte_quality_criteria(my_data = example_data,
#'                                                min_ppm_deviation = -20,
#'                                                max_ppm_deviation = 20,
#'                                                max_ipq = 0.2,
#'                                                min_sn = 9,
#'                                                criteria_to_consider = c("Mass accuracy",
#'                                                                         "S/N",
#'                                                                         "IPQ"))
#'
#' summarized_checks <- summarize_spectra_checks(checked_data = checked_data)
#'
#' cut_offs_total <- calculate_cut_offs(summarized_checks = summarized_checks,
#'                                      control_sample_types = "PBS",
#'                                      exclude_sample_types = NULL,
#'                                      group_keyword = "Total",
#'                                      percentile = 97,
#'                                      use_mean_SD = FALSE,
#'                                      SD_factor = NULL,
#'                                      uncalibrated_as_NA = TRUE)
#'
#' cut_offs_specific <- calculate_cut_offs(summarized_checks = summarized_checks,
#'                                         control_sample_types = "PBS",
#'                                         exclude_sample_types = NULL,
#'                                         group_keyword = "Spike",
#'                                         percentile = 97,
#'                                         use_mean_SD = FALSE,
#'                                         SD_factor = NULL,
#'                                         uncalibrated_as_NA = TRUE)
#'
#' cut_offs <- dplyr::full_join(cut_offs_total,
#'                              cut_offs_specific)
#'
#' curated_spectra <- curate_spectra(checked_data = checked_data,
#'                                   summarized_checks = summarized_checks,
#'                                   cut_offs = cut_offs)
#'
#' passing_spectra <- kick_out_spectra(curated_spectra = curated_spectra)
#'
#' for_analyte_curation <- remove_unneeded_columns(passing_spectra = passing_spectra)
#'
#' # Then analyte curation is performed:
#' without_samples_to_ignore <- throw_out_samples(
#'    passing_spectra = for_analyte_curation,
#'    samples_to_ignore = c("PBS", "Visucon", "IVIGg", "Total")
#' )
#'
#' checked_analytes <- check_analyte_quality_criteria(my_data = without_samples_to_ignore,
#'                                                    min_ppm_deviation = -20,
#'                                                    max_ppm_deviation = 20,
#'                                                    max_ipq = 0.2,
#'                                                    min_sn = 9,
#'                                                    criteria_to_consider = c("Mass accuracy",
#'                                                                             "S/N",
#'                                                                             "IPQ"))
#'
#' curated_analytes <- curate_analytes(checked_analytes = checked_analytes,
#'                                     cut_off_percentage = 25)
#'
#' analyte_curated_data <- dplyr::full_join(curated_analytes,
#'                                          for_analyte_curation) %>%
#'    dplyr::filter(has_passed_analyte_curation) %>%
#'    dplyr::select(-c(has_passed_analyte_curation, passing_percentage))
#'
#' # Then we calculate the total intensities for each analyte:
#' total_intensities <- calculate_total_intensity(analyte_curated_data)
#'
#' # And then we can perform total area normalization:
#' normalize_data(total_intensities)
normalize_data <- function(total_intensities) {

  normalized_data <- total_intensities %>%
    dplyr::group_by(cluster,
                    sample_name) %>%
    dplyr::reframe(sum_intensity = sum(total_absolute_intensity),
                     across(everything())) %>%
    dplyr::mutate(relative_abundance = total_absolute_intensity / sum_intensity * 100) %>% 
    dplyr::select(-total_absolute_intensity)

  return(normalized_data)
}





#' sample_heatmap
#' 
#' Creates a simple heatmap for normalized data. 
#' Sample names are on the y-axis, glycans are on the x-axis.
#' Heatmap is created for a specific cluster.
#' 
#'
#' @param normalized_data Normalized data in long format.
#' @param cluster_name Cluster name (character) for which to make the plot
#' @param exclude_sample_types Character vector with sample types to exclude.
#' Empty vector is not applicable.
#' @param group_facet Character: name of column that contains biological groups for facets.
#' Use an empty character ("") when not applicable. 
#' @param color_low Color for lowest value.
#' @param color_mid Color for middle value
#' @param color_high Color for highest value. 
#' @param color_na Color to for the background / missing values.
#'
#' @return A ggplot heatmap.
sample_heatmap <- function(normalized_data,
                           cluster_name, 
                           exclude_sample_types,
                           group_facet,
                           color_low,
                           color_mid,
                           color_high,
                           color_na) {
  
  # Clean data to plot
  to_plot <- normalized_data %>% 
    dplyr::select(-cluster) %>% 
    tidyr::separate(analyte, sep = "1", into = c("cluster", "glycan"),
                    extra = "merge", remove = FALSE) %>% 
    dplyr::filter(
      cluster == cluster_name,
      !sample_type %in% exclude_sample_types
    )
  
  # Check if the data is empty
  if (nrow(to_plot) == 0) {
    return("Oops, you excluded all sample types! There is no data to show...")
  }
  
  # Check if the plot should be facetted by biological group.
  # In that case, remove samples that have no biological group assigned.
  if (group_facet != "") {
    to_plot <- to_plot %>% 
      dplyr::filter(!is.na(!!dplyr::sym(group_facet)))
  }
  
  # Create simple plot
  p <- ggplot2::ggplot(to_plot, ggplot2::aes(
    x = glycan, y = sample_name, fill = relative_abundance,
    text = paste(
      "Sample name:", sample_name,
      "\nSample ID:", sample_id,
      "\nSample type:", sample_type,
      "\nGlycan:", glycan,
      "\nRelative abundance:", paste0(format(round(relative_abundance, digits = 2), nsmall = 2), "%")
    )
  )) +
    ggplot2::geom_tile() +
    ggplot2::labs(x = "", y = "Sample", fill = "Relative abundance (%)") +
    ggplot2::theme_classic() +
    ggplot2::theme(
      panel.border = ggplot2::element_rect(colour = "black", fill = NA, size = 0.5),
      panel.background = ggplot2::element_rect(fill = color_na),
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, size = 11),
      axis.text.y = ggplot2::element_blank()
    ) +
    ggplot2::scale_fill_gradientn(
      colors = c(color_low, color_mid, color_high),
      values = scales::rescale(c(0, 50, 100))
    )
  
  # Check for biological groups facetting
  if (group_facet != "") {
    p <- p + 
      ggplot2::facet_wrap(~get(group_facet), scales = "free_y")  # Need get() because group_facet is a character
  }
  
  return(p)
}




#' cluster_heatmap
#' 
#' Creates a heatmap with glycan on x-axis and cluster on y-axis. The median
#' relative abundance is calculated and shown for each analyte.
#'
#' @param normalized_data Normalized data in long format.
#' @param exclude_sample_types Character vector with sample types to exclude.
#' Empty character vector when not applicable.
#' @param group_facet Character: name of column that contains biological groups for facets.
#' Use an empty character ("") when not applicable. 
#' @param color_low Color of lowest value
#' @param color_mid Color of middle value
#' @param color_high Color of highest value
#' @param color_na Color of background/missing values.
#'
#' @return A heatmap
cluster_heatmap <- function(normalized_data,
                            exclude_sample_types,
                            group_facet,
                            color_low,
                            color_mid,
                            color_high,
                            color_na) {
  
  # Calculate median relative abundances of each analyte
  # Check for faceting per group
  if (group_facet != "") {
    to_plot <- normalized_data %>% 
      dplyr::filter(
        !is.na(!!dplyr::sym(group_facet)),
        !sample_type %in% exclude_sample_types
      ) %>% 
      dplyr::select(-cluster) %>% 
      tidyr::separate(analyte, sep = "1", into = c("cluster", "glycan"),
                      extra = "merge", remove = FALSE) %>% 
      dplyr::group_by(!!dplyr::sym(group_facet), cluster, glycan) %>% 
      dplyr::summarize(
        median_relative_abundance = median(relative_abundance, na.rm = TRUE)
      )
  } else {
    to_plot <- normalized_data %>% 
      dplyr::filter(!sample_type %in% exclude_sample_types) %>% 
      dplyr::select(-cluster) %>% 
      tidyr::separate(analyte, sep = "1", into = c("cluster", "glycan"),
                      extra = "merge", remove = FALSE) %>% 
      dplyr::group_by(cluster, glycan) %>% 
      dplyr::summarize(
        median_relative_abundance = median(relative_abundance, na.rm = TRUE)
      )
  }
  
  # Check if the data is empty
  if (nrow(to_plot) == 0) {
    return("Oops, you excluded all sample types! There is no data to show...")
  }
  
  # Simple plot
  p <- ggplot2::ggplot(to_plot, ggplot2::aes(
    x = glycan, y = cluster, fill = median_relative_abundance,
    text = paste(
      "Glycan:", glycan,
      "\nCluster", cluster,
      "\nMedian relative abundance:", paste0(format(round(median_relative_abundance, digits = 2), nsmall = 2), "%")
    )
  )) +
    ggplot2::geom_tile() +
    ggplot2::labs(x = "", y = "", fill = "Median relative abundance (%)") +
    ggplot2::theme_classic() +
    ggplot2::theme(
      panel.border = ggplot2::element_rect(color = "black", fill = NA, size = 0.5),
      panel.background = ggplot2::element_rect(fill = color_na),
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, size = 11),
      axis.text.y = ggplot2::element_text(size = 11)
    ) +
    ggplot2::scale_fill_gradientn(
      colors = c(color_low, color_mid, color_high),
      values = scales::rescale(c(0, 50, 100))
    )
    
  # Check for biological groups faceting
  if (group_facet != "") {
    p <- p + 
      ggplot2::facet_wrap(~get(group_facet))
  }
  
  
  return(p)
}
