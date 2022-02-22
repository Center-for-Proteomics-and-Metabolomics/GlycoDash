#' Calculate the total intensity of an analyte in a spectrum
#'
#' The function \code{calculate_total_intensity} calculates the total intensity
#' of an analyte in a spectrum, by combining the intensities from each charge
#' state. For each charge state, the intensity is multiplied by the fraction.
#' Then these multiplied intensities are summed up to get the total intensity of
#' the analyte.
#'
#' @param data The data in long format (one row for each analyte + charge
#'   combination per spectrum). The data should contain at least the columns
#'   \code{absolute_intensity_background_subtracted}, \code{fraction},
#'   \code{sample_name} and \code{analyte}.
#'
#' @return This function returns the dataframe given as \code{data} with the
#'   column "absolute_intensity_background_subtracted" replaced by the column
#'   "total_absolute_intensity". The dataframe contains one row per analyte per
#'   sample.
#' @export
#'
#' @examples
#' data("long_data")
#' calculate_total_intensity(long_data)
calculate_total_intensity <- function(data) {
  
  total_intensities <- data %>% 
    dplyr::mutate(intensity_times_fraction = absolute_intensity_background_subtracted * fraction) %>% 
    dplyr::group_by(sample_name, 
                    analyte) %>% 
    dplyr::summarize(total_absolute_intensity = sum(intensity_times_fraction))
  
  data <- dplyr::full_join(data, 
                           total_intensities) %>% 
    dplyr::ungroup() %>% 
    dplyr::distinct(sample_name, 
                    analyte, 
                    .keep_all = TRUE) %>% 
    # remove any columns that were charge-state specific:
    dplyr::select(-any_of(c("charge", 
                            "passing_percentage", 
                            "absolute_intensity_background_subtracted",
                            "mass_accuracy_ppm",
                            "isotopic_pattern_quality",
                            "sn",
                            "fraction",
                            "exact_mass",
                            "criteria_check")))
  
  return(data)
}

#' Perform total area normalization
#'
#' This function performs total area normalization per cluster for each
#' spectrum. First, the sum of the total absolute intensities is calculated per
#' cluster in each spectrum. Then the total absolute intensity for each analyte
#' within that cluster and spectrum is divided by that sum intensity. This
#' yields the relative abundance of the analyte in that spectrum.
#'
#' @param data The dataframe resulting from the
#'   \code{\link{calculate_total_intensity}} function.
#'
#' @return The dataframe given as the \code{data} argument, but with the column
#'   "relative_abundance" instead of "total_absolute_intensity".
#' @export
#'
#' @examples
#' data("long_data")
#' 
#' # First spectra curation is performed:
#' long_data <- curate_spectra(data = long_data,
#'                             min_ppm_deviation = -20,
#'                             max_ppm_deviation = 20,
#'                             max_ipq = 0.2,
#'                             min_sn = 9,
#'                             clusters_regex = "IgGI1",
#'                             group_to_filter = "Spike",
#'                             sample_type_to_filter = "CN")
#'
#' # Only the spectra that passed curation are kept:
#' curated_spectra <- long_data %>%
#'    dplyr::filter(passed_curation == TRUE)
#'
#' # Then analyte curation is performed:
#' curate_analytes(data = curated_spectra,
#'                 group_to_ignore = "Total",
#'                 sample_types_to_ignore = c("pool", 
#'                                            "IVIGg", 
#'                                            "CN", 
#'                                            "Visucon", 
#'                                            "PBS"),
#'                 cut_off_percentage = 25)
#' 
#' # Only the analytes that passed curation are kept:
#' passing_analytes <- curated_analytes %>% 
#'    dplyr::filter(passed_curation == TRUE) %>% 
#'    dplyr::select(-passed_curation)
#' analyte_curated_data <- dplyr::left_join(passing_analytes, curated_spectra)
#' 
#' # Then the total absolute intensities are calculated:
#' final_data <- calculate_total_intensity(data = analyte_curated_data)
#' 
#' # Finally, total area normalization is performed:
#' normalized_data <- normalize_data(data = final_data)
#' 
normalize_data <- function(data) {
  
  normalized_data <- data %>% 
    dplyr::group_by(cluster, 
                    sample_name) %>% 
    dplyr::summarise(sum_intensity = sum(total_absolute_intensity),
                     across()) %>% 
    dplyr::mutate(relative_abundance = total_absolute_intensity / sum_intensity,
                  .keep = "unused")
  
}