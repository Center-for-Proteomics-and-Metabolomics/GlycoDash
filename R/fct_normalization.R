#' Calculate the total intensity of an analyte in a spectrum
#'
#' The function \code{calculate_total_intensity} calculates the total intensity
#' of an analyte in a spectrum, by combining the intensities from each charge
#' state. For each charge state, the intensity is divided by the fraction.
#' Then these adjusted intensities are summed up to get the total intensity of
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
#' data("example_data")
#' calculate_total_intensity(example_data)
calculate_total_intensity <- function(data) {
  
  required_columns <- c("absolute_intensity_background_subtracted",
                        "fraction",
                        "sample_name", 
                        "analyte")
  
  missing_columns <- required_columns[!(required_columns %in% colnames(data))]
  
  if(!rlang::is_empty(missing_columns)) {
    rlang::abort(class = "missing_columns",
                 message = paste("The required column(s)",
                                 missing_columns,
                                 "are not present in the data."))
  }
  
  total_intensities <- data %>% 
    dplyr::mutate(intensity_by_fraction = absolute_intensity_background_subtracted / fraction) %>% 
    # For each sample + analyte combination, the intensities for all charge
    # states should be added together:
    dplyr::group_by(sample_name, 
                    analyte) %>% 
    dplyr::summarize(total_absolute_intensity = sum(intensity_by_fraction,
                                                    na.rm = TRUE))
  
  data <- dplyr::full_join(data, 
                           total_intensities,
                           by = c("sample_name", 
                                  "analyte")) %>% 
    # Now there is still multiple rows per sample + analyte combination (one
    # for each charge state). We only want to keep one row for each sample +
    # analyte combination:
    dplyr::ungroup() %>% 
    dplyr::distinct(sample_name, 
                    analyte, 
                    # Keep all columns:
                    .keep_all = TRUE) %>% 
    # Remove any columns that were charge-state specific:
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
#' data("example_data")
#' 
#' # First spectra curation is performed:
#' example_data <- curate_spectra(data = example_data,
#'                                min_ppm_deviation = -20,
#'                                max_ppm_deviation = 20,
#'                                max_ipq = 0.2,
#'                                min_sn = 9,
#'                                clusters_regex = "IgGI1",
#'                                cut_off_basis = c("Spike PBS", "Total PBS"))
#'
#' # Only the spectra that passed curation are kept:
#' curated_spectra <- example_data$curated_data %>%
#'    dplyr::filter(has_passed_spectra_curation == TRUE) %>% 
#'    dplyr::select(-has_passed_spectra_curation)
#'
#' # Then analyte curation is performed:
#' curated_analytes <- curate_analytes(data = curated_spectra,
#'                                     group_to_ignore = "Total",
#'                                     sample_types_to_ignore = c("Visucon", 
#'                                                                "PBS"),
#'                                     cut_off_percentage = 25)
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
