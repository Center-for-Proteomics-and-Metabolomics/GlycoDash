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
#' calculate_total_intensity(analyte_curated_data)
#' 
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
    dplyr::group_by(
      dplyr::across(tidyselect::any_of(
        c("sample_name", 
          "analyte",
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

    dplyr::summarize(total_absolute_intensity = sum(intensity_by_fraction,
                                                    na.rm = TRUE # check if this is needed?
                                                    )) %>%
    dplyr::ungroup()
  
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
    dplyr::mutate(relative_abundance = total_absolute_intensity / sum_intensity * 100,
                  .keep = "unused")

  return(normalized_data)
}
