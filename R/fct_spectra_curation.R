# This file contains all functions that are used within the module
# mod_spectra_curation.R and within its sub-modules mod_curate_based_on_controls.R,
# mod_curate_based_on_percentiles.R, mod_tab_cut_offs.R and mod_tab_curated_spectra_plot.R.


#' Perform an analyte quality criteria check for every spectrum in LaCyTools data.
#'
#' \code{check_analyte_quality_criteria_lacytools()} performs an analyte quality criteria check for
#' every spectrum in the data.
#'
#' @param my_data A dataframe in long format (one row for each analyte + sample
#'   + charge combination).
#' @param min_ppm_deviation The lowest allowed value for the mass accuracy (in
#'   ppm).
#' @param max_ppm_deviation The highest allowed value for the mass accuracy (in
#'   ppm).
#' @param max_ipq The highest allowed value for the Isotopic Pattern Quality
#'   (IPQ). The IPQ indicates how much the isotopic pattern deviates from the
#'   theoretic isotopic pattern.
#' @param min_sn The lowest allowed value for the signal to noise ratio (S/N).
#' @param criteria_to_consider A character vector that indicates which quality
#'  criteria should be taken into account during spectra curation. The analyte 
#'  quality criteria that can be included are "Mass accuracy", "S/N" and "IPQ".
#'
#' @return The original dataframe given as the my_data argument, but with 3
#'   additional columns: \describe{\item{analyte_meets_criteria}{A Boolean:
#'   \code{TRUE} indicates that the analyte + charge combination in that sample fulfilled
#'   the quality criteria in criteria_to_consider, whereas \code{FALSE} indicates that 
#'   one or more criteria were not fulfilled.}\item{uncalibrated}{A Boolean:
#'   \code{TRUE} if the combination of cluster and sample failed calibration in LaCyTools}
#'   \item{failed_criteria}{A character string describing which criteria in 
#'   criteria_to_consider were not fulfilled. If all criteria were fulfilled 
#'   failed_criteria is \code{NA}}}
#' @export
#'
#' @examples
#' data(example_data)
#' 
#' check_analyte_quality_criteria(my_data = example_data,
#'                                min_ppm_deviation = -20,
#'                                max_ppm_deviation = 20,
#'                                max_ipq = 0.2,
#'                                min_sn = 9,
#'                                criteria_to_consider = c("Mass accuracy",
#'                                                         "Isotopic pattern quality",
#'                                                         "S/N"))
check_analyte_quality_criteria_lacytools <- function(my_data, 
                                                    min_ppm_deviation, 
                                                    max_ppm_deviation, 
                                                    max_ipq, 
                                                    min_sn,
                                                    criteria_to_consider) {
  
  data_checked <- my_data %>% 
    dplyr::group_by(sample_name,  cluster) %>% 
    dplyr::mutate(uncalibrated = all(
      all(is.na(absolute_intensity_background_subtracted)),
      all(is.na(mass_accuracy_ppm)),
      all(is.na(isotopic_pattern_quality)),
      all(is.na(sn))
    )) %>% 
    dplyr::ungroup() %>% 
    check_criteria_lacytools(., 
                            min_ppm_deviation,
                            max_ppm_deviation,
                            max_ipq,
                            min_sn) %>% 
    apply_chosen_criteria(.,
                          criteria_to_consider) %>%
    report_failed_criteria(.,
                           criteria_to_consider,
                           "LaCyTools")
  
  return(data_checked)
}



#' Perform an analyte quality criteria check for every spectrum in Skyline data.
#'
#' \code{check_analyte_quality_criteria_skyline()} performs an analyte quality criteria check for
#' every spectrum in the data.
#'
#' @param my_data A dataframe in long format (one row for each analyte + sample
#'   + charge combination).
#' @param min_ppm_deviation The lowest allowed value for the mass accuracy (in
#'   ppm).
#' @param max_ppm_deviation The highest allowed value for the mass accuracy (in
#'   ppm).
#' @param min_idp The lowest allowed value for the Isotope Dot Product
#'   (IDP). The IDP indicates how much the isotopic pattern deviates from the
#'   theoretic isotopic pattern.
#' @param min_total_area The lowest allowed value for the total analyte area (per charge state).
#' @param criteria_to_consider A character vector that indicates which quality
#'  criteria should be taken into account during spectra curation.
#'
#' @return The original dataframe given as the my_data argument, but with 3
#'   additional columns: \describe{\item{analyte_meets_criteria}{A Boolean:
#'   \code{TRUE} indicates that the analyte + charge combination in that sample fulfilled
#'   the quality criteria in criteria_to_consider, whereas \code{FALSE} indicates that 
#'   one or more criteria were not fulfilled.}\item{uncalibrated}{A Boolean:
#'   \code{TRUE} if the combination of cluster and sample failed calibration in LaCyTools}
#'   \item{failed_criteria}{A character string describing which criteria in 
#'   criteria_to_consider were not fulfilled. If all criteria were fulfilled 
#'   failed_criteria is \code{NA}}}
#' @export
#'
#' @examples
#' data(example_data)
#' 
#' check_analyte_quality_criteria(my_data = example_data,
#'                                min_ppm_deviation = -20,
#'                                max_ppm_deviation = 20,
#'                                min_idp = 0.9,
#'                                min_total_area = 0,
#'                                criteria_to_consider = c("Mass accuracy",
#'                                                         "Isotope dot product",
#'                                                         "Total area"))
check_analyte_quality_criteria_skyline <- function(my_data, 
                                                  min_ppm_deviation, 
                                                  max_ppm_deviation, 
                                                  min_idp, 
                                                  min_total_area,
                                                  criteria_to_consider) {
  
  data_checked <- my_data %>% 
    dplyr::group_by(sample_name,  cluster) %>%
    dplyr::mutate(uncalibrated = all(
      all(is.na(total_area)),
      all(is.na(mass_accuracy_ppm)),
      all(is.na(isotope_dot_product))
    )) %>%
    dplyr::ungroup() %>%
    check_criteria_skyline(.,
                          min_ppm_deviation,
                          max_ppm_deviation,
                          min_idp,
                          min_total_area) %>%
    apply_chosen_criteria(.,
                          criteria_to_consider) %>%
    report_failed_criteria(.,
                           criteria_to_consider,
                           "Skyline")

  return(data_checked)
}



#' Check all analyte quality criteria for LaCyTools data.
#'
#' \code{check_criteria_lacytools()} checks for each analyte quality criterium
#' separately if it is fulfilled, and is used in the functions
#' \code{\link{check_analyte_quality_criteria_lacytools}} and #' \code{\link{check_analyte_quality_criteria_skyline}}.
#'
#' @return The original dataframe, but with an extra column for each quality
#'   criterium that is \code{TRUE} or \code{FALSE}.
#' @export
#'
#' @examples
#' data(example_data)
#' 
#' check_each_criterium(my_data = example_data,
#'                      min_ppm_deviation = -20,
#'                      max_ppm_deviation = 20,
#'                      max_ipq = 0.2,
#'                      min_sn = 9)
#' 
check_criteria_lacytools <- function(my_data, 
                                    min_ppm_deviation,
                                    max_ppm_deviation,
                                    max_ipq,
                                    min_sn) {
  my_data %>% 
    dplyr::mutate(`Mass accuracy` = dplyr::between(mass_accuracy_ppm, 
                                                   min_ppm_deviation, 
                                                   max_ppm_deviation),
                  `Isotopic pattern quality` = isotopic_pattern_quality <= max_ipq,
                  `S/N` = sn >= min_sn,
                  dplyr::across(c(`Mass accuracy`, `Isotopic pattern quality`, `S/N`),
                                ~ tidyr::replace_na(.x, FALSE))
    )
}



#' Check all analyte quality criteria for LaCyTools data.
#'
#' \code{check_criteria_lacytools()} checks for each analyte quality criterium
#' separately if it is fulfilled, and is used in the functions
#' \code{\link{check_analyte_quality_criteria_lacytools}} and #' \code{\link{check_analyte_quality_criteria_skyline}}.
#'
#' @return The original dataframe, but with an extra column for each quality
#'   criterium that is \code{TRUE} or \code{FALSE}.
#' @export
#'
#' @examples
#' data(example_data)
#' 
#' check_each_criterium(my_data = example_data,
#'                      min_ppm_deviation = -20,
#'                      max_ppm_deviation = 20,
#'                      max_ipq = 0.2,
#'                      min_sn = 9)
#' 
check_criteria_skyline <- function(my_data, 
                                  min_ppm_deviation,
                                  max_ppm_deviation,
                                  min_idp,
                                  min_total_area) {
  my_data %>% 
    dplyr::mutate(`Mass accuracy` = dplyr::between(mass_accuracy_ppm, 
                                                   min_ppm_deviation, 
                                                   max_ppm_deviation),
                  `Isotope dot product` = isotope_dot_product >= min_idp,
                  `Total area` = total_area > min_total_area,
                  dplyr::across(c(`Mass accuracy`, `Isotope dot product`, `Total area`),
                                ~ tidyr::replace_na(.x, FALSE))
    )
}




#' Apply chosen analyte quality criteria
#'
#' \code{apply_chosen_criteria()} determines if the analyte quality criteria in
#' criteria to consider were all fulfilled and is used inside the function
#' \code{\link{check_analyte_quality_criteria}}.
#'
#' @param my_data The return value of the function
#'   \code{\link{check_each_criterium}}.
#'
#' @return The original dataframe my_data with an additional column
#'   \code{analyte_meets_criteria} that is \code{TRUE} if the analyte fulfills
#'   all criteria in \code{criteria_to_consider} and otherwise is \code{FALSE}.
#'
#' @export
#'
#' @examples
#' data(example_data)
#' 
#' checked <- check_criteria_lacytools(my_data = example_data,
#'                      min_ppm_deviation = -20,
#'                      max_ppm_deviation = 20,
#'                      max_ipq = 0.2,
#'                      min_sn = 9)
#' 
#' apply_chosen_criteria(my_data = checked,
#'                       criteria_to_consider = c("Mass accuracy",
#'                                                "S/N",
#'                                                "IPQ"))
#' 
apply_chosen_criteria <- function(my_data,
                                  criteria_to_consider) {
  to_return <- my_data 
  
  if (length(criteria_to_consider) == 0) {
    to_return$analyte_meets_criteria <- TRUE
  } else {
    qc_cols <- to_return[, criteria_to_consider, drop = FALSE]
    to_return$analyte_meets_criteria <- rowSums(qc_cols) == length(criteria_to_consider)
  }
  
  return(to_return)
}



#' Report which analyte quality criteria were not met
#'
#' @param my_data The return value of the function
#'   \code{\link{apply_chosen_criteria}}.
#'   
#'@param data_type "LaCyTools" or "Skyline", determined by file input.
#'
#' @return The original dataframe my_data with an additional column
#'   \code{failed_criteria} that describes which criteria from criteria_to_consider
#'   were not fulfilled. If all criteria were met the value will be \code{NA}.
#' @export
#'
#' @examples
#' data(example_data)
#' 
#' checked <- check_criteria_lacytools(my_data = example_data,
#'                      min_ppm_deviation = -20,
#'                      max_ppm_deviation = 20,
#'                      max_ipq = 0.2,
#'                      min_sn = 9)
#' 
#' applied <- apply_chosen_criteria_lacytools(my_data = checked,
#'                                           criteria_to_consider = c("Mass accuracy",
#'                                                                    "S/N",
#'                                                                    "IPQ"))
#' 
#' report_failed_criteria(my_data = applied,
#'                        criteria_to_consider = c("Mass accuracy",
#'                                                "S/N",
#'                                                "IPQ"),
#'                        data_type = "LaCyTools")
report_failed_criteria <- function(my_data,
                                   criteria_to_consider,
                                   data_type) {
  
  failed_criteria_dataframe <- my_data %>% 
    # Only criteria in criteria_to_consider will be reported as failed criteria:
    tidyr::pivot_longer(cols = tidyselect::all_of(criteria_to_consider),
                        names_to = "criterium",
                        values_to = "passed") %>% 
    dplyr::filter(!passed) %>% 
    dplyr::group_by(sample_name, analyte, charge) %>% 
    dplyr::summarize(failed_criteria = comma_and(criterium))
  
  if (data_type == "LaCyTools") {
    to_return <- dplyr::full_join(my_data, failed_criteria_dataframe) %>% 
      dplyr::select(-c(`Mass accuracy`, `Isotopic pattern quality`, `S/N`))
  } else if (data_type == "Skyline") {
    to_return <- dplyr::full_join(my_data, failed_criteria_dataframe) %>% 
      dplyr::select(-c(`Mass accuracy`, `Isotope dot product`, `Total area`))
  }
  
  return(to_return)
}

#' Summarize analyte quality criteria checks
#'
#' \code{summarize_spectra_checks()} calculates the percentage of passing
#' analytes per spectrum and the sum intensity of passing analytes per spectrum.
#' \code{summarize_spectra_checks()} should be used after \code{\link{check_analyte_quality_criteria_lacytools}} or 
#' \code{\link{check_analyte_quality_criteria_skyline}} has been used to perform analyte quality
#' criteria checks for every spectrum and analyte combination in the data.
#'
#' @param checked_data The dataframe that is returned by
#'   \code{\link{check_analyte_quality_criteria_lacytools}} or \code{\link{check_analyte_quality_criteria_skyline}}.
#'
#'@param data_type "LaCyTools data" or "Skyline data", determined by file input.
#'
#' @return A dataframe that contains one row per spectrum for each cluster (
#'   Thus the number of rows is equal to the number of spectra times the number
#'   of clusters). The dataframe contains five columns: \describe{
#'   \item{sample_name}{The name of the sample for which this spectrum was
#'   recorded.} \item{sample_type}{The type of sample (e.g. negative control,
#'   blank etc.).} \item{group}{The group (Total or Specific) that this sample
#'   belongs to.} \item{cluster}{The cluster for which the metrics were calculated.}
#'   \item{passing_analyte_percentage}{The percentage of analytes that passed the
#'   criteria checks in this spectrum.} \item{sum_intensity}{The sum intensity
#'   of all passing analytes in this spectrum} }
#' @export
#'
#' @examples
#' data("example_data")
#'
#' checked_data <- check_analyte_quality_criteria(my_data = example_data,
#'                                   min_ppm_deviation = -20,
#'                                   max_ppm_deviation = 20,
#'                                   max_ipq = 0.2,
#'                                   min_sn = 9,
#'                                   criteria_to_consider = c("Mass accuracy",
#'                                                           "S/N",
#'                                                           "IPQ"))
#'
#' summarize_spectra_checks(checked_data = checked_data, data_type = results_data_import$data_type())
summarize_spectra_checks <- function(checked_data, data_type) {
  
  grouping_variables <- c("sample_name", "group", "sample_type", "cluster", "sample_id")
  
  if (data_type == "LaCyTools data") {
    summarized_checks <- checked_data %>% 
      dplyr::mutate(intensity_divided_by_fraction = absolute_intensity_background_subtracted / fraction) %>% 
      dplyr::group_by(dplyr::across(tidyselect::any_of(grouping_variables))) %>% 
      dplyr::summarise(
        passing_analyte_percentage = sum(analyte_meets_criteria) / dplyr::n() * 100,
        sum_intensity = sum(
          intensity_divided_by_fraction[analyte_meets_criteria == TRUE], na.rm = TRUE
        ),
        uncalibrated = unique(uncalibrated)
      ) %>% 
      dplyr::ungroup(.)
  } else if (data_type == "Skyline data") {
    # Don't have fraction, so just use total area
    summarized_checks <- checked_data %>% 
      dplyr::group_by(dplyr::across(tidyselect::any_of(grouping_variables))) %>% 
      dplyr::summarise(
        passing_analyte_percentage = sum(analyte_meets_criteria) / dplyr::n() * 100,
        sum_intensity = sum(total_area[analyte_meets_criteria == TRUE], na.rm = TRUE),
        uncalibrated = unique(uncalibrated)
      ) %>% 
      dplyr::ungroup(.)
  }
  
  return(summarized_checks)
}



#' Calculate cut-off values
#'
#' \code{calculate_cut_offs()} calculates the cut-off values for the sum
#' intensity and percentage of passing analytes during spectra curation.
#'
#' @param summarized_checks The return value of the function
#'   \code{\link{summarize_spectra_checks}}.
#' @param control_sample_types The sample type(s) that should be used as
#'   negative control samples to base the cut-offs on. This argument is only
#'   relevant if the method "Based on negative controls" is chosen, otherwise
#'   this argument should be NULL (default).
#' @param exclude_sample_types The sample type(s) that should not be taken into
#'   account in the calculation of the cut-off values. This argument is only
#'   relevant when the method "Based on percentiles" is chosen, otherwise this
#'   argument should be NULL (default).
#' @param group_keyword If the data contains total and specific Ig samples, the
#'   group_keyword indicates for which group (total or specific) cut-offs are
#'   being calculated. The cut-offs need to be calculated separately because the
#'   control_sample_types can differ between total and specific.
#' @param percentile When control_sample_types is not NULL, this is the
#'   percentile of the negative control samples' sum intensity and percentage of
#'   passing analytes that will be used as cut-off values. When
#'   control_sample_types is NULL, this is the percentile of all samples (except
#'   exclude_sample_types)' sum intensity and percentage of passing analytes
#'   that will be used as cut-off values.
#' @param use_mean_SD TRUE or FALSE, if TRUE then the mean and standard
#'   deviation (SD) instead of a percentile will be used to calculate the
#'   cut-off value for the sum intensity. The cut-off for the percentage of
#'   passing analytes will still be calculated with a percentile.
#' @param SD_factor Only relevant when use_mean_SD is TRUE. The cut-off value
#'   will be calculated as mean + SD_factor * SD.
#' @param uncalibrated_as_NA Should uncalibrated spectra be regarded as missing
#'   values (TRUE) or as zeroes (FALSE)?
#'
#' @return A tibble with five or six columns: \describe{\item{group}{This column
#'   will only exist if the data contained both total and specific samples,
#'   because in that case the cut-offs are calculated separately for total and
#'   specific.} \item{cluster}{This column indicates for which cluster the
#'   cut-off was calculated.} \item{cut_off_sum_intensity}{This numeric column
#'   gives the cut-off value for the sum intensity.}
#'   \item{cut_off_passing_analyte_percentage}{This column gives the cut-off
#'   value for the percentage of passing analytes.} \item{curation_method}{This
#'   character column can be either "based_on_percentiles" or
#'   "based_on_negative_controls". It indicates which method for spectra
#'   curation was used.} \item{sample_type_list}{This column contains a list
#'   with the sample types that were used as a basis for the analyte curation.}}
#' @export
#'
#' @examples
#' data("example_data")
#'
#' checked_data <- check_analyte_quality_criteria_lacytools(my_data = example_data,
#'                                                min_ppm_deviation = -20,
#'                                                max_ppm_deviation = 20,
#'                                                max_ipq = 0.2,
#'                                                min_sn = 9,
#'                                                criteria_to_consider = c("Mass accuracy",
#'                                                                         "S/N",
#'                                                                         "IPQ"))
#'
#' summarized_checks <- summarize_spectra_checks(checked_data = checked_data, data_type == "LaCyTools")
#'
#' # In this example we calculate cut-offs for total Ig samples, using PBS
#' # samples as negative controls. Both cut-offs are set at the 97th percentile
#' # of the PBS samples' distributions of the sum intensity and percentage of
#' # passing analytes. Uncalibrated spectra are not included in the cut-off
#' # calculations:
#' calculate_cut_offs(summarized_checks = summarized_checks,
#'                    control_sample_types = "PBS",
#'                    exclude_sample_types = NULL,
#'                    group_keyword = "Total",
#'                    percentile = 97,
#'                    use_mean_SD = FALSE,
#'                    SD_factor = NULL,
#'                    uncalibrated_as_NA = TRUE)
#'                                                                                      
calculate_cut_offs <- function(summarized_checks,
                               control_sample_types = NULL,
                               exclude_sample_types = NULL,
                               group_keyword = NULL,
                               percentile,
                               use_mean_SD = FALSE,
                               SD_factor = NULL,
                               uncalibrated_as_NA = FALSE) {
  #TODO: rewrite this function so that one of the arguments is
  #spectra_curation_method and it can also be applied when spectra curation
  #should be skipped.
  
  cut_off_basis <- summarized_checks %>% 
    dplyr::filter(if (!is.null(group_keyword)) group == group_keyword else TRUE) %>% 
    dplyr::filter(if (!is.null(control_sample_types)) sample_type %in% control_sample_types else TRUE) %>% 
    dplyr::filter(if (!is.null(exclude_sample_types)) !(sample_type %in% exclude_sample_types) else TRUE) %>% 
    dplyr::filter(if (uncalibrated_as_NA) !uncalibrated else TRUE)
  
  grouping_variables <- c("group", "cluster")
  
  cut_offs <- cut_off_basis %>%  
    dplyr::group_by(dplyr::across(tidyselect::any_of(grouping_variables))) %>% 
    dplyr::reframe(
      cut_off_sum_intensity = if (use_mean_SD) { 
        mean_plus_SD(sum_intensity, SD_factor, uncalibrated_as_NA) } else { 
          percentile_cutoff(
            data = sum_intensity,
            percentage_to_exclude = percentile,
            remove_NA = uncalibrated_as_NA
          )},
      cut_off_passing_analyte_percentage = percentile_cutoff(
        data = passing_analyte_percentage,
        percentage_to_exclude = percentile,
        remove_NA = uncalibrated_as_NA
      ),
      sample_type = unique(sample_type),
      curation_method = ifelse(is.null(control_sample_types), 
                               "based_on_percentiles",
                               "based_on_negative_controls")) %>% 
    tidyr::nest(., "sample_type_list" = sample_type)
    
  return(cut_offs)
}

mean_plus_SD <- function(x, SD_factor, na.rm) {
  mean_x <- mean(x, na.rm = na.rm)
  mean_x + SD_factor * sd(x, na.rm = na.rm)
}

#' Perform spectra curation
#'
#' \code{curate_spectra()} performs spectra curation on mass spectrometry data.
#' This function should be used after the functions
#' \code{\link{check_analyte_quality_criteria}} and
#' \code{\link{summarize_spectra_checks}} have been used to calculate the sum
#' intensities and percentages of passing analytes for each spectrum and after
#' cut-off values have been calculated using \code{\link{calculate_cut_offs}}.
#' All spectra with sum intensities and percentages of passing analytes above
#' the cut-off values pass spectra curation.
#'
#' @param checked_data The return value of the function
#'   \code{\link{check_analyte_quality_criteria}}.
#' @inheritParams calculate_cut_offs
#' @param cut_offs The return value of the function
#'   \code{\link{calculate_cut_offs}}.
#'
#' @return The function returns the three dataframes given as function arguments
#'   joined together, with two additional columns:
#'   \describe{\item{has_passed_spectra_curation}{This column is \code{TRUE} if
#'   the spectrum passed curation and \code{FALSE} if it did not.}
#'   \item{reason_for_failure}{This column describes why the spectrum failed
#'   curation. The possible reasons are "Calibration failed.", "Percentage of
#'   passing analytes and sum intensity below cut-offs.", "Percentage of passing
#'   analytes below cut-off." or "Sum intensity below cut-off.". If the spectrum
#'   passed curation, reason_for_failure is \code{NA}.}}
#' @export
#'
#' @examples
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
#' cut_offs <- calculate_cut_offs(summarized_checks = summarized_checks,
#'                                control_sample_types = "PBS",
#'                                exclude_sample_types = NULL,
#'                                group_keyword = "Total",
#'                                percentile = 97,
#'                                use_mean_SD = FALSE,
#'                                SD_factor = NULL,
#'                                uncalibrated_as_NA = TRUE)
#'
#' curate_spectra(checked_data = checked_data,
#'                summarized_checks = summarized_checks,
#'                cut_offs = cut_offs)
#' 
curate_spectra <- function(checked_data, summarized_checks, cut_offs) {
  
  summarized_checks_with_cut_offs <- dplyr::left_join(summarized_checks, 
                                                      cut_offs) %>% 
    dplyr::ungroup(.)
  
  curated_spectra <- summarized_checks_with_cut_offs %>% 
    # Can't use all() instead of & because all() is not vectorized
    dplyr::mutate(has_passed_spectra_curation = passing_analyte_percentage >= cut_off_passing_analyte_percentage &
                    sum_intensity >= cut_off_sum_intensity,
                  has_passed_spectra_curation = ifelse(uncalibrated, 
                                                       FALSE,
                                                       has_passed_spectra_curation)) %>% 
    determine_reason_for_failure()
  
  curated_data <- dplyr::full_join(curated_spectra, 
                                   checked_data) %>% 
    dplyr::relocate(c(has_passed_spectra_curation, reason_for_failure), 
                    .after = sample_name) %>% 
    dplyr::relocate(c(analyte_meets_criteria,
                      failed_criteria),
                    .after = charge) %>% 
    # any_of() because if sample_list was used to add sample ID's the plate_well
    # column doesn't exist:
    dplyr::relocate(tidyselect::any_of(c("sample_id", "plate_well")),
                    .after = sample_name)
  
  without_uncalibrated <- curated_data %>% 
    dplyr::filter(!uncalibrated)
  
  if (all(!without_uncalibrated$has_passed_spectra_curation)) {
    rlang::warn("None of the spectra passed curation.")
  } else if (all(without_uncalibrated$has_passed_spectra_curation)) {
    rlang::warn("All spectra passed curation.")
  }
  
  return(curated_data)
}


determine_reason_for_failure <- function(data) {
  with_reasons <- data %>% 
    dplyr::mutate(reason_for_failure = dplyr::case_when(
      uncalibrated ~ "Calibration failed.",
      passing_analyte_percentage <= cut_off_passing_analyte_percentage & sum_intensity <= cut_off_sum_intensity ~ "Percentage of passing analytes and sum intensity below cut-offs.",
      passing_analyte_percentage <= cut_off_passing_analyte_percentage ~ "Percentage of passing analytes below cut-off.",
      sum_intensity <= cut_off_sum_intensity ~ "Sum intensity below cut-off.",
      TRUE ~ as.character(NA) # as.character(), because case_When requires that 
      # all possible values are of the same data type
    ))
  
  return(with_reasons)
}


#' Filter out spectra that failed spectra curation
#'
#' This function filters out spectra that failed spectra curation and spectra
#' that are uncalibrated.
#'
#' @param curated_data The return value of \code{\link{curate_spectra}}.
#'
#' @return The filtered dataframe given as \code{curated_data}.
#' @export
#'
#' @examples
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
#' cut_offs <- calculate_cut_offs(summarized_checks = summarized_checks,
#'                                control_sample_types = "PBS",
#'                                exclude_sample_types = NULL,
#'                                group_keyword = "Total",
#'                                percentile = 97,
#'                                use_mean_SD = FALSE,
#'                                SD_factor = NULL,
#'                                uncalibrated_as_NA = TRUE)
#'
#' curated_spectra <- curate_spectra(checked_data = checked_data,
#'                                   summarized_checks = summarized_checks,
#'                                   cut_offs = cut_offs)
#'                                   
#' kick_out_spectra(curated_spectra)
kick_out_spectra <- function(curated_spectra) {
  
  passing_spectra <- curated_spectra %>% 
    dplyr::filter(has_passed_spectra_curation,
                  !uncalibrated)
  
  return(passing_spectra)
}



#' Remove columns no longer needed after spectra curation
#'
#' The \code{sum_intensity} column is the only column from spectra curation that
#' is kept, because it is needed during normalization.
#'
#' @param passing_spectra The return value of \code{\link{kick_out_spectra}}.
#'
#' @return The same dataframe given as \code{passing_spectra}, but without the
#'   columns created during the spectra curation process (except for the
#'   sum_intensity column).
#' @export
#'
#' @examples
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
#' cut_offs <- calculate_cut_offs(summarized_checks = summarized_checks,
#'                                control_sample_types = "PBS",
#'                                exclude_sample_types = NULL,
#'                                group_keyword = "Total",
#'                                percentile = 97,
#'                                use_mean_SD = FALSE,
#'                                SD_factor = NULL,
#'                                uncalibrated_as_NA = TRUE)
#'
#' curated_spectra <- curate_spectra(checked_data = checked_data,
#'                                   summarized_checks = summarized_checks,
#'                                   cut_offs = cut_offs)
#'                                   
#' passing_spectra <- kick_out_spectra(curated_spectra = curated_spectra) 
#' 
#' remove_unneeded_columns(passing_spectra = passing_spectra)
#' 
remove_unneeded_columns <- function(passing_spectra) {
  
  without_extra_columns <- passing_spectra %>% 
    # During the spectra curation process a number of extra columns are created.
    # All of these except for sum_intensity can be removed. sum_intensity will
    # be used in mod_normalization to calculate relative abundances.:
    dplyr::select(-c(
      # Created by summarize_spectra_checks:
      passing_analyte_percentage,
      
      # Created by calculate_cut_offs:
      cut_off_sum_intensity,
      cut_off_passing_analyte_percentage,
      curation_method,
      
      # Created by check_analyte_quality_criteria:
      analyte_meets_criteria,
      uncalibrated,
      failed_criteria,
      
      # Created by curate_spectra:
      has_passed_spectra_curation,
      reason_for_failure
    )) %>% 
    # If for all clusters manual cut-offs have been used, then the column
    # sample_type_list doesn't exist, so we have to remove it with across() and
    # any_off() to prevent an error:
    dplyr::select(-tidyselect::any_of(c("sample_type_list")))
  
  return(without_extra_columns)
}




#' Create the dataframe for next steps when spectra curation is skipped
#'
#' @inheritParams curate_spectra
#'
#' @return The \code{checked_data} and \code{summarized_checks} dataframes
#'   joined together, with unneeded columns removed and with uncalibrated
#'   spectra filtered out.
#' @export
#'
#' @examples
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
#' return_when_spectra_curation_is_skipped(checked_data = checked_data,
#'                                         summarized_checks = summarized_checks)
#'                                         
return_when_spectra_curation_is_skipped <- function(checked_data,
                                                    summarized_checks) {
  dplyr::full_join(checked_data,
                   summarized_checks) %>% 
    dplyr::filter(!uncalibrated) %>% 
    dplyr::select(-c(failed_criteria,
                     passing_analyte_percentage,
                     analyte_meets_criteria,
                     uncalibrated))
  # Leave 'sum_intensity' for the relative abundance
  # calculation 
}




#' Get the sample types to put in the menu for negative control samples
#'
#' Find out which sample types are present in the total or in the specific
#' samples. These sample types can then be shown in the selection menu for the
#' negative control samples.
#'
#' @inheritParams calculate_cut_offs
#' @param total_or_specific_keyword A character string with the keyword for
#'   either the total or the specific samples (depending on which menu you are
#'   creating).
#'
#' @return A named character vector. The elements are the sample types and the
#'   names are the sample types with the \code{total_or_specific_keyword} as
#'   prefix and "samples" as suffix. The names are what will be shown in the
#'   selectInput.
#' @export
#'
#' @examples
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
#' # To get the sample type options for the specific samples:
#' get_sample_type_options(summarized_checks = summarized_checks,
#'                         total_or_specific_keyword = "Spike")
get_sample_type_options <- function(summarized_checks,
                                    total_or_specific_keyword) {
  
  is_group <- summarized_checks$group == total_or_specific_keyword
  # Weird tibble behavior: tibble[rows, single_column] results in a
  # tibble, even though dataframe[rows, single_column] results in a
  # vector. We need a vector here and summarized_checks() is a tibble so I
  # have to use $sample_type instead of [is_specific, "sample_type"]:
  options_for_group <- unique(summarized_checks[is_group, ]$sample_type)
  
  # The names is what will be shown in the selection menu:
  names(options_for_group) <- paste(total_or_specific_keyword,
                                   options_for_group,
                                   "samples")
  return(options_for_group)
}



#'Visualize the spectra curation process
#'
#'This function can be used to visualize spectra curation. It will create a
#'scatter plot with the sum intensity plotted against the percentage of passing
#'analytes. Each point represents one sum spectrum (one cluster from one
#'sample). Colors represent sample types. Uncalibrated spectra are shown as squares, while
#'calibrated spectra are shown as dots. If the data contains total and specific
#'samples, the plot is faceted by group (total or specific).
#'
#'The ggplot has a text aesthetic that can be used to show as hover
#'info if the ggplot object is converted to a ggplotly object (see example).
#'
#'@inheritParams calculate_cut_offs
#'
#'@return A ggplot object.
#'
#'@export
#'
#'@importFrom grDevices colorRampPalette
#'
#' @examples
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
#' plot <- create_cut_off_plot(summarized_checks = summarized_checks)
#'
#' # The plot can be made interactive with plotly. Use the "text" aesthetic to
#' # show hover info:
#' plotly::ggplotly(plot,
#'                  tooltip = "text")
#' 
create_cut_off_plot <- function(summarized_checks, color_palette) {
  
  for_plot <- summarized_checks %>% 
    # in case there are NAs when uncalibrated_as_NA is TRUE:
    # TODO: check if this is still needed
    tidyr::replace_na(replace = list(sum_intensity = 0,
                                     passing_analyte_percentage = 0))
  
  p <- for_plot %>% 
    ggplot2::ggplot(
      ggplot2::aes( text = paste0("Sample name: ",
                                 sample_name,
                                 "\n",
                                 "Sample ID: ",
                                 sample_id,
                                 "\n",
                                 "Passing analyte percentage: ",
                                 paste0(
                                   format(round(passing_analyte_percentage, digits = 2), nsmall = 2),
                                   "%"
                                 ),
                                 "\n",
                                 "Sum intensity: ",
                                 round(sum_intensity, digits = 0),
                                 "\nUncalibrated: ",
                                 uncalibrated))
    ) +
    ggplot2::geom_point(data = for_plot[!for_plot$uncalibrated, ],
                         ggplot2::aes(color = sample_type,
                                      x = passing_analyte_percentage,
                                      y = sum_intensity),
                         size = 1,
                         alpha = 0.7) +
    ggplot2::geom_point(data = for_plot[for_plot$uncalibrated, ],
                         ggplot2::aes(color = sample_type,
                                      x = passing_analyte_percentage,
                                      y = sum_intensity),
                         shape = 15,
                         size = 1,
                         alpha = 0.7) +
    ggplot2::theme_classic() +
    ggplot2::theme(panel.border = ggplot2::element_rect(colour = "black", fill=NA, size=0.5),
                   strip.background = ggplot2::element_rect(fill = "#F6F6F8")) +
    ggplot2::scale_color_manual(values = color_palette,
                                name = "Sample type") +
    ggplot2::labs(y = "Sum intensity of passing analytes") +
    ggplot2::scale_x_continuous(labels = function(x) paste0(x, "%"), 
                                name = "Percentage of passing analytes")
  
  if ("group" %in% colnames(for_plot)) {
    p <- p +
      ggplot2::facet_wrap(cluster ~ group)
  } else {
    p <- p +
      ggplot2::facet_wrap(~ cluster)
  }
  
  return(p)
}


#' Visualize the results of the spectra curation
#' 
#' This function can be used to visualize how many spectra per sample type passed
#' spectra curation and how many failed and why. It will create a bar plot with 
#' sample types on the x-axis and the percentage of spectra on the y-axis. Colors 
#' represent if and why the spectrum failed curation. If the data contains total 
#' and specific samples, the plot is faceted by group (total or specific). If the 
#' data contains 4 or less clusters the plot is also faceted by cluster.
#' 
#' The ggplot has a text aesthetic that can be used to show the number of spectra 
#' and the percentage of spectra as hover info if the ggplot object is converted 
#' to a ggplotly object (see example).
#'
#' @param curated_data The return value from the function
#'   \code{\link{curate_spectra}}.
#' @param total_and_specific Character string, TRUE or NO. This argument
#'   indicates whether there are total and specific Ig samples in the data.
#'
#' @return A ggplot object.
#' @export
#'
#' @examples
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
#' cut_offs <- calculate_cut_offs(summarized_checks = summarized_checks,
#'                                control_sample_types = NULL,
#'                                exclude_sample_types = "PBS",
#'                                group_keyword = NULL,
#'                                percentile = 2,
#'                                use_mean_SD = FALSE,
#'                                SD_factor = NULL,
#'                                uncalibrated_as_NA = TRUE)
#'
#' curated_data <- curate_spectra(checked_data = checked_data,
#'                                summarized_checks = summarized_checks,
#'                                cut_offs = cut_offs)
#'                                
#' plot <- plot_spectra_curation_results(curated_data = curated_data,
#'                                       total_and_specific = TRUE)
#'                                       
#' # The plot can be made interactive with plotly. Use the "text" aesthetic to
#' # show hover info:
#' plotly::ggplotly(plot,
#'                  tooltip = "text")
#' 
plot_spectra_curation_results <- function(curated_data,
                                          total_and_specific) {
  
  # Consistent fill colors.
  my_palette <- c(
    "Yes" = "#3498DB",
    "No, calibration failed." = "#BF9C9C",
    "No, percentage of passing\nanalytes and sum\nintensity below cut-offs." = "#660000",
    "No, percentage of passing\nanalytes below\ncut-off." = "#ff0000",
    "No, sum intensity below\ncut-off." = "#ff6600"
  )
  
  my_data <- curated_data %>% 
    dplyr::distinct(dplyr::across(tidyselect::any_of(c("group", 
                                                       "sample_type", 
                                                       "cluster", 
                                                       "sample_name", 
                                                       "has_passed_spectra_curation",
                                                       "reason_for_failure")))) %>% 
    dplyr::mutate(
      `Passed curation?` = dplyr::case_when(
        is.na(reason_for_failure) ~ "Yes",
        reason_for_failure == "Percentage of passing analytes and sum intensity below cut-offs." ~ 
                           "No, percentage of passing\nanalytes and sum\nintensity below cut-offs.",
        reason_for_failure == "Percentage of passing analytes below cut-off." ~ 
                           "No, percentage of passing\nanalytes below\ncut-off.",
        reason_for_failure == "Sum intensity below cut-off." ~ 
                           "No, sum intensity below\ncut-off.",
        .default = "No, calibration failed."
      )
    ) %>% 
    calculate_number_and_percentage_per_reason()
  
  plot <- my_data %>% 
    ggplot2::ggplot(
      ggplot2::aes(
        text = paste(
          "Number of spectra:",
          number,
          "\nPercentage of spectra:",
          percentage,
          "\nPassed curation:",
          ifelse(
            !is.na(reason_for_failure),
            paste("No", tolower(reason_for_failure), sep = ", "),
            "Yes"
          )
        )
      )
    ) +
    ggplot2::geom_bar(ggplot2::aes(x = sample_type,
                                   fill = `Passed curation?`), 
                      position = "fill") +
    ggplot2::xlab("Sample type") +
    ggplot2::scale_y_continuous(labels = function(x) paste0(x * 100, "%"), 
                                name = "Percentage of spectra") +
    ggplot2::scale_fill_manual(values = my_palette) +
    ggplot2::theme_classic() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
                   strip.background = ggplot2::element_rect(fill = "#F6F6F8"),
                   panel.border = ggplot2::element_rect(colour = "black", fill=NA, size=0.5))
  
  if (total_and_specific) {
    plot <- plot +
      ggplot2::facet_wrap(cluster ~ group)
  } else if (!total_and_specific) {
    plot <- plot +
      ggplot2::facet_wrap(~ cluster)
  } 
  
  
  return(plot)
}


# Helper function
calculate_number_and_percentage_per_reason <- function(curated_data) {
  
  curated_data %>% 
    dplyr::group_by(dplyr::across(tidyselect::any_of(c("group",
                                                       "cluster",
                                                       "sample_type",
                                                       "reason_for_failure")))) %>% 
    dplyr::mutate(
      number_true = sum(has_passed_spectra_curation),
      number_false = sum(!has_passed_spectra_curation)) %>% 
    dplyr::ungroup(reason_for_failure) %>% # grouping by group, cluster and 
    # sample_type will remain, so that n() can be used later to calculate the
    # percentages
    dplyr::mutate(
      number = dplyr::case_when(
        has_passed_spectra_curation ~ number_true,
        !has_passed_spectra_curation ~ number_false,
        TRUE ~ as.integer(NA)
      ),
      percentage = scales::label_percent(accuracy = 0.01)(number / dplyr::n())
    )
  
}



create_downloadHandler <- function(data_to_download, download_format, paste) {
  downloadHandler(
    filename = function() {
      current_datetime <- paste0(format(Sys.Date(), "%Y%m%d"), "_", format(Sys.time(), "%H%M"))
      if (download_format == "R object") {
        print(download_format)
        return(paste0(current_datetime, paste, ".rds"))
      } else {
        print(download_format)
        return(paste0(current_datetime, paste, ".xlsx"))
      }
    },
    content = function(file) {
      if (download_format == "R object") {
        save(data_to_download, file = file)
      } else {
        writexl::write_xlsx(data_to_download, path = file)
      }
    }
  )
}
