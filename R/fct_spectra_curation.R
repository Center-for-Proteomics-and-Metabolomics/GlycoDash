#' Perform an analyte quality criteria check for every spectrum in the data.
#'
#' \code{do_criteria_check()} performs an analyte quality criteria check for
#' every spectrum in the data. This function is used within the function
#' \code{create_spectra_check_data_frame()}.
#'
#' @param data A dataframe in long format (one row for each analyte + sample
#'   combination).
#' @param min_ppm_deviation The lowest allowed value for the mass accuracy (in
#'   ppm).
#' @param max_ppm_deviation The highest allowed value for the mass accuracy (in
#'   ppm).
#' @param max_ipq The highest allowed value for the Isotopic Pattern Quality
#'   (IPQ). The IPQ indicates how much the isotopic pattern deviates from the
#'   theoretic isotopic pattern.
#' @param min_sn The lowest allowed value for the signal to noise ratio (S/N).
#'
#' @return A logical vector of which the length corresponds to the number of
#'   rows in the data. TRUE indicates that the analyte + sample combination
#'   passed all three quality criteria checks, whereas FALSE indicates one or
#'   more criteria were not fulfilled.
#' @export
#'
#' @examples
#' data(long_data)
#' do_criteria_check(data = long_data,
#'                   min_ppm_deviation = -20,
#'                   max_ppm_deviation = 20,
#'                   max_ipq = 0.2,
#'                   min_sn = 9)
do_criteria_check <- function(data, 
                              min_ppm_deviation, 
                              max_ppm_deviation, 
                              max_ipq, 
                              min_sn) {
  
  if (!all(is.numeric(min_ppm_deviation),
           is.numeric(max_ppm_deviation),
           is.numeric(max_ipq),
           is.numeric(min_sn))) {
    stop("One or more quality criteria arguments are non-numeric.")
  }
  
  if (!(all(c("mass_accuracy_ppm",
              "isotopic_pattern_quality",
              "sn") %in% colnames(data)))) {
    stop("The data doesn't contain the required columns with the quality criteria.")
  }
  
  mass_acc_check <- dplyr::between(data$mass_accuracy_ppm, 
                                   min_ppm_deviation, 
                                   max_ppm_deviation)
  IPQ_check <- data$isotopic_pattern_quality <= max_ipq
  sn_check <- data$sn >= min_sn
  all_checks <- mass_acc_check & IPQ_check & sn_check %>% 
    tidyr::replace_na(FALSE)
  return(all_checks)
}

