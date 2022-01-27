#' Perform an analyte quality criteria check for every spectrum in the data.
#'
#' \code{do_criteria_check()} performs an analyte quality criteria check for
#' every spectrum in the data. This function is used within the function
#' \code{\link{summarize_spectra_checks()}}.
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
  all_checks <- (mass_acc_check & IPQ_check & sn_check) %>% 
    tidyr::replace_na(., FALSE)
  return(all_checks)
}

#' Summarize analyte quality criteria checks
#'
#' \code{summarize_spectra_checks()} first performs analyte quality criteria checks
#' for every spectrum in the data using \code{\link{do_criteria_check()}}. Then it
#' calculates the proportion of passing analytes per spectrum and the sum
#' intensity of passing analytes per spectrum.
#'
#' @inheritParams do_criteria_check
#'
#' @return A dataframe in which each row represents one spectrum. The dataframe
#' contains five columns: 
#' \describe{
#'     \item{sample_name}{The name of the sample for which this spectrum was recorded.}
#'     \item{sample_type}{The type of sample (e.g. negative control, blank etc.).}
#'     \item{group}{The group (Total or Specific) that this sample belongs to.}
#'     \item{passing_proportion}{The proportion of analytes that 
#'     passed the criteria checks in this spectrum.}
#'     \item{sum_intensity}{The sum intensity of all passing analytes in this spectrum}
#'     }
#' @export
#'
#' @examples
#' data("long_data")
#' summarize_spectra_checks(data = long_data,
#'                                min_ppm_deviation = -20,
#'                                max_ppm_deviation = 20,
#'                                max_ipq = 0.2,
#'                                min_sn = 9)
summarize_spectra_checks <- function(data, min_ppm_deviation, max_ppm_deviation, max_ipq, min_sn) {
  required_columns <- list("group",
                           "sample_type",
                           "sample_name")
  
  if(any(!(required_columns %in% colnames(data)))) {
    missing_columns <- required_columns[!(required_columns %in% colnames(data))]
    stop(paste0("The data doesn't contain the required column(s) ",
                paste0(missing_columns,
                       collapse = " and "),
                "."))
  }
  
  spectra_check <- data %>% 
    dplyr::mutate(criteria_check = do_criteria_check(data,
                                                     min_ppm_deviation = min_ppm_deviation,
                                                     max_ppm_deviation = max_ppm_deviation,
                                                     max_ipq = max_ipq,
                                                     min_sn = min_sn)) %>% 
    dplyr::group_by(group, sample_type, sample_name) %>% 
    dplyr::summarise(passing_proportion = sum(criteria_check)/dplyr::n(), 
                     sum_intensity = sum(absolute_intensity_background_subtracted[criteria_check == TRUE]))
  return(spectra_check)
}

#' Calculate the population standard deviation.
#'
#' @param x A numeric vector or an R object but not a factor coercible to numeric by as.double(x).
#' @param na.rm Logical that indicates whether missing values should be removed.
#'
#' @return The population standard deviation (numeric).
#' @export
#'
#' @examples
#' numbers <- c(2, 5, 3, 9, 6)
#' sd_p(numbers)
#' 
#' numbers_na <- c(2, 5, 3, 9, 6, NA)
#' sd_p(numbers, na.rm = TRUE)
sd_p <- function(x, na.rm = FALSE) {
  sd(x, na.rm = na.rm) * sqrt((length(x)-1)/length(x))
}
