#'Define clusters based on analyte names
#'
#'This function determines which cluster an analyte belongs to based on the
#'analyte name.
#'
#'@inheritParams do_criteria_check
#'@param clusters_regex A vector containing character strings. These strings are
#'  used as regular expressions to classify analytes into clusters.
#'  \code{clusters_regex} should contain one character string per cluster in the
#'  data.
#'
#'@return The function returns the original dataframe given as the data
#'  argument, but with an additional column named "cluster". This character
#'  column indicates to what cluster the analyte belongs.
#'@export
#'
#' @examples
#' data("long_data")
#' define_clusters(data = long_data,
#'                 clusters_regex = "IgGI1")
define_clusters <- function(data, clusters_regex) {
  
  regex_found <- purrr::map_lgl(
    clusters_regex,
    function(regex) {
      any(stringr::str_detect(string = data$analyte,
                              pattern = regex))
                 })
  
  if (any(regex_found == FALSE)) {
    rlang::abort(class = "unmatched_regex",
                 message = paste("The regular expression(s)",
                                 paste0(clusters_regex[!regex_found],
                                        collapse = " and "),
                                 "matched no analytes in the \"analyte\" column of the data."))
  }
  
  if (length(clusters_regex) > 1) {
    regex_overlap <- purrr::imap_lgl(
      clusters_regex,
      function(regex, i) {
        other_regexes <- unlist(clusters_regex)[-i]
        any(purrr::map_lgl(other_regexes,
                       function(other_regex) {
                         stringr::str_detect(string = other_regex,
                                             pattern = regex)
                       }))
      })
    
    if(any(regex_overlap == TRUE)) {
      rlang::abort(class = "regex_overlap",
                   message = paste("There is overlap between the cluster keywords.",
                                   "Please make sure that each analyte matches only one cluster keyword."))
    }
  }
  
  clusters <- data %>% 
    tidyr::extract(col = analyte,
                   into = "cluster",
                   regex = paste0("(",
                                  paste0(clusters_regex, 
                                         collapse = "|"),
                                  ")"),
                   remove = FALSE)
  
  if (anyNA(clusters$cluster)) {
    rlang::abort(class = "unmatched_analytes",
                 message = paste("Some analytes could not be assigned into a cluster.",
                                 "Please reconsider the regular expressions you gave as clusters_regex."))
  }
  return(clusters)
}

#' Perform an analyte quality criteria check for every spectrum in the data.
#'
#' \code{do_criteria_check()} performs an analyte quality criteria check for
#' every spectrum in the data. This function is used within the function
#' \code{\link{summarize_spectra_checks}}.
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
#' @return The original dataframe given as the data argument, but with an
#'   additional column named "criteria_check". This column is a logical vector:
#'   TRUE indicates that the analyte + sample combination passed all three
#'   quality criteria checks, whereas FALSE indicates one or more criteria were
#'   not fulfilled.
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
  IPQ_check <- data$isotopic_pattern_quality < max_ipq
  sn_check <- data$sn > min_sn
  all_checks <- (mass_acc_check & IPQ_check & sn_check) %>% 
    tidyr::replace_na(., FALSE)
  
  data_checked <- data %>% 
    dplyr::mutate(criteria_check = all_checks)
  
  return(data_checked)
}

#' Summarize analyte quality criteria checks
#'
#' \code{summarize_spectra_checks()} calculates the proportion of passing
#' analytes per spectrum and the sum intensity of passing analytes per spectrum.
#' \code{summarize_spectra_checks()} should be used after
#' \code{\link{do_criteria_check}} has been used to perform analyte quality
#' criteria checks for every spectrum and analyte combination in the data.
#'
#' @inheritParams do_criteria_check
#'
#' @return A dataframe that contains one row per spectrum for each cluster (
#'   Thus the number of rows is equal to the number of spectra times the number
#'   of clusters). The dataframe contains five columns: \describe{
#'   \item{sample_name}{The name of the sample for which this spectrum was
#'   recorded.} \item{sample_type}{The type of sample (e.g. negative control,
#'   blank etc.).} \item{group}{The group (Total or Specific) that this sample
#'   belongs to.} \item{cluster}{The cluster that the analyte belongs to.}
#'   \item{passing_proportion}{The proportion of analytes that passed the
#'   criteria checks in this spectrum.} \item{sum_intensity}{The sum intensity
#'   of all passing analytes in this spectrum} }
#' @export
#'
#' @examples
#' data("long_data")
#' summarize_spectra_checks(data = long_data,
#'                          min_ppm_deviation = -20,
#'                          max_ppm_deviation = 20,
#'                          max_ipq = 0.2,
#'                          min_sn = 9)
summarize_spectra_checks <- function(data_checked) {
  required_columns <- list("sample_type",
                           "sample_name",
                           "cluster",
                           "criteria_check")
  
  if(any(!(required_columns %in% colnames(data_checked)))) {
    missing_columns <- required_columns[!(required_columns %in% colnames(data_checked))]
    stop(paste0("The data doesn't contain the required column(s) ",
                paste0(missing_columns,
                       collapse = " and "),
                "."))
  }
  
  grouping_variables <- c("group", "sample_type", "cluster", "sample_name")
  
  spectra_check <- data_checked %>% 
    # I'm using across() and any_of() because if the data is not Ig data, the
    # column "group" doesn't exist:
    dplyr::group_by(dplyr::across(tidyselect::any_of(grouping_variables))) %>% 
    dplyr::summarise(passing_proportion = sum(criteria_check)/dplyr::n(), 
                     sum_intensity = sum(absolute_intensity_background_subtracted[criteria_check == TRUE])) %>% 
    dplyr::ungroup(.)
  
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

#' Calculate cut-off values for spectra curation
#'
#' Calculate the cutoffs for the proportion of passing analytes per spectrum and
#' for the sum intensity of passing analytes per spectrum. This function is used
#' within the function \code{\link{curate_spectra}}.
#'
#' @param spectra_check The dataframe returned by
#'   \code{\link{summarize_spectra_checks()}}.
#' @param groups_to_filter The group (Specific or Total) to base spectra curation
#'   cut-offs on.
#' @param sample_types_to_filter The sample type to base spectra curation
#'   cut-offs on.
#'
#' @return
#' @export
#'
#' @examples
#' data("long_data")
#' spectra_check <- summarize_spectra_checks(data = long_data,
#'                                           min_ppm_deviation = -20,
#'                                           max_ppm_deviation = 20,
#'                                           max_ipq = 0.2,
#'                                           min_sn = 9)
#' calculate_cut_offs(spectra_check = spectra_check,
#'                    group_to_filter = "Spike",
#'                    sample_type_to_filter = "CN")
calculate_cut_offs <- function(spectra_check, cut_off_basis) {
  
  cut_off_basis_samples <- filter_cut_off_basis(cut_off_basis = cut_off_basis,
                                                data = spectra_check)
  
  cut_offs <- cut_off_basis_samples %>%  
    dplyr::group_by(cluster) %>% 
    dplyr::summarise(av_prop = mean(passing_proportion, na.rm = FALSE),
                     sd_prop = sd_p(passing_proportion, na.rm = FALSE),
                     cut_off_prop = av_prop + (3 * sd_prop),
                     av_sum_int = mean(sum_intensity, na.rm = FALSE),
                     sd_sum_int = sd_p(sum_intensity, na.rm = FALSE),
                     cut_off_sum_int = av_sum_int + (3 * sd_sum_int))
  return(cut_offs)
}

#' Perform spectra curation
#'
#' \code{curate_spectra()} performs spectra curation on mass spectrometry data.
#' For each spectrum, analytes are curated based on the quality criteria (mass
#' accuracy, isotopic pattern quality (IPQ) and signal to noise ratio (S/N)).
#' Then the proportion of passing analytes and the sum intensity of passing
#' analytes is calculated for each spectrum (using the function
#' \code{\link{summarize_spectra_checks}}). Based on the average proportion and
#' sum intensity in a chosen group of samples that should not pass curation
#' (e.g. Specific Ig negative control samples), cut-off values for spectra
#' curation are defined (using the function \code{\link{calculate_cut_offs}}).
#' All spectra with values above those cut-off values pass the spectra curation.
#'
#' @inheritParams do_criteria_check
#' @inheritParams calculate_cut_offs
#' @inheritParams define_clusters
#'
#' @return The function returns the original dataframe given as the data
#'   argument, but with two additional columns. One column is named
#'   "passed_spectra_curation"; This logical column is \code{TRUE} for spectra that have
#'   passed curation and \code{FALSE} for spectra that did not pass curation.
#'   The other column is named "criteria_check" and is \code{TRUE} for analyte +
#'   sample combinations that passed all three quality criteria checks, whereas
#'   \code{FALSE} indicates that one or more criteria were not fulfilled.
#' @export
#'
#' @examples
#' data("long_data")
#' curate_spectra(data = long_data,
#'                min_ppm_deviation = -20,
#'                max_ppm_deviation = 20,
#'                max_ipq = 0.2,
#'                min_sn = 9,
#'                group_to_filter = "Spike",
#'                sample_type_to_filter = "CN")
curate_spectra <- function(data, clusters_regex, min_ppm_deviation, max_ppm_deviation, 
                           max_ipq, min_sn, cut_off_basis) {
  data <- define_clusters(data = data,
                          clusters_regex = clusters_regex)
  checked_data <- do_criteria_check(data = data, 
                                    min_ppm_deviation = min_ppm_deviation,
                                    max_ppm_deviation = max_ppm_deviation,
                                    max_ipq = max_ipq,
                                    min_sn = min_sn)
  spectra_check <- summarize_spectra_checks(checked_data)
  cut_offs <- calculate_cut_offs(spectra_check = spectra_check,
                                 cut_off_basis = cut_off_basis) %>% 
    dplyr::ungroup(.)
  
  spectra_check <- dplyr::left_join(spectra_check, cut_offs, by = "cluster") %>% 
    dplyr::ungroup(.)
  
  passing_spectra <- spectra_check %>% 
    dplyr::filter((passing_proportion > cut_off_prop) & (sum_intensity > cut_off_sum_int))
  
  curated_data <- checked_data %>% 
    dplyr::mutate(passed_spectra_curation = dplyr::case_when(
      sample_name %in% passing_spectra$sample_name ~ TRUE,
      TRUE ~ FALSE))
  
  no_NAs <- curated_data %>% 
    dplyr::filter(dplyr::if_all(.cols = c(mass_accuracy_ppm,
                                          isotopic_pattern_quality,
                                          sn),
                                .fns = ~ !is.na(.x)))
  
  if (all(no_NAs$passed_spectra_curation == FALSE)) {
    warning("None of the spectra passed curation.")
  } else {
    if (all(no_NAs$passed_spectra_curation == TRUE)) {
      warning("All spectra passed curation.")
    }
  }
  
  return(list(curated_data = curated_data,
              spectra_check = spectra_check))
}

filter_cut_off_basis <- function(cut_off_basis, data) {
  
  sample_types_to_filter <- stringr::str_extract(
    string = cut_off_basis,
    pattern = paste0(unique(data$sample_type),
                     collapse = "|")) %>% 
    na.omit(.)
  
  if (any(!(sample_types_to_filter %in% data$sample_type))) {
    stop("One or more of the sample types in cut_off_basis are not present in the data.")
  }
  
  if ("group" %in% colnames(data)) {
    
    groups_to_filter <- stringr::str_extract(
      string = cut_off_basis,
      pattern = paste0(unique(data$group),
                       collapse = "|")) %>% 
      na.omit(.)
    
    if (any(!(groups_to_filter %in% data$group))) {
      stop("One or more of the groups in cut_off_basis are not present in the data.")
    } 
    
    cut_off_basis_samples <- purrr::map2_dfr(
      groups_to_filter,
      sample_types_to_filter,
      function(group_to_filter, sample_type_to_filter) {
        data %>% 
          dplyr::filter(group == group_to_filter & sample_type == sample_type_to_filter)
      })
    
  } else {
    
    cut_off_basis_samples <- purrr::map_dfr(sample_types_to_filter,
                                            function(sample_type_to_filter) {
                                              data %>% 
                                                dplyr::filter(sample_type == sample_type_to_filter)
                                            })
  }
  
  return(cut_off_basis_samples)
}

create_cut_off_plot <- function(spectra_check, cut_off_basis) {
  
  #my_palette <- colorRampPalette(RColorBrewer::brewer.pal(8, "Set2"))(20)
  
  p <- spectra_check %>% 
    ggplot2::ggplot(ggplot2::aes(x = passing_proportion,
                                 y = sum_intensity)) +
    ggplot2::geom_jitter(ggplot2::aes(color = sample_type),
                        size = 1) +
    ggplot2::theme_classic() +
    ggpubr::border(size = 0.5) +
    ggplot2::geom_hline(yintercept = spectra_check$cut_off_sum_int,
                        linetype = "dashed") +
    ggplot2::geom_vline(xintercept = spectra_check$cut_off_prop,
                        linetype = "dashed") +
    #ggplot2::scale_color_manual(values = my_palette)
    ggplot2::scale_color_brewer(palette = "Set2")
  
  if ("group" %in% colnames(spectra_check)) {
    p <- p +
      ggplot2::facet_wrap(~ group)
  }
  
  cut_off_basis_samples <- filter_cut_off_basis(cut_off_basis = cut_off_basis,
                                                data = spectra_check)

  distinct_data_points <- cut_off_basis_samples %>%
    dplyr::distinct(passing_proportion,
                    sum_intensity,
                    .keep_all = TRUE) %>%
    dplyr::group_by(dplyr::across(tidyselect::any_of("group"))) %>%
    dplyr::summarise(n = dplyr::n())

  ellipse_possible <- all(distinct_data_points$n >= 3)

  if (ellipse_possible) {
    p <- p +
      ggplot2::stat_ellipse(data = cut_off_basis_samples)
  } else {
    p <- p +
      ggplot2::geom_point(data = cut_off_basis_samples,
                          shape = 1, size = 2.5)
  }
  
  return(p)
}