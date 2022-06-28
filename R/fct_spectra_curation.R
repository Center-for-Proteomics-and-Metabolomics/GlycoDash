

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
#' data(example_data)
#' do_criteria_check(data = example_data,
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
  
  data_checked <- data %>% 
    dplyr::mutate(`mass accuracy` = dplyr::between(data$mass_accuracy_ppm, 
                                            min_ppm_deviation, 
                                            max_ppm_deviation),
                  IPQ = data$isotopic_pattern_quality < max_ipq,
                  `S/N` = data$sn > min_sn,
                  criteria_check = (`mass accuracy` & IPQ & `S/N`) %>% 
                    tidyr::replace_na(., FALSE)) %>% 
    tidyr::pivot_longer(cols = c(`mass accuracy`, IPQ, `S/N`),
                        names_to = "criterium",
                        values_to = "passed") %>% 
    dplyr::mutate(failed_criteria = ifelse(passed == FALSE, criterium, NA)) %>% 
    dplyr::select(-c(criterium, passed)) %>% 
    dplyr::group_by(sample_name, analyte, charge) %>% 
    dplyr::summarise(failed_criteria = dplyr::if_else(all(is.na(failed_criteria)),
                                                      "none",
                                                      comma_and(unique(failed_criteria[!is.na(failed_criteria)]))),
    across()) %>% 
    dplyr::distinct()
  
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
#' @param data_checked The dataframe that is returned by
#'   \code{\link{do_criteria_check}}.
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
#' data("example_data")
#'
#' example_data <- define_clusters(data = example_data,
#'                                 clusters_regex = "IgGI1")
#'
#' checked_data <- do_criteria_check(data = example_data,
#'                                   min_ppm_deviation = -20,
#'                                   max_ppm_deviation = 20,
#'                                   max_ipq = 0.2,
#'                                   min_sn = 9)
#'
#' summarize_spectra_checks(data_checked = checked_data)
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
  
  grouping_variables <- c("group", "sample_type", "cluster", "sample_name", "sample_id")
  
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
#'   \code{\link{summarize_spectra_checks}}.
#' @param cut_off_basis A character vector or a single character string
#'   specifying which sample types (and optionally which group (Specific or
#'   Total Ig)) the spectra curation cut-offs should be based on. For each
#'   sample type or sample type-group combination that you want to base the
#'   cut-offs on you should add a character string. For example. if you want to
#'   base the cut-offs on all PBS samples and on the Specific negative_control
#'   samples, \code{cut_off_basis} should be \code{c("PBS", "Specific
#'   negative_control")}
#'
#' @return
#' @export
#'
#' @examples
#' data("example_data")
#'
#' example_data <- define_clusters(data = example_data,
#'                                 clusters_regex = "IgGI1")
#'
#' checked_data <- do_criteria_check(data = example_data,
#'                                   min_ppm_deviation = -20,
#'                                   max_ppm_deviation = 20,
#'                                   max_ipq = 0.2,
#'                                   min_sn = 9)
#'
#' spectra_check <- summarize_spectra_checks(data_checked = checked_data)
#'
#' calculate_cut_offs(spectra_check = spectra_check,
#'                    cut_off_basis = c("Spike PBS", "Total PBS"))
#'                    
calculate_cut_offs <- function(spectra_check, cut_off_basis, sd_factor, central_tendency_measure) {
  
  cut_off_basis_samples <- filter_cut_off_basis(cut_off_basis = cut_off_basis,
                                                data = spectra_check)
  
  cut_offs <- cut_off_basis_samples %>%  
    dplyr::group_by(cluster) %>% 
    dplyr::summarise(av_prop = mean(passing_proportion, na.rm = FALSE),
                     med_prop = median(passing_proportion, na.rm = FALSE),
                     sd_prop = sd_p(passing_proportion, na.rm = FALSE),
                     cut_off_prop = dplyr::if_else(central_tendency_measure == "Mean", 
                                           av_prop + (sd_factor * sd_prop),
                                           med_prop + (sd_factor * sd_prop)),
                     av_sum_int = mean(sum_intensity, na.rm = FALSE),
                     med_sum_int = median(sum_intensity, na.rm = FALSE),
                     sd_sum_int = sd_p(sum_intensity, na.rm = FALSE),
                     cut_off_sum_int = dplyr::if_else(central_tendency_measure == "Mean", 
                                                      av_sum_int + (sd_factor * sd_sum_int),
                                                      med_sum_int + (sd_factor * sd_sum_int)),
                     across(tidyselect::any_of(c("group", "sample_type")))) %>% 
    dplyr::mutate(type = "based_on_samples") %>% 
    dplyr::distinct() %>% 
    dplyr::select(tidyselect::any_of(c("cluster",
                                     "sample_type",
                                     "group",
                                     "cut_off_prop",
                                     "cut_off_sum_int",
                                     "type"))) %>% 
    tidyr::nest(., "sample_type_list" = sample_type)
  
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
#' data("example_data")
#' curate_spectra(data = example_data,
#'                clusters_regex = "IgGI1",
#'                min_ppm_deviation = -20,
#'                max_ppm_deviation = 20,
#'                max_ipq = 0.2,
#'                min_sn = 9,
#'                cut_off_basis = c("Spike PBS", "Total PBS"))
curate_spectra <- function(checked_data, summarized_checks, cut_offs) {
  
  # if ("cluster" %in% colnames(cut_offs)) {
  #   summarized_checks <- dplyr::left_join(summarized_checks, 
  #                                         cut_offs) %>% 
  #     dplyr::ungroup(.)
  # } else {
  #   summarized_checks <- summarized_checks %>% 
  #     dplyr::mutate(cut_off_sum_int = cut_offs$cut_off_sum_int,
  #                   cut_off_prop = cut_offs$cut_off_prop)
  # }
  
  summarized_checks <- dplyr::left_join(summarized_checks, 
                                        cut_offs) %>% 
    dplyr::ungroup(.)
  
  passing_spectra <- summarized_checks %>% 
    dplyr::mutate(passed_spectra_curation = ifelse((passing_proportion > cut_off_prop) &
                                                     (sum_intensity > cut_off_sum_int),
                                                   TRUE,
                                                   FALSE),
                  reason_for_failure = dplyr::case_when(
                    passing_proportion < cut_off_prop & sum_intensity < cut_off_sum_int ~ "Proportion of passing analytes and sum intensity below cut-offs",
                    passing_proportion < cut_off_prop ~ "Proportion of passing analytes below cut-off",
                    sum_intensity < cut_off_sum_int ~ "Sum intensity below cut-off",
                    TRUE ~ ""
                  )) %>%
    dplyr::select(-tidyselect::any_of(c("type", 
                                        "sample_type_list")))
  
  curated_data <- dplyr::full_join(passing_spectra, 
                                   checked_data) %>% 
    dplyr::mutate(reason_for_failure = dplyr::case_when(
      is.na(absolute_intensity_background_subtracted) &
        is.na(mass_accuracy_ppm) &
        is.na(isotopic_pattern_quality) &
        is.na(sn) ~ "Empty line in LacyTools summary file",
      TRUE ~ reason_for_failure
    )) %>% 
    dplyr::relocate(c(passed_spectra_curation, reason_for_failure), 
                    .after = sample_name) %>% 
    dplyr::relocate(c(criteria_check,
                      failed_criteria),
                    .after = charge) %>% 
    dplyr::relocate(c(sample_id, plate_well),
                    .after = sample_name)
  
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
  
  return(curated_data)
}

check_spectra <- function(data, min_ppm_deviation, max_ppm_deviation, 
                            max_ipq, min_sn) {
  checked_data <- do_criteria_check(data = data, 
                                    min_ppm_deviation = min_ppm_deviation,
                                    max_ppm_deviation = max_ppm_deviation,
                                    max_ipq = max_ipq,
                                    min_sn = min_sn)
  spectra_check <- summarize_spectra_checks(checked_data)
    
  return(spectra_check)
}

calculate_cut_offs_per_type <- function(checked_spectra) {
  
  grouping_variables <- c("group", "cluster", "sample_type")
  
  cut_offs <- checked_spectra %>%  
    dplyr::group_by(dplyr::across(tidyselect::any_of(grouping_variables))) %>% 
    dplyr::summarise(av_prop = mean(passing_proportion, na.rm = FALSE),
                     sd_prop = sd_p(passing_proportion, na.rm = FALSE),
                     cut_off_prop = av_prop + (3 * sd_prop),
                     av_sum_int = mean(sum_intensity, na.rm = FALSE),
                     sd_sum_int = sd_p(sum_intensity, na.rm = FALSE),
                     cut_off_sum_int = av_sum_int + (3 * sd_sum_int))
  return(cut_offs)
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
    
    if (!rlang::is_empty(groups_to_filter)) {
      
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
  } else {
    
    cut_off_basis_samples <- purrr::map_dfr(sample_types_to_filter,
                                            function(sample_type_to_filter) {
                                              data %>% 
                                                dplyr::filter(sample_type == sample_type_to_filter)
                                            })
  }
  
  return(cut_off_basis_samples)
}

#' Create a plot to show the cut_offs for spectra curation
#'
#' @param spectra_check The dataframe returned by \code{\link{curate_spectra}}.
#'   \code{curate_spectra} returns two dataframes in a list. The dataframe that
#'   should be passed as the \code{spectra_check} argument to
#'   \code{create_cut_off_plot} is named "spectra_check".
#'
#' @return This function returns a ggplot object.
#' @export
#'
#' @importFrom grDevices colorRampPalette
#'
#' @examples
#' data("example_data")
#' spectra_curation <- curate_spectra(data = example_data,
#'                                    clusters_regex = "IgGI1",
#'                                    min_ppm_deviation = -20,
#'                                    max_ppm_deviation = 20,
#'                                    max_ipq = 0.2,
#'                                    min_sn = 9,
#'                                    cut_off_basis = c("Spike PBS", "Total PBS"))
#' 
#' create_cut_off_plot(spectra_check = spectra_curation$spectra_check,
#'                     cut_off_basis = c("Spike PBS", "Total PBS"))
create_cut_off_plot <- function(spectra_check) {
  
  n_colors <- length(unique(spectra_check$sample_type))
  my_palette <- colorRampPalette(RColorBrewer::brewer.pal(8, "Set2"))(n_colors)
  
  p <- spectra_check %>% 
    ggplot2::ggplot() +
    ggplot2::geom_jitter(ggplot2::aes(color = sample_type,
                                      x = passing_proportion,
                                      y = sum_intensity,
                                      text = paste0("Sample name: ", 
                                                    sample_name,
                                                    "\n",
                                                    "Sample ID: ",
                                                    sample_id,
                                                    "\n",
                                                    "Passing proportion: ",
                                                    passing_proportion,
                                                    "\n",
                                                    "Sum intensity: ",
                                                    sum_intensity)),
                         size = 1) +
    ggplot2::theme_classic() +
    ggplot2::theme(panel.border = ggplot2::element_rect(colour = "black", fill=NA, size=0.5),
                   #text = ggplot2::element_text(size = 16),
                   strip.background = ggplot2::element_rect(fill = "#F6F6F8")) +
    ggplot2::scale_color_manual(values = my_palette,
                                name = "Sample type") +
    ggplot2::labs(y = "Sum intensity of passing analytes") +
    ggplot2::scale_x_continuous(labels = function(x) paste0(x * 100, "%"), 
                                name = "Proportion of analytes that passed curation (%)")
  
  if ("group" %in% colnames(spectra_check)) {
    p <- p +
      ggplot2::facet_wrap(cluster ~ group)
  } else {
    p <- p +
      ggplot2::facet_wrap(~ cluster)
  }
}

#' Title
#'
#' @param gp 
#' @param size 
#'
#' @return
#' @export
#'
#' @examples
facet_strip_bigger <- function(ggplotly, size = 38){
  if(missing(ggplotly)){
    rlang::abort(class = "no_ggplotly_object",
                 message = "This function needs a facet_wrap ggplotly object.")
  }
  
  ggplotly[["x"]][["layout"]][["margin"]][["t"]] <- as.numeric(size)
  
  n_facets <- c(1:length(ggplotly[["x"]][["layout"]][["shapes"]]))
  
  for(i in n_facets){
    if(n_facets[i] %% 2 == 0){
      ggplotly[["x"]][["layout"]][["shapes"]][[i]][["y0"]] <- as.numeric(size)
      ggplotly[["x"]][["layout"]][["shapes"]][[i]][["y1"]] <- 0
    }
  }
  
  return(ggplotly)
}