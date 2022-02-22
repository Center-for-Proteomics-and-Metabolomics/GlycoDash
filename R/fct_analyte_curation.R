#' Perform analyte curation
#'
#' Curate analytes based on the percentage of spectra for which the analyte
#' passes the quality criteria. These quality criteria are the minimum and
#' maximum allowed mass accuracy deviations, the isotopic pattern quality (IPQ)
#' and the signal-to-noise ratio (S/N) (see \code{\link{do_criteria_check}}).
#' Analyte curation should not be based on spectra for which you don't expect
#' any analytes to pass, or for which you expect all analytes to pass. The
#' arguments \code{sample_types_to_ignore} and \code{groups_to_ignore} should be
#' used to indicate which spectra should not be used as basis for analyte
#' curation. This function should be used only after spectra curation has been
#' performed with \code{\link{curate_spectra}} and after the spectra that did
#' not pass spectra curation have been filtered out (see example below).
#'
#' @param data The result of the \code{\link{curate_spectra}} function, but
#'   filtered so that only spectra that passed curation remain (see example
#'   below).
#' @param group_to_ignore A character string with the keyword for total Ig
#'   samples or the keyword for specific Ig samples. These samples will not be
#'   used as a basis for analyte curation. Specify \code{NULL} to exclude
#'   neither total nor specific Ig samples.
#' @param sample_types_to_ignore A character vector with the sample types that
#'   you don't want to base analyte curation on (e.g. standards).
#' @param cut_off_percentage The minimum percentage of spectra in which an
#'   analyte needs to fulfill the quality criteria in order for that analyte to
#'   pass curation.
#'
#' @return A dataframe with one row for each combination of analyte and charge
#'   per cluster, a column named \code{passing_percentage} with the percentage
#'   of spectra that pass the analyte quality criteria for that analyte and a
#'   logical column named \code{passed_curation} that is \code{TRUE} for
#'   analytes with a \code{passing_percentage} above the
#'   \code{cut_off_percentage} and \code{FALSE} for analytes with a
#'   \code{passing_percentage} under or equal to the \code{cut_off_percentage}.
#' @export
#'
#' @examples
#' data("long_data")
#' long_data <- curate_spectra(data = long_data,
#'                             min_ppm_deviation = -20,
#'                             max_ppm_deviation = 20,
#'                             max_ipq = 0.2,
#'                             min_sn = 9,
#'                             clusters_regex = "IgGI1",
#'                             group_to_filter = "Spike",
#'                             sample_type_to_filter = "CN")
#'
#' curated_spectra <- long_data %>%
#'    dplyr::filter(passed_curation == TRUE)
#'
#' curate_analytes(data = curated_spectra,
#'                 group_to_ignore = "Total",
#'                 sample_types_to_ignore = c("pool", 
#'                                            "IVIGg", 
#'                                            "CN", 
#'                                            "Visucon", 
#'                                            "PBS"),
#'                 cut_off_percentage = 25)
#'                 
curate_analytes <- function(data, group_to_ignore, sample_types_to_ignore, cut_off_percentage) {
  
  if (!(group_to_ignore %in% data$group)) {
    rlang::abort(class =  "wrong_group",
                 message = paste("The group_to_ignore",
                                 group_to_ignore,
                                 "is not present in the group column of the data."))
  } 
  
  if (any(!(sample_types_to_ignore %in% data$sample_type))) {
    rlang::abort(class =  "wrong_sample_type",
                 message = "One or more of sample_types_to_ignore is not present in the \"sample_type\" column of the data.")
  }
  
  required_columns <- c("cluster", 
                        "charge", 
                        "analyte", 
                        "criteria_check")
  missing_columns <- required_columns[!(required_columns %in% colnames(data))] 
  if(!rlang::is_empty(missing_columns)) {
    rlang::abort(class = "missing_columns",
                 message = paste("The required column(s)",
                                 missing_columns,
                                 "are not present in the data.",
                                 "Attention: curate_analytes() can only be used after spectra curation has been performed with curate_spectra()"))
  }
  
  curated_analytes <- data %>% 
    dplyr::filter(!(group %in% group_to_ignore) & !(sample_type %in% sample_types_to_ignore)) %>% 
    dplyr::group_by(cluster, charge, analyte) %>% 
    dplyr::summarise(passing_percentage = sum(criteria_check) / dplyr::n() * 100) %>% 
    dplyr::mutate(passed_curation = dplyr::if_else(passing_percentage > cut_off_percentage, 
                                                   TRUE,
                                                   FALSE))
  
  return(curated_analytes)
}

#' Create a visual summary of the analyte curation process
#'
#' Create a plot showing the results of analyte curation for a single cluster.
#'
#' @param curated_analytes The result of the \code{\link{curate_analytes}}
#'   function: A dataframe with all analytes and charge combinations in the
#'   data, their percentage of passing spectra and whether or not they passed
#'   analyte curation.
#' @param selected_cluster The cluster for which the analyte curation results
#'   should be plotted.
#' @inheritParams curate_analytes
#'
#' @return A bar plot with all analytes in the selected cluster on the x-axis,
#'   and the \code{passing_percentage} on the y-axis. The
#'   \code{cut_off_percentage} is indicated with a red horizontal dashed line.
#'   Analytes that passed curation are shown in blue, while analytes that didn't
#'   pass curation are shown in red.
#' @export
#'
#' @examples
#' data("long_data")
#' long_data <- curate_spectra(data = long_data,
#'                             min_ppm_deviation = -20,
#'                             max_ppm_deviation = 20,
#'                             max_ipq = 0.2,
#'                             min_sn = 9,
#'                             clusters_regex = "IgGI1",
#'                             group_to_filter = "Spike",
#'                             sample_type_to_filter = "CN")
#'
#' curated_spectra <- long_data %>%
#'    dplyr::filter(passed_curation == TRUE)
#'
#' curated_analytes <- curate_analytes(
#'                 data = curated_spectra,
#'                 groups_to_ignore = "Total",
#'                 sample_types_to_ignore = c("pool",
#'                                            "IVIGg",
#'                                            "CN",
#'                                            "Visucon",
#'                                            "PBS"),
#'                 cut_off_percentage = 25)
#'
#' plot_analyte_curation(curated_analytes = curated_analytes,
#'                       cut_off_percentage = 25,
#'                       selected_cluster = "IgGI1")
plot_analyte_curation <- function(curated_analytes, 
                                  cut_off_percentage, 
                                  selected_cluster) {
  
  data_to_plot <- curated_analytes %>% 
    dplyr::filter(cluster == selected_cluster)
  
  plot <- ggplot2::ggplot(data_to_plot) + 
    ggplot2::geom_col(ggplot2::aes(x = analyte, 
                                   y = passing_percentage,
                                   fill = passed_curation)) +
    ggplot2::scale_fill_discrete(name = "Passed curation?", 
                                 labels = c(`TRUE`= "Yes",
                                            `FALSE` = "No"),
                                 type = c(`TRUE` = "#3498DB",
                                          `FALSE` = "#E74C3C")) +
    ggplot2::geom_hline(yintercept = cut_off_percentage, 
                        linetype = "dashed",
                        color = "#E74C3C", 
                        size = 1) +
    ggplot2::facet_wrap(~ charge,
                        ncol = 1) +
    ggplot2::theme_classic() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, 
                                                       hjust = 1),
                   strip.background = ggplot2::element_rect(fill = "#F6F6F8"),
                   text = ggplot2::element_text(size = 16),
                   legend.position = "top") +
    ggplot2::xlab("Analyte") +
    ggplot2::scale_y_continuous(labels = function(x) paste0(x, "%"), 
                                name = "Proportion of passing spectra (%)") +
    ggpubr::border(size = 0.5)
  
  return(plot)
  
}

#' Create a datatable with all analytes that passed curation.
#'
#' This function can be used to show the results from the
#' \code{\link{curate_analytes}} function. It will create a datatable with all
#' analytes and their charge states that passed curation.
#' 
#' @param analyte_curated_data Your data after spectra curation (see
#'   \code{\link{curate_spectra}}), but filtered so that only analytes are
#'   included that passed curation with the \code{\link{curate_analytes}}
#'   function (see example below).
#' @param selected_cluster The cluster for which the analyte curation results
#'   should be shown.
#' @return A \code{\link[DT]{datatable}} with one row per analyte that passed
#'   curation, and one column per charge state present in the data to indicate
#'   whether that charge state of the analyte passed curation.
#' @export
#'
#' @examples
#' data("long_data")
#' long_data <- curate_spectra(data = long_data,
#'                             min_ppm_deviation = -20,
#'                             max_ppm_deviation = 20,
#'                             max_ipq = 0.2,
#'                             min_sn = 9,
#'                             clusters_regex = "IgGI1",
#'                             group_to_filter = "Spike",
#'                             sample_type_to_filter = "CN")
#'
#' curated_spectra <- long_data %>%
#'    dplyr::filter(passed_curation == TRUE)
#'
#' curated_analytes <- curate_analytes(
#'                 data = curated_spectra,
#'                 groups_to_ignore = "Total",
#'                 sample_types_to_ignore = c("pool",
#'                                            "IVIGg",
#'                                            "CN",
#'                                            "Visucon",
#'                                            "PBS"),
#'                 cut_off_percentage = 25)
#'
#' passing_analytes <- curated_analytes %>%
#'    dplyr::filter(passed_curation == TRUE) %>%
#'    dplyr::select(-passed_curation)
#'
#' analyte_curated_data <- dplyr::left_join(passing_analytes, curated_spectra)
#'
#' create_analyte_curation_table(analyte_curated_data = curated_analytes,
#'                               selected_cluster = "IgGI1")
create_analyte_curation_table <- function(analyte_curated_data, selected_cluster) {
  
  analyte_curation_dataframe <- analyte_curated_data %>% 
    dplyr::ungroup() %>% 
    dplyr::filter(cluster == selected_cluster) %>% 
    dplyr::select(analyte, charge) %>% 
    dplyr::distinct() %>% 
    tidyr::pivot_wider(names_from = charge, 
                       values_from = charge,
                       values_fn = function(value) dplyr::if_else(!is.na(value), 
                                                                  "Yes", 
                                                                  "No"),
                       values_fill = "No")
  
  analyte_curation_table <- DT::datatable(analyte_curation_dataframe) %>% 
    DT::formatStyle(columns = 2:ncol(analyte_curation_dataframe),
                    color = DT::styleEqual(levels = c("Yes", 
                                                      "No"), 
                                           values = c("#3498DB", 
                                                      "#E74C3C")))
  
  return(analyte_curation_table)
  
}
