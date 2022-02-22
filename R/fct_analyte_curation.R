#' Perform analyte curation
#'
#' @param data The result of the \code{\link{spectra_curation}} function, but
#'   filtered so that only spectra that passed curation remain (see examples
#'   below).
#' @param groups_to_ignore A character string. Specify if you don't want to base
#'   analyte curation on samples from one of the groups (total Ig or specific
#'   Ig).
#' @param sample_types_to_ignore A character vector with all sample types that
#'   you don't want to base analyte curation on (e.g. standards).
#' @param cut_off_percentage The minimum percentage of spectra in which an
#'   analyte needs to fulfill the quality criteria in order for that analyte to
#'   pass curation.
#'
#' @return A dataframe with one row for each combination of analyte and charge
#'   in each cluster, a column named "passing_percentage" with the percentage of
#'   spectra that pass the analyte quality criteria for that analyte and a
#'   logical column named "passed_curation" that is \code{TRUE} for analytes
#'   with a passing_percentage above the cut_off_percentage and \code{FALSE} for
#'   analytes with a passing_percentage under or equal to the cut_off_percentage
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
#'                            sample_type_to_filter = "CN")
#'
#' curated_spectra <- long_data %>%
#'    dplyr::filter(passed_curation == TRUE)
#'
#' curate_analytes(data = curated_spectra,
#'                 groups_to_ignore = "Total",
#'                 sample_types_to_ignore = c("pool", "IVIGg", "CN", "Visucon", "PBS"),
#'                 cut_off_percentage = 25)
#'                 
curate_analytes <- function(data, groups_to_ignore, sample_types_to_ignore, cut_off_percentage) {
  
  curated_analytes <- data %>% 
    dplyr::filter(!(group %in% groups_to_ignore) & !(sample_type %in% sample_types_to_ignore)) %>% 
    dplyr::group_by(cluster, charge, analyte) %>% 
    dplyr::summarise(passing_percentage = sum(criteria_check) / dplyr::n() * 100) %>% 
    dplyr::mutate(passed_curation = dplyr::if_else(passing_percentage > cut_off_percentage, 
                                                   TRUE,
                                                   FALSE))
  
  return(curated_analytes)
}

#' Create a visual summary of the analyte curation process
#'
#' @param curated_analytes The result of the \code{\link{curate_analytes}}
#'   function: A dataframe with all analytes and charge combinations in the
#'   data, their percentage of passing spectra and whether or not they passed
#'   analyte curation.
#' @inheritParams curate_analytes
#'
#' @return
#' @export
#'
#' @examples
plot_analyte_curation <- function(curated_analytes, cut_off_percentage, selected_cluster) {
  
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

#' Create a datatable with all analytes that passed curation
#'
#' @param analyte_curated_data Your data filtered so that only the analytes that
#'   passed curation are included.
#'
#' @return
#' @export
#'
#' @examples
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
