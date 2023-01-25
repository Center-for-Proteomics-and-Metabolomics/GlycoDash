# This file contains all functions that are used within the module
# mod_analyte_curation.R and within its sub-module mod_tab_curated_analytes.R.

#' Read an analyte list Excel or .rds file
#'
#' @param filepath The path to the Excel or .rds file containing a list of
#'   analytes. If it is an Excel file it should contain only one sheet. If it's
#'   a .rds file it should be a dataframe or tibble. In both cases there should
#'   be one column named "analyte". The analyte column should contain all
#'   analytes that should pass curation.
#' @param filename The name of the Excel or .rds file including the file
#'   extension.
#'
#' @return A tibble with one column named "analyte" with the analytes that
#'   should pass curation.
#' @export
#'
#' @examples
#' path <- system.file("extdata",
#'                     "Analyte_list.xlsx",
#'                     package = "glycodash")
#'
#' read_analyte_list_file(filepath = path,
#'                       filename = "Analyte_list.xlsx")
#' 
read_analyte_list_file <- function(filepath, filename) {
  
  extension <- tools::file_ext(filename)
  
  if (extension == "rds") {
    analyte_list <- load_and_assign(filepath)
  } else { if (extension %in% c("xlsx", "xls")) {
    analyte_list <- readxl::read_excel(filepath, 
                                       col_names = TRUE)
  } else {
    rlang::abort(class = "wrong_extension",
                 message = "Please upload a .xlsx, .xls or .rds file.")
  }
  }
  
  required_columns <- c("analyte")
  
  missing_columns <- required_columns[!(required_columns %in% colnames(analyte_list))]
  
  if (!rlang::is_empty(missing_columns)) {
    rlang::abort(class = "missing_columns",
                 message = paste("The column(s)",
                                 comma_and(missing_columns),
                                 "could not be found. Please name the column in your Excel file",
                                 "\"analyte\"."
                 ))
  }
  
  return(analyte_list)
}

#' Filter out samples to ignore during analyte curation
#'
#' With this function samples you can filter out samples that you don't want to
#' base the analyte curation on.
#'
#' @param passing_spectra The return value from the function
#'   \code{\link{curate_spectra}}, but filtered so that the dataframe contains
#'   only the rows for which has_passed_curation == TRUE.
#' @param samples_to_ignore A character vector in which element is either a
#'   group (specific or total) or sample_type that is present in the data
#'   followed by " samples".
#'
#' @return
#' @export
#'
#' @examples
#' 
throw_out_samples <- function(passing_spectra,
                              samples_to_ignore) {
  
  samples_to_ignore <- stringr::str_remove(samples_to_ignore,
                                           " samples")
  
  if (!is.factor(passing_spectra$sample_type)) {
    rlang::abort(message = "sample_type is not a factor")
  }
  
  sample_types_to_ignore <- samples_to_ignore[samples_to_ignore %in% levels(passing_spectra$sample_type)]
  
  if ("group" %in% colnames(passing_spectra)) {
    if (!is.factor(passing_spectra$group)) {
      rlang::abort(message = "group is not a factor")
    }
    groups_to_ignore <- samples_to_ignore[samples_to_ignore %in% levels(passing_spectra$group)]
  } else {
    groups_to_ignore <- vector()
  }
  
  without_samples_to_ignore <- passing_spectra %>% 
    dplyr::filter(if (!rlang::is_empty(groups_to_ignore)) !(group %in% groups_to_ignore) else TRUE,
                  if (!rlang::is_empty(sample_types_to_ignore)) !(sample_type %in% sample_types_to_ignore) else TRUE)
  
  return(without_samples_to_ignore)
}

#' Perform analyte curation
#'
#' Curate analytes based on the percentage of spectra in which the analyte
#' passes the quality criteria. Before this function is used, the function
#' \code{\link{check_analyte_quality_criteria}} should be used to check the
#' analyte quality criteria for each analyte in each sample. Analyte curation should not be
#' based on spectra for which you don't expect any analytes to pass, or for
#' which you expect all analytes to pass. The arguments
#' \code{sample_types_to_ignore} and \code{group_to_ignore} should be used to
#' indicate which spectra should not be used as basis for analyte curation. This
#' function should be used only after spectra curation has been performed with
#' \code{\link{curate_spectra}} and after the spectra that did not pass spectra
#' curation have been filtered out (see example below).
#'
#' @param checked_analytes The result of the
#'   \code{\link{check_analyte_quality_criteria}} function.
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
#' data("example_data")
#' example_data <- curate_spectra(data = example_data,
#'                             min_ppm_deviation = -20,
#'                             max_ppm_deviation = 20,
#'                             max_ipq = 0.2,
#'                             min_sn = 9,
#'                             clusters_regex = "IgGI1",
#'                             cut_off_basis = c("Spike PBS", "Total PBS"))
#'
#' curated_spectra <- example_data$curated_data %>%
#'    dplyr::filter(has_passed_spectra_curation == TRUE) %>%
#'    dplyr::select(-has_passed_spectra_curation)
#'
#' curate_analytes(data = curated_spectra,
#'                 group_to_ignore = "Total",
#'                 sample_types_to_ignore = c("Visucon",
#'                                            "PBS"),
#'                 cut_off_percentage = 25)
#'                 
curate_analytes <- function(checked_analytes, cut_off_percentage) {
  
  required_columns <- c("cluster", 
                        "charge", 
                        "analyte", 
                        "analyte_meets_criteria")
  
  missing_columns <- required_columns[!(required_columns %in% colnames(checked_analytes))] 
  if(!rlang::is_empty(missing_columns)) {
    rlang::abort(class = "missing_columns",
                 message = paste("The required column(s)",
                                 missing_columns,
                                 "are not present in the data.",
                                 "Attention: curate_analytes() can only be used after spectra curation has been performed with curate_spectra()"))
  }
  
  curated_analytes <- checked_analytes %>% 
    dplyr::group_by(cluster, charge, analyte) %>% 
    dplyr::summarise(passing_percentage = sum(analyte_meets_criteria) / dplyr::n() * 100) %>% 
    dplyr::mutate(passed_curation = dplyr::if_else(passing_percentage > cut_off_percentage, 
                                                   TRUE,
                                                   FALSE))
  
  return(curated_analytes)
}

#' Analyte curation based on a list
#'
#' @inheritParams curate_analytes
#' @param analyte_list The list of analytes that should pass the analyte
#'   curation process. This can be a list with character strings, a character
#'   vector or a dataframe consisting of a single column with character strings.
#'
#' @return This function returns the same dataframe that was given as the
#'   \code{data} argument, but filtered so that only those analytes that are in
#'   \code{analyte_list} remain.
#'   
#' @export
#'
#' @examples
#' data("example_data")
#' example_data <- curate_spectra(data = example_data,
#'                             min_ppm_deviation = -20,
#'                             max_ppm_deviation = 20,
#'                             max_ipq = 0.2,
#'                             min_sn = 9,
#'                             clusters_regex = "IgGI1",
#'                             cut_off_basis = c("Spike PBS", "Total PBS"))
#'
#' curated_spectra <- example_data$curated_data %>%
#'    dplyr::filter(has_passed_spectra_curation == TRUE) %>% 
#'    dplyr::select(-has_passed_spectra_curation)
#'
#' analyte_list_file <- system.file("extdata",
#'                                  "Analyte_list.xlsx",
#'                                  package = "glycodash")
#'
#' analyte_list <- readxl::read_excel(analyte_list_file)
#'
#' curate_analytes_with_list(data = curated_spectra,
#'                           analyte_list = analyte_list)
#'                           
curate_analytes_with_list <- function(data,
                                      analyte_list) {
  
  if (is.data.frame(analyte_list)) {
    if (ncol(analyte_list) > 1) {
      rlang::abort(class = "too_many_columns",
                   message = "The Excel file (or R dataframe) should contain only one column.")
    }
    analyte_list <- analyte_list[[1]]
  }
  
  missing_analytes <- analyte_list[!(analyte_list %in% data$analyte)]
    
  if (!rlang::is_empty(missing_analytes)) {
    if (!identical(missing_analytes, analyte_list[1])) {
      rlang::warn(class = "missing_analytes",
                  message = paste("The analyte(s)", 
                                  paste(missing_analytes, 
                                        collapse = "and"),
                                  "from the analyte list are not present in",
                                  "the \"analyte\" column of the data."))
    }
  }
  
  analytes_to_include <- purrr::map(
    analyte_list,
    function(analyte) {
      stringr::str_subset(string = unique(data$analyte),
                          pattern = paste0("^",
                                           analyte, 
                                           "$"))
    }) %>% 
    unlist(.)
  
  analyte_curated_data <- data %>% 
    dplyr::filter(analyte %in% analytes_to_include) %>% 
    dplyr::mutate(passed_curation = TRUE)
  
  return(analyte_curated_data)
  
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
#' data("example_data")
#' example_data <- curate_spectra(data = example_data,
#'                             min_ppm_deviation = -20,
#'                             max_ppm_deviation = 20,
#'                             max_ipq = 0.2,
#'                             min_sn = 9,
#'                             clusters_regex = "IgGI1",
#'                             cut_off_basis = c("Spike PBS", "Total PBS"))
#'
#' curated_spectra <- example_data$curated_data %>%
#'    dplyr::filter(has_passed_spectra_curation == TRUE) %>% 
#'    dplyr::select(-has_passed_spectra_curation)
#'
#' curated_analytes <- curate_analytes(
#'                 data = curated_spectra,
#'                 group_to_ignore = "Total",
#'                 sample_types_to_ignore = c("Visucon",
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
    dplyr::filter(cluster == selected_cluster) %>% 
    dplyr::mutate(
      `Passed curation?` = dplyr::case_when(
        passed_curation == "TRUE" ~ "Yes",
        passed_curation == "FALSE" ~ "No"
      ))
  
  plot <- ggplot2::ggplot(data_to_plot) + 
    ggplot2::geom_col(ggplot2::aes(x = analyte, 
                                   y = passing_percentage,
                                   fill = `Passed curation?`,
                                   text = paste(
                                     "Analyte:",
                                     analyte,
                                     "\nPercentage of passing spectra:",
                                     paste0(signif(passing_percentage,
                                                   4), 
                                            "%")
                                   ))) +
    ggplot2::scale_fill_discrete(type = c("Yes" = "#3498DB",
                                          "No" = "#E74C3C")) +
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
                   #text = ggplot2::element_text(size = 16),
                   legend.position = "top",
                   panel.border = ggplot2::element_rect(colour = "black", fill=NA, size=0.5)) +
    ggplot2::xlab("Analyte") +
    ggplot2::scale_y_continuous(labels = function(x) paste0(x, "%"), 
                                name = "Proportion of passing spectra (%)")
  
  return(plot)
  
}

#' Create a datatable with all analytes that passed curation.
#'
#' This function can be used to show the results from the
#' \code{\link{curate_analytes}} function. It will create a datatable with all
#' analytes and their charge states that passed curation.
#'
#' @param dataframe_for_table The result of the
#'   \code{\link{prepare_analyte_curation_table}} function.
#' @return A \code{\link[DT]{datatable}} with one row per analyte that passed
#'   curation, and one column per charge state present in the data to indicate
#'   whether that charge state of the analyte passed curation.
#' @export
#'
#' @examples
#' data("example_data")
#' example_data <- curate_spectra(data = example_data,
#'                             min_ppm_deviation = -20,
#'                             max_ppm_deviation = 20,
#'                             max_ipq = 0.2,
#'                             min_sn = 9,
#'                             clusters_regex = "IgGI1",
#'                             cut_off_basis = c("Spike PBS", "Total PBS"))
#'
#' curated_spectra <- example_data$curated_data %>%
#'    dplyr::filter(has_passed_spectra_curation == TRUE) %>% 
#'    dplyr::select(-has_passed_spectra_curation)
#'
#' curated_analytes <- curate_analytes(
#'                 data = curated_spectra,
#'                 group_to_ignore = "Total",
#'                 sample_types_to_ignore = c("Visucon",
#'                                            "PBS"),
#'                 cut_off_percentage = 25)
#'
#' passing_analytes <- curated_analytes %>%
#'    dplyr::filter(passed_curation == TRUE) %>%
#'    dplyr::select(-passed_curation)
#'
#' analyte_curated_data <- dplyr::left_join(passing_analytes, curated_spectra)
#' 
#' dataframe <- prepare_analyte_curation_table(analyte_curated_data = curated_analytes,
#'                                             selected_cluster = "IgGI1")
#'
#' create_analyte_curation_table(dataframe_for_table = dataframe)
#' 
create_analyte_curation_table <- function(dataframe_for_table) {
  
  charge_columns <- stringr::str_subset(colnames(dataframe_for_table)[-1],
                                        "Include",
                                        negate = TRUE)
  
  new_charge_column_names <- purrr::map(charge_columns,
                                        ~ paste(.x,
                                                "charge state passed curation?"))
  
  name_pairs <- rlang::set_names(charge_columns,
                                 new_charge_column_names)
  
  DT::datatable(
    dataframe_for_table,
    escape = FALSE,
    selection = "none",
    colnames = name_pairs,
    options = list(
      searching = FALSE,
      paging = FALSE,
      preDrawCallback = DT::JS('function() {
Shiny.unbindAll(this.api().table().node()); }'),
drawCallback = DT::JS('function() {
Shiny.bindAll(this.api().table().node()); } ')
    )
  ) %>%
    DT::formatStyle(columns = 2:ncol(dataframe_for_table),
                    color = DT::styleEqual(levels = c("Yes", 
                                                      "No"), 
                                           values = c("#3498DB", 
                                                      "#E74C3C")))
  
}

#' Prepare a dataframe for the analyte curation table
#'
#' @param analyte_curated_data Your data after spectra curation (see
#'   \code{\link{curate_spectra}}), but filtered so that only analytes are
#'   included that passed curation with the \code{\link{curate_analytes}}
#'   function (see example below).
#' @param selected_cluster The cluster for which the analyte curation results
#'   should be shown.
#'
#' @return This function returns a dataframe that can be passed as the
#'   \code{dataframe_for_table} argument to the
#'   \code{\link{create_analyte_curation_table}} function.
#' @export
#'
#' @examples
#' data("example_data")
#' example_data <- curate_spectra(data = example_data,
#'                             min_ppm_deviation = -20,
#'                             max_ppm_deviation = 20,
#'                             max_ipq = 0.2,
#'                             min_sn = 9,
#'                             clusters_regex = "IgGI1",
#'                             cut_off_basis = c("Spike PBS", "Total PBS"))
#'
#' curated_spectra <- example_data$curated_data %>%
#'    dplyr::filter(has_passed_spectra_curation == TRUE) %>% 
#'    dplyr::select(-has_passed_spectra_curation)
#'
#' curated_analytes <- curate_analytes(
#'                 data = curated_spectra,
#'                 group_to_ignore = "Total",
#'                 sample_types_to_ignore = c("Visucon",
#'                                            "PBS"),
#'                 cut_off_percentage = 25)
#'
#' passing_analytes <- curated_analytes %>%
#'    dplyr::filter(passed_curation == TRUE) %>%
#'    dplyr::select(-passed_curation)
#'
#' analyte_curated_data <- dplyr::left_join(passing_analytes, curated_spectra)
#' 
#' dataframe <- prepare_analyte_curation_table(analyte_curated_data = curated_analytes,
#'                                             selected_cluster = "IgGI1")
#'                                             
prepare_analyte_curation_table <- function(analyte_curated_data, selected_cluster) {
  
  analyte_curation_dataframe <- analyte_curated_data %>% 
    dplyr::ungroup() %>% 
    dplyr::filter(cluster == selected_cluster) %>% 
    dplyr::select(analyte, charge, passed_curation) %>% 
    dplyr::distinct() %>% 
    tidyr::pivot_wider(names_from = charge,
                       values_from = passed_curation,
                       values_fn = function(value) dplyr::if_else(isTRUE(value),
                                                                  "Yes",
                                                                  "No"),
                       values_fill = "No")
  
  return(analyte_curation_dataframe)
}

