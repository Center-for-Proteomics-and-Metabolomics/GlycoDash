#' Calculate fucosylation
#'
#' This function calculates the derived trait fucosylation based on the relative
#' abundances of glycans. This function is used in the function
#' \code{\link{calculate_derived_traits}}.
#'
#' @param .data A data frame. Can be passed to the function via a pipe (%>%)
#'   (see Examples below).
#'
#' @return
#' @export
#'
#' @examples
#' example_data <- data.frame(
#'                    relative_abundance = c(0.45, 0.15, 0.03, 0.37),
#'                    analyte = c("H5N4F1", "H4N3F1", "H3N4S1", "H3N4F1S1"),
#'                    sum_all_analytes = rep(1, 4))
#'
#' example_data %>% calculate_fucosylation(.)
#' 
calculate_fucosylation <- function(.data) {
  
  fucosylated_analytes <- stringr::str_subset(string = unique(.data$analyte),
                                              pattern = "F\\d")
  
  formulas_per_cluster <- rlang::set_names(unique(.data$cluster)) %>% 
    purrr::map(
      .,
      function(cluster) {
        stringr::str_subset(string = fucosylated_analytes,
                            pattern = cluster)
      }) %>% 
    purrr::imap_dfc(
      .,
      function(analytes, name) {
        paste0("(",
               paste(analytes, collapse = " + "), 
               ") / (", 
               paste(unique(.data[.data$cluster == name, ]$analyte), collapse = " + "),
               ")") 
      }) %>% 
    tidyr::pivot_longer(cols = tidyselect::everything(),
                        names_to = "cluster",
                        values_to = "fuc_formula")
  
  .data %>% 
    dplyr::summarise(
      Fucosylation = sum(relative_abundance[analyte %in% fucosylated_analytes]) / sum_all_analytes) %>% 
    dplyr::distinct() %>% 
    dplyr::full_join(., formulas_per_cluster)
  
}

#' Calculate sialylation
#' 
#' This function calculates the derived trait sialylation based on the
#' relative abundances of glycans. This function is used in the function
#' \code{\link{calculate_derived_traits}}.
#'
#' @inheritParams calculate_fucosylation 
#'
#' @return
#' @export
#'
#' @examples
#' example_data <- data.frame(
#'                    relative_abundance = c(0.45, 0.15, 0.03, 0.37),
#'                    analyte = c("H5N4F1", "H4N3F1", "H3N4S1", "H3N4F1S2"),
#'                    sum_all_analytes = rep(1, 4))
#'                    
#' example_data %>% calculate_sialylation(.)
calculate_sialylation <- function(.data) {
  
  monosialylated_analytes <- stringr::str_subset(string = unique(.data$analyte),
                                                 pattern = "S1")
  disialylated_analytes <- stringr::str_subset(string = unique(.data$analyte),
                                               pattern = "S2")
  
  formulas_per_cluster <- rlang::set_names(unique(.data$cluster)) %>% 
    purrr::map(
      .,
      function(cluster) {
        list(
          mono = stringr::str_subset(string = monosialylated_analytes,
                                     pattern = cluster),
          di = stringr::str_subset(string = disialylated_analytes,
                                   pattern = cluster)
        )
      }) %>% 
    purrr::imap_dfc(
      .,
      function(analytes, name) {
        paste0("((",
               paste(analytes$mono, collapse = " + "),
               ") * 1/2 + (",
               paste(analytes$di, collapse = " + "),
               ")) / (", 
               paste(unique(.data[.data$cluster == name, ]$analyte), collapse = " + "),
               ")")
      }) %>% 
    tidyr::pivot_longer(cols = tidyselect::everything(),
                        names_to = "cluster",
                        values_to = "sial_formula")
  
  .data %>% 
    dplyr::summarise(monosialylation = sum(relative_abundance[analyte %in% monosialylated_analytes]),
                     disialylation = sum(relative_abundance[analyte %in% disialylated_analytes]),
                     Sialylation = (monosialylation * 1/2 + disialylation) / sum_all_analytes) %>% 
    dplyr::select(-c(monosialylation, disialylation)) %>% 
    dplyr::distinct() %>% 
    dplyr::full_join(., formulas_per_cluster)
  
}

#' Calculate galactosylation
#' 
#' This function calculates the derived trait galactosylation based on the
#' relative abundances of glycans. This function is used in the function
#' \code{\link{calculate_derived_traits}}.
#'
#' @inheritParams calculate_fucosylation
#'
#' @return
#' @export
#'
#' @examples
#' example_data <- data.frame(
#'                    relative_abundance = c(0.45, 0.15, 0.03, 0.37),
#'                    analyte = c("H5N4F1", "H4N3F1", "H3N4S1", "H3N4F1S2"),
#'                    sum_all_analytes = rep(1, 4))
#'                    
#' example_data %>% calculate_galactosylation(.)
#' 
calculate_galactosylation <- function(.data) {
  
  monogalactosylated_analytes <- stringr::str_subset(string = unique(.data$analyte),
                                                     pattern = "H4")
  digalactosylated_analytes <- stringr::str_subset(string = unique(.data$analyte),
                                                   pattern = "H5")
  
  formulas_per_cluster <- rlang::set_names(unique(.data$cluster)) %>% 
    purrr::map(
      .,
      function(cluster) {
        list(
          mono = stringr::str_subset(string = monogalactosylated_analytes,
                                     pattern = cluster),
          di = stringr::str_subset(string = digalactosylated_analytes,
                                   pattern = cluster)
        )
      }) %>% 
    purrr::imap_dfc(
      .,
      function(analytes, name) {
        paste0("((",
               paste(analytes$mono, collapse = " + "),
               ") * 1/2 + (",
               paste(analytes$di, collapse = " + "),
               ")) / (", 
               paste(unique(.data[.data$cluster == name, ]$analyte), collapse = " + "),
               ")")
      }) %>% 
    tidyr::pivot_longer(cols = tidyselect::everything(),
                        names_to = "cluster",
                        values_to = "gal_formula")
  
  .data %>% 
    dplyr::summarise(monogalactosylation = sum(relative_abundance[analyte %in% monogalactosylated_analytes]),
                     digalactosylation = sum(relative_abundance[analyte %in% digalactosylated_analytes]),
                     Galactosylation = (monogalactosylation * 1/2 + digalactosylation) / sum_all_analytes) %>% 
    dplyr::select(-c(monogalactosylation, digalactosylation)) %>% 
    dplyr::distinct() %>% 
    dplyr::full_join(., formulas_per_cluster)
}

#' Calculate bisection
#' 
#' This function calculates the derived trait bisection based on the
#' relative abundances of glycans. This function is used in the function
#' \code{\link{calculate_derived_traits}}.
#'
#' @inheritParams calculate_fucosylation
#'
#' @return
#' @export
#'
#' @examples
#' example_data <- data.frame(
#'                    relative_abundance = c(0.45, 0.15, 0.03, 0.37),
#'                    analyte = c("H5N4F1", "H4N3F1", "H3N4S1", "H3N4F1S1"),
#'                    sum_all_analytes = rep(1, 4))
#'                    
#' example_data %>% calculate_bisection(.)
#' 
calculate_bisection <- function(.data) {
  
  bisected_analytes <- stringr::str_subset(string = unique(.data$analyte),
                                           pattern = "N5")
  
  formulas_per_cluster <- rlang::set_names(unique(.data$cluster)) %>% 
    purrr::map(
      .,
      function(cluster) {
        stringr::str_subset(string = bisected_analytes,
                            pattern = cluster)
      }) %>% 
    purrr::imap_dfc(
      .,
      function(analytes, name) {
        paste0("(",
               paste(analytes, collapse = " + "), 
               ") / (", 
               paste(unique(.data[.data$cluster == name, ]$analyte), collapse = " + "),
               ")") 
      }) %>% 
    tidyr::pivot_longer(cols = tidyselect::everything(),
                        names_to = "cluster",
                        values_to = "bis_formula")
  
  .data %>% 
    dplyr::summarise(Bisection = sum(relative_abundance[analyte %in% bisected_analytes]) / sum_all_analytes) %>% 
    dplyr::distinct() %>% 
    dplyr::full_join(., formulas_per_cluster)
  
}

#' Calculate derived glycosylation traits
#'
#' With this function derived glycosylation traits of IgG can be calculated
#' based on the measured relative abundances of glycans. This function can only
#' be used on IgG data, because it assumes that all glycans are diantennary.
#'
#' @param data A dataframe that contains at least the columns "sample_name", "cluster",
#'   "relative_abundance" and "analyte".
#' @param selected_derived_traits A character vector containing the names of the derived
#'   traits that should be calculated. The derived traits that can be
#'   calculated with this function are: \describe{ \item{"Fucosylation"}{The
#'   proportion of detected glycans that is fucosylated.} \item{"Bisection"}{The
#'   proportion of detected glycans that contain a bisecting GlcNaC}
#'   \item{"Galactosylation"}{} \item{"Sialylation"}{}}
#'
#' @return A dataframe...
#' @export
#'
#' @examples
#' example_data <- data.frame(
#'                    sample_name = c("sample01", "sample02", "sample02", "sample04"),
#'                    group = c("Spike", "Total", "Spike", "Total"),
#'                    cluster = c("C1", "C1", "C1", "C2"),
#'                    analyte = c("H5N4F1", "H4N3F1", "H3N4S1", "H3N4F1S1"),
#'                    relative_abundance = c(0.3, 0.8, 0.2, 0.3))
#'                    
#' calculate_derived_traits(data = example_data,
#'                          selected_derived_traits = c("Galactosylation",
#'                                                      "Bisection"))
calculate_derived_traits <- function(data, selected_derived_traits) {
  
  data <- data %>%
    dplyr::group_by(dplyr::across(dplyr::any_of(c("sample_name", 
                                                  "cluster", 
                                                  "group")))) %>% 
    dplyr::summarize(sum_all_analytes = sum(relative_abundance),
                     across(),
                     .groups = "keep")
  
  if("Fucosylation" %in% selected_derived_traits) {
    Fucosylation <- data %>%
      calculate_fucosylation(.)
  }
  
  if("Sialylation" %in% selected_derived_traits) {
    Sialylation <- data %>%
      calculate_sialylation(.)
  }
  
  if("Galactosylation" %in% selected_derived_traits) {
    Galactosylation <- data %>%
      calculate_galactosylation(.)
  }
  
  if("Bisection" %in% selected_derived_traits) {
    Bisection <- data %>%
      calculate_bisection(.)
  }
  
  to_join <- purrr::map(selected_derived_traits,
                        function(trait) {
                          get0(trait)
                        })
  
  # create an error for when to_join is empty
  
  to_join <- to_join[!sapply(to_join, is.null)]
  
  derived_traits <- purrr::reduce(to_join,
                                  dplyr::full_join)
  
  return(derived_traits)
}
