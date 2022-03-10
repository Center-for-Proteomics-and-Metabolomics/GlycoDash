#' Calculate fucosylation
#'
#' This function calculates the derived trait fucosylation based on the
#' relative abundances of glycans. This function is used in the function
#' \code{\link{calculate_derived_traits}}.
#'
#' @param .data
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
#' calculate_fucosylation(example_data)
#' 
calculate_fucosylation <- function(.data) {
  
  fucosylated_analytes <- stringr::str_subset(string = unique(.data$analyte),
                                              pattern = "F\\d")
  .data %>% 
    dplyr::summarise(
      Fucosylation = sum(relative_abundance[analyte %in% fucosylated_analytes]) / sum_all_analytes) %>% 
    dplyr::distinct()
  
}

#' Calculate sialylation
#' 
#' This function calculates the derived trait sialylation based on the
#' relative abundances of glycans. This function is used in the function
#' \code{\link{calculate_derived_traits}}.
#'
#' @param .data 
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
#' calculate_sialylation(example_data)
calculate_sialylation <- function(.data) {
  
  monosialylated_analytes <- stringr::str_subset(string = unique(.data$analyte),
                                                 pattern = "S1")
  disialylated_analytes <- stringr::str_subset(string = unique(.data$analyte),
                                               pattern = "S2")
  
  .data %>% 
    dplyr::summarise(monosialylation = sum(relative_abundance[analyte %in% monosialylated_analytes]),
                     disialylation = sum(relative_abundance[analyte %in% disialylated_analytes]),
                     Sialylation = (monosialylation * 1/2 + disialylation) / sum_all_analytes) %>% 
    dplyr::select(-c(monosialylation, disialylation)) %>% 
    dplyr::distinct()
  
}

#' Calculate galactosylation
#' 
#' This function calculates the derived trait galactosylation based on the
#' relative abundances of glycans. This function is used in the function
#' \code{\link{calculate_derived_traits}}.
#'
#' @param .data 
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
#' calculate_galactosylation(example_data)
#' 
calculate_galactosylation <- function(.data) {
  
  monogalactosylated_analytes <- stringr::str_subset(string = unique(.data$analyte),
                                                     pattern = "H4")
  digalactosylated_analytes <- stringr::str_subset(string = unique(.data$analyte),
                                                   pattern = "H5")
  
  .data %>% 
    dplyr::summarise(monogalactosylation = sum(relative_abundance[analyte %in% monogalactosylated_analytes]),
                     digalactosylation = sum(relative_abundance[analyte %in% digalactosylated_analytes]),
                     Galactosylation = (monogalactosylation * 1/2 + digalactosylation) / sum_all_analytes) %>% 
    dplyr::select(-c(monogalactosylation, digalactosylation)) %>% 
    dplyr::distinct()
}

#' Calculate bisection
#' 
#' This function calculates the derived trait bisection based on the
#' relative abundances of glycans. This function is used in the function
#' \code{\link{calculate_derived_traits}}.
#'
#' @param .data 
#' @param analytes 
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
#' calculate_bisection(example_data)
#' 
calculate_bisection <- function(.data, analytes) {
  
  bisected_analytes <- stringr::str_subset(string = unique(.data$analyte),
                                           pattern = "N5")
  .data %>% 
    dplyr::summarise(Bisection = sum(relative_abundance[analyte %in% bisected_analytes]) / sum_all_analytes) %>% 
    dplyr::distinct()
  
}

#' Calculate derived glycosylation traits
#'
#' With this function derived glycosylation traits of IgG can be calculated
#' based on the measured relative abundances of glycans. This function can only
#' be used on IgG data, because it assumes that all glycans are diantennary.
#'
#' @param data A dataframe that contains at least the columns "sample_name", "cluster",
#'   "relative_abundance" and "analyte".
#' @param derived_traits A character vector containing the names of the derived
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
#'                    sum_all_analytes = rep(1, 4))
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
