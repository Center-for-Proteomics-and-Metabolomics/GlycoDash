#' Calculate fucosylation
#'
#' This function calculates the derived trait fucosylation based on the relative
#' abundances of glycans. This function is used in the function
#' \code{\link{calculate_derived_traits}}. It considers every analyte with "F"
#' followed by a single digit in its name to be a fucosylated analyte.
#'
#' @param .data A data frame / tibble. Can be passed to the function via a pipe
#'   (\code{\%>\%}) (see Examples below).
#'
#' @return A tibble with the columns \code{sample_name}, \code{cluster},
#'   \code{group}, \code{Fucosylation} and \code{fuc_formula}.
#' @export
#'
#' @examples
#' See usage in the function calculate_derived_traits.
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
      Fucosylation = sum(relative_abundance[analyte %in% fucosylated_analytes])) %>% 
    dplyr::distinct() %>% 
    dplyr::full_join(., formulas_per_cluster)
  
}

#' Calculate sialylation
#'
#' This function calculates the derived trait sialylation based on the relative
#' abundances of glycans. This function is used in the function
#' \code{\link{calculate_derived_traits}}. It considers analytes with "S1" in
#' their name to be half sialylated and analytes with "S2" in their name to be
#' fully sialylated.
#'
#' @inheritParams calculate_fucosylation
#'
#' @return A tibble with the columns \code{sample_name}, \code{cluster},
#'   \code{group}, \code{Sialylation} and \code{sial_formula}.
#' @export
#'
#' @examples
#' See usage in the function calculate_derived_traits.
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
                     Sialylation = (monosialylation * 1/2 + disialylation)) %>% 
    dplyr::select(-c(monosialylation, disialylation)) %>% 
    dplyr::distinct() %>% 
    dplyr::full_join(., formulas_per_cluster)
  
}

#' Calculate galactosylation
#' 
#' This function calculates the derived trait galactosylation based on the
#' relative abundances of glycans. This function is used in the function
#' \code{\link{calculate_derived_traits}}. It considers analytes with "H4" in
#' their name to be half galactosylated and analytes with "H5" in their name to be
#' fully galactosylated.
#'
#' @inheritParams calculate_fucosylation
#'
#' @return A tibble with the columns \code{sample_name}, \code{cluster},
#'   \code{group}, \code{Galactosylation} and \code{gal_formula}.
#' @export
#'
#' @examples
#' See usage in the function calculate_derived_traits.
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
                     Galactosylation = (monogalactosylation * 1/2 + digalactosylation)) %>% 
    dplyr::select(-c(monogalactosylation, digalactosylation)) %>% 
    dplyr::distinct() %>% 
    dplyr::full_join(., formulas_per_cluster)
}

#' Calculate bisection
#'
#' This function calculates the derived trait bisection based on the relative
#' abundances of glycans. This function is used in the function
#' \code{\link{calculate_derived_traits}}. Analytes with "N5" in their name are
#' considered to be bisected.
#'
#' @inheritParams calculate_fucosylation
#'
#' @return A tibble with the columns \code{sample_name}, \code{cluster},
#'   \code{group}, \code{Bisection} and \code{bis_formula}.
#' @export
#'
#' @examples
#' See usage in the function calculate_derived_traits.
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
    dplyr::summarise(Bisection = sum(relative_abundance[analyte %in% bisected_analytes])) %>% 
    dplyr::distinct() %>% 
    dplyr::full_join(., formulas_per_cluster)
  
}

#' Calculate derived glycosylation traits
#'
#' With this function derived glycosylation traits of IgG can be calculated
#' based on the measured relative abundances of glycans. This function can only
#' be used on IgG data, because it assumes that all glycans are diantennary. The
#' derived traits are calculated per cluster.
#'
#' @param data A dataframe that contains at least the columns "sample_name",
#'   "cluster", "relative_abundance" and "analyte".
#' @param selected_derived_traits A character vector containing the names of the
#'   derived traits that should be calculated. The derived traits that can be
#'   calculated with this function are: \describe{ \item{"Fucosylation"}{The
#'   percentage of detected glycans that is fucosylated.} \item{"Bisection"}{The
#'   percentage of detected glycans that contain a bisecting GlcNaC}
#'   \item{"Galactosylation"}{The extent to which the detected glycans are
#'   galactosylated. Monogalactosylated glycans are considered to be half
#'   galactosylated, while digalactosylated glycans are fully galactosylated.}
#'   \item{"Sialylation"}{The extent to which the detected glycans are
#'   sialylated. Monosialylated glycans are considered to be half sialylated,
#'   while disialylated glycans are fully sialylated.}}
#'
#' @return A tibble with the following columns:
#'   \describe{\item{sample_name}{The name of the measured
#'   sample.}\item{cluster}{The cluster of analytes that the derived traits were
#'   calculated for.}\item{group}{Only when there are both total and specific Ig
#'   samples in the data.}} In addition, for each derived trait given in the argument
#'   \code{selected_derived_traits} there is one column with the relative
#'   abundance of that derived trait (in percent) and one column with the formula used
#'   to calculate that derived trait.
#' @export
#'
#' @examples
#' # First spectra curation has to be performed:
#' data("example_data")
#'
#' example_data <- define_clusters(data = example_data,
#'                                 cluster_keywords = "IgGI")
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
#' cut_offs_total <- calculate_cut_offs(summarized_checks = summarized_checks,
#'                                      control_sample_types = "PBS",
#'                                      exclude_sample_types = NULL,
#'                                      group_keyword = "Total",
#'                                      percentile = 97,
#'                                      use_mean_SD = FALSE,
#'                                      SD_factor = NULL,
#'                                      uncalibrated_as_NA = TRUE)
#'
#' cut_offs_specific <- calculate_cut_offs(summarized_checks = summarized_checks,
#'                                         control_sample_types = "PBS",
#'                                         exclude_sample_types = NULL,
#'                                         group_keyword = "Spike",
#'                                         percentile = 97,
#'                                         use_mean_SD = FALSE,
#'                                         SD_factor = NULL,
#'                                         uncalibrated_as_NA = TRUE)
#'
#' cut_offs <- dplyr::full_join(cut_offs_total,
#'                              cut_offs_specific)
#'
#' curated_spectra <- curate_spectra(checked_data = checked_data,
#'                                   summarized_checks = summarized_checks,
#'                                   cut_offs = cut_offs)
#'
#' passing_spectra <- kick_out_spectra(curated_spectra = curated_spectra)
#'
#' for_analyte_curation <- remove_unneeded_columns(passing_spectra = passing_spectra)
#'
#' # Then analyte curation is performed:
#' without_samples_to_ignore <- throw_out_samples(
#'    passing_spectra = for_analyte_curation,
#'    samples_to_ignore = c("PBS", "Visucon", "IVIGg", "Total")
#' )
#'
#' checked_analytes <- check_analyte_quality_criteria(my_data = without_samples_to_ignore,
#'                                                    min_ppm_deviation = -20,
#'                                                    max_ppm_deviation = 20,
#'                                                    max_ipq = 0.2,
#'                                                    min_sn = 9,
#'                                                    criteria_to_consider = c("Mass accuracy",
#'                                                                             "S/N",
#'                                                                             "IPQ"))
#'
#' curated_analytes <- curate_analytes(checked_analytes = checked_analytes,
#'                                     cut_off_percentage = 25)
#'
#' analyte_curated_data <- dplyr::full_join(curated_analytes,
#'                                          for_analyte_curation) %>%
#'    dplyr::filter(has_passed_analyte_curation) %>%
#'    dplyr::select(-c(has_passed_analyte_curation, passing_percentage))
#'
#' # Then we calculate the total intensities for each analyte:
#' total_intensities <- calculate_total_intensity(analyte_curated_data)
#'
#' # And then we can perform total area normalization:
#' normalized_data <- normalize_data(total_intensities)
#'
#' calculate_derived_traits(normalized_data,
#'                         c("Fucosylation", "Bisection"))
calculate_derived_traits <- function(data, selected_derived_traits) {
  
  data <- data %>%
    dplyr::group_by(dplyr::across(dplyr::any_of(c("sample_name", 
                                                  "cluster", 
                                                  "group"))))
  
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
  
  to_join <- to_join[!sapply(to_join, is.null)]
  
  derived_traits <- purrr::reduce(to_join,
                                  dplyr::full_join) %>% 
    dplyr::ungroup()
  
  return(derived_traits)
}



#' create_expr_ls
#' 
#' Function to transform trait formula string into named list containing the 
#' expression of the right-hand side of the equation. The name is the left-hand
#' side of the equation. Used in the "calculate_custrom_trait" function.
#' Based on:
#' 
#' https://stackoverflow.com/questions/70821721/how-to-use-an-expression-in-dplyrmutate-in-r
create_expr_ls <- function(str_expr) {
  expr_nm <- stringr::str_extract(str_expr, "^\\w+")
  expr_code <- stringr::str_replace_all(str_expr, "(^\\w+\\s?=\\s?)(.*)", "\\2")
  rlang::set_names(list(str2lang(expr_code)), expr_nm)
}



#' Calculate custom derived glycosylation traits
#' 
#' Calculate custom derived traits of IgG based on formulas provided in an 
#' Excel file.
#' 
#'
#' @param normalized_data A dataframe that contains at least the columns
#' "analyte", "plate_well" and "relative_abundance". 
#' Entries in the column "analyte" must have the form <cluster>1<glycan composition>,
#' e.g.:
#' 
#' IgGII1H3N4F1.
#' @param custom_traits_formulas An Excel file loaded with the "load_excel" function
#' from the loadxl package. The Excel file contains formulas for the custom
#' derived traits. Each formula must be placed on a new row, in the first column. 
#' A formula must have the name of the trait on the left-hand side, 
#' and an expression on the right-hand side. E.g.:
#' 
#' my_trait = (0.5 * H3N4 + H4N4) / (H3N4F1 + H4N4F1)
#'
#' @return
#' A tibble with a separate row for each sample and cluster. Contains a column
#' for each glycan and its relative abundance, and a column for each calculated trait. 
#' 
#' @export
#'
#' @examples
calculate_custom_traits <- function(normalized_data, custom_traits_formulas){
  calculated_traits <- normalized_data %>% 
    # Separate analyte into cluster and glycan
    dplyr::select(-cluster) %>%   # remove existing cluster column
    tidyr::separate(analyte, sep = "1", into = c("cluster", "glycan"), 
                    extra = "merge", remove = TRUE) %>% 
    # Create column for each glycan with relative abundance as value
    tidyr::pivot_wider(names_from = "glycan", values_from = relative_abundance) 
  
  
  # Calculate traits per sample (plate well) and per cluster
  # Iterate through formulas provided in Excel file
  for (i in range(1:nrow(custom_traits_formulas))){
    # Get formula as string
    formula_string <- as.character(custom_traits_formulas[i, 1])
    # Convert to expression that can be used in dplyr mutate function
    formula_expr_ls <- create_expr_ls(formula_string)
    # Calculate trait per sample (plate well) and per cluster
    calculated_traits <- calculated_traits %>%
      dplyr::group_by(plate_well, cluster) %>%
      dplyr::mutate(!!! formula_expr_ls) %>% 
      dplyr::ungroup()
  }
  
  return(calculated_traits)
}
