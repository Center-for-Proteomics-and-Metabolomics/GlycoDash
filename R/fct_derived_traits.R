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
                            pattern = ifelse(
                              # Check if cluster name ends with "1" or not
                              test = grepl("1$", cluster),
                              yes = cluster,
                              no = paste0(cluster, "1")
                            ))
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
                                     pattern = ifelse(grepl("1$", cluster), cluster, paste0(cluster, "1"))
                                     ),
          di = stringr::str_subset(string = disialylated_analytes,
                                   pattern = ifelse(grepl("1$", cluster), cluster, paste0(cluster, "1")))
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
                                     pattern = ifelse(grepl("1$", cluster), cluster, paste0(cluster, "1"))),
          di = stringr::str_subset(string = digalactosylated_analytes,
                                   pattern = ifelse(grepl("1$", cluster), cluster, paste0(cluster, "1")))
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
                            pattern = ifelse(grepl("1$", cluster), cluster, paste0(cluster, "1")))
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
#' glycosylation traits are calculated per cluster.
#'
#' @param data A dataframe that contains at least the columns "sample_name",
#'   "cluster", "relative_abundance" and "analyte".
#' @param selected_derived_traits A character vector containing the names of the
#'   glycosylation traits that should be calculated. The glycosylation traits that can be
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
#'   sample.}\item{cluster}{The cluster of analytes that the glycosylation traits were
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
                                                  "group"))))  # "group" refers to Specific vs Total
  
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
#'
#'expr_rm extracts whatever comes before the "=" sign (spaces around the "=" sign do not matter)
#'
create_expr_ls <- function(str_expr) {
  expr_nm <- stringr::str_extract(str_expr, "^\\w+")
  expr_code <- stringr::str_replace_all(str_expr, "(^\\w+\\s?=\\s?)(.*)", "\\2")
  rlang::set_names(list(str2lang(expr_code)), expr_nm)
}




#' Calculate custom derived glycosylation traits
#' 
#' Calculate custom glycosylation traits of IgG based on formulas provided in an 
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
#' from the loadxl package. The first column in the Excel file contains the cluster.
#' The second column contains a trait to calculate for that cluster.
#' Each custom trait must be places on a new row. 
#' A formula must have the name of the trait on the left-hand side, 
#' and an expression on the right-hand side. E.g.:
#' 
#' my_trait = (0.5 * H3N4 + H4N4) / (H3N4F1 + H4N4F1)
#'
#' @return
#' A tibble with the following columns: sample_name, cluster, group, custom traits
#' and formulas used to calculate custom traits.
#' 
#' @export
#'
#' @examples fill this in...
calculate_custom_traits <- function(normalized_data, custom_traits_formulas){
  
  calculated_custom_traits <- normalized_data %>% 
    # Separate analyte into cluster and glycan
    dplyr::select(-cluster, -sum_intensity) %>% 
    tidyr::separate(analyte, sep = "1", into = c("cluster", "glycan"), 
                    extra = "merge", remove = TRUE) %>% 
    # Replace NA relative abundances by zero
    dplyr::mutate(relative_abundance = ifelse(is.na(relative_abundance), 0, relative_abundance)) %>% 
    # Create column for each glycan with relative abundance as value
    tidyr::pivot_wider(names_from = "glycan", values_from = relative_abundance) 
  
  
  
  # Calculate traits per sample (plate well) and per cluster
  # Iterate through formulas provided in Excel file
  
  # Create empty vector, will append column to names to select at the end
  columns_to_select <- c()
  
  # Another empty vector which will only contrain the names of the custom traits.
  # Is used to relocate columns.
  custom_traits_names <- c()
  
  # Start for-loop
  for (i in seq(1:nrow(custom_traits_formulas))){
    # Get cluster for which to calculate the trait
    cluster_specified <- as.character(custom_traits_formulas[i, 1])   # E.g.: IgGI
    
    # Get formula as string
    formula_string <- as.character(custom_traits_formulas[i, 2])      # E.g.:  first_trait = 0.5 * H4N4 + H5N4
    
    # Convert to expression that can be used in dplyr mutate function
    formula_expr_ls <- create_expr_ls(formula_string)
    
    # Get name of custom trait including cluster: <cluster>_<trait name>
    custom_trait_name <- paste(cluster_specified, names(formula_expr_ls)[1], sep = "_")  # E.g. IgGI_first_trait
    
    # Add trait names and formulas to "columns_to_select" (these will be the column names)
    columns_to_select <- append(columns_to_select, custom_trait_name)
    custom_traits_names <- append(custom_traits_names, custom_trait_name)
    formula_column_name <- paste(custom_trait_name, "formula", sep = "_")   # E.g IgGI_first_trait_formula
    columns_to_select <- append(columns_to_select, formula_column_name)

    # Calculate trait per sample, cluster has to match specified cluster
    # Gives a tibble with 3 columns: cluster, plate_well, <custom trait>, and the used formula.
    calculated_trait_cluster <- calculated_custom_traits %>%
      dplyr::filter(cluster == cluster_specified) %>%
      dplyr::mutate(!!! formula_expr_ls) %>%
      dplyr::select(sample_name:replicates, names(formula_expr_ls)[1]) %>%  # includes group if it exists
      # Change name of column <custom trait> to <cluster_specified>_<custom trait>
      dplyr::rename(!!custom_trait_name := names(formula_expr_ls)[1]) %>%
      # Add a column with the formula that was used to calculate the trait
      dplyr::mutate(
        !!paste(custom_trait_name, "formula", sep = "_") := formula_string
      )


    # Add to "calculated traits" data frame
    calculated_custom_traits <- calculated_custom_traits %>%
      dplyr::left_join(., calculated_trait_cluster)  # "by = " automatically

  }

  # Get "calculated_custom_traits" in correct format
  calculated_custom_traits <- calculated_custom_traits %>%
    dplyr::select(sample_name:replicates, any_of(columns_to_select)) %>% 
    dplyr::select(-cluster) %>% 
    dplyr::group_by(sample_name) %>% 
    tidyr::fill(tidyr::everything(), .direction = "downup") %>% 
    dplyr::ungroup() %>% 
    dplyr::distinct() %>% 
    dplyr::relocate(all_of(custom_traits_names), .after = replicates)

  # Return calculated custom traits tibble
  return(calculated_custom_traits)
}
