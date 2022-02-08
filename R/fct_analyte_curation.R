#' Perform analyte curation
#'
#' @param data 
#' @param groups_to_ignore 
#' @param sample_types_to_ignore 
#' @param cut_off_percentage 
#'
#' @return
#' @export
#'
#' @examples
curate_analytes <- function(data, groups_to_ignore, sample_types_to_ignore, cut_off_percentage) {
  
  # --> Need to rewrite functions in the spectra_curation module so that we don't have
  # to reperform the criteria check!
  
  curated_analytes <- data %>% 
    dplyr::mutate(criteria_check = do_criteria_check(data = data,
                                                     min_ppm_deviation = -20,
                                                     max_ppm_deviation = 20,
                                                     max_ipq = 0.2,
                                                     min_sn = 9)) %>% 
    dplyr::filter(!(group %in% groups_to_ignore) & !(sample_type %in% sample_types_to_ignore)) %>% 
    dplyr::group_by(cluster, charge, analyte) %>% 
    dplyr::summarise(passing_percentage = sum(criteria_check) / dplyr::n() * 100) %>% 
    dplyr::filter(passing_percentage > cut_off_percentage)
  
  return(curated_analytes)
}
