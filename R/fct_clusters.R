#'Define clusters based on analyte names
#'
#'This function determines which cluster an analyte belongs to based on the
#'analyte name.
#'
#'@inheritParams check_analyte_quality_criteria
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
#' data("example_data")
#' define_clusters(data = example_data,
#'                 clusters_regex = "IgGI1")
define_clusters <- function(data, cluster_keywords) {
  
  # By ordering from longest to shortest keyword, the longest matching keyword
  # will be preferred if an analyte matches more than one keyword. This can
  # occur when one keyword is a substring of another keyword (e.g. IgGI and
  # IgGII):
  ordered_keywords <- cluster_keywords[order(nchar(cluster_keywords), 
                                             decreasing = TRUE)]
  
  data <- data %>% 
    dplyr::mutate(analyte = factor(analyte)) 
  
  clusters <- tibble::tibble(analyte = levels(data$analyte)) %>% 
    dplyr::mutate(cluster = stringr::str_extract(analyte,
                                                 paste0("(",
                                                        paste0(ordered_keywords, 
                                                               collapse = "|"),
                                                        ")")))
  
  if (anyNA(clusters$cluster)) {
    rlang::abort(class = "unmatched_analytes",
                 message = paste("Some analytes could not be assigned into a cluster.",
                                 "Please reconsider your clusters keywords."))
  }
  
  data_with_clusters <- dplyr::left_join(data, clusters) %>% 
    dplyr::relocate(cluster, .after = analyte)
  
  return(data_with_clusters)
}

find_cluster_keyword_match <- function(unique_analytes, cluster_keyword) {
  keyword_found <- any(stringr::str_detect(string = unique_analytes,
                                           pattern = stringr::fixed(cluster_keyword)))
  return(keyword_found)
}
