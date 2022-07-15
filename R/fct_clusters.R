#'Define clusters based on analyte names
#'
#'This function determines which cluster an analyte belongs to based on the
#'analyte name.
#'
#'@inheritParams do_criteria_check
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
  
  clusters <- data %>% 
    tidyr::extract(col = analyte,
                   into = "cluster",
                   regex = paste0("(",
                                  paste0(cluster_keywords, 
                                         collapse = "|"),
                                  ")"),
                   remove = FALSE)
  
  if (anyNA(clusters$cluster)) {
    rlang::abort(class = "unmatched_analytes",
                 message = paste("Some analytes could not be assigned into a cluster.",
                                 "Please reconsider your clusters keywords."))
  }
  return(clusters)
}

find_cluster_keyword_match <- function(unique_analytes, cluster_keyword) {
  keyword_found <- any(stringr::str_detect(string = unique_analytes,
                                           pattern = stringr::fixed(cluster_keyword)))
  return(keyword_found)
}
