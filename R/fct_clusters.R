#'Define clusters based on analyte names
#'
#'This function determines to which cluster an analyte belongs based on the
#'analyte name.
#'
#' @param data A dataframe in long format (one row for each analyte + sample
#'   combination).
#'@param cluster_keywords A character vector with keywords
#'  used as regular expressions to classify analytes into clusters.
#'  \code{cluster_keywords} should have the same length as the number of clusters
#'  in the data.
#'
#'@return The function returns the original dataframe given as the data
#'  argument, but with an additional column named "cluster". This character
#'  column indicates to what cluster the analyte belongs.
#'@export
#'
#' @examples
#' data("example_data")
#' define_clusters(data = example_data,
#'                 cluster_keywords = "IgGI")
define_clusters <- function(data, cluster_keywords) {
  
  # By ordering from longest to shortest keyword, the longest matching keyword
  # will be preferred if an analyte matches more than one keyword. This can
  # occur when one keyword is a substring of another keyword (e.g. IgGI and
  # IgGII):
  ordered_keywords <- cluster_keywords[order(nchar(cluster_keywords), 
                                             decreasing = TRUE)]
  
  data <- data %>% 
    dplyr::mutate(analyte = factor(analyte)) 
  
  # By using str_extract() only on the levels of factor(analyte) it is (maybe?)
  # faster than using it on the original dataframe
  clusters <- tibble::tibble(analyte = levels(data$analyte)) %>% 
    dplyr::mutate(cluster = stringr::str_extract(analyte,
                                                 paste0("(",
                                                        paste0(ordered_keywords, 
                                                               collapse = "|"),
                                                        ")")))
  
  if (anyNA(clusters$cluster)) {
    rlang::abort(class = "unmatched_analytes",
                 message = paste("Some analytes could not be assigned into a cluster.",
                                 "Please reconsider your cluster keywords."))
  }
  
  data_with_clusters <- dplyr::left_join(data, clusters) %>% 
    dplyr::relocate(cluster, .after = analyte)
  
  return(data_with_clusters)
}

#' Check if a cluster keyword matches at least one analyte in the data
#'
#' @param unique_analytes A character vector with all unique analytes in the data.
#' @param cluster_keyword A single cluster keyword (a character string).
#'
#' @return A boolean, TRUE if a match was found, FALSE if no match was found.
#' @export
#'
#' @examples
#' analytes <- c("IgGIH3N4", "IgGIH3N4F1", "IgGIIH5N4F1")
#' 
#' find_cluster_keyword_match(unique_analytes = analytes,
#'                            cluster_keyword = "IgGII")
#'                            
find_cluster_keyword_match <- function(unique_analytes, cluster_keyword) {
  keyword_found <- any(stringr::str_detect(string = unique_analytes,
                                           pattern = stringr::fixed(cluster_keyword)))
  return(keyword_found)
}
