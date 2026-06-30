#' Detect whether a sample is specific or total Ig based on the sample name.
#'
#' @param data A dataframe containing a LaCyTools summary with a column
#'   "sample_name".
#' @param keyword_specific The word(s)/characters within the sample name used to
#'   refer to Specific samples.
#' @param keyword_total The word(s)/characters within the sample name used to
#'   refer to Total samples.
#'
#' @return The dataframe containing a block from a LaCyTools summary file, with
#'   an additional column named "group" that indicates whether a sample is
#'   specific or total.
#' @export
#'
#' @examples
#' block_example <- data.frame(sample_name = c("s_0216_Specific", "s_568_Total", "s_8759"),
#'                             values = c(13.56, 738.34, 4.56))
#' detect_group(data = block_example, keyword_specific = "Specific", keyword_total = "Total")
detect_group <- function(data, keyword_specific, keyword_total) {
  data <- data %>% 
    tidyr::extract(
      col = sample_name,
      into = "group",
      regex = paste0("(", keyword_specific, "|", keyword_total, ")"),
      remove = FALSE
    ) %>% 
    dplyr::mutate(group = as.factor(group))
  
  if (!(keyword_specific %in% levels(data$group))) {
    rlang::abort(
      class = "unmatched_keyword_specific",
      message = paste(
        "This keyword for specific samples did not match", 
        "any sample names in your data. Please choose a different keyword."
      )
    )
  }
  
  if (!(keyword_total %in% levels(data$group))) {
    rlang::abort(
      class = "unmatched_keyword_total",
      message = paste(
        "This keyword for total samples did not match", 
        "any sample names in your data. Please choose a different keyword."
      )
    )
  }
  
  if (any(is.na(data$group))) {
    rlang::warn(
      class = "NAs",
      message = paste(
        "Some sample names could not be classified as total or specific Ig.",
        "Please reconsider your keywords."
      )
    )
  }
  
  return(data)
}



#' Generate an ordinal suffix for a number
#'
#' Returns a character string consisting of the number followed by its English
#' ordinal suffix ("st", "nd", "rd", or "th").
#'
#' @param num A positive integer.
#'
#' @return A character string, e.g. \code{"1st"}, \code{"2nd"}, \code{"3rd"},
#'   \code{"4th"}.
getOrdinalSuffix <- function(num) {
  if (num %% 10 == 1 && num %% 100 != 11) {
    return(paste0(num, "st"))
  } 
  else if (num %% 10 == 2 && num %% 100 != 12) {
    return(paste0(num, "nd"))
  } 
  else if (num %% 10 == 3 && num %% 100 != 13) {
    return(paste0(num, "rd"))
  } 
  else {
    return(paste0(num, "th"))
  }
}



#' Abbreviate glycosylation site identifiers
#'
#' Generates short abbreviations for unique protein–peptide combinations. Each
#' protein is assigned a label \code{"PrA"}, \code{"PrB"}, etc. Each unique
#' peptide within a protein is given a three-letter prefix derived from the
#' peptide sequence. When peptides share the same three-letter prefix, the
#' suffixes \code{"a"}, \code{"b"}, \code{"c"}, etc. are appended to
#' distinguish them.
#'
#' @param protein_peptide_df A dataframe with (at least) columns \code{protein}
#'   and \code{peptide}, where each row represents one protein–peptide pair.
#'
#' @return A dataframe with columns \code{protein}, \code{peptide}, and
#'   \code{abbreviation}.
abbreviate_glycosites <- function(protein_peptide_df) {
  
  df <- protein_peptide_df %>% 
    dplyr::distinct() %>% 
    dplyr::mutate(
      protein_number = paste0("Pr", LETTERS[dplyr::dense_rank(protein)]),
      site_prefix = paste0(protein_number, "_", stringr::str_sub(peptide, 1, 3))
    ) %>% 
    dplyr::group_by(site_prefix) %>% 
    dplyr::mutate(
      suffix = if (dplyr::n() == 1) {
        ""
      } else {
        letters[dplyr::row_number()]
      },
      abbreviation = paste0(site_prefix, suffix)
    ) %>% 
    dplyr::ungroup() %>% 
    dplyr::select(protein, peptide, abbreviation)
  
  return(df)
}

