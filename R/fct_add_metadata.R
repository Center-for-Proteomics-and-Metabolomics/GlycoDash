# This file contains all functions that are used within the module
# mod_add_metadata.R.

#' Read in one or more metadata files
#'
#' This function can be used to read in a list of metadata files (.xlsx, .xls or
#' .rds files). In the case of an Excel file empty cells or "NA" cells will be
#' read in as \code{NA}. All column names will be converted to snake case
#' (lowercase, words separated by underscores).
#'
#' @param filepaths A list of paths to the metadata files.
#' @param filenames A list of filenames (including file extension) corresponding
#'   to the filepaths.
#'
#' @return A named list of dataframes containing the metadata. The names
#'   correspond to the filenames.
#' @export
#'
#' @examples
#' path <- system.file("extdata",
#'                     "Metadata_example.xlsx",
#'                     package = "GlycoDash")
#'
#' read_metadata(filepaths = list(path),
#'               filenames = list("Metadata_example.xlsx"))
read_metadata <- function(filepaths, filenames) {
  
  metadata_list <- purrr::pmap(
    list(path = filepaths, name = filenames),
    function(path, name) {
      extension <- tools::file_ext(name)
      if (extension %in% c("xlsx", "xls")) {
        metadata <- readxl::read_excel(path, na = c("", "NA"), col_types = "text")
      } 
      else if (extension == "rds") {
        metadata <- load_and_assign(path)
      } 
      else {
        rlang::abort(class = "error")
      }
    }
  )
  
  names(metadata_list) <- filenames
  
  return(metadata_list)
}


#' Rename the metadata column with sample IDs to "sample_id"
#'
#' This function renames the column in the metadata file that contains sample
#' IDs to "sample_id".
#'
#' In case there already is a column named sample_id that is NOT the column
#' chosen by the user as the sample ID column, a warning is issued and the old
#' sample_id column is renamed to "sample_id_original" to avoid duplicate column
#' names.
#'
#' @param metadata A dataframe or tibble with metadata.
#' @param sample_id_column A character string, the name of the column in
#'   \code{metadata} that contains the sample IDs.
#'
#' @return The input \code{metadata} dataframe with the column specified by
#'   \code{sample_id_column} renamed to "sample_id". If a conflicting
#'   "sample_id" column already existed, it is first renamed to
#'   "sample_id_original" and a warning is issued.
#' @export
#'
#' @examples
#' path <- system.file("extdata",
#'                     "Metadata_example.xlsx",
#'                     package = "GlycoDash")
#'
#' metadata_list <- read_metadata(filepaths = list(path),
#'                                filenames = list("Metadata_example.xlsx"))
#' sample_id_columns_list <- list("Sample ID")
#' 
#' purrr::pmap(list(metadata_list,
#'                  sample_id_columns_list),
#'             function(metadata, sample_id_column) {
#'               rename_sample_id_column(metadata = metadata,
#'                                       sample_id_column = sample_id_column)
#'             })
rename_sample_id_column <- function(metadata, sample_id_column) {
  
  conflict <- "sample_id" %in% colnames(metadata) & sample_id_column != "sample_id"
  
  if (conflict) {
    metadata <- metadata %>% 
      dplyr::rename(sample_id_original = sample_id)
    
    rlang::warn(
      class = "sample_id_conflict",
      message = paste(
        "The column originally named \"sample_id\" was renamed",
        "as \"sample_id_original\" to avoid duplicate column names."
      )
    )
  }
  
  metadata <- metadata %>% 
    dplyr::rename(sample_id = sample_id_column)
  
  return(metadata)
}



#' Check for forbidden column names in metadata
#'
#' Checks whether the metadata contains column names that are reserved for
#' internal use. Some column names are not allowed because they conflict with
#' columns created elsewhere in the pipeline.
#'
#' @param merged_metadata A dataframe or tibble containing the merged metadata
#'   to check.
#'
#' @return A character vector of forbidden column names found in
#'   \code{merged_metadata}. Returns a zero-length character vector if no
#'   forbidden column names are present.
#' @export
#'
#' @examples
#' metadata <- data.frame(sample_id = c("S1", "S2"),
#'                        sample_name = c("Sample 1", "Sample 2"),
#'                        group = c("A", "B"))
#' check_column_names(metadata)
check_column_names <- function(merged_metadata) {
  
  not_allowed <- c("sample_name", "analyte", "charge", "mass_accuracy_ppm",
                  "absolute_intensity_background_subtracted", "isotopic_pattern_quality", 
                  "sn", "fraction", "exact_mass", "group", "sample_type", "cluster",
                  "peptide_sequence", "methionine_oxidation", "note", "protein")
  
  colnames <- colnames(merged_metadata)
  
  forbidden <- character()  # Initiate empty character vector
  
  # Loop to check for forbidden column names
  for (i in colnames) {
    if (i %in% not_allowed) {
      forbidden <- append(forbidden, i)
    }
  }
  
  return(forbidden)
}

