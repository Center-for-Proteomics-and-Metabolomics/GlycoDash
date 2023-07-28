# This file contains all functions that are used within the module
# mod_add_sample_types.R and within its sub-module
# mod_process_sample_type_file.R.

#' Read a sample type Excel or .rds file
#'
#' @param filepath The path to the Excel or .rds file containing a list of sample types.
#'  If it is an Excel file it should contain only one sheet. If it's a .rds file
#'  it should be a dataframe or tibble. In both cases there should be two columns named 
#' "sample_id" and "sample_type". The sample_id column should contain all
#' sample ID's that are present in your LaCyTools summary. The sample_type column
#' should contain the sample type corresponding to that sample ID. Standards 
#' and blanks should be included.
#' @param filename The name of the Excel or .rds file including the file extension.
#'
#' @return A tibble with the sample ID's and sample types.
#' @export
#'
#' @examples
#' path <- system.file("extdata",
#'                     "Sample_types_example.xlsx",
#'                     package = "glycodash")
#' 
#' read_sample_type_file(filepath = path, 
#'                       filename = "Sample_types_example.xlsx")
#' 
read_sample_type_file <- function(filepath, filename) {
  
  extension <- tools::file_ext(filename)
  
  if (extension == "rds") {
    sample_types <- load_and_assign(filepath)
  } else { if (extension %in% c("xlsx", "xls")) {
    sample_types <- readxl::read_excel(filepath, 
                                       col_names = TRUE)
  } else {
    rlang::abort(class = "wrong_extension",
                 message = "Please upload a .xlsx, .xls or .rds file.")
  }
  }
  
  required_columns <- c("sample_id", "sample_type")
  
  missing_columns <- required_columns[!(required_columns %in% colnames(sample_types))]
  
  if (!rlang::is_empty(missing_columns)) {
    rlang::abort(class = "missing_columns",
                 message = paste("The column(s)",
                                 comma_and(missing_columns),
                                 "could not be found. Please name the columns in your Excel file",
                                 "\"sample_name\" and \"sample_id\"."
                 ))
  }
  
  sample_types <- sample_types %>% 
    dplyr::mutate(dplyr::across(.cols = tidyselect::everything(),
                                .fns = as.character))
  
  return(sample_types)
  
}
