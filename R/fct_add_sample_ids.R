
#' Process a sample list Excel file
#'
#' @return
#' @export
#'
#' @examples
process_sample_list <- function(sample_list_file) {
  
  sample_list <- readxl::read_excel(sample_list_file,
                     col_names = TRUE)
  
  required_columns <- c("sample_name", "sample_id")
  missing_columns <- required_columns[!(required_columns %in% colnames(sample_list))]
  
  if (!rlang::is_empty(missing_columns)) {
    rlang::abort(class = "wrong_column_names",
                 message = paste("The column(s)",
                                 comma_and(missing_columns),
                                 "could not be found. Please name the columns in your Excel file",
                                 "\"sample_name\" and \"sample_id\"."
                 ))
  }
  
  return(sample_list)
}
