
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
  
  return(sample_types)
  
}
