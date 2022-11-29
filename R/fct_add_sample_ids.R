# This file contains all functions that are used within the module
# mod_add_sample_ids.R and within its sub-modules mod_process_plate_design.R and
# mod_process_sample_list.R.

#' Detect the plate and well of a sample from the sample name.
#'
#' This function detects the plate and well position of a sample, based on it's
#' sample name. The sample name should be in a column named "sample_name" and
#' should contain either "plate" or "pl" (not case-sensitive) followed by the
#' plate number or a capital letter. Then the well position should be indicated
#' by a single capital letter between A and H followed directly by a number
#' between 1 and 12 (numbers smaller than 10 may be preceded by a zero, e.g. A01
#' and A1 will both be recognized). The plate number and well position should be
#' separated by an underscore "_".
#'
#' @param data A dataframe. Should include a column named "sample_name".
#'
#' @return The input dataframe with an added column named "plate_well" that
#'   indicates on which plate and in which well a sample was analysed.
#' @export
#'
#' @examples
#' example <- data.frame(sample_name = c("s_0216_Specific_Plate4_A10",
#'                                       "s_568_Total_pl5_H4",
#'                                       "plate23_B6.s_8759",
#'                                       "sample3857_Pl8_D05.568"),
#'                       values = c(8, 
#'                                  12, 
#'                                  3, 
#'                                  45))
#' detect_plate_and_well(example)
detect_plate_and_well <- function(data) {
  
  if (!("sample_name" %in% colnames(data))) {
    rlang::abort(class = "no_sample_name_column",
                 message = "The data doesn't contain the required column \"sample_name\".")
  }
  
  data <- data %>% 
    tidyr::extract(
      col = sample_name, 
      into = c("plate", "well"),
      # "\D" in regex is anything but a digit
      regex = "([Pp][Ll](?:[Aa][Tt][Ee])?\\d+|[A-Z])_([A-H](?:0?\\d\\D|0?\\d$|1[012]))",
      remove = FALSE)
  
  if (any(anyNA(data$plate), anyNA(data$well))) {
    NA_samples <- data$sample_name[is.na(data$plate) | is.na(data$well)]
    if (length(NA_samples) > 15) {
      rlang::abort(class = "plate_well_NAs",
                   message = paste("For",
                                   length(NA_samples),
                                   "samples the plate and well could not be determined.",
                                   "Run `?detect_plate_and_well` and check if your sample names are in a suitable format."))
    } else {
      rlang::abort(class = "plate_well_NAs",
                   message = paste("For the sample(s)",
                                   paste(NA_samples, collapse = " and "),
                                   "the plate and well could not be determined."))
    }
  }
  
  data <- data %>% 
    dplyr::mutate(
      plate = stringr::str_match(plate, 
                                 "[Pp][Ll](?:ate|ATE)?(\\d+|[A-Z])")[ , 2],
      well = stringr::str_extract(well, 
                                  "[A-H]\\d{1,2}"),
      plate_well = paste(plate, 
                         well, 
                         sep = "_")
    ) %>% 
    dplyr::select(-c(plate, well)) %>% 
    dplyr::relocate(plate_well, .after = sample_name)
  
  return(data)
}

#'Read and process a plate design file
#'
#' The function \code{read_and_process_plate_design} reads in a plate design
#' Excel file and processes it. It automatically determines sample types based
#' on the sample ID's and finds the samples on a plate design that are specified
#' as duplicate samples. The sample ID's and sample types of these duplicate
#' samples are determined by copying the sample ID and type of the preceding
#' sample.
#' 
#'@inheritParams read_plate_design
#'
#'@section Plate design format: The top-left cell of the Excel sheet should
#'  contain the plate number (e.g. "Plate 1"). The cells to the right of the
#'  top-left cell need to be labelled 1-12 (for a 96-well plate), while the
#'  cells below the top-left cell need to be labelled A-H. The cells within the
#'  plate should contain the sample ID's.
#'
#'  \preformatted{ 
#'  Plate number  1             2            3             ... 
#'  A            sample_ID_A1  sample_ID_A2  sample_ID_A3 
#'  B            sample_ID_B1  sample_ID_B2  sample_ID_B3 
#'  C            sample_ID_C1  sample_ID_C2  sample_ID_C3  
#'  ...          ...           ...           ...           ...}
#'
#'  At the bottom of the plate, leave one row blank and then add the next plate
#'  in the same format.
#'
#'@return This function returns a dataframe with four columns:
#'  \describe{\item{sample_id}{The sample ID's as given in the plate design
#'  file.} \item{sample_type}{Automatically determined sample types. The
#'  function takes the first string of letters within the sample ID as the
#'  sample type. This might not work for your sample ID's so you may need to
#'  alter the sample types manually after using this function.}
#'  \item{plate_well}{This column indicated the plate and well that a sample was
#'  analyzed in. The format is as follows: the plate number followed by the well
#'  ID, separated by an underscore (e.g. plate 1 well A1 is 1_A01).}
#'  \item{duplicate}{This column is \code{TRUE} for samples that were specified
#'  on the plate design as duplicate samples and \code{FALSE} for the remaining
#'  samples.}}
#'
#'@export
#'
#' @examples
#' path <- system.file("extdata",
#'                     "Plate_design_example.xlsx",
#'                     package = "glycodash")
#' 
#' read_and_process_plate_design(plate_design_file = path)
read_and_process_plate_design <- function(plate_design_file) {
  plate_design <- tryCatch(
    expr = {
    read_plate_design(plate_design_file)
  },
  incorrect_formatting = function(c) {
    rlang::abort(message = c$message,
                 class = "incorrect_formatting")
  })
  
  plate_design <- process_plate_design(plate_design)
  
  return(plate_design)
}

#'Read in a plate design file
#'
#'The function \code{read_plate_design} reads in a plate design Excel
#'file and returns a dataframe. It uses the \code{\link[plater]{read_plate}}
#'function from the plater package.
#'
#'@param plate_design_file The path to the plate design Excel file. The file
#'  should be in the format described below.
#'
#'@return This function returns a dataframe with a column named "well" that
#'  indicates in which well a sample was analyzed. In addition, there is one
#'  column for each plate in the plate design file. Each plate column contains
#'  the sample ID's of the samples on that plate.
#'@export
#'
#'@section Plate design format: 
#'The top-left cell of the Excel sheet should contain the plate number (e.g. 
#'"Plate 1"). The cells to the right of the top-left cell need to be labelled 
#'1-12 (for a 96-well plate), while the cells below the top-left cell need to be
#'labelled A-H. The cells within the plate should contain the sample ID's.
#'
#'\preformatted{
#'Plate number  1             2            3             ...
#'A            sample_ID_A1  sample_ID_A2  sample_ID_A3  
#'B            sample_ID_B1  sample_ID_B2  sample_ID_B3
#'C            sample_ID_C1  sample_ID_C2  sample_ID_C3
#'...          ...           ...           ...           ...}
#'
#'At the bottom of the plate, leave one row blank and then add the next plate in
#'the same format.
#'
#'
#' @examples
#' path <- system.file("extdata",
#'                     "Plate_design_example.xlsx",
#'                     package = "glycodash")
#' read_plate_design(plate_design_file = path)
read_plate_design <- function(plate_design_file) {
  
  plate_design <- readxl::read_excel(plate_design_file,
                                     .name_repair = "minimal")
  path_to_platedesign_csv <- file.path(tempdir(), "glycodash_platedesign.csv")
  write.csv(plate_design,
            file = path_to_platedesign_csv, 
            row.names = FALSE,
            na = "",
            quote = FALSE)
  plate_design <- tryCatch(expr = {
    plater::read_plate(file = path_to_platedesign_csv, 
                       well_ids_column = "well")
  },
  # Throw custom error when plater::read_plate() throws an error:
  error = function(e) { 
    rlang::abort(class = "incorrect_formatting",
                 message = paste(
                   "Please check that your plate design file is formatted correctly.",
                   "Run `?read_and_process_plate_design` to find the required format."))
  },
  # Throw custom error when plater::read_plate() throws a warning:
  warning = function(w) { 
    rlang::abort(class = "incorrect_formatting",
                 message = paste(
                   "Please check that your plate design file is formatted correctly.",
                   "Run `?read_and_process_plate_design` to find the required format."))
  })
  
  return(plate_design)
}

#' Process the result of read_plate_design()
#'
#' This function takes the result from the \code{\link{read_plate_design}}
#' function and converts it to a different format.
#'
#' @param plate_design The dataframe that is returned by the
#'   \code{\link{read_plate_design}} function.
#'
#' @return A dataframe with two columns: \describe{\item{sample_id}{The sample
#'   ID's as given in the plate design file.} \item{plate_well}{This column 
#'   indicated the plate and well that a sample was analyzed in. The format is 
#'   as follows: the plate number followed by the well ID, separated by an 
#'   underscore (e.g. plate 1 well A1 is 1_A01).}}
#' @export
#'
#' @examples
#' path <- system.file("extdata",
#'                     "Plate_design_example.xlsx",
#'                     package = "glycodash")
#'
#' plate_design <- read_plate_design(path)
#' process_plate_design(plate_design)
process_plate_design <- function (plate_design) {
  plate_design <- plate_design %>%
    tidyr::pivot_longer(cols = -well,
                        names_to = "plate",
                        values_to = "sample_id") %>%
    dplyr::mutate(
      sample_id = as.character(sample_id),
      plate = stringr::str_match(
        plate, 
        "[Pp][Ll](?:[Aa][Tt][Ee])?[\\s_\\-\\.]*(\\d+|[A-Z])")[ , 2],
      well = stringr::str_extract(well, "[A-H]\\d+"))
  
  if (any(is.na(plate_design$plate))) {
    rlang::abort(class = "plate_numbers",
                 message = paste(
                   "The plate numbers could not be detected. Please check if",
                   "your plate design Excel file is in the correct format."))
  }
  
  plate_design <- plate_design %>% 
    dplyr::mutate(plate_well = paste(plate, well, sep = "_")) %>% 
    dplyr::arrange(plate_well) %>% 
    dplyr::select(-c(plate, well)) %>% 
    tidyr::replace_na(list(
      sample_id = "empty cell in plate design"
    ))
  
  return(plate_design)
}

#' Process a sample list Excel file
#' 
#' This function reads and processes a sample list Excel file, so that sample 
#' ID's can be linked to your data.
#'
#' @param sample_list_file The path to an Excel file containing the sample list.
#' The Excel file should contain only one sheet with two columns named 
#' "sample_name" and "sample_id". The sample_name column should contain all
#' sample names that are present in your LacyTools summary. The sample_id column
#' should contain the sample ID corresponding to that sample name. Standards 
#' and blanks should be included.
#'
#' @return This function returns a dataframe with the contents of the Excel file.
#' @export
#'
#' @examples
#' path <- system.file("inst",
#'                     "extdata",
#'                     "Sample_list_example.xlsx",
#'                     package = "glycodash")
#' 
#' process_sample_list(sample_list_file = path)
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
  
  sample_list <- sample_list %>% 
    dplyr::mutate(sample_id = as.character(sample_id))
  
  return(sample_list)
}
