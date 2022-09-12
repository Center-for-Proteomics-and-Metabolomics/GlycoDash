
#' Detect the plate and well of a sample from the sample name.
#'
#' This function detects the plate and well position of a sample, based on it's
#' sample name. The sample name should be in a column named "sample_name" and
#' should contain either "P", "p", "Pl" or "PL" followed by the plate number or
#' a capital letter possibly separated by an underscore (_), a score (-) or a
#' dot (.). In addition, the sample name should contain a single capital letter
#' between A and H followed directly by a number between 1 and 12 (numbers
#' smaller than 10 may be preceded by a zero, e.g. A01 and A1 are both allowed).
#' This letter-number combination indicates the well.
#'
#' @param data A dataframe. Should include a column named "sample_name".
#'
#' @return The input dataframe with an added column named "plate_well" that
#'   indicates on which plate and in which well a sample was analysed.
#' @export
#'
#' @examples
#' block_example <- data.frame(sample_name = c("s_0216_Specific_pl_3.A10",
#'                                             "s_568_Total_P5_H4",
#'                                             "pl23_B6.s_8759"),
#'                             values = c(13.56, 738.34, 4.56))
#' detect_plate_and_well(block_example)
detect_plate_and_well <- function(data) {
  
  if (!("sample_name" %in% colnames(data))) {
    rlang::abort(class = "no_sample_name_column",
                 message = "The data doesn't contain the required column \"sample_name\".")
  }
  
  data <- data %>% 
    tidyr::extract(
      col = sample_name, 
      into = c("plate", "well"),
      # Plate number/letter does not have to be preceded by a P/p, but between
      # the plate number and well position only a _ - . or space are allowed:
      regex = "([Pp]?(?:[Ll]?|late)[_\\-.\\s]?(?:\\d+|[A-Z]))[_\\-.\\s]?([A-H][_\\-.\\s]?(?:0?\\d\\D|0?\\d$|1[012]))",
      remove = FALSE)
  
  if (any(anyNA(data$plate), anyNA(data$well))) {
    data <- data %>% 
      dplyr::select(-c(plate, well)) %>% 
      tidyr::extract(
        col = sample_name, 
        into = c("plate", "well"),
        # Plate number/letter has to be preceded by at least a P/p but there can
        # be all kinds of characters between the plate number and the well position:
        regex = "([Pp](?:[Ll]?|late)[_\\-.\\s]?(?:\\d+|[A-Z])).*([A-H][_\\-.\\s]?(?:0?\\d\\D|0?\\d$|1[012]))",
        remove = FALSE)
  }
  
  if (any(anyNA(data$plate), anyNA(data$well))) {
    data <- data %>% 
      dplyr::select(-c(plate, well)) %>% 
      tidyr::extract(
        col = sample_name, 
        into = c("well", "plate"),
        # The well position directly precedes the plate number (can only be
        # separated by a _ - . or space), but the plate number/letter does not
        # need to be preceded by a P/p:
        regex = "([A-H][_\\-.\\s]?(?:0?\\d\\D|0?\\d$|1[012]))[_\\-.\\s]?([Pp]?(?:[Ll]?|late)[_\\-.\\s]?(?:\\d+|[A-Z]))",
        remove = FALSE)
  }
  
  if (any(anyNA(data$plate), anyNA(data$well))) {
    NA_samples <- data$sample_name[is.na(data$plate) | is.na(data$well)]
    if (length(NA_samples) > 15) {
      rlang::abort(class = "plate_well NAs",
                   message = paste("For",
                                   length(NA_samples),
                                   "samples the plate and well could not be determined.",
                                   "Run `?detect_plate_and_well` and check if your sample names are in a suitable format."))
    } else {
      rlang::abort(class = "plate_well NAs",
                   message = paste("For the sample(s)",
                                   paste(NA_samples, collapse = " and "),
                                   "the plate and well could not be determined."))
    }
  }
  
  data <- data %>% 
    dplyr::mutate(plate = stringr::str_match(plate, "[Pp]?[Ll]?(?:ate)?[_\\-.\\s]?(\\d+|[A-Z])")[ , 2],
                  well = stringr::str_extract(well, "[A-H][_\\-.\\s]?\\d{1,2}"),
                  plate_well = paste(plate, well, sep = "_")) %>% 
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
#'  Duplicate samples should be indicated in the plate design Excel file as
#'  "duplicate". \code{read_and_process_plate_design} assumes that duplicate 
#'  samples are always the duplicate of the preceding sample, that is the 
#'  sample one well to the left or, in case the duplicate is positioned in the 
#'  left-most column of the plate, the sample in the right-most well one row up.
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
  # Pass along any errors (if applicable) from read_plate_design:
  tryCatch(expr = {
    plate_design <- read_plate_design(plate_design_file)
  },
  incorrect_formatting = function(cnd) {
    rlang::abort(message = cnd$message,
                 class = "incorrect_formatting")
  })
  
  plate_design <- process_plate_design(plate_design)
  
  # Pass along any errors (if applicable) from handle_duplicates. Warnings are
  # passed along automatically.
  # tryCatch(expr = {
  #   plate_design <- handle_duplicates(plate_design)
  # },
  # error = function(e) {
  #   rlang::abort(class = e$class,
  #                message = e$message)
  # })
  
  return(plate_design)
}

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
