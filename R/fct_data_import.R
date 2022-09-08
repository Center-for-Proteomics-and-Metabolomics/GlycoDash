
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
  tryCatch(expr = {
    plate_design <- plater::read_plate(file = path_to_platedesign_csv, 
                                       well_ids_column = "well")
  },
  # Ignore error thrown by plater::read_plate():
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
                   "Run `?read_and_process_plate_design` to find the required format."))})
  
  return(plate_design)
}

#' Process the result of read_plate_design()
#'
#' This function takes the result from the \code{\link{read_plate_design}}
#' function and converts it to a different format. In addition it automatically
#' determines sample types based on the sample ID's.
#'
#' @param plate_design The dataframe that is returned by the
#'   \code{\link{read_plate_design}} function.
#'
#' @return A dataframe with three columns: \describe{\item{sample_id}{The sample
#'   ID's as given in the plate design file.} \item{sample_type}{Automatically
#'   determined sample types. The function takes the first string of letters
#'   within the sample ID as the sample type. This might not work for your
#'   sample ID's so you may need to alter the sample types manually after using
#'   this function.} \item{plate_well}{This column indicated the plate and well
#'   that a sample was analyzed in. The format is as follows: the plate number
#'   followed by the well ID, separated by an underscore (e.g. plate 1 well A1
#'   is 1_A01).}}
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
    dplyr::mutate(plate = stringr::str_match(plate, "[Pp][Ll]?(?:ate)?[\\s_#]*(\\d+|[A-Z])")[ , 2],
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
    # tidyr::extract(col = sample_id, 
    #                into = c("sample_type"), 
    #                regex = "([[:alpha:]]+)",
    #                remove = FALSE) %>% 
    # Remove rows where sample_type is NA (these were empty cells
    # in the plate design Excel file)
    tidyr::replace_na(list(
      #sample_type = "unknown",
      sample_id = "empty cell in plate design"
    ))
  # dplyr::filter(dplyr::if_all(.cols = c(sample_type),
  #                             .fns = ~ !is.na(.x)))
  
  return(plate_design)
}

#'Handle duplicate samples in a plate design file
#'
#'\code{handle_duplicates()} finds the samples on a plate design that are
#'specified as duplicate samples and finds the sample ID's and sample types of
#'these samples by copying the sample ID and type of the preceding sample. This
#'function assumes that duplicates are indicated in the plate design Excel file
#'as "duplicate" and that duplicate samples are always the duplicate of the
#'sample one well to the left, or, in case the duplicate is positioned in the
#'left-most column of the plate, the duplicate of the right-most well one row
#'up.
#'
#'@param plate_design The dataframe that is returned by the
#'  \code{\link{process_plate_design}} function.
#'
#'@return The original dataframe passed as the \code{plate_design} argument with
#'  an additional column named "duplicate". This column is \code{TRUE} for
#'  samples that were specified on the plate design as duplicate samples and
#'  \code{FALSE} for the remaining samples. In addition, the sample ID's in the
#'  sample_id column and the sample types in the sample_type column of duplicate
#'  samples are replaced with the sample ID's and sample types of the preceding
#'  sample.
#'@export
#'
#' @examples
#' path <- system.file("extdata",
#'                     "Plate_design_example.xlsx",
#'                     package = "glycodash")
#'
#' plate_design <- read_plate_design(path)
#' plate_design <- process_plate_design(plate_design)
#' handle_duplicates(plate_design)
handle_duplicates <- function(plate_design) {
  new_sample_ids <- vector()
  new_sample_types <- vector()
  duplicate <- vector()
  for (i in 1:length(plate_design$sample_id)) {
    if (i == 1 & plate_design$sample_id[i] == "duplicate") {
      rlang::abort(class = "first_sample_duplicate", 
                   message = "Error: first sample is a duplicate.")
    }
    if (plate_design$sample_id[i] == "duplicate") {
      new_sample_ids[i] <- plate_design$sample_id[i-1]
      new_sample_types[i] <- plate_design$sample_type[i-1]
      duplicate[i] <- TRUE
    } else {
      new_sample_ids[i] <- plate_design$sample_id[i]
      new_sample_types[i] <- plate_design$sample_type[i]
      duplicate[i] <- FALSE
    }
  }
  
  if (identical(new_sample_ids, plate_design$sample_id)) {
    rlang::warn(class = "no_duplicates",
                message = paste("No duplicates were found in your plate design file.",
                                "If there should be duplicate samples to be found,",
                                "please check if those samples are specified literally as \"duplicate\" in your plate design file."))
  }
  
  new_plate_design <- plate_design %>% 
    dplyr::mutate(sample_id = new_sample_ids,
                  sample_type = new_sample_types,
                  duplicate = duplicate)
  return(new_plate_design)
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

#' Read in a metadata file
#'
#' \code{read_metadata()} can be used to read in an Excel file containing
#' metadata. It reads in all columns as class character and interprets both "NA"
#' and empty cells as \code{NA}. The values in the first row are converted to
#' snake case (see: \code{\link[snakecase]{to_snake_case}}) and used as column
#' names. If the file contains multiple sheets, only the first sheet is read in.
#'
#' @param metadata_file The path to the metadata Excel file.
#'
#' @return This function returns a dataframe with the metadata.
#' @export
#'
#' @examples
#' path <- system.file("extdata",
#'                     "Metadata_example.xlsx",
#'                     package = "glycodash")
#'                     
#' read_metadata(path)
# read_metadata <- function (metadata_file) {
#   metadata <- readxl::read_excel(metadata_file,
#                                  col_types = "text", 
#                                  na = c("", "NA"))
#   metadata <- metadata %>% 
#     dplyr::rename_with(.cols = tidyselect::everything(), 
#                        .fn = snakecase::to_snake_case) 
#   return(metadata)
# }

#'Convert serial dates with comments to dates
#'
#'\code{date_with_text()} tries to convert a vector with serial dates (given as
#'character strings) to objects of the class "Date". In Excel dates are stored
#'as serial numbers. These serial dates are the number of days passed since a
#'set origin date. When Excel data is read into R the serial dates are usually
#'recognized as dates and converted to objects of class "Date". However, when in
#'Excel a cell contains both a date and a comment, the column containing this
#'cell is no longer recognized as class "Date" and is instead converted to class
#'"Character". \code{date_with_text()} attempts to convert serial dates (given
#'as character strings) to objects of class "Date". If some serial dates include
#'comments, the function instead converts the serial dates to character strings
#'with the dates formatted as "year-month-day". \strong{Important: This function
#'can only be used within the function \code{\link[dplyr]{across}} from the
#'dplyr package!}
#'
#'@param date_text_values A column in a dataframe or tibble with serial dates in
#'  the form of character strings.
#'@param origin The origin date used to calculate the serial dates. Defaults to
#'  1899-12-30 (year-month-day) which is the origin used in Excel on Windows for
#'  all dates from 1900-01-01 and on.
#'
#'@return
#'@export
#'
#' @section Credits:
#' This function was based on a solution found on StackOverflow.\cr
#' Question asked by Ryan C. Thompson:\cr
#' \url{https://stackoverflow.com/questions/63027147/how-to-read-in-a-date-column-from-an-excel-sheet-with-ambiguous-formats-into-r}\cr
#' Solution provided by Rui Barradas:\cr
#' \url{https://stackoverflow.com/users/8245406/rui-barradas}
#'
#' @examples
#' path <- system.file("extdata",
#'                     "for_tests",
#'                     "Dates_with_comment.xlsx",
#'                     package = "glycodash")
#'
#'# Dates with comments in Excel are usually interpreted
#'# as character strings in R:
#'
#' dates_with_comment <- readxl::read_excel(path)
#'
#' print(dates_with_comment)
#'
#'# # A tibble: 6 x 1
#'# date
#'# <chr>
#'# 1 44603
#'# 2 44604
#'# 3 13-02-2022 oh no a comment
#'# 4 44606
#'# 5 44607
#'# 6 44608
#'
#' dates_with_comment <- dates_with_comment %>%
#'    dplyr::mutate(dplyr::across(.cols = date,
#'                                .fns = date_with_text))
#'
#'# Because text is found within the dates
#'# column the following message is shown:
#'# "Some date entries in date contain text.
#'# Output will have class character."
#'
#'print(dates_with_comment)
#'
#' # # A tibble: 6 x 1
#' # date
#' # <chr>
#' # 1 2022-02-11
#' # 2 2022-02-12
#' # 3 13-02-2022 oh no a comment
#' # 4 2022-02-14
#' # 5 2022-02-15
#' # 6 2022-02-16
#'
#' path <- system.file("extdata",
#'                     "for_tests",
#'                     "Dates.xlsx",
#'                     package = "glycodash")
#'
#' # Forcing read_excel() to read the dates in as character strings 
#' # (even though there are no comments in this file) for 
#' # demonstration purposes:
#' dates <- readxl::read_excel(path,
#'                             col_types = "text")
#'
#' print(dates)
#'
#'# # A tibble: 6 x 1
#'# date
#'# <chr>
#'# 1 44603
#'# 2 44604
#'# 3 44605
#'# 4 44606
#'# 5 44607
#'# 6 44608
#'
#' dates <- dates %>%
#'    dplyr::mutate(dplyr::across(.cols = date,
#'                                .fns = date_with_text))
#'                                
#' # When no text is found within the dates column,
#' # the serial dates are converted to objects of class "date":
#' print(dates)
#'
#' # # A tibble: 6 x 1
#' # date
#' # <date>
#' # 1 2022-02-11
#' # 2 2022-02-12
#' # 3 2022-02-13
#' # 4 2022-02-14
#' # 5 2022-02-15
#' # 6 2022-02-16
#'           
date_with_text <- function(date_text_values, origin = "1899-12-30"){
  no_nas <- date_text_values[!is.na(date_text_values)]
  test <- suppressWarnings(as.numeric(no_nas))
  num <- suppressWarnings(as.numeric(date_text_values))
  dates <- as.Date(num, origin = origin)
  if(anyNA(test)){
    dates <- as.character(dates)
    dates[is.na(num)] <- as.character(date_text_values[is.na(num)])
    rlang::warn(class = "text_in_dates",
                message = paste("Some date entries in", 
                                tryCatch(dplyr::cur_column(),
                                         error = function(e) {}), 
                                "contain text. Output will have class character."))
  }
  return(dates)
}

#' Load R-objects and assign them a new name
#'
#' When you load an R-object with \code{\link[base]{load}} the object keeps the
#' name it had at the moment that it was saved. Sometimes it's more convenient
#' to assign a new name to the object when it is being loaded.
#' \code{load_and_assign()} allows you to do that.
#' 
#' @section Credits:
#' The code for this function was found on StackOverflow.\cr
#' Question asked by Ryan C. Thompson:\cr
#' \url{https://stackoverflow.com/questions/5577221/how-can-i-load-an-object-into-a-variable-name-that-i-specify-from-an-r-data-file}\cr
#' Solution provided by ricardo:\cr
#' \url{https://stackoverflow.com/users/1453172/ricardo}
#' 
#' @param file_path The path to the R-object that you want to load.
#'
#' @return
#' @export
#'
#' @examples
#' old_name <- c("This is a random vector",
#'               "to serve as an example")
#'
#' # Creating a temporary directory:
#' file_path <- file.path(tempdir(),
#'                        "R_object.rds")
#'
#' # Saving the vector as an R-object to the temporary directory:
#' save(old_name,
#'      file = file_path)
#'
#' # Loading the R object and assigning a new name to it:
#' new_name <- load_and_assign(file_path)
#' 
load_and_assign <- function(file_path){
  tryCatch(
    load(file_path),
    error = function(e) {
      
    },
    warning = function(w) { 
      stop(paste("Check if the file_path was not misspelled.",
                 "Was the R-object you're trying to load saved with the function saveRDS()?",
                 "load_and_assign only works with objects saved with the function save()"))
      })
  get(ls()[ls() != "file_path"])
}

comma_and <- function(string_components) {
  comma_string <- paste(string_components, collapse = ", ")
  comma_and_string <- sub(pattern = ",\\s([^,]+)$", 
                          replacement = " and \\1",
                          x = comma_string)
  return(comma_and_string)
}

comma_or <- function(string_components) {
  comma_string <- paste(string_components, collapse = ", ")
  comma_and_string <- sub(pattern = ",\\s([^,]+)$", 
                          replacement = " or \\1",
                          x = comma_string)
  return(comma_and_string)
}

is_truthy <- function(x) {
  valid <- tryCatch(
    expr = {
      x
      TRUE
    },
    error = function(e) {
      FALSE
    })
  if (valid) {
    return(isTruthy(x))
  } else {
    return(FALSE)
  }
}
