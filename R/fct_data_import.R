

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
