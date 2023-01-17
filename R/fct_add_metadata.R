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
#'                     package = "glycodash")
#'
#' read_metadata(filepaths = list(path),
#'               filenames = list("Metadata_example.xlsx"))
read_metadata <- function(filepaths, filenames) {
  
  metadata_list <- purrr::pmap(
    list(path = filepaths,
         name = filenames),
    function(path, name) {
      extension <- tools::file_ext(name)
      if (extension %in% c("xlsx", "xls")) {
        metadata <- readxl::read_excel(path,
                                       col_types = "text",
                                       na = c("", "NA"))
      } else {
        if (extension == "rds") {
          metadata <- load_and_assign(path)
        } else {
          rlang::abort(
            class = "wrong_extension",
            message = "Please upload a .xlsx, .xls or .rds file."
          )
        }
      }
      metadata <- metadata %>%
        dplyr::rename_with(.cols = tidyselect::everything(),
                           .fn = snakecase::to_snake_case)
      
      return(metadata)
    })
  
  names(metadata_list) <- filenames
  
  return(metadata_list)
}


prep_metadata <- function(input,
                          metadata_list,
                          sample_id_inputIds,
                          date_column_inputIds) {
  
  metadata_list_prepped <- purrr::pmap(
    list(metadata_list,
         sample_id_inputIds,
         date_column_inputIds),
    function(metadata, 
             sample_id_inputId,
             date_column_inputId) {
      # Check whether the metadata contains a column named "sample_id" that
      # is not chosen as the sample ID column. If this is the case, the
      # column needs to be renamed to prevent duplicated names (which would
      # cause the app to crash)
      conflict <- "sample_id" %in% colnames(metadata) & input[[sample_id_inputId]] != "sample_id"
      if (conflict) {
        metadata <- metadata %>% 
          dplyr::rename(sample_id_original = sample_id)
        
        rlang::warn(class = "sample_id_conflict",
                    message = paste("The column originally named \"sample_id\" was renamed",
                                    "as \"sample_id_original\" to avoid duplicate column names."))
      }
      
      prepped_metadata <- withCallingHandlers(
        expr = {
          metadata %>% 
            dplyr::mutate(dplyr::across(tidyselect::any_of(input[[date_column_inputId]]), 
                                        date_with_text)) %>% 
            dplyr::rename(sample_id = input[[sample_id_inputId]])
        },
        text_in_dates = function(c) {
          rlang::warn(class = "text_in_dates",
                      message = c$message)
        })
      
      return(prepped_metadata)
    })
  
  merged_metadata <- purrr::reduce(metadata_list_prepped,
                                   dplyr::full_join,
                                   by = "sample_id")
  return(merged_metadata)
}

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
