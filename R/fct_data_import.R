#' Detect plate and well
#'
#' @param data A dataframe. Should include a column named "sample_name".
#'
#' @return
#' @export
#'
#' @examples
detect_plate_and_well <- function(data) {
  data %>% 
    extract(col = sample_name, 
            into = c("plate", "well"),
            regex = "([-_][Pp]l\\d).+(_[A-H]\\d{1,2})", 
            remove = FALSE) %>% 
    mutate(plate = str_extract(plate, "\\d+"),
           well = str_extract(well, "[A-H]\\d{1,2}"),
           plate_well = paste(plate, well, sep = "_")) %>% 
    select(-c(plate, well)) %>% 
    relocate(plate_well, .after = sample_name)
}

read_non_rectangular <- function(path, delim) {
  lines <- readLines(path, n = -1, ok = TRUE)
  columns <- stringr::str_split(lines, pattern = delim)
  n_columns <- max(purrr::map_int(columns, ~ length(.x)))
  column_names <- vector()
  for (i in 1:n_columns) {
    column_names[i] <- paste("col", i, sep = "_")
  }
  data <- read.table(path, fill = TRUE, header = FALSE, col.names = column_names, 
                     sep = delim, blank.lines.skip = FALSE, na.strings = "")
  return(data)
}