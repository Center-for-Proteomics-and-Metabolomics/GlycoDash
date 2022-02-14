test_that("read_non_rectangular can read in files with different delimiters", {
  path_to_csv <- system.file("inst",
                             "extdata",
                             "flatfile.csv",
                             package = "glycodash")
  
  path_to_txt <- system.file("inst",
                             "extdata",
                             "flatfile.txt",
                             package = "glycodash")
  
  expect_error(read_non_rectangular(path = path_to_csv,
                                    delim = ";"),
               regexp = NA)
  
  expect_error(read_non_rectangular(path = path_to_txt,
                                    delim = "\t"), 
               regexp = NA)
  
  
  
  expect_equal(object = {
    data <- read_non_rectangular(path_to_txt)
    na_rows <- data %>% 
      dplyr::filter(dplyr::if_all(.cols = tidyr::everything(),
                                  .fns = ~ is.na(.x)))
    return(nrow(na_rows))
  },
  expected = 1)
  
})

test_that("number of columns of dataframe read in with read_non_rectangular() is equal to the widest part of the flat file", {
  path_to_csv <- system.file("inst",
                             "extdata",
                             "flatfile.csv",
                             package = "glycodash")
  
  expect_length(object = read_non_rectangular(path_to_csv,
                                              delim = ";"),
               expected = 4)
})

test_that("read_non_rectangular reads blank lines as a row with only NA's", {
  path_to_txt <- system.file("inst",
                             "extdata",
                             "flatfile.txt",
                             package = "glycodash")
  
  expect_equal(object = {
    data <- read_non_rectangular(path_to_txt)
    na_rows <- data %>% 
      dplyr::filter(dplyr::if_all(.cols = tidyr::everything(),
                                  .fns = ~ is.na(.x)))
    return(nrow(na_rows))
  },
  expected = 1)
  
})

test_that("read_non_rectangular() throws an error if the resulting dataframe has only one column", {
  path_to_lacytools <- system.file("inst",
                                   "extdata",
                                   "LacyTools_summary.txt",
                                   package = "glycodash")
  expect_warning(read_non_rectangular(path_to_lacytools, delim = ";"),
               "The file seems to consist of a single column\\. Are you sure that you chose the correct delimiter for your file?" )
})

test_that("find_next_na() returns an empty integer vector if there are no next lines with NA's", {
  df <- data.frame(name = c("John", NA, "Melany", "Bobby"),
                   age = c(56, NA, 34, 26))
  expect_length(find_next_na(data = df, 
                             row = 3), 
                0)
})

test_that("detect_plate_and_well() can identify plate numbers in all allowed formats", {
  df <- data.frame(sample_name = c("Testname_P-8_well_H5", 
                                   "Testname_PL_8_well_H5",
                                   "Testname_p.8_well_H5",
                                   "Testname_pl8_well_H5",
                                   "Testname_Pl8_well_H5",
                                   "Testname_pL8_well_H5",
                                   "Testname_Plate.8_well_H8",
                                   "Testname_plate.8_well_H8"))
  expect_error(detect_plate_and_well(df),
               regexp = NA)
})

test_that("detect_plate_and_well() doesn't interpret a number larger than 12 or a letter not A-H as a well", {
  df <- data.frame(sample_name = c("Testname_PL.8_well_H87", 
                                   "Testname_pl5_well_L3"))
  expect_error(detect_plate_and_well(df),
               regexp = "For the sample\\(s\\) Testname_PL\\.8_well_H87 and Testname_pl5_well_L3 the plate and well could not be determined\\.")
})

test_that("read_and_process_plate_design() throws an error when plate design file is formatted incorrectly.", {
  file_no_blank_line <- system.file("inst",
                                    "extdata",
                                    "Plate_design_no_blank_line_between_plates.xlsx",
                                    package = "glycodash")
  
  expect_error(read_and_process_plate_design(file_no_blank_line),
               regexp = "Please check that your plate design file is formatted correctly\\. Run `\\?read_and_process_plate_design` to find the required format\\.")
  
  file_with_title <- system.file("inst",
                                 "extdata",
                                 "Plate_design_with_title.xlsx",
                                 package = "glycodash")
  
  expect_error(read_and_process_plate_design(file_with_title),
               regexp = "Please check that your plate design file is formatted correctly\\. Run `\\?read_and_process_plate_design` to find the required format\\.")
})

test_that("read_and_process_plate_design() throws a warning when no duplicates are found.", {
  file_duplicates_wrong <- system.file("inst",
                                       "extdata",
                                       "Plate_design_duplicates_wrong.xlsx",
                                       package = "glycodash")
  
  expect_warning(read_and_process_plate_design(file_duplicates_wrong),
                 regexp = "No duplicates were found in your plate design file\\.")
})

test_that("read_and_process_plate_design() returns a dataframe with the number of plates * 96 as number of rows", {
  plate_design_file <- system.file("inst",
                                   "extdata",
                                   "Plate_design.xlsx",
                                   package = "glycodash")
  expect_equal(nrow(read_and_process_plate_design(plate_design_file = plate_design_file)),
               96 * 7)
})

test_that("load_and_assign() doesn't alter the R-object", {
  old_name <- c("This is a random vector",
                "to serve as an example")
  
  file_path <- file.path(tempdir(),
                         "R_object.rds")
  
  save(old_name,
       file = file_path)
  
  new_name <- load_and_assign(file_path)
  
  expect_identical(object = new_name,
                   expected = old_name)
})

test_that("load_and_assign() throws an error for objects saved with saveRDS()", {
  old_name <- c("This is a random vector",
                "to serve as an example")
  
  file_path <- file.path(tempdir(),
                         "R_object.rds")
  
  saveRDS(old_name,
          file = file_path)
  
  expect_error(
    object = {
    new_name <- load_and_assign(file_path)
    },
    regexp = "Check if the file_path was not misspelled\\.") 
  
})

test_that("date_with_text() converts date without comments to class \"Date\"", {
  path <- system.file("inst",
                      "extdata",
                      "Dates.xlsx",
                      package = "glycodash")
  
  dates <- readxl::read_excel(path,
                              col_types = "text")
  
  dates <- dates %>%
    dplyr::mutate(dplyr::across(.cols = date,
                                .fns = date_with_text))
  
  expect_s3_class(object = dates$date,
                  class = "Date")
  
})

test_that("date_with_text() converts date with comments to class \"Character\"", {
  path <- system.file("inst",
                      "extdata",
                      "Dates_with_comment.xlsx",
                      package = "glycodash")
  
  dates <- readxl::read_excel(path,
                              col_types = "text")
  
  dates <- dates %>%
    dplyr::mutate(dplyr::across(.cols = date,
                                .fns = date_with_text))
  
  expect_type(object = dates$date,
              type = "character")
  
})

test_that("date_with_text() throws an error when used outside of dplyr::across()", {
  path <- system.file("inst",
                      "extdata",
                      "Dates_with_comment.xlsx",
                      package = "glycodash")
  
  dates <- readxl::read_excel(path,
                              col_types = "text")
  
  date_with_text(dates$date)
  
  expect_type(object = dates$date,
              type = "character")
  
})
