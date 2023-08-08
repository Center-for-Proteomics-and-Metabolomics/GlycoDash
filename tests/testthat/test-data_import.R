test_that("read_non_rectangular can read in files with different delimiters", {
  path_to_csv <- system.file("extdata",
                             "for_tests",
                             "flatfile.csv",
                             package = "GlycoDash")
  
  path_to_txt <- system.file("extdata",
                             "for_tests",
                             "flatfile.txt",
                             package = "GlycoDash")
  
  expect_no_error(read_non_rectangular(path = path_to_csv,
                                    delim = ";"))
  
  expect_no_error(read_non_rectangular(path = path_to_txt,
                                    delim = "\t"))
  
  
  
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
  path_to_csv <- system.file("extdata",
                             "for_tests",
                             "flatfile.csv",
                             package = "GlycoDash")
  
  expect_length(object = read_non_rectangular(path_to_csv,
                                              delim = ";"),
                n = 4)
})

test_that("read_non_rectangular reads blank lines as a row with only NAs", {
  path_to_txt <- system.file("extdata",
                             "for_tests",
                             "flatfile.txt",
                             package = "GlycoDash")
  
  data <- read_non_rectangular(path_to_txt)
  
  na_rows <- data %>% 
    dplyr::filter(dplyr::if_all(.cols = tidyr::everything(),
                                .fns = ~ is.na(.x)))
  
  expect_equal(
    object = nrow(na_rows),
    expected = 1)
  
})

test_that("read_non_rectangular() throws an error if the resulting dataframe has only one column", {
  path_to_lacytools <- system.file("extdata",
                                   "LaCyTools_summary_example.txt",
                                   package = "GlycoDash")
  expect_warning(read_non_rectangular(path_to_lacytools, delim = ";"),
               "The file seems to consist of a single column\\. Are you sure that you chose the correct delimiter for your file?" )
})

test_that("read_non_rectangular() throws an error if an empty file or dummy file is uploaded", {
  path_to_dummy <- system.file("extdata",
                               "dummy.txt",
                               package = "GlycoDash")
  
  expect_error(read_non_rectangular(path_to_dummy, delim = "\t"),
               class = "embedded_null")
  
  path_to_wrong_encoding <- system.file("extdata",
                                        "for_tests",
                                        "LaCyTools_summary_example_UTF16LE.txt",
                                        package = "GlycoDash")
  
  expect_error(read_non_rectangular(path_to_wrong_encoding, delim = "\t"),
               class = "embedded_null")
  
  path_to_empty <- system.file("extdata",
                               "for_tests",
                               "empty.txt",
                               package = "GlycoDash")
  
  expect_error(read_non_rectangular(path_to_empty, delim = "\t"),
               class = "empty_file")
})

test_that("find_next_na() returns an empty integer vector if there are no next lines with NA's", {
  df <- data.frame(name = c("John", NA, "Melany", "Bobby"),
                   age = c(56, NA, 34, 26))
  expect_length(find_next_na(data = df, 
                             row = 3), 
                0)
})

test_that("detect_plate_and_well() can identify plate numbers in all allowed formats", {
  df <- data.frame(sample_name = c("Testname_plate8_well_H5", 
                                   "Testname_pl8_well_H5",
                                   "Testname_Plate8_well_H5",
                                   "Testname_Pl8_well_H5",
                                   "Testname_PLATE8_well_H5",
                                   "Testname_PL8_well_H5",
                                   "Testname_PL8_well_H05",
                                   "Testname_PL8H5"))
  expect_no_error(detect_plate_and_well(df))
})

test_that("detect_plate_and_well() doesn't interpret a letter and number combination as the well position if it precedes the plate number", {
  df <- data.frame(sample_name = c("Testname1593E6_plate8_H5",
                                   "TestnameE71593_plate8_H6"))
  
  expect_equal(detect_plate_and_well(df)$plate_well, c("8_H5",
                                                       "8_H6"))
  
  df_well_missing <- data.frame(sample_name = c("Testname1593E6_plate8",
                                                "TestnameE71593_plate8"))
  
  expect_condition(detect_plate_and_well(df_well_missing),
                   class = "well_precedes_plate")
})

test_that("detect_plate_and_well() doesn't interpret a number larger than 12 or a letter not A-H as a well", {
  df <- data.frame(sample_name = c("Testname_PL.8_well_H87", 
                                   "Testname_pl5_well_L3"))
  expect_error(detect_plate_and_well(df),
               regexp = "For the sample\\(s\\) Testname_PL\\.8_well_H87 and Testname_pl5_well_L3 the plate and well could not be determined\\.")
})

test_that("read_and_process_plate_design() throws an error when plate design file is formatted incorrectly.", {
  file_no_blank_line <- system.file("extdata",
                                    "for_tests",
                                    "Plate_design_no_blank_line_between_plates.xlsx",
                                    package = "GlycoDash")
  
  expect_error(read_and_process_plate_design(file_no_blank_line),
               regexp = "Please check that your plate design file is formatted correctly\\. Run `\\?read_and_process_plate_design` to find the required format\\.")
  
  file_with_title <- system.file("extdata",
                                 "for_tests",
                                 "Plate_design_with_title.xlsx",
                                 package = "GlycoDash")
  
  expect_error(read_and_process_plate_design(file_with_title),
               regexp = "Please check that your plate design file is formatted correctly\\. Run `\\?read_and_process_plate_design` to find the required format\\.")
})

test_that("read_and_process_plate_design() returns a dataframe with the number of plates * 96 as number of rows", {
  plate_design_file <- system.file("extdata",
                                   "Plate_design_example.xlsx",
                                   package = "GlycoDash")
  
    expect_equal(
      object = nrow(read_and_process_plate_design(plate_design_file)),
      expected = 96 * 7
      )
})


test_that("define_clusters() works if cluster_keywords is a list.", {
  df <- data.frame(analyte = c("IgGI1H5N3", "IgGII1H5N3"),
                   number = c(1:2))
  
  expect_no_error(define_clusters(data = df,
                               cluster_keywords = list("IgGI1", "IgGII1")))
})

test_that("define_clusters() throws an error when any analytes in the data don't match with any of the keywords", {
  df <- data.frame(analyte = c("IgGI1H5N3", "IgGII1H5N3", "unmatched"),
                   number = c(1:3))
  expect_error(define_clusters(data = df,
                               cluster_keywords = c("IgGI1", "IgGII1")),
               "Some analytes could not be assigned into a cluster\\. Please reconsider your cluster keywords\\.")
  # Same test but with cluster_keywords as a list:
  expect_error(define_clusters(data = df,
                               cluster_keywords = list("IgGI1", "IgGII1")),
               "Some analytes could not be assigned into a cluster\\. Please reconsider your cluster keywords\\.")
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

test_that("read_metadata() does not throw an error when colnames contain special characters", {
  
  path_to_metadata <- system.file("extdata",
                                  "for_tests",
                                  "Metadata_example_special_chars.xlsx",
                                  package = "GlycoDash")
  
  expect_no_error(read_metadata(filepaths = list(path_to_metadata),
                                filenames = list("Metadata_example_special_chars.xlsx")))
  
})
