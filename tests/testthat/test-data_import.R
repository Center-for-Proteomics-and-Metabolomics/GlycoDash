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

