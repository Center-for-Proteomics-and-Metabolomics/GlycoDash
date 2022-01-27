library(glycodash)

data("long_data")

test_that("do_criteria_check() arguments fulfill the requirements.", {
  expect_error(do_criteria_check(data = long_data, 
                                 min_ppm_deviation = "oops",
                                 max_ppm_deviation = "this is not numeric",
                                 max_ipq = 0.2,
                                 min_sn = as.Date.character("02-01-1997",
                                                            format = "%d-%m-%Y")), 
               "One or more quality criteria arguments are non-numeric.")
  wrong_data <- long_data %>% 
    dplyr::select(-sn)
  expect_error(do_criteria_check(data = wrong_data, 
                                 min_ppm_deviation = -20,
                                 max_ppm_deviation = 20,
                                 max_ipq = 0.2,
                                 min_sn = 9), 
               "The data doesn't contain the required columns with the quality criteria.")
})

test_that("do_criteria_check() returns a logical vector without NA's.", {
  data("long_data")
  criteria_check <- do_criteria_check(data = long_data, 
                                      min_ppm_deviation = -20,
                                      max_ppm_deviation = 20,
                                      max_ipq = 0.2,
                                      min_sn = 9)
  expect_type(criteria_check,
              "logical")
  expect_false(anyNA(criteria_check))
})

test_that("summarize_spectra_checks() verifies the existence of required columns", {
  data("long_data")
  wrong_data <- long_data %>% 
    dplyr::select(-c(sample_name, group))
  expect_error(summarize_spectra_checks(data = wrong_data,
                                        min_ppm_deviation = -20,
                                        max_ppm_deviation = 20,
                                        max_ipq = 0.2,
                                        min_sn = 9),
               regexp = "The data doesn't contain the required column(s) group and sample_name.", 
               fixed = TRUE)
  
})
