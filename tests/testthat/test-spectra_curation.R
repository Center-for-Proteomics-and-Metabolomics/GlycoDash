library(glycodash)

test_that("do_criteria_check function arguments fulfill the requirements.", {
  data("long_data")
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
