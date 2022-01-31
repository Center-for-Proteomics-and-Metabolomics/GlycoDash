library(glycodash)

test_that("define_clusters() works if clusters_regex is a list.", {
  df <- data.frame(analyte = c("IgGI1H5N3", "IgGII1H5N3"),
                   number = c(1:2))
  # Using expect_error with regexp = NA means that there should be no errors.
  expect_error(define_clusters(data = df,
                               clusters_regex = list("IgGI1", "IgGII1")),
               regexp = NA)
})

test_that("define_clusters() throws error when any element of clusters_regex results in zero matches", {
  data("long_data")
  expect_error(define_clusters(data = long_data,
                               clusters_regex = c("nonsense",
                                                  "non-existent")),
               "The regular expression\\(s\\) nonsense and non-existent matched no analytes in the \"analyte\" column of the data\\.")
  # Same test but with clusters_regex as a list:
  expect_error(define_clusters(data = long_data,
                               clusters_regex = list("nonsense",
                                                     "non-existent")),
               "The regular expression\\(s\\) nonsense and non-existent matched no analytes in the \"analyte\" column of the data\\.")
})

test_that("define_clusters() throws an error when any analytes in the data don't match with any of the regular expressions", {
  df <- data.frame(analyte = c("IgGI1H5N3", "IgGII1H5N3", "unmatched"),
                   number = c(1:3))
  expect_error(define_clusters(data = df,
                               clusters_regex = c("IgGI1", "IgGII1")),
               "Some analytes could not be assigned into a cluster\\. Please reconsider the regular expressions you gave as clusters_regex\\.")
  # Same test but with clusters_regex as a list:
  expect_error(define_clusters(data = df,
                               clusters_regex = list("IgGI1", "IgGII1")),
               "Some analytes could not be assigned into a cluster\\. Please reconsider the regular expressions you gave as clusters_regex\\.")
})

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
               regexp = "The data doesn't contain the required column\\(s\\) group and sample_name and cluster\\.")
  
})

test_that("summarize_spectra_checks() returns one row per cluster per spectrum", {
  data("long_data")
  long_data <- define_clusters(data = long_data,
                               clusters_regex = "IgGI1")
  to_replace <- sample(1:69888, 69888/2)
  long_data$cluster[to_replace] <- "IgGII1"
  expect_equal(nrow(summarize_spectra_checks(data = long_data,
                                        min_ppm_deviation = -20,
                                        max_ppm_deviation = 20,
                                        max_ipq = 0.2,
                                        min_sn = 9)),
               length(unique(long_data$cluster)) * length(unique(long_data$sample_name)))
})

test_that("curate_spectra() issues warnings if all/no spectra pass curation.", {
  # If quality criteria are very lenient, even the spectra that cut-offs are
  # based on (spectra that should not pass) pass the quality criteria checks.
  # This leads to very high cut-offs, which causes all spectra to fail curation:
  expect_warning(curate_spectra(data = long_data,
                                clusters_regex = "IgGI1",
                                min_ppm_deviation = 10000,
                                max_ppm_deviation = 10000,
                                max_ipq = 10000,
                                min_sn = 0,
                                group_to_filter = "Spike",
                                sample_type_to_filter = "CN"),
               "None of the spectra passed curation\\.")
  # On the other hand, when criteria are really strict, none of the spectra pass
  # either.
  expect_warning(curate_spectra(data = long_data,
                                clusters_regex = "IgGI1",
                                min_ppm_deviation = 0,
                                max_ppm_deviation = 0,
                                max_ipq = 0,
                                min_sn = 10000,
                                group_to_filter = "Spike",
                                sample_type_to_filter = "CN"),
                 "None of the spectra passed curation\\.")
})
