test_that("curate_analytes() returns a dataframe with one row for each analyte + charge combination per cluster", {
  data("example_data")
  example_data <- curate_spectra(data = example_data,
                                 min_ppm_deviation = -20,
                                 max_ppm_deviation = 20,
                                 max_ipq = 0.2,
                                 min_sn = 9,
                                 clusters_regex = "IgGI1",
                                 cut_off_basis = "PBS")
  
  curated_spectra <- example_data$curated_data %>%
    dplyr::filter(has_passed_spectra_curation == TRUE)
  
  curated_analytes <- curate_analytes(data = curated_spectra,
                                      group_to_ignore = "Total",
                                      sample_types_to_ignore = c("IVIGg",
                                                                 "Visucon", 
                                                                 "PBS"),
                                      cut_off_percentage = 25)
  
  n_analyte_charge_per_cluster <- curated_spectra %>% 
    dplyr::distinct(., analyte, charge, cluster) %>% 
    dplyr::summarise(n = nrow(.)) %>% 
    as.numeric()
  
  expect_equal(nrow(curated_analytes),
               n_analyte_charge_per_cluster)
})
