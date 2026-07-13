testthat::test_that(
  desc = "BEAT LaCyTools data completes the standard pipeline", 
  code = {
    
    # TODO: Test actual pipeline
    # Later write a generic pipeline function that is called here.
    df <- data.frame(Name = c("Alice", "Bob"), Age = c(25, 30))
    
    testthat::expect_s3_class(df, "data.frame")
    
    testthat::expect_true(
      all(c("Name", "Age") %in% names(df))
    )
    
  }
)
