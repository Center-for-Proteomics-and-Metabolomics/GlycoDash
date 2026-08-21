# Basic smoke tests for the GlycoDash Golem application.
#
# These tests verify that the main application entry points still exist,
# that the Golem configuration can be read, and that the application can
# start. They do not test the scientific data-processing pipeline; those
# tests should be added in separate test files.

testthat::test_that("app_ui returns a valid Shiny tag list", {
  ui <- app_ui()
  
  # The UI function should return a valid Shiny tag or tag list.
  golem::expect_shinytaglist(ui)
  
  # Keep this check because Golem expects app_ui() to retain the
  # request argument used when the application is started.
  ui_arguments <- names(formals(app_ui))
  
  testthat::expect_true(
    "request" %in% ui_arguments
  )
})


testthat::test_that("app_server has the expected interface", {
  # app_server should remain a function.
  testthat::expect_type(
    app_server,
    "closure"
  )
  
  # These arguments form the standard Shiny server interface.
  # Removing or renaming them would prevent the app from starting normally.
  server_arguments <- names(formals(app_server))
  
  testthat::expect_true(
    all(c("input", "output", "session") %in% server_arguments)
  )
})


testthat::test_that("golem configuration file is installed", {
  config_file <- app_sys("golem-config.yml")
  
  # app_sys() should resolve the file inside the installed package.
  testthat::expect_true(
    nzchar(config_file)
  )
  
  testthat::expect_true(
    file.exists(config_file)
  )
})


testthat::test_that("golem configuration contains the expected app_prod values", {
  # GlycoDash should run in production mode when the production
  # configuration is selected.
  testthat::expect_true(
    get_golem_config(
      "app_prod",
      config = "production"
    )
  )
  
  # The development configuration should not set app_prod to TRUE.
  testthat::expect_false(
    get_golem_config(
      "app_prod",
      config = "dev"
    )
  )
})


testthat::test_that("app can be launched", {
  # This is a lightweight launch smoke test.
  #
  # golem::expect_running() may be skipped automatically when tests are
  # executed in a non-interactive subprocess. Such a skip is expected and
  # is not a test failure.
  golem::expect_running(
    sleep = 5
  )
})
