extdata_path <- function(...) {
  system.file(
    "extdata",
    ...,
    package = "GlycoDash",
    mustWork = TRUE
  )
}
