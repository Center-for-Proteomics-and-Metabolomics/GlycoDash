extdata_path <- function(...) {
  path <- system.file(
    "extdata",
    ...,
    package = "GlycoDash",
    mustWork = TRUE
  )
  
  if (!nzchar(path)) {
    stop(
      "Could not locate the requested file in inst/extdata.",
      call. = FALSE
    )
  }
  
  return(path)
}
