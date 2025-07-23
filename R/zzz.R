.onLoad <- function(libname, pkgname) {
  op <- options()
  op.trackr <- list(
    trackr.targetyear                = 2030,
    trackr.pctl                      = FALSE,
    trackr.speed                     = TRUE,
    trackr.future                    = TRUE,
    trackr.default_sequence_pctl     = seq(20, 80, 20),
    trackr.default_speedseq          = c(0.25, 0.5, 1, 2, 4),
    trackr.extreme_pctl              = 0.5

  )
  toset <- !(names(op.trackr) %in% names(op))
  if (any(toset)) options(op.trackr[toset])
  invisible()
}

# Get options
## Example
## source <- get_sgtrackr_opt("default_source")

get_sgtrackr_opt <- function(name) {
  getOption(paste0("trackr.", name))
}
