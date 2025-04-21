.onLoad <- function(libname, pkgname) {
  op <- options()
  op.sgtrackr <- list(
    sgtrackr.source      = "wdi",
    sgtrackr.targetyear  = 2030,
    #sgtrackr.default_granularity = 0.1, #think about it
    sgtrackr.pctl        = FALSE,
    sgtrackr.speed       = TRUE,
    sgtrackr.future      = TRUE,
    sgtrackr.default_pctlseq     = seq(20, 80, 20),
    sgtrackr.default_speedseq    = c(0.25, 0.5, 1, 2, 4)
  )
  toset <- !(names(op.sgtrackr) %in% names(op))
  if (any(toset)) options(op.sgtrackr[toset])
  invisible()
}

# Get options
## Example
## source <- get_sgtrackr_opt("default_source")

get_sgtrackr_opt <- function(name) {
  getOption(paste0("sgtrackr.", name))
}
