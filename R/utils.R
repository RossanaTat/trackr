#' Export progress results to Excel
#' @param res_list output of `track_progress()`
#' @param file_path path to save file
#' @export
export_results_to_excel <- function(res_list, file_path) {
  # Recursive helper to flatten nested list and keep names
  flatten_dts <- function(x, parent_name = NULL) {
    out <- list()

    for (nm in names(x)) {
      item <- x[[nm]]
      full_name <- if (!is.null(parent_name)) paste(parent_name, nm, sep = "_") else nm

      if (is.data.table(item) || is.data.frame(item)) {
        out[[full_name]] <- item
      } else if (is.list(item)) {
        out <- c(out, flatten_dts(item, full_name))
      }
    }

    return(out)
  }

  # Flatten the nested list into a named list of data.tables
  flat_list <- flatten_dts(res_list)

  if (length(flat_list) == 0) {
    stop("No data.tables found in the list.")
  }

  # Write to Excel — each data.table gets its own sheet
  write_xlsx(flat_list, path = file_path)

  message("Excel file saved to: ", file_path)
}
