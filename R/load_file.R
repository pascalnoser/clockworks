#' Read data file
#'
#' @description
#' Reads a file and creates a data frame from it. Tries to infer the delimiter
#' from the file extension and uses the appropriate function among `read_csv()`,
#' `read_tsv()` and `read_delim()` from the `readr` package.
#'
#' @param filename A string containing the path to the meta data file.
#' @param delim A single character specifying the field separator. If `filename`
#'   points to a `.csv` or `.tsv` file this defaults to "," or TAB,
#'   respectively. Otherwise, needs to be specified.
#' @param ... Arguments to be passed to `read_delim()`
#'
#' @returns A `data.frame`.
#'
#' @importFrom readr read_delim read_csv read_tsv
#' @export
#'
#' @examples
#' filename <- system.file(
#'   "extdata",
#'   "SynthData_example_metadata.txt.gz",
#'   package = "clockworks",
#'   mustWork = TRUE
#' )
#' metadata <- load_file(filename = filename, delim = "\t")
#' head(metadata)
load_file <- function(filename, delim = NULL, ...) {
  # Get file extension
  file_extension <- get_extension(filename)

  # Unless `delim` is specified, determine right function to load file
  if (is.null(delim)) {
    dat <- switch(file_extension,
      csv = readr::read_csv(file = filename, ...),
      tsv = readr::read_tsv(file = filename, ...),
      stop("`delim` must be specified if file type is not `.csv` or `.tsv`")
    )
  } else {
    dat <- readr::read_delim(file = filename, delim = delim, ...)
  }

  return(dat)
}
