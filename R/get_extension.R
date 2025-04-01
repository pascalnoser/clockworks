#' Get the extension of a file
#'
#' @param path Character vector with one element containing the path to a file.
#'
#' @returns A character vector containing the file extension. The extensions
#'   `.gz`, `.bz2`, `.xz`, and `.zip` will be ignored, so the returned value for
#'   e.g. `myfile.txt.gz` would be `txt`.
#' @export
#'
#' @examples
#' x <- "path/to/my/file.txt"
#' get_extension(x)
get_extension <- function(path) {
  split_elements <- strsplit(basename(path), split = "\\.")[[1]]
  n_elements <- length(split_elements)
  last_element <- split_elements[n_elements]

  # If compressed file format, return second to last element (e.g. for
  # "file.txt.gz" return "txt")
  if (last_element %in% c("gz", "bz2", "xz", "zip")) {
    extension <- split_elements[length(split_elements) - 1]
  } else {
    extension <- last_element
  }

  return(extension)
}
