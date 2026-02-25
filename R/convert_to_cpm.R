#' Convert count matrix in a CircadianData object to CPM / log-CPM
#'
#' Calculate counts-per-million (CPM) for the count matrix stored inside a
#' CircadianData object. Library sizes are adjusted using the sample
#' normalization factors calculated by edgeR's `normLibSizes` which are
#' stored in the CircadianData object. CPM values are then calculated using
#' edgeR's `cpm` function with the adjusted library sizes.
#'
#' @param cd A `CircadianData` object containing count data in the `results` slot and a
#'   \code{norm_factors} vector of length equal to the number of samples.
#' @param log Logical, whether to return log2-transformed CPM values. Default is TRUE.
#'
#' @importFrom edgeR cpm
#'
#' @returns A `CircadianData` object with the dataset slot replaced by the CPM or
#'   log-CPM values.
convert_to_cpm <- function(cd, log = TRUE) {
  counts <- get_dataset(cd)
  meta <- get_metadata(cd)

  # Calculate CPM values using edgeR's cpm function with library size normalization
  lib_sizes = colSums(counts) * meta$norm_factors
  logCPM = edgeR::cpm(counts, lib.size = lib_sizes, log = log)

  # # Manual calculation of CPM values
  # lib_sizes = colSums(counts) * meta$norm_factors
  # ave_lib_size = mean(lib_sizes)
  # prior_count = 2

  # # Adjust prior count for each library
  # adj_prior = sweep(
  #   matrix(prior_count, nrow(counts), ncol(counts)),
  #   2,
  #   lib_sizes / ave_lib_size,
  #   FUN = "*"
  # )

  # # Add adjusted prior to counts
  # counts_with_prior = counts + adj_prior

  # # Calculate log CPM
  # logCPM_manual = sweep(counts_with_prior, 2, lib_sizes / 1e6, FUN = "/") |> log2()

  # Replace dataset in CD object with logCPM values
  dataset(cd) <- logCPM

  return(cd)
}
