#' Title
#'
#' @param cd A `CircadianData` object.
#' @param colname_time A string specifying the name of the column in the meta
#'   data of `cd` that contains the time information.
#'
#' @importFrom limorhyde limorhyde
#'
#' @returns A data frame with the results of a LimoRhyde analysis.
run_limorhyde <- function(cd, colname_time) {
  limo <- limorhyde::limorhyde(
    time = metadata(cd)[[colname_time]],
    colnamePrefix = "time_",
    period = experimentInfo(cd)$period
  )

  metadata(cd) <- cbind(metadata(cd), limo)
}
