#' Define the CircadianData Class
#'
#' @slot dataset A matrix containing measurement data (features x samples).
#' @slot metadata A data.frame containing sample annotations (samples x attributes).
#' @slot experiment_info A list to store additional arbitrary information or
#'   parameters related to the experiment (e.g., period, design details).
#'
#' @details
#' This class stores data from biological experiments (e.g., circadian studies),
#' ensuring that the samples (columns of `dataset`, rows of `metadata`) remain
#' synchronized during operations like subsetting or renaming. The rows of
#' `dataset` represent features (e.g., genes, proteins, metabolites).
#'
#' @name CircadianData-class
#' @rdname CircadianData-class
#' @exportClass CircadianData
setClass("CircadianData",
         slots = c(
           dataset = "matrix",
           metadata = "data.frame",
           experiment_info = "list" # Renamed from otherData
         )
)


# --- Validity Check ---

setValidity("CircadianData", function(object) {
  errors <- character()

  # Check 1: dataset columns and metadata rows must have names
  if (is.null(colnames(object@dataset))) {
    errors <- c(errors, "The 'dataset' matrix must have column names (sample IDs).")
  }
  if (is.null(rownames(object@metadata))) {
    errors <- c(errors, "The 'metadata' data.frame must have row names (sample IDs).")
  }

  # Check 2: Dimensions must match
  if (ncol(object@dataset) != nrow(object@metadata)) {
    msg <- sprintf(
      "Number of columns in 'dataset' (%d) does not match number of rows in 'metadata' (%d).",
      ncol(object@dataset), nrow(object@metadata)
    )
    errors <- c(errors, msg)
  } else {
    # Check 3: Sample IDs must match exactly (if names exist and dimensions match)
    if (!is.null(colnames(object@dataset)) && !is.null(rownames(object@metadata))) {
      if (!identical(colnames(object@dataset), rownames(object@metadata))) {
        errors <- c(errors, "Column names of 'dataset' must be identical to row names of 'metadata'. Check order and values.")
      }
    }
  }

  # Check 4: dataset should ideally have row names (feature IDs)
  if (is.null(rownames(object@dataset))) {
    warning("The 'dataset' matrix ideally should have row names (feature IDs).")
    # Not enforcing as an error, but good practice.
  }

  # Check 5: experiment_info must be a list
  if (!is.list(object@experiment_info)) {
    errors <- c(errors, "'experiment_info' slot must be a list.")
  }

  if (length(errors) == 0) TRUE else errors
})


# --- Constructor Function ---

#' Create a CircadianData Object
#'
#' @param dataset A numeric matrix with features as rows and samples as columns.
#'        Must have column names (sample IDs) and preferably row names (feature IDs).
#'        Features could be genes, proteins, metabolites, etc.
#' @param metadata A data.frame with samples as rows and annotations as columns.
#'        Must have row names (sample IDs) that match the column names of `dataset`
#'        exactly (in order and value).
#' @param experiment_info An optional list to store additional experiment-level
#'        data or parameters (e.g., experimental period, batch information, notes).
#'
#' @return A \code{CircadianData} object.
#' @export
#' @examples
#' # Sample Data
#' counts <- matrix(rpois(100, lambda = 10), nrow = 10, ncol = 10,
#'                  dimnames = list(paste0("Feature", 1:10), paste0("Sample", 1:10)))
#' meta <- data.frame(
#'   row.names = paste0("Sample", 1:10),
#'   group = rep(c("Control", "Treated"), each = 5),
#'   time = rep(seq(0, 8, by = 2), 2)
#' )
#'
#' # Create object
#' cd_obj <- CircadianData(dataset = counts, metadata = meta)
#' print(cd_obj)
#'
#' # Add experiment info
#' expInfo <- list(period = 24, notes = "Initial experiment")
#' experiment_info(cd_obj) <- expInfo
#' print(experiment_info(cd_obj))
#'
CircadianData <- function(dataset, metadata, experiment_info = list()) {

  # Basic checks before creating the object
  if (!is.matrix(dataset)) stop("'dataset' must be a matrix.")
  if (!is.data.frame(metadata)) stop("'metadata' must be a data.frame.")
  if (!is.list(experiment_info)) stop("'experiment_info' must be a list.")

  # Ensure names match - crucial step before calling new() which triggers validity
  if (is.null(colnames(dataset))) stop("'dataset' must have column names.")
  if (is.null(rownames(metadata))) stop("'metadata' must have row names.")

  if (ncol(dataset) != nrow(metadata)) {
    stop(sprintf(
      "Number of columns in 'dataset' (%d) must match number of rows in 'metadata' (%d).",
      ncol(dataset), nrow(metadata)
    ))
  }

  # Ensure sample IDs match exactly
  if (!identical(colnames(dataset), rownames(metadata))) {
    stop("Column names of 'dataset' must be identical to, and in the same order as, row names of 'metadata'.")
  }

  # Create the object (validity check runs automatically)
  new("CircadianData",
      dataset = dataset,
      metadata = metadata,
      experiment_info = experiment_info)
}


# --- Accessor Methods ---

#' Get the Dataset Matrix
#' @param x A \code{CircadianData} object.
#' @return The dataset matrix (features x samples).
#' @export
#' @rdname CircadianData-accessors
setGeneric("dataset", function(x) standardGeneric("dataset"))

#' @rdname CircadianData-accessors
setMethod("dataset", "CircadianData", function(x) x@dataset)

#' Get the Metadata Data Frame
#' @param x A \code{CircadianData} object.
#' @return The metadata data.frame (samples x attributes).
#' @export
#' @rdname CircadianData-accessors
setGeneric("metadata", function(x) standardGeneric("metadata"))

#' @rdname CircadianData-accessors
setMethod("metadata", "CircadianData", function(x) x@metadata)

#' Set the Metadata Data Frame
#' @param x A \code{CircadianData} object.
#' @param value A data.frame to replace the current metadata
#' @return The modified \code{CircadianData} object.
#' @export
#' @rdname CircadianData-accessors
setGeneric("metadata<-", function(x, value) standardGeneric("metadata<-"))

#' @rdname CircadianData-accessors
setReplaceMethod("metadata", "CircadianData", function(x, value) {
  if (!inherits(value, "data.frame")) stop("'value' must be a data frame.")
  x@metadata <- value
  validObject(x) # Re-validate
  x
})

#' Get the Experiment Information List
#' @param x A \code{CircadianData} object.
#' @return The experiment_info list.
#' @export
#' @rdname CircadianData-accessors
setGeneric("experiment_info", function(x) standardGeneric("experiment_info"))

#' @rdname CircadianData-accessors
setMethod("experiment_info", "CircadianData", function(x) x@experiment_info)

#' Set the Experiment Information List
#' @param x A \code{CircadianData} object.
#' @param value A list to replace the current experiment_info.
#' @return The modified \code{CircadianData} object.
#' @export
#' @rdname CircadianData-accessors
setGeneric("experiment_info<-", function(x, value) standardGeneric("experiment_info<-"))

#' @rdname CircadianData-accessors
setReplaceMethod("experiment_info", "CircadianData", function(x, value) {
  if (!is.list(value)) stop("'value' must be a list.")
  x@experiment_info <- value
  validObject(x)
  x
})


# --- Basic Methods (Dimensions, Dimnames) ---

#' Get Dimensions
#' @param x A \code{CircadianData} object.
#' @return Integer vector of length 2 (features, samples).
#' @export
setMethod("dim", "CircadianData", function(x) {
  dim(x@dataset)
})

#' Get Number of Rows (Features)
#' @param x A \code{CircadianData} object.
#' @return Integer, number of rows (features).
#' @export
setMethod("nrow", "CircadianData", function(x) {
  nrow(x@dataset)
})

#' Get Number of Columns (Samples)
#' @param x A \code{CircadianData} object.
#' @return Integer, number of columns (samples).
#' @export
setMethod("ncol", "CircadianData", function(x) {
  ncol(x@dataset)
})

#' Get Dimension Names
#' @param x A \code{CircadianData} object.
#' @return A list containing row names (features) and column names (samples).
#' @export
setMethod("dimnames", "CircadianData", function(x) {
  dimnames(x@dataset)
})

#' Get Row Names (Features)
#' @param x A \code{CircadianData} object.
#' @return Character vector of row names (feature IDs).
#' @export
setMethod("rownames", "CircadianData", function(x) {
  rownames(x@dataset)
})

#' Get Column Names (Samples)
#' @param x A \code{CircadianData} object.
#' @return Character vector of column names (sample IDs).
#' @export
setMethod("colnames", "CircadianData", function(x) {
  colnames(x@dataset)
})


# --- Replacement Methods (Dimnames - Crucial for Sync) ---

#' Set Dimension Names
#' @param x A \code{CircadianData} object.
#' @param value A list of two character vectors (feature names, sample names).
#' @return The modified \code{CircadianData} object.
#' @export
setReplaceMethod("dimnames", "CircadianData", function(x, value) {
  if (!is.list(value) || length(value) != 2) {
    stop("'value' must be a list of length 2 (row names, column names).")
  }
  rn <- value[[1]] # Feature names
  cn <- value[[2]] # Sample names

  if (!is.null(rn)) {
    if(length(rn) != nrow(x)) stop("Incorrect number of row names (features) provided.")
    rownames(x@dataset) <- rn
  }
  if (!is.null(cn)) {
    if(length(cn) != ncol(x)) stop("Incorrect number of column names (samples) provided.")
    # Sync names
    colnames(x@dataset) <- cn
    rownames(x@metadata) <- cn
  }

  # Check validity after modification
  validObject(x)
  x
})

#' Set Row Names (Features)
#' @param x A \code{CircadianData} object.
#' @param value A character vector of row names (feature IDs).
#' @return The modified \code{CircadianData} object.
#' @export
setReplaceMethod("rownames", "CircadianData", function(x, value) {
  if(length(value) != nrow(x)) stop("Incorrect number of row names (features) provided.")
  rownames(x@dataset) <- value
  validObject(x) # Should still be valid
  x
})

#' Set Column Names (Samples)
#' @param x A \code{CircadianData} object.
#' @param value A character vector of column names (sample IDs).
#' @return The modified \code{CircadianData} object.
#' @export
setReplaceMethod("colnames", "CircadianData", function(x, value) {
  if(length(value) != ncol(x)) stop("Incorrect number of column names (samples) provided.")
  # Sync names
  colnames(x@dataset) <- value
  rownames(x@metadata) <- value
  validObject(x) # Check validity after modification
  x
})


# --- Subsetting Method (Crucial for Sync) ---

#' Subset a CircadianData Object
#'
#' Subsets the object by features (rows) and/or samples (columns), ensuring
#' that the dataset and metadata remain synchronized. The experiment_info
#' slot is carried over unchanged.
#'
#' @param x A \code{CircadianData} object.
#' @param i Row indices or names (features).
#' @param j Column indices or names (samples).
#' @param ... Additional arguments (not used).
#' @param drop Logical, currently ignored (always FALSE for dataset/metadata).
#'
#' @return A new, subsetted \code{CircadianData} object.
#' @export
#' @aliases [,CircadianData,ANY,ANY,ANY-method
#' @examples
#' # Sample Data
#' counts <- matrix(rpois(100, lambda = 10), nrow = 10, ncol = 10,
#'                  dimnames = list(paste0("Feature", 1:10), paste0("Sample", 1:10)))
#' meta <- data.frame(
#'   row.names = paste0("Sample", 1:10),
#'   group = rep(c("Control", "Treated"), each = 5),
#'   time = rep(seq(0, 8, by = 2), 2)
#' )
#' cd_obj <- CircadianData(dataset = counts, metadata = meta,
#'                         experiment_info = list(type = "RNA-Seq"))
#'
#' # Subset features (rows)
#' cd_obj_sub1 <- cd_obj[c("Feature1", "Feature5"), ]
#' dim(cd_obj_sub1) # 2 10
#'
#' # Subset samples (columns) - metadata is subsetted automatically
#' cd_obj_sub2 <- cd_obj[ , c("Sample1", "Sample3", "Sample8")]
#' dim(cd_obj_sub2) # 10  3
#' print(metadata(cd_obj_sub2))
#' print(experiment_info(cd_obj_sub2)) # Carried over
#'
#' # Subset both
#' cd_obj_sub3 <- cd_obj[1:3, c(TRUE, FALSE, TRUE, FALSE, TRUE, FALSE, TRUE, FALSE, TRUE, FALSE)]
#' dim(cd_obj_sub3) # 3 5
#' print(colnames(cd_obj_sub3))
#' print(rownames(metadata(cd_obj_sub3)))
#'
setMethod("[", c("CircadianData", "ANY", "ANY", "ANY"),
          function(x, i, j, ..., drop = FALSE) {

            # Handle missing indices - select all
            if (missing(i)) {
              i <- seq_len(nrow(x))
            }
            if (missing(j)) {
              j <- seq_len(ncol(x))
            }

            # Subset the dataset - use drop = FALSE to keep matrix structure
            new_dataset <- x@dataset[i, j, drop = FALSE]

            # Subset the metadata by rows corresponding to the selected samples (j)
            # use drop = FALSE to keep data.frame structure
            new_metadata <- x@metadata[j, , drop = FALSE]

            # Keep experiment_info as is (subsetting doesn't naturally apply)
            new_experiment_info <- x@experiment_info

            # Create and return the new object
            new("CircadianData",
                dataset = new_dataset,
                metadata = new_metadata,
                experiment_info = new_experiment_info)
          })


# --- Methods for element-wise access/modification of experiment_info ---

#' Access Elements of experiment_info using `[[`
#'
#' Allows retrieving individual elements from the `experiment_info` list slot
#' using the double-bracket subset operator.
#'
#' @param x A \code{CircadianData} object.
#' @param i A single character string representing the name of the element
#'   to retrieve from `experiment_info`.
#' @param j Optional, not used for accessing `experiment_info`.
#' @param ... Optional arguments, not used.
#'
#' @return The value of the element `i` within the `experiment_info` list.
#' @export
#' @rdname CircadianData-subset-expinfo
#' @aliases [[,CircadianData,character,missing-method
#' @examples
#' # Assuming cd_obj is a CircadianData object with experiment_info(cd_obj)$period <- 24
#' # period_val <- cd_obj[["period"]]
setMethod("[[", c("CircadianData", "character", "missing"),
          function(x, i, j, ...) {
            # Ensure i is a single element character string
            if (length(i) != 1) {
              stop("Index 'i' must be a single character string for accessing experiment_info.")
            }
            # Access the element within the experiment_info slot
            return(x@experiment_info[[i]])
          }
)

#' Assign or Add Elements to experiment_info using `[[<-`
#'
#' Allows assigning a value to an existing element or adding a new element
#' to the `experiment_info` list slot using the double-bracket subset assignment operator.
#'
#' @param x A \code{CircadianData} object.
#' @param i A single character string representing the name of the element
#'   in `experiment_info` to assign to or add.
#' @param j Optional, not used for assigning to `experiment_info`.
#' @param value The value to assign to the element `i`.
#'
#' @return The modified \code{CircadianData} object.
#' @export
#' @rdname CircadianData-subset-expinfo
#' @aliases [[<-,CircadianData,character,missing-method
#' @examples
#' # Assuming cd_obj is a CircadianData object
#' # cd_obj[["period"]] <- 24
#' # cd_obj[["notes"]] <- "Updated notes"
#' # print(experiment_info(cd_obj))
setReplaceMethod("[[", c("CircadianData", "character", "missing"),
                 function(x, i, j, value) {
                   # Ensure i is a single element character string
                   if (length(i) != 1) {
                     stop("Index 'i' must be a single character string for assigning to experiment_info.")
                   }
                   # Assign the value within the experiment_info slot list
                   x@experiment_info[[i]] <- value
                   # Re-validate the object (optional, but ensures experiment_info remains a list)
                   validObject(x)
                   # Return the modified object
                   return(x)
                 }
)


#' Access Elements of experiment_info using `$`
#'
#' Allows retrieving individual elements from the `experiment_info` list slot
#' using the `$` operator.
#'
#' @param x A \code{CircadianData} object.
#' @param name The name (as a symbol or character string) of the element
#'   to retrieve from `experiment_info`.
#'
#' @return The value of the element `name` within the `experiment_info` list.
#' @export
#' @rdname CircadianData-subset-expinfo
#' @aliases $,CircadianData-method
#' @examples
#' # Assuming cd_obj is a CircadianData object with experiment_info(cd_obj)$period <- 24
#' # period_val <- cd_obj$period
setMethod("$", "CircadianData",
          function(x, name) {
            # name is automatically a character string here
            return(x@experiment_info[[name]])
            # Alternative: return(slot(x, "experiment_info")[[name]])
          }
)


#' Assign or Add Elements to experiment_info using `$<-`
#'
#' Allows assigning a value to an existing element or adding a new element
#' to the `experiment_info` list slot using the `$` assignment operator.
#'
#' @param x A \code{CircadianData} object.
#' @param name The name (as a symbol or character string) of the element
#'   in `experiment_info` to assign to or add.
#' @param value The value to assign to the element `name`.
#'
#' @return The modified \code{CircadianData} object.
#' @export
#' @rdname CircadianData-subset-expinfo
#' @aliases $<-,CircadianData-method
#' @examples
#' # Assuming cd_obj is a CircadianData object
#' # cd_obj$period <- 24
#' # cd_obj$notes <- "Updated notes"
#' # print(experiment_info(cd_obj))
setMethod("$<-", "CircadianData",
          function(x, name, value) {
            # name is automatically a character string here
            x@experiment_info[[name]] <- value
            # Re-validate the object
            validObject(x)
            # Return the modified object
            return(x)
          }
)


# --- Generic Function Definition for Sorting ---

#' Order Samples in a CircadianData Object
#'
#' Reorders the samples (columns in `dataset`, rows in `metadata`) of a
#' \code{CircadianData} object based on the values in specified columns
#' of the metadata.
#'
#' @param x A \code{CircadianData} object.
#' @param by_columns A character vector specifying the column name(s) in
#'   `metadata(x)` to sort by. Sorting is done sequentially by these columns.
#' @param decreasing Logical vector indicating the direction of sorting for
#'   each column specified in `by_columns`. If a single value, it is recycled.
#'   Defaults to FALSE (ascending order) for all columns.
#' @param ... Additional arguments (currently unused).
#'
#' @return A new \code{CircadianData} object with samples sorted according
#'   to the specified criteria. The `dataset` columns and `metadata` rows
#'   remain synchronized.
#'
#' @export
#' @rdname order_samples
#' @examples
#' # Create minimal reproducible data
#' set.seed(456)
#' counts <- matrix(rpois(80, lambda = 50), nrow = 10, ncol = 8,
#'                  dimnames = list(paste0("Feature", 1:10), paste0("Sample", 1:8)))
#' meta <- data.frame(
#'   row.names = paste0("Sample", 1:8),
#'   time = rep(c(12, 0, 6, 18), each = 2), # Unordered times
#'   subject_id = paste0("S", rep(1:4, 2)), # Unordered subjects
#'   group = rep(c("A", "B"), 4)
#' )
#' cd_obj <- CircadianData(counts, meta, experiment_info = list(period = 24))
#'
#' print("Original Metadata Order:")
#' print(metadata(cd_obj))
#'
#' # Sort samples by time
#' cd_sorted_time <- order_samples(cd_obj, "time")
#' print("Metadata sorted by time:")
#' print(metadata(cd_sorted_time))
#' print(colnames(dataset(cd_sorted_time))) # Check dataset colnames match metadata rownames
#'
#' # Sort samples first by time (ascending), then by subject_id (descending)
#' cd_sorted_multi <- order_samples(cd_obj, c("time", "subject_id"), decreasing = c(FALSE, TRUE))
#' print("Metadata sorted by time (asc) then subject_id (desc):")
#' print(metadata(cd_sorted_multi))
#'
setGeneric("order_samples", function(x, by_columns, decreasing = FALSE, ...) standardGeneric("order_samples"))

# --- Method for CircadianData Class ---

#' @rdname order_samples
#' @export
setMethod("order_samples", "CircadianData",
          function(x, by_columns, decreasing = FALSE) {

            # --- Input Validation ---
            if (!is.character(by_columns) || length(by_columns) == 0) {
              stop("'by_columns' must be a non-empty character vector of metadata column names.")
            }
            if (!is.logical(decreasing)) {
              stop("'decreasing' must be a logical vector.")
            }

            mdata <- metadata(x)
            available_cols <- colnames(mdata)

            # Check if specified columns exist in metadata
            missing_cols <- setdiff(by_columns, available_cols)
            if (length(missing_cols) > 0) {
              stop("The following columns specified in 'by_columns' were not found in metadata: ",
                   paste(missing_cols, collapse = ", "))
            }

            # Handle 'decreasing' argument length
            n_sort_cols <- length(by_columns)
            if (length(decreasing) == 1 && n_sort_cols > 1) {
              decreasing <- rep(decreasing, n_sort_cols)
            } else if (length(decreasing) != n_sort_cols) {
              stop("Length of 'decreasing' (", length(decreasing),
                   ") must be 1 or match the length of 'by_columns' (", n_sort_cols, ").")
            }

            # Handle case with no samples
            if (ncol(x) == 0) {
              warning("Object has 0 samples, returning unchanged.")
              return(x)
            }

            # --- Get Sorting Order ---
            # Extract the columns to sort by into a list
            sort_cols_list <- as.list(mdata[, by_columns, drop = FALSE])

            # Prepare arguments for do.call with order()
            # Note: The default 'shell" method in order() can only handle the
            # case where `decreasing` is identical for all columns
            order_args <- c(sort_cols_list,
                            list(decreasing = decreasing, method = "radix"))

            if (length(decreasing) > 1) {
              # Note: We add this check because the default 'shell' method
              # in order() doesn't support vector 'decreasing'. Radix does.
              order_args$method <- "radix"
            }

            # Get the indices that define the new order
            new_order_indices <- do.call(order, order_args)

            # --- Apply Ordering ---
            # Reorder dataset columns
            new_dataset <- dataset(x)[, new_order_indices, drop = FALSE]

            # Reorder metadata rows
            new_metadata <- mdata[new_order_indices, , drop = FALSE]

            # --- Create and Return New Object ---
            # experiment_info remains unchanged
            new_expInfo <- experiment_info(x)

            # Use the constructor (or new()) to create the sorted object
            # This ensures validity checks are run on the result
            CircadianData(dataset = new_dataset,
                          metadata = new_metadata,
                          experiment_info = new_expInfo)
          }
)


# --- Generic Function Definition for Filtering ---

#' Filter Samples by Matching Values in a Metadata Column
#'
#' Subsets a \code{CircadianData} object, keeping only samples where the value
#' in a specified metadata column matches one of the provided values.
#'
#' @param x A \code{CircadianData} object.
#' @param col A single character string specifying the column name in
#'   `metadata(x)` to filter by.
#' @param value A vector of values to match against in the specified column.
#'   Samples where the column value is found in this vector will be kept.
#' @param ... Additional arguments (currently unused).
#'
#' @return A new \code{CircadianData} object containing only the samples
#'   that satisfy the filter condition.
#' @export
#' @rdname filter_samples
#' @examples
#' # Create minimal reproducible data
#' set.seed(789)
#' counts <- matrix(rpois(80, lambda = 50), nrow = 10, ncol = 8,
#'                  dimnames = list(paste0("Feature", 1:10), paste0("Sample", 1:8)))
#' meta <- data.frame(
#'   row.names = paste0("Sample", 1:8),
#'   time = rep(c(0, 6, 12, 18), each = 2),
#'   subject_id = paste0("S", rep(1:4, 2)),
#'   group = rep(c("Control", "Treated"), 4)
#' )
#' cd_obj <- CircadianData(counts, meta, experiment_info = list(period = 24))
#'
#' # Filter samples belonging to the "Control" group
#' cd_control <- filter_samples(cd_obj, col = "group", value = "Control")
#' print(metadata(cd_control))
#'
#' # Filter samples from subject S1 or S3
#' cd_s1_s3 <- filter_samples(cd_obj, col = "subject_id", value = c("S1", "S3"))
#' print(metadata(cd_s1_s3))
#'
#' # Filter samples collected at time 0 or 6
#' cd_t0_t6 <- filter_samples(cd_obj, col = "time", value = c(0, 6))
#' print(metadata(cd_t0_t6))
#'
setGeneric("filter_samples", function(x, col, value, ...) standardGeneric("filter_samples"))

# --- Method for CircadianData Class ---

#' @rdname filter_samples
setMethod("filter_samples", "CircadianData",
          function(x, col, value) {

            # --- Input Validation ---
            if (missing(col) || !is.character(col) || length(col) != 1) {
              stop("'col' must be a single character string.")
            }
            if (missing(value)) {
              stop("'value' argument is missing.")
            }

            mdata <- metadata(x)

            if (!(col %in% colnames(mdata))) {
              stop("Column '", col, "' not found in metadata.")
            }

            # Handle case with no samples
            if (ncol(x) == 0) {
              warning("Object has 0 samples, returning unchanged.")
              return(x)
            }

            # --- Perform Filtering ---
            # Use %in% for flexibility (handles single or multiple values in 'value')
            keep_logical <- mdata[[col]] %in% value

            # Ensure NAs are FALSE
            keep_logical[is.na(keep_logical)] <- FALSE

            # --- Subset the Object ---
            # Leverage the existing synchronized subsetting method
            filtered_obj <- x[, keep_logical, drop = FALSE]

            # --- Updated number of groups ---
            if (col == ".group" && "n_groups" %in% names(experiment_info(x))) {
              filtered_obj$n_groups <- length(unique(metadata(filtered_obj)[[".group"]]))
            }

            # --- Update replicate table ---
            # If filtering group, update replicates in experiment info
            if (col == ".group" && "n_replicates" %in% names(experiment_info(x))) {
              # Get replicate table
              reps_orig <- x$n_replicates

              # Filter
              grps_filtered <- unique(metadata(filtered_obj)[[".group"]])
              reps_filtered <- reps_orig[grps_filtered]

              # If only 1 group remaining, extract table from list
              if (length(reps_filtered) == 1) reps_filtered <- reps_filtered[[1]]

              # Add to filtered object
              filtered_obj$n_replicates <- reps_filtered
            }

            return(filtered_obj)
          }
)


# --- Show Method ---

#' Show Method for CircadianData
#'
#' Provides a comprehensive summary of the CircadianData object, including
#' dimensions, snippets of feature/sample names, previews of the metadata
#' and dataset, and the contents of the experiment_info list.
#'
#' @param object A \code{CircadianData} object.
#' @export
setMethod("show", "CircadianData", function(object) {
  n_features <- nrow(object)
  n_samples <- ncol(object)

  cat("An object of class 'CircadianData'\n")
  cat(" Dimensions:", n_features, "features,", n_samples, "samples\n")

  # --- Dimnames Snippets ---
  rn <- rownames(object)
  cn <- colnames(object)
  # Show snippet of row/col names
  if (is.null(rn)) {
    rn_show <- "[NULL]"
  } else if (length(rn) > 6)
    rn_show <- c(head(rn, 3), "...", tail(rn, 2))
  else
    rn_show <- rn
  if (is.null(cn)) {
    cn_show <- "[NULL]"
  } else if (length(cn) > 6)
    cn_show <- c(head(cn, 3), "...", tail(cn, 2))
  else
    cn_show <- cn
  cat(sprintf(" %s:", "Feature names"), paste(rn_show, collapse = " "), "\n")
  cat(sprintf(" %s:", "Sample names"), paste(cn_show, collapse = " "), "\n")

  # --- Metadata Preview ---
  cat("\nMetadata preview:\n")
  mdata <- metadata(object)
  if (n_samples > 0 && ncol(mdata) > 0) {
    # Show head of metadata
    print(head(mdata))
  } else if (n_samples == 0) {
    cat(" [No samples]\n")
  } else { # ncol(mdata) == 0
    cat(" [No metadata columns]\n")
  }

  # --- Dataset Preview ---
  cat("\nDataset preview:\n")
  dset <- dataset(object)
  if (n_features > 0 && n_samples > 0) {
    # Determine rows/cols to show (e.g., up to 6x6)
    rows_to_show <- min(n_features, 6)
    cols_to_show <- min(n_samples, 6)
    cat(sprintf(" [showing %d features x %d samples]\n", rows_to_show, cols_to_show))
    # Extract subset
    dset_preview <- dset[1:rows_to_show, 1:cols_to_show, drop = FALSE]
    # Print the subsetted matrix
    print(dset_preview)
  } else if (n_features == 0) {
    cat(" [No features]\n")
  } else { # n_samples == 0
    cat(" [No samples]\n")
  }

  # --- Experiment Info ---
  cat("\nExperiment Info:\n")
  exp_info <- experiment_info(object)
  if (length(exp_info) > 0) {
    for (i in seq_along(exp_info)) {
      key <- names(exp_info)[i]
      value <- exp_info[[i]]
      # Format the value for display (using capture.output for complex values)
      # For simple values, direct print might be okay, but capture.output is safer
      value_str <- capture.output(print(value))
      # Indent multi-line values for clarity
      if (length(value_str) > 1) {
        cat(sprintf(" $ %s:\n", key))
        cat(paste("   ", value_str, collapse = "\n"), "\n")
      } else {
        cat(sprintf(" $ %s: %s\n", key, value_str))
      }
    }
  } else {
    cat(" [Empty list]\n")
  }

  # Add a final newline for spacing
  cat("\n")
})


# --- Example Usage ---
if (FALSE) { # Don't run automatically
  # Sample Data
  counts <- matrix(rpois(100, lambda = 10), nrow = 10, ncol = 10,
                   dimnames = list(paste0("Feature", 1:10), paste0("Sample", 1:10)))
  meta <- data.frame(
    row.names = paste0("Sample", 1:10),
    group = rep(c("Control", "Treated"), each = 5),
    time = rep(seq(0, 8, by = 2), 2)
  )

  # Create object
  cd_obj <- CircadianData(dataset = counts, metadata = meta)
  print(cd_obj)

  # Access data
  dset <- dataset(cd_obj)
  mdata <- metadata(cd_obj)

  # Check dimensions and names
  dim(cd_obj)
  rownames(cd_obj) # Feature names
  colnames(cd_obj) # Sample names

  # --- Synchronization Examples ---

  # 1. Subsetting samples
  cd_subset_samples <- cd_obj[, c("Sample1", "Sample5", "Sample9")]
  print(cd_subset_samples)
  print(dim(dataset(cd_subset_samples))) # 10 x 3
  print(dim(metadata(cd_subset_samples))) # 3 x 2
  print(colnames(dataset(cd_subset_samples)))
  print(rownames(metadata(cd_subset_samples))) # Should match!

  # 2. Subsetting features (metadata unaffected)
  cd_subset_features <- cd_obj[c(1, 3, 5), ]
  print(cd_subset_features)
  print(dim(dataset(cd_subset_features))) # 3 x 10
  print(dim(metadata(cd_subset_features))) # 10 x 2

  # 3. Renaming samples
  cd_renamed <- cd_obj
  new_names <- paste0("S", 1:10)
  colnames(cd_renamed) <- new_names
  print(colnames(dataset(cd_renamed))) # S1, S2, ...
  print(rownames(metadata(cd_renamed))) # S1, S2, ... (synchronized!)

  # --- Adding Experiment Information ---
  info_list <- list(
    period = 24,
    is_missing = FALSE,
    replicates_per_tp = table(metadata(cd_obj)$time),
    assay_type = "RNA-Seq"
  )
  experiment_info(cd_obj) <- info_list
  print(cd_obj)

  # Add/modify elements using `[[<-`
  cd_obj[["experiment_id"]] <- "EXP_001"
  cd_obj[["period"]] <- 24
  cd_obj[["has_replicates"]] <- TRUE
  print(experiment_info(cd_obj))

  # Access elements using `[[`
  exp_id <- cd_obj[["experiment_id"]]
  cat("Experiment ID:", exp_id, "\n")
  # Access non-existent element (returns NULL, standard list behavior)
  print(cd_obj[["non_existent"]])

  # Modify existing element using `[[<-`
  cd_obj[["period"]] <- 23.8
  print(cd_obj[["period"]])

  # Add/modify elements using `$<-`
  cd_obj$operator <- "John Doe"
  cd_obj$has_replicates <- FALSE # Modify existing
  print(experiment_info(cd_obj))

  # Access elements using `$`
  op <- cd_obj$operator
  cat("Operator:", op, "\n")
  # Access non-existent element (returns NULL)
  print(cd_obj$tissue)

  # Verify the main object is updated
  print(cd_obj)

  # Access all experiment information
  full_info <- experiment_info(cd_obj)
  print(full_info)
}
