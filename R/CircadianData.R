#' Define the CircadianData Class
#'
#' @slot dataset A matrix containing measurement data (features x samples).
#' @slot metadata A data.frame containing sample annotations (samples x attributes).
#' @slot experimentInfo A list to store additional arbitrary information or
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
           experimentInfo = "list" # Renamed from otherData
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

  # Check 5: experimentInfo must be a list
  if (!is.list(object@experimentInfo)) {
    errors <- c(errors, "'experimentInfo' slot must be a list.")
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
#' @param experimentInfo An optional list to store additional experiment-level
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
#' experimentInfo(cd_obj) <- expInfo
#' print(experimentInfo(cd_obj))
#'
CircadianData <- function(dataset, metadata, experimentInfo = list()) {

  # Basic checks before creating the object
  if (!is.matrix(dataset)) stop("'dataset' must be a matrix.")
  if (!is.data.frame(metadata)) stop("'metadata' must be a data.frame.")
  if (!is.list(experimentInfo)) stop("'experimentInfo' must be a list.")

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
      experimentInfo = experimentInfo)
}


# --- Accessor Methods ---

#' Get the Dataset Matrix
#' @param x A \code{CircadianData} object.
#' @return The dataset matrix (features x samples).
#' @export
#' @rdname CircadianData-accessors
setGeneric("dataset", function(x) standardGeneric("dataset"))
setMethod("dataset", "CircadianData", function(x) x@dataset)

#' Get the Metadata Data Frame
#' @param x A \code{CircadianData} object.
#' @return The metadata data.frame (samples x attributes).
#' @export
#' @rdname CircadianData-accessors
setGeneric("metadata", function(x) standardGeneric("metadata"))
setMethod("metadata", "CircadianData", function(x) x@metadata)

#' Set the Metadata Data Frame
#' @param x A \code{CircadianData} object.
#' @param value A data.frame to replace the current metadata
#' @return The modified \code{CircadianData} object.
#' @export
#' @rdname CircadianData-accessors
setGeneric("metadata<-", function(x, value) standardGeneric("metadata<-"))
setReplaceMethod("metadata", "CircadianData", function(x, value) {
  if (!inherits(value, "data.frame")) stop("'value' must be a data frame.")
  x@metadata <- value
  validObject(x) # Re-validate
  x
})

#' Get the Experiment Information List
#' @param x A \code{CircadianData} object.
#' @return The experimentInfo list.
#' @export
#' @rdname CircadianData-accessors
setGeneric("experimentInfo", function(x) standardGeneric("experimentInfo"))
setMethod("experimentInfo", "CircadianData", function(x) x@experimentInfo)

#' Set the Experiment Information List
#' @param x A \code{CircadianData} object.
#' @param value A list to replace the current experimentInfo.
#' @return The modified \code{CircadianData} object.
#' @export
#' @rdname CircadianData-accessors
setGeneric("experimentInfo<-", function(x, value) standardGeneric("experimentInfo<-"))
setReplaceMethod("experimentInfo", "CircadianData", function(x, value) {
  if (!is.list(value)) stop("'value' must be a list.")
  x@experimentInfo <- value
  validObject(x) # Re-validate: checks if value is a list
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
#' that the dataset and metadata remain synchronized. The experimentInfo
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
#'                         experimentInfo = list(type = "RNA-Seq"))
#'
#' # Subset features (rows)
#' cd_obj_sub1 <- cd_obj[c("Feature1", "Feature5"), ]
#' dim(cd_obj_sub1) # 2 10
#'
#' # Subset samples (columns) - metadata is subsetted automatically
#' cd_obj_sub2 <- cd_obj[ , c("Sample1", "Sample3", "Sample8")]
#' dim(cd_obj_sub2) # 10  3
#' print(metadata(cd_obj_sub2))
#' print(experimentInfo(cd_obj_sub2)) # Carried over
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

            # Keep experimentInfo as is (subsetting doesn't naturally apply)
            new_experimentInfo <- x@experimentInfo

            # Create and return the new object
            new("CircadianData",
                dataset = new_dataset,
                metadata = new_metadata,
                experimentInfo = new_experimentInfo)
          })


# --- Show Method ---

#' Show Method for CircadianData
#'
#' @param object A \code{CircadianData} object.
#' @export
setMethod("show", "CircadianData", function(object) {
  cat("An object of class 'CircadianData'\n")
  cat(" Dimensions:", nrow(object), "features,", ncol(object), "samples\n")

  # Show snippet of row/col names
  rn <- rownames(object)
  cn <- colnames(object)
  if (is.null(rn)) {
    rn_show <- "[NULL]"
  } else if (length(rn) > 4)
    rn_show <- c(head(rn, 2), "...", tail(rn, 1))
  else
    rn_show <- rn
  if (is.null(cn)) {
    cn_show <- "[NULL]"
  } else if (length(cn) > 4)
    cn_show <- c(head(cn, 2), "...", tail(cn, 1))
  else
    cn_show <- cn
  cat(" Feature names:", paste(rn_show, collapse = " "), "\n")
  cat(" Sample names:", paste(cn_show, collapse = " "), "\n")

  # Show snippet of metadata
  cat(" Metadata columns:", paste(colnames(metadata(object)), collapse=", "), "\n")

  # Show snippet of experimentInfo keys
  ei_keys <- names(experimentInfo(object))
  if (length(ei_keys) > 0) {
    if (length(ei_keys) > 5) ei_keys_show <- c(head(ei_keys, 5), "...") else ei_keys_show <- ei_keys
    cat(" experimentInfo keys:", paste(ei_keys_show, collapse=", "), "\n")
  } else {
    cat(" experimentInfo: Empty list\n")
  }
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
  experimentInfo(cd_obj) <- info_list
  print(cd_obj)
  print(experimentInfo(cd_obj)$period)
  print(experimentInfo(cd_obj)$replicates_per_tp)

  # Modifying experimentInfo
  einfo <- experimentInfo(cd_obj)
  einfo$new_info <- "Some processing notes"
  experimentInfo(cd_obj) <- einfo
  print(experimentInfo(cd_obj))
}
