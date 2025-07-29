# ---- Class definition ----

#' Define the CircadianData Class
#'
#' @description
#' An S4 class to store and manage data from biological experiments, such as
#' circadian studies. It serves as an integrated container for raw measurement
#' data, sample annotations, experimental parameters, and analysis results.
#'
#' @details
#' This class bundles the main components of an experimental dataset. Its
#' primary goal is to ensure that the samples (columns of `dataset`, rows of
#' `metadata`) remain synchronized during data manipulation operations like
#' subsetting, ordering, and filtering. Analysis functions within the package
#' are designed to take this object as input and can store their output in the
#' `results` slot, creating a self-contained analysis workflow. The rows of the
#' `dataset` represent features (e.g., genes, proteins, metabolites). The
#' columns of `dataset` and rows of `metadata` represent samples. The
#' `experiment_info` stores parameters (e.g. period) that are relevant for the
#' analysis.
#'
#' To manipulate the object:
#' \itemize{
#'   \item To access main data slots, see \code{\link{CircadianData-accessors}}.
#'   \item To subset by features or samples, see \code{\link{CircadianData-subsetting}}.
#'   \item To sort samples by metadata columns, see \code{\link{order_samples}}.
#'   \item To filter samples by metadata values, see \code{\link{filter_samples}}.
#'   \item To interact with the `experiment_info` list, see \code{\link{CircadianData-subset-expinfo}}.
#' }
#'
#' @slot dataset A matrix containing measurement data (features x samples).
#' @slot metadata A data.frame containing sample annotations (samples x
#'   attributes).
#' @slot experiment_info A list for storing experiment-level parameters.
#' @slot results A list for storing the output from various analysis functions.
#'
#' @name CircadianData-class
#' @rdname CircadianData-class
#' @exportClass CircadianData
setClass("CircadianData",
         slots = c(
           dataset = "matrix",
           metadata = "data.frame",
           experiment_info = "list",
           results = "list"
         )
)


# ---- Validity Check ----

setValidity("CircadianData", function(object) {
  errors <- character()
  dset <- object@dataset
  mdata <- object@metadata

  # --- Structural Invariants ---
  if (!is.matrix(dset) || !is.numeric(dset)) {
    errors <- c(errors, "'dataset' slot must be a numeric matrix.")
  }

  # TODO: Probably NA values are fine?
  if (anyNA(dset)) {
    errors <- c(errors, "'dataset' slot cannot contain NA values.")
  }
  if (is.null(rownames(dset))) {
    errors <- c(errors, "'dataset' slot must have feature names (row names).")
  }

  # --- Metadata Invariants ---
  if (!"time" %in% colnames(mdata) || !is.numeric(mdata$time)) {
    errors <- c(errors, "A valid object must have a numeric 'time' column in its metadata.")
  }

  # --- Synchronization Invariant ---
  if (!identical(colnames(dset), rownames(mdata))) {
    errors <- c(errors, "Column names of 'dataset' must be identical to and in the same order as row names of 'metadata'.")
  }

  if (length(errors) == 0) TRUE else errors
})


# ---- Constructor Function ----

#' Create a CircadianData Object
#'
#' @description
#' This constructor function validates and standardizes user-provided dataset and
#' metadata, creating a `CircadianData` object. It serves as the primary entry
#' point for creating a valid object, integrating checks for data integrity,
#' required columns, and consistent sample IDs.
#'
#' @param dataset A data frame or numeric matrix with features as rows and
#'   samples as columns. Must have column names (sample IDs).
#' @param metadata A data frame containing sample annotations.
#' @param colname_time A character string specifying the name of the column in `metadata`
#'   that contains the time information (e.g., collection time). This column
#'   must be numeric.
#' @param colname_sample A character string specifying the name of the column in
#'   `metadata` that contains the sample IDs. These IDs must be unique and match
#'   the column names of `dataset`. Use `"rownames"` if sample IDs are in the
#'   metadata's row names.
#' @param colname_group An optional character string specifying the name of the
#'   column in `metadata` that contains group information.
#' @param colname_subject An optional character string specifying the name of the
#'   column in `metadata` containing subject IDs for repeated measures designs.
#' @param experiment_info An optional list for storing experiment-level parameters.
#' @param results An optional list for storing analysis results.
#'
#' @return A valid `CircadianData` object.
#' @export
#' @examples
#' counts <- matrix(rpois(80, 50), nrow=10, ncol=8,
#'                  dimnames=list(paste0("Feature", 1:10), paste0("Sample", 1:8)))
#' meta_df <- data.frame(
#'   sample_id = paste0("Sample", 1:8),
#'   zeitgeber_time = rep(c(0,6,12,18), each=2),
#'   condition = rep(c("WT", "KO"), 4),
#'   mouse_id = rep(1:4, 2)
#' )
#'
#' # Create the object, mapping user column names to standard internal names
#' cd_obj <- CircadianData(
#'   dataset = counts,
#'   metadata = meta_df,
#'   colname_sample = "sample_id",
#'   colname_time = "zeitgeber_time",
#'   colname_group = "condition",
#'   colname_subject = "mouse_id"
#' )
#'
#' print(cd_obj)
#' # Note how metadata columns have been renamed and standardized
#' head(metadata(cd_obj))
#'
CircadianData <- function(dataset,
                          metadata,
                          colname_sample,
                          colname_time,
                          colname_group = NULL,
                          colname_subject = NULL,
                          experiment_info = list(),
                          results = list()) {

  # === 1. Metadata Processing and Validation ===
  ## -- 1. Check if metadata is a data.frame --
  if (!inherits(metadata, "data.frame")) {
    stop("'metadata' must be a data.frame.", call. = FALSE)
  }


  ## -- 2. Ensure it's a data frame and not e.g. a tibble or data.table --
  metadata <- as.data.frame(metadata)


  ## -- 3. Check if columns are present --
  # Get column names
  cnames <- colnames(metadata)

  ### 3a. Required columns (`colname_sample` and `colname_time`)
  # Handle case where sample IDs are in the rownames
  if (colname_sample == "rownames") {
    if (is.null(rownames(metadata))) stop("`colname_sample` is 'rownames', but metadata has no row names.", call.=FALSE)
    metadata$`.internal_sample_id` <- rownames(metadata)
    colname_sample <- ".internal_sample_id"
  }

  # Check if the columns exist
  required_args <- c("colname_sample", "colname_time")
  user_cols <- stats::setNames(c(colname_sample, colname_time), required_args)

  missing_cols <- user_cols[!user_cols %in% cnames]
  if (length(missing_cols) > 0) {
    stop("The following required columns are missing from metadata: ",
         paste0("'", missing_cols, "' (specified by '", names(missing_cols), "') ", collapse=", "),
         call. = FALSE)
  }


  ### 3b. Optional columns (`colname_group` and `colname_subject`)
  if (!is.null(colname_group) && !colname_group %in% cnames) {
    stop(
      paste0(
        "Missing column '", colname_group, "' in `metadata`. ",
        "If applicable, please make sure that `colname_group` corresponds to the ",
        "column containing the group information or leave it as NULL otherwise."
      ),
      call. = FALSE
    )
  }

  if (!is.null(colname_subject) && !colname_subject %in% cnames) {
    stop(
      paste0(
        "Missing column '", colname_subject, "' in `metadata`. ",
        "If your data contains repeated measures, please make sure that ",
        "`colname_subject` corresponds to the column containing the subject IDs ",
        "or set it to NULL (default) otherwise."
      ),
      call. = FALSE
    )
  }

  ## -- 4. Check if the values in colname_time are numeric --
  if (!is.numeric(metadata[[colname_time]])) {
    stop(
      paste0(
        "Column '", colname_time, "' of `metadata` is of type ", class(metadata[[colname_time]]),
        ". The column defined as `colname_time` in `metadata` must be of type numeric."
      ),
      call. = FALSE
    )
  }

  ## -- 5. Check if the values in colname_sample are unique --
  if (any(duplicated(metadata[[colname_sample]]))) {
    stop(
      paste0("Values in metadata column '", colname_sample, "' must be unique."),
      call. = FALSE
    )
  }

  ## -- 6. Check subject ID logic --
  # Print a message if all values are unique because this suggests the user
  # simply has replicates rather than repeated measures, in which case
  # `colname_sample` need not be defined.
  if (!is.null(colname_subject) && !any(duplicated(metadata[[colname_subject]]))) {
    message("All values in column '", colname_subject, "' are unique. ",
            "Assuming no repeated measures and ignoring this column.")
    colname_subject <- NULL
  }


  ## -- 7. Warn the user that additional columns will be ignored --
  ignored_cols <- setdiff(
    cnames,
    c(colname_time, colname_sample, colname_group, colname_subject)
  )
  if (length(ignored_cols) > 0) {
    message_cols <- paste0(
      "\nThe following columns in `metadata` will be ignored: ",
      paste(ignored_cols, collapse = ", ")
    )

    # if (is.null(colname_group)) {
    #   message_group <- paste0(
    #     "If your data set contains groups which should be analysed ",
    #     "separately, make sure to define `colname_group`."
    #   )
    #   message_cols <- paste0(message_cols, "\n", message_group)
    # }

    # if (is.null(colname_subject)) {
    #   message_rpt <- paste0(
    #     "If your data set contains repeated measures, make sure to define ",
    #     "`colname_subject`."
    #   )
    #   message_cols <- paste0(message_cols, "\n", message_rpt)
    # }

    message(paste0(message_cols, "\n"))
  }

  ## -- 8. Standardize Metadata: Select and rename columns ---
  # TODO: Enforce type `factor` for group and subject ID?
  final_meta <- data.frame(row.names = as.character(metadata[[colname_sample]]))
  final_meta$time <- metadata[[colname_time]]
  if (!is.null(colname_group)) {
    final_meta$group <- metadata[[colname_group]]
  }
  if (!is.null(colname_subject)) {
    final_meta$subject_ID <- metadata[[colname_subject]]
  }


  # === 2. Dataset Processing and Validation ===
  ## -- 1. Check if dataset is a data.frame or matrix ---
  if (!inherits(dataset, c("data.frame", "matrix"))) {
    stop("`dataset` must be a data frame or matrix.", call. = FALSE)
  }

  ## -- 2. Check if dataset is numeric --
  if (is.data.frame(dataset)) {
    non_numeric_cols <- which(sapply(dataset, function(col) !is.numeric(col)))
    if (length(non_numeric_cols) > 0) {
      cols_types <- sapply(dataset, function(col) class(col))[non_numeric_cols]
      str_combined <- paste0(names(cols_types), " (", cols_types, ")")
      stop(paste(
        "All columns of `dataset` are expected to be of type numeric.",
        "The following columns have a different data type:\n",
        paste(str_combined, collapse = "\n ")
      ), call. = FALSE)
    }

    # Convert to matrix
    dataset <- as.matrix(dataset)
  }

  if (is.matrix(dataset) && !is.numeric(dataset)) {
    stop(paste0(
      "The `dataset` matrix is of type ", typeof(dataset),
      " but is expected to be numerical"
    ), call. = FALSE)
  }

  ## -- 3. Check if dataset has row numbers --
  if (is.null(rownames(dataset))) {
    warning("Dataset has no feature IDs (row names). Using row numbers as feature IDs.", call. = FALSE)
    rownames(dataset) <- as.character(seq_len(nrow(dataset)))
  }


  # === 3. Synchronize Dataset and Metadata ===
  meta_samples <- rownames(final_meta)
  dset_samples <- colnames(dataset)

  if (is.null(dset_samples)) {
    stop("'dataset' must have column names that correspond to sample IDs.", call. = FALSE)
  }

  missing_in_meta <- setdiff(dset_samples, meta_samples)
  if (length(missing_in_meta) > 0) {
    stop("The following sample IDs in the dataset are not found in the metadata: ",
         paste(missing_in_meta, collapse = ", "), call. = FALSE)
  }
  missing_in_dset <- setdiff(meta_samples, dset_samples)
  if (length(missing_in_dset) > 0) {
    stop("The following sample IDs in the metadata are not found in the dataset: ",
         paste(missing_in_dset, collapse = ", "), call. = FALSE)
  }

  # Reorder dataset columns to match the standardized metadata order
  final_dataset <- dataset[, meta_samples, drop = FALSE]


  # === 4. Create the Object ===
  # The validity check will run automatically here on the final, processed data
  cd_obj <- tryCatch({
    new("CircadianData",
        dataset = final_dataset,
        metadata = final_meta,
        experiment_info = experiment_info,
        results = results)
  }, error = function(e) {
    stop("Failed to create CircadianData object. Please check the following error(s):\n",
         e$message, call. = FALSE)
  })

  return(cd_obj)
}



# ---- Accessor Methods ----

#' Access or Replace Object Components
#'
#' @description
#' Functions to access or replace the main components of a \code{CircadianData}
#' object, including the dataset matrix, metadata data.frame, experiment_info
#' list, and results list.
#'
#' @param x A \code{CircadianData} object.
#' @param value A value to replace a component with. See individual function
#'   details for required class (e.g., a matrix for `dataset<-`, a list for
#'   `results<-`).
#'
#' @details
#' \itemize{
#'   \item \code{dataset()}: Retrieves the feature x sample matrix.
#'   \item \code{metadata()}: Retrieves the sample x attribute data.frame.
#'   \item \code{experiment_info()}: Retrieves the list of experiment-level details.
#'   \item \code{results()}: Retrieves the list of analysis results.
#' }
#'
#' @return The requested component (for accessors) or the modified
#'   \code{CircadianData} object (for replacement methods).
#'
#' @name CircadianData-accessors
#' @aliases dataset dataset<- metadata metadata<- experiment_info experiment_info<- results results<-
NULL # A NULL object to hold the main documentation block


## ---- Dataset Accessor/Replacement ----

#' @rdname CircadianData-accessors
#' @export
setGeneric("dataset", function(x) standardGeneric("dataset"))

#' @rdname CircadianData-accessors
setMethod("dataset", "CircadianData", function(x) x@dataset)

#' @rdname CircadianData-accessors
#' @export
setGeneric("dataset<-", function(x, value) standardGeneric("dataset<-"))

#' @rdname CircadianData-accessors
setReplaceMethod("dataset", "CircadianData", function(x, value) {
  if (!is.matrix(value)) stop("'value' must be a matrix.")
  x@dataset <- value
  validObject(x)
  x
})


## ---- Metadata Accessor/Replacement ----

#' @rdname CircadianData-accessors
#' @export
setGeneric("metadata", function(x) standardGeneric("metadata"))

#' @rdname CircadianData-accessors
setMethod("metadata", "CircadianData", function(x) x@metadata)

#' @rdname CircadianData-accessors
#' @export
setGeneric("metadata<-", function(x, value) standardGeneric("metadata<-"))

#' @rdname CircadianData-accessors
setReplaceMethod("metadata", "CircadianData", function(x, value) {
  if (!is.data.frame(value)) stop("'value' must be a data frame.")
  x@metadata <- value
  validObject(x)
  x
})


## ---- Experiment Info Accessor/Replacement ----

#' @rdname CircadianData-accessors
#' @export
setGeneric("experiment_info", function(x) standardGeneric("experiment_info"))

#' @rdname CircadianData-accessors
setMethod("experiment_info", "CircadianData", function(x) x@experiment_info)

#' @rdname CircadianData-accessors
#' @export
setGeneric("experiment_info<-", function(x, value) standardGeneric("experiment_info<-"))

#' @rdname CircadianData-accessors
setReplaceMethod("experiment_info", "CircadianData", function(x, value) {
  if (!is.list(value)) stop("'value' must be a list.")
  x@experiment_info <- value
  validObject(x)
  x
})


## ---- Results Accessor/Replacement ----

#' @rdname CircadianData-accessors
#' @export
setGeneric("results", function(x) standardGeneric("results"))

#' @rdname CircadianData-accessors
setMethod("results", "CircadianData", function(x) x@results)

#' @rdname CircadianData-accessors
#' @export
setGeneric("results<-", function(x, value) standardGeneric("results<-"))

#' @rdname CircadianData-accessors
setReplaceMethod("results", "CircadianData", function(x, value) {
  if (!is.list(value)) stop("'value' must be a list.")
  x@results <- value
  validObject(x)
  x
})


# ---- Generics for Base Functions ----
# Note: Necessary to avoid startup messages

# Check if a generic already exists before creating it.

if (!isGeneric("nrow")) {
  setGeneric("nrow", function(x) standardGeneric("nrow"))
}

if (!isGeneric("ncol")) {
  setGeneric("ncol", function(x) standardGeneric("ncol"))
}

if (!isGeneric("rownames")) {
  setGeneric("rownames", function(x, do.NULL = TRUE, prefix = "row") standardGeneric("rownames"))
}

if (!isGeneric("colnames")) {
  setGeneric("colnames", function(x, do.NULL = TRUE, prefix = "col") standardGeneric("colnames"))
}

if (!isGeneric("rownames<-")) {
  setGeneric("rownames<-", function(x, value) standardGeneric("rownames<-"))
}

if (!isGeneric("colnames<-")) {
  setGeneric("colnames<-", function(x, value) standardGeneric("colnames<-"))
}


# ---- Basic Methods (Dimensions, Dimnames) ----

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


# ---- Replacement Methods (Dimnames - Crucial for Sync) ----

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


# ---- Subsetting Method (Crucial for Sync) ----

#' Subset a CircadianData Object
#'
#' @description
#' Subsets the object by features (rows) and/or samples (columns) using the `[`
#' operator. This method ensures that the `dataset` and `metadata` slots remain
#' synchronized after subsetting.
#'
#' @param x A \code{CircadianData} object.
#' @param i Row indices or names (features).
#' @param j Column indices or names (samples).
#' @param ... Additional arguments (not used).
#' @param drop Logical, currently ignored (always FALSE for dataset/metadata).
#'
#' @return A new, subsetted \code{CircadianData} object.
#'
#' @name CircadianData-subsetting
#' @aliases
#' [,CircadianData,ANY,ANY,ANY-method
#' [.CircadianData
#'
#' @examples
#' # Sample Data
#' counts <- matrix(rpois(100, lambda = 10), nrow = 10, ncol = 10,
#'                  dimnames = list(paste0("Feature", 1:10), paste0("Sample", 1:10)))
#' meta <- data.frame(
#'   row.names = paste0("Sample", 1:10),
#'   group = rep(c("Control", "Treated"), each = 5),
#'   time = rep(seq(0, 8, by = 2), 2)
#' )
#' cd_obj <- CircadianData(dataset = counts, metadata = meta)
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


# ---- Adding experiment_info ----

#' Add or Update Experiment Parameters for Analysis
#'
#' This function populates or updates the `experiment_info` slot of a
#' `CircadianData` object with parameters required for downstream analysis.
#'
#' @details
#' On the first call, core parameters can be omitted to use sensible defaults
#' (e.g., `period = 24`, `data_type = "norm"`). Subsequent calls will use values
#' already stored in the object unless explicitly provided again to be overwritten.
#'
#' @param cd_obj A \code{CircadianData} object.
#' @param period An optional numeric vector of length 1 or 2. Defaults to 24
#'   if not already set.
#' @param data_type An optional character string specifying the data type. Must be
#'   one of "count" or "norm". Defaults to "norm" if not already set.
#' @param log_transformed An optional logical value (`TRUE`/`FALSE`). Defaults to
#'   `FALSE` if not already set.
#' @param log_base An optional single numeric value specifying the base of the
#'   logarithm. Defaults to 2 if `log_transformed` is `TRUE` and no base is set.
#' @param estimate_delta_t A logical value. If `TRUE` (default), the sampling
#'   interval (`delta_t`) will be automatically estimated.
#'
#' @return A \code{CircadianData} object with an updated `experiment_info` slot.
#' @export
#' @examples
#' # --- Setup ----
#' counts <- matrix(rpois(80, 50), nrow=10, ncol=8,
#'                  dimnames=list(paste0("Feature", 1:10), paste0("Sample", 1:8)))
#' meta_df <- data.frame(
#'   sample_id = paste0("Sample", 1:8),
#'   time = rep(c(0,6,12,18), each=2)
#' )
#' cd_obj <- CircadianData(
#'   dataset = counts, metadata = meta_df,
#'   colname_sample = "sample_id", colname_time = "time"
#' )
#'
#' # --- Configuration using defaults ----
#' # We only need to provide the required 'cd_obj' argument.
#' cd_configured <- add_experiment_info(cd_obj)
#'
#' # Check the configured values
#' experiment_info(cd_configured)$period # Should be 24
#' experiment_info(cd_configured)$log_transformed # Should be FALSE
#' experiment_info(cd_configured)$data_type # Should be "norm"
add_experiment_info <- function(cd_obj, period = NULL, data_type = NULL,
                                log_transformed = NULL, log_base = NULL,
                                estimate_delta_t = TRUE) {

  # --- Type Check ---
  if (!inherits(cd_obj, "CircadianData")) {
    stop("'cd_obj' must be an object of class CircadianData.", call. = FALSE)
  }

  exp_info <- experiment_info(cd_obj)
  mdata <- metadata(cd_obj)

  # === 1. Validate and Update Core Parameters ===
  # Priority: User Argument > Existing Value > Default Value

  # --- Period ---
  if (!is.null(period)) {
    if (!is.numeric(period) || !(length(period) %in% c(1, 2))) {
      stop("'period' must be a numeric vector of length 1 or 2.", call. = FALSE)
    }
    exp_info$period <- period
  } else if (is.null(exp_info$period)) {
    message("`period` not provided. Using default value of 24.")
    exp_info$period <- 24
  }

  # --- Data Type ---
  if (!is.null(data_type)) {
    exp_info$data_type <- match.arg(data_type, c("count", "norm"))
  } else if (is.null(exp_info$data_type)) {
    message("`data_type` not provided. Using default value of 'norm'.")
    exp_info$data_type <- "norm"
  }

  # --- Log Transformed ---
  if (!is.null(log_transformed)) {
    if (!is.logical(log_transformed) || length(log_transformed) != 1) {
      stop("'log_transformed' must be TRUE or FALSE.", call. = FALSE)
    }
    exp_info$log_transformed <- log_transformed
  } else if (is.null(exp_info$log_transformed)) {
    message("`log_transformed` not provided. Using default value of FALSE.")
    exp_info$log_transformed <- FALSE
  }

  # --- Log Base (dependent on log_transformed) ---
  if (exp_info$log_transformed) {
    if (!is.null(log_base)) {
      if (!is.numeric(log_base) || length(log_base) != 1) {
        stop("'log_base' must be a single number.", call. = FALSE)
      }
      exp_info$log_base <- log_base
    } else if (is.null(exp_info$log_base)) {
      message("Data is log-transformed but `log_base` not provided. Using default value of 2.")
      exp_info$log_base <- 2
    }
  } else {
    exp_info$log_base <- NULL # Ensure log_base is NULL if not transformed
  }

  # === 2. Automatically Re-calculate Inferred Information ===
  meta_cnames <- colnames(mdata)

  # --- Group info ---
  groups <- unique(mdata$group)
  n_groups <- length(groups)
  if ("group" %in% meta_cnames) {
    # exp_info$groups <- levels(mdata$group)
    exp_info$n_groups <- n_groups
  } else {
    # exp_info$groups <- NA
    exp_info$n_groups <- NA
  }

  # --- Repeated measures ---
  exp_info$repeated_measures <- "subject_ID" %in% meta_cnames

  # --- Replicate info ---
  # Make sure to show all time points in every group
  t_unique <- sort(unique(mdata$time))
  if (!is.na(exp_info$n_groups)) {
    exp_info$n_replicates <- tapply(mdata$time, mdata$group, function(g_time)
      table(factor(g_time, levels = t_unique)), simplify = FALSE)
  } else {
    exp_info$n_replicates <- table(factor(mdata$time, levels = t_unique))
  }

  # --- Sampling interval ---
  if (estimate_delta_t) {
    # Extract time differences
    delta_ts <- diff(sort(unique(mdata$time)))
    if (length(delta_ts) == 0) {
      exp_info$delta_t <- NA
    } else {
      delta_freqs <- sort(table(delta_ts), decreasing = TRUE)
      delta_t_unique <- as.numeric(names(delta_freqs))
      most_common <- delta_t_unique[1]
      if (length(delta_t_unique) == 1 || all(delta_t_unique[-1] %% most_common == 0)) {
        exp_info$delta_t <- most_common
      } else {
        exp_info$delta_t <- NA
        # message(
        #   "WARNING: Unable to determine a regular sampling interval. Proceeding ",
        #   "with the analysis under the assumption of irregular sampling."
        # )
      }
    }
  }

  # --- Number of cycles ---
  dt <- exp_info$delta_t
  if (!is.null(dt) && !is.na(dt)) {
    mean_period <- mean(exp_info$period)

    if (is.na(exp_info$n_groups)) {
      # No groups
      t_min <- min(mdata$time)
      t_max <- max(mdata$time)
      exp_info$n_cycles <- ((t_max + dt) - t_min) / mean_period
    } else {
      # Group-wise
      exp_info$n_cycles <- tapply(mdata$time, mdata$group,
                                  function(g_time) (max(g_time) - min(g_time) + dt) / mean_period,
                                  simplify = FALSE)
    }
  } else {
    # Sampling interval NA or NULL
    exp_info$n_cycles <- NA
  }


  # === 3. Put the updated list back into the object ===
  experiment_info(cd_obj) <- exp_info

  return(cd_obj)
}



# ---- Element-wise access/modification of experiment_info ----

#' Access and modify elements of `experiment_info`
#'
#' @description Provides methods for getting and setting elements within the
#' `experiment_info` list slot of a \code{CircadianData} object using standard R
#' operators.
#'
#' @param x A \code{CircadianData} object.
#' @param i,name A single character string specifying the name of the element.
#' @param value The value to assign to the element.
#' @param j, ... Not used in these methods.
#'
#' @details These methods allow for intuitive interaction with the
#' `experiment_info` list:
#' \itemize{
#'   \item `x$name` or `x[["name"]]`: Retrieve an element.
#'   \item `x$name <- value` or `x[["name"]] <- value`: Assign or add an element.
#' }
#'
#' @name CircadianData-subset-expinfo
#' @aliases [[,CircadianData,character,missing-method
#'   [[<-,CircadianData,character,missing-method $,CircadianData-method
#'   $<-,CircadianData-method
#' @examples
#' counts <- matrix(rpois(10, 50), 2, 5)
#' meta <- data.frame(row.names = paste0("S", 1:5))
#' cd_obj <- CircadianData(counts, meta)
#'
#' # Add a new element to experiment_info using `$`
#' cd_obj$period <- 24
#'
#' # Add another element using `[[`
#' cd_obj[["data_type"]] <- "norm"
#'
#' # Retrieve the information
#' cd_obj$period
#' cd_obj[["data_type"]]
#'
#' # See the entire updated list
#' experiment_info(cd_obj)
NULL # Attach documentation to a NULL object


## ---- Methods for `[[` ----

#' @rdname CircadianData-subset-expinfo
setMethod("[[", c("CircadianData", "character", "missing"),
          function(x, i, j, ...) {
            if (length(i) != 1) {
              stop("Index 'i' must be a single character string for accessing experiment_info.")
            }
            return(x@experiment_info[[i]])
          }
)

#' @rdname CircadianData-subset-expinfo
setReplaceMethod("[[", c("CircadianData", "character", "missing"),
                 function(x, i, j, value) {
                   if (length(i) != 1) {
                     stop("Index 'i' must be a single character string for assigning to experiment_info.")
                   }
                   x@experiment_info[[i]] <- value
                   validObject(x)
                   return(x)
                 }
)


## ---- Methods for `$` ----

#' @rdname CircadianData-subset-expinfo
setMethod("$", "CircadianData",
          function(x, name) {
            return(x@experiment_info[[name]])
          }
)

#' @rdname CircadianData-subset-expinfo
setMethod("$<-", "CircadianData",
          function(x, name, value) {
            x@experiment_info[[name]] <- value
            validObject(x)
            return(x)
          }
)


# ---- Sorting ----

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
#'   to the specified criteria.
#'
#' @export
#' @rdname order_samples
#' @examples
#' # --- Create a valid CircadianData object first ---
#' set.seed(456)
#' counts <- matrix(rpois(80, lambda = 50), nrow = 10, ncol = 8,
#'                  dimnames = list(paste0("Feature", 1:10), paste0("Sample", 1:8)))
#' meta_df <- data.frame(
#'   sample_name = paste0("Sample", 1:8),
#'   time_point = rep(c(12, 0, 6, 18), each = 2), # Unordered times
#'   subject = paste0("S", rep(1:4, 2)), # Unordered subjects
#'   condition = rep(c("A", "B"), 4)
#' )
#' cd_obj <- CircadianData(
#'   dataset = counts,
#'   metadata = meta_df,
#'   colname_sample = "sample_name",
#'   colname_time = "time_point",
#'   colname_subject = "subject",
#'   colname_group = "condition"
#' )
#'
#' print("Original Metadata Order:")
#' print(metadata(cd_obj))
#'
#' # --- Now run the sorting function ---
#' # Sort samples by time
#' cd_sorted_time <- order_samples(cd_obj, "time")
#' print("Metadata sorted by time:")
#' print(metadata(cd_sorted_time))
#'
#' # Sort samples first by time (asc), then by subject_ID (desc)
#' cd_sorted_multi <- order_samples(cd_obj, c("time", "subject_ID"), decreasing = c(FALSE, TRUE))
#' print("Metadata sorted by time (asc) then subject_ID (desc):")
#' print(metadata(cd_sorted_multi))
#'
setGeneric("order_samples", function(x, by_columns, decreasing = FALSE, ...) standardGeneric("order_samples"))

#' @rdname order_samples
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
            # Always use method = "radix" for consistency and vector `decreasing` support
            order_args <- c(sort_cols_list,
                            list(decreasing = decreasing, method = "radix"))

            # Get the indices that define the new order
            new_order_indices <- do.call(order, order_args)

            # --- Apply Ordering ---
            # Reorder dataset columns
            new_dataset <- dataset(x)[, new_order_indices, drop = FALSE]

            # Reorder metadata rows
            new_metadata <- mdata[new_order_indices, , drop = FALSE]

            # --- Create and Return New Object ---
            # Use new() instead of the user-facing CircadianData() constructor
            # This correctly builds the object from already-processed slots.
            new("CircadianData",
                dataset = new_dataset,
                metadata = new_metadata,
                experiment_info = experiment_info(x),
                results = results(x)
            )
          }
)


# ---- Filtering ----
# TODO: Remove `...` param after checking that it doesn't break anything?
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
#'   subject_ID = paste0("S", rep(1:4, 2)),
#'   group = rep(c("Control", "Treated"), 4)
#' )
#' cd_obj <- CircadianData(counts, meta, experiment_info = list(period = 24))
#'
#' # Filter samples belonging to the "Control" group
#' cd_control <- filter_samples(cd_obj, col = "group", value = "Control")
#' print(metadata(cd_control))
#'
#' # Filter samples from subject S1 or S3
#' cd_s1_s3 <- filter_samples(cd_obj, col = "subject_ID", value = c("S1", "S3"))
#' print(metadata(cd_s1_s3))
#'
#' # Filter samples collected at time 0 or 6
#' cd_t0_t6 <- filter_samples(cd_obj, col = "time", value = c(0, 6))
#' print(metadata(cd_t0_t6))
#'
setGeneric("filter_samples", function(x, col, value, ...) standardGeneric("filter_samples"))

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

            # --- Update experiment info ---
            # Update e.g. number of groups and replicates. Don't accidentally
            # change sampling interval
            filtered_obj <- add_experiment_info(filtered_obj, estimate_delta_t = FALSE)

            return(filtered_obj)
          }
)


# ---- Show Method ----

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

  # --- Results Info ---
  cat("\nResults:\n")
  res <- results(object)
  if (length(res) > 0) {
    cat(" Stored results for:", paste(names(res), collapse=", "), "\n")
  } else {
    cat(" [No results stored]\n")
  }

  # Add a final newline for spacing
  cat("\n")
})



# ---- Plotting ----

#' Plot Feature Values Over Time
#'
#' Creates a scatterplot of the values for a specific feature against time.
#' If a 'group' column exists in the metadata, points are colored by group,
#' and the plot can be optionally filtered to a single group.
#'
#' @param x A \code{CircadianData} object.
#' @param feature A single character string specifying the name of the feature
#'   (from `rownames(dataset(x))`) to plot.
#' @param group An optional single character string specifying a group to filter
#'   by. This is only applicable if a 'group' column exists in the metadata.
#'   If `NULL` (the default) and a 'group' column exists, data from all
#'   groups are plotted with different colors.
#' @param ... Additional arguments passed to the base `plot()` function.
#'
#'
#' @export
#' @rdname plot_feature
#' @examples
#' # --- With a 'group' column in metadata ---
#' set.seed(123)
#' counts <- matrix(rpois(80, lambda = 50), nrow = 10, ncol = 8,
#'                  dimnames = list(paste0("Feature", 1:10), paste0("Sample", 1:8)))
#' meta <- data.frame(
#'   row.names = paste0("Sample", 1:8),
#'   time = rep(c(0, 6, 12, 18), each = 2),
#'   group = factor(rep(c("Control", "Treated"), 4))
#' )
#' cd_obj_grouped <- CircadianData(counts, meta, experiment_info = list(period = 24))
#'
#' # Plot a feature for all groups (colored by group)
#' plot_feature(cd_obj_grouped, feature = "Feature3")
#'
#' # Plot a feature for only the "Treated" group
#' plot_feature(cd_obj_grouped, feature = "Feature3", group = "Treated")
#'
#' # --- Without a 'group' column in metadata ---
#' meta_no_group <- meta[, "time", drop = FALSE] # Create metadata without group col
#' cd_obj_no_group <- CircadianData(counts, meta_no_group)
#'
#' # Plot a feature (all points will have the same color)
#' plot_feature(cd_obj_no_group,
#'              feature = "Feature5",
#'              main = "Feature 5 (No Groups Defined)")
#'
setGeneric("plot_feature", function(x, feature, group = NULL, ...) standardGeneric("plot_feature"))

#' @rdname plot_feature
setMethod("plot_feature", "CircadianData",
          function(x, feature, group = NULL, ...) {

            # --- 1. Input Validation ---
            mdata <- metadata(x)
            if (!("time" %in% colnames(mdata))) {
              stop("Metadata must contain a 'time' column for plotting.")
            }
            if (!is.character(feature) || length(feature) != 1) {
              stop("'feature' must be a single character string.")
            }
            if (!(feature %in% rownames(dataset(x)))) {
              stop("Feature '", feature, "' not found in the dataset.")
            }

            # Check for group column existence
            has_group_col <- "group" %in% colnames(mdata)

            # Validate the 'group' argument if the column exists
            if (!is.null(group)) {
              if (!has_group_col) {
                stop("'group' argument was provided, but no 'group' column exists in metadata.")
              }
              if (!is.character(group) || length(group) != 1) {
                stop("'group' argument must be NULL or a single character string.")
              }
              if (!(group %in% mdata$group)) {
                stop("Group '", group, "' not found in the 'group' column of metadata.")
              }
            }

            # --- 2. Prepare Data for Plotting ---
            feature_values <- dataset(x)[feature, , drop = TRUE]
            plot_df <- data.frame(
              time = mdata$time,
              value = feature_values
            )

            # Add group information conditionally
            if (has_group_col) {
              plot_df$group <- mdata$group
            } else {
              # Assign a dummy group if none exists
              plot_df$group <- factor("all_samples")
            }

            # Apply filtering if a specific group is requested
            if (!is.null(group)) {
              plot_df <- plot_df[plot_df$group == group, , drop = FALSE]
              if (nrow(plot_df) == 0) {
                stop("No data available for feature '", feature, "' in group '", group, "'.")
              }
            }

            # --- 3. Set Up Plotting Parameters ---
            user_args <- list(...)
            plot_args <- list()

            # Set default shape
            plot_args$pch = 21

            # Set default labels and title
            plot_args$xlab <- "Time"
            plot_args$ylab <- "Value"
            default_main <- if (is.null(group)) {
              paste("Expression of", feature)
            } else {
              paste("Expression of", feature, "in group", group)
            }
            plot_args$main <- default_main

            # Determine colors and if a legend is needed
            unique_groups <- unique(as.character(plot_df$group))
            n_groups <- length(unique_groups)
            show_legend <- has_group_col && n_groups > 1

            if (show_legend) {
              # Background colour ---
              if ("bg" %in% names(user_args)) {
                if (length(user_args$bg) < n_groups) {
                  stop("Number of background colours (`bg`) must not be smaller than number of groups")
                }
                bgs <- user_args$bg[1:n_groups]
                user_args$bg <- NULL
              } else {
                bgs <- grDevices::palette.colors(n = n_groups, palette = "Okabe-Ito")
              }
              names(bgs) <- unique_groups
              plot_args$bg <- bgs[as.character(plot_df$group)]

              # Colour ---
              if ("col" %in% names(user_args)) {
                if (length(user_args$col) < n_groups) {
                  stop("Number of colours (`col`) must not be smaller than number of groups")
                }
                cols <- user_args$col[1:n_groups]
                user_args$col <- NULL
              } else {
                cols <- rep("black", n_groups)
              }
              names(cols) <- unique_groups
              plot_args$col <- cols[as.character(plot_df$group)]

            } else {
              plot_args$bg <- "lightgrey"
              plot_args$col <- "black"
            }

            # Merge and potentially overwrite with user arguments
            final_args <- utils::modifyList(plot_args, user_args)

            # Enforce x and y parameters
            final_args$x <- plot_df$time
            final_args$y <- plot_df$value

            # --- 4. Create the Plot ---
            do.call("plot", final_args)

            # --- 5. Add a Legend (if needed) ---
            if (show_legend) {
              legend("topright",
                     legend = names(bgs),
                     pt.bg = bgs,
                     col = final_args$col,
                     pch = 21,
                     bty = "n")
            }

            invisible(NULL)
          }
)


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
