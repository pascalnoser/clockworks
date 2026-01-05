# TODO: Change most S4 functions to regular functions


# ---- Class definition ----

#' Define the CircadianData Class
#'
#' @description An S4 class to store and manage data from biological
#' experiments, such as circadian studies. It serves as an integrated container
#' for raw measurement data, sample annotations, experimental parameters, and
#' analysis results.
#'
#' @details This class bundles the main components of an experimental dataset.
#' Its primary goal is to ensure that the samples (columns of `dataset`, rows of
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
#' @slot wave_params A data frame containing estimated wave parameters
#'   of a sine wave fit to each feature using a harmonic regression.
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
           wave_params = "data.frame",
           results = "list"
         )
)


# ---- Validity Check ----

setValidity("CircadianData", function(object) {
  errors <- character()
  dset <- object@dataset
  mdata <- object@metadata
  wave_par <- object@wave_params
  exp_info <- object@experiment_info
  res <- object@results

  # --- Dataset ---
  if (!is.matrix(dset) || !is.numeric(dset)) {
    errors <- c(errors, "'dataset' slot must be a numeric matrix.")
  }

  if (is.null(rownames(dset))) {
    errors <- c(errors, "'dataset' slot must have feature names (row names).")
  }

  # --- Metadata ---
  if (!"time" %in% colnames(mdata) || !is.numeric(mdata$time)) {
    errors <- c(errors, "A valid object must have a numeric 'time' column in its metadata.")
  }

  # --- Synchronization ---
  if (!identical(colnames(dset), rownames(mdata))) {
    errors <- c(errors, "Column names of 'dataset' must be identical to and in the same order as row names of 'metadata'.")
  }


  # --- Experiment Info ---
  if (!is.list(exp_info)) {
    errors <- c(errors, "'experiment_info' slot must be a list.")
  }

  # --- Wave parameters ---
  required_cols <- c(
    "feature",
    "period",
    "phase_estimate",
    "mesor_estimate",
    "amplitude_estimate",
    "relative_amplitude_estimate"
  )
  if ("group" %in% colnames(mdata)) required_cols <- c(required_cols, "group")

  if (!is.data.frame(wave_par)) {
    errors <- c(errors, "'wave_params' slot must be a data frame")
  } else {
    missing_cols <- setdiff(required_cols, colnames(wave_par))
    if (ncol(wave_par) > 0 && length(missing_cols) > 1) {
      str_missing_cols <- paste(missing_cols, collapse = ", ")
      errors <- c(errors, paste("'wave_params' data frame is missing the following columns:", str_missing_cols))
    }
  }

  # --- Results ---
  if (!is.list(res)) {
    errors <- c(errors, "'results' slot must be a list.")
  }

  if (length(errors) == 0) TRUE else errors
})


# ---- Constructor Function ----

#' Create a CircadianData Object
#'
#' This constructor function validates and standardizes user-provided dataset
#' and metadata, creating a `CircadianData` object. It serves as the primary
#' entry point for creating a valid object, integrating checks for data
#' integrity, required columns, and consistent sample IDs.
#'
#' @param dataset A data frame or numeric matrix with features as rows and
#'   samples as columns. Must have column names (sample IDs).
#' @param metadata A data frame containing sample annotations.
#' @param colname_time A character string specifying the name of the column in
#'   `metadata` that contains the time information (e.g., collection time). This
#'   column must be numeric.
#' @param colname_sample A character string specifying the name of the column in
#'   `metadata` that contains the sample IDs. These IDs must be unique and match
#'   the column names of `dataset`. Use `"rownames"` if sample IDs are in the
#'   metadata's row names.
#' @param colname_group An optional character string specifying the name of the
#'   column in `metadata` that contains group information.
#' @param colname_subject An optional character string specifying the name of
#'   the column in `metadata` containing subject IDs for repeated measures
#'   designs.
#' @param period A numeric vector of length 1 or 2 specifying the period range
#'   of interest. If `clockworks()` is ran with a method that requires a single
#'   value and `period` is a vector of length 2, the mean value will be used.
#'   Defaults to 24.
#' @param data_type A character string specifying the data type. Must be one of
#'   "count" or "norm". Defaults to "norm" if not already set.
#' @param log_transformed A logical value specifying if the input data is
#'   log-transformed. This has no influence on the rhythmicity analysis itself
#'   but is used to calculate the relative amplitude in the original scale of
#'   the data. Defaults to `FALSE`.
#' @param log_base A single numeric value specifying the base of the logarithm.
#'   Defaults to 2 if `log_transformed` is `TRUE` and no base is specified.
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
                          period = 24,
                          data_type = "norm",
                          log_transformed = FALSE,
                          log_base = NULL) {

  # === 1. Metadata Processing and Validation ===
  ## -- 1.1 Check if metadata is a data.frame --
  if (!inherits(metadata, "data.frame")) {
    stop("'metadata' must be a data.frame.", call. = FALSE)
  }


  ## -- 1.2 Ensure it's a data frame and not e.g. a tibble or data.table --
  metadata <- as.data.frame(metadata)


  ## -- 1.3 Check if columns are present --
  ### 1.3a Required columns (`colname_sample` and `colname_time`)
  # Handle case where sample IDs are in the rownames
  if (colname_sample == "rownames") {
    if (is.null(rownames(metadata))) stop("`colname_sample` is 'rownames', but metadata has no row names.", call.=FALSE)
    metadata$`.internal_sample_id` <- rownames(metadata)
    colname_sample <- ".internal_sample_id"
  }

  # Get column names
  cnames <- colnames(metadata)

  # Check if the columns exist
  required_args <- c("colname_sample", "colname_time")
  user_cols <- stats::setNames(c(colname_sample, colname_time), required_args)

  missing_cols <- user_cols[!user_cols %in% cnames]
  if (length(missing_cols) > 0) {
    stop("The following required columns are missing from metadata: ",
         paste0("'", missing_cols, "' (specified by '", names(missing_cols), "') ", collapse=", "),
         call. = FALSE)
  }


  ### 1.3b Optional columns (`colname_group` and `colname_subject`)
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

  ## -- 1.4 Check if the values in colname_time are numeric --
  if (!is.numeric(metadata[[colname_time]])) {
    stop(
      paste0(
        "Column '", colname_time, "' of `metadata` is of type ", class(metadata[[colname_time]]),
        ". The column defined as `colname_time` in `metadata` must be of type numeric."
      ),
      call. = FALSE
    )
  }

  ## -- 1.5 Check if the values in colname_sample are unique --
  if (any(duplicated(metadata[[colname_sample]]))) {
    stop(
      paste0("Values in metadata column '", colname_sample, "' must be unique."),
      call. = FALSE
    )
  }

  ## -- 1.6 Check subject ID logic --
  # Print a message if all values are unique because this suggests the user
  # simply has replicates rather than repeated measures, in which case
  # `colname_sample` need not be defined.
  if (!is.null(colname_subject) && !any(duplicated(metadata[[colname_subject]]))) {
    message("All values in column '", colname_subject, "' are unique. ",
            "Assuming no repeated measures and ignoring this column.")
    colname_subject <- NULL
  }


  ## -- 1.7 Warn the user that additional columns will be ignored --
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

  ## -- 1.8 Standardize Metadata: Select and rename columns ---
  # TODO: Enforce type `factor` for group and subject ID?
  final_meta <- data.frame(row.names = as.character(metadata[[colname_sample]]))
  final_meta$time <- metadata[[colname_time]]
  if (!is.null(colname_group)) {
    final_meta$group <- as.factor(metadata[[colname_group]])
  }
  if (!is.null(colname_subject)) {
    final_meta$subject_ID <- as.factor(metadata[[colname_subject]])
  }


  # === 2. Dataset Processing and Validation ===
  ## -- 2.1 Check if dataset is a data.frame or matrix ---
  if (!inherits(dataset, c("data.frame", "matrix"))) {
    stop("`dataset` must be a data frame or matrix.", call. = FALSE)
  }

  ## -- 2.2 Check if dataset is numeric --
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

  ## -- 2.3 Check if dataset has row numbers --
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
        experiment_info = list(),
        wave_params = data.frame(),
        results = list())
  }, error = function(e) {
    stop("Failed to create CircadianData object. Please check the following error(s):\n",
         e, call. = FALSE)
  })

  # === 5. Add Experiment Info ===
  cd_obj <- add_experiment_info(
    cd_obj = cd_obj,
    period = period,
    data_type = data_type,
    log_transformed = log_transformed,
    log_base = log_base,
    estimate_delta_t = TRUE
  )

  # Validate info
  # TODO: Should I run this here or just in `clockworks()`?
  validate_exp_info(cd_obj)

  # === 6. Run Harmonic Regression ===
  cd_obj@wave_params <- estimate_wave_params(cd_obj)

  return(cd_obj)
}



# ---- Accessor Methods ----

#' Access or Replace Object Components
#'
#' @description Functions to access or replace the main components of a
#' \code{CircadianData} object, including the dataset matrix, metadata
#' data.frame, experiment_info list, and results list.
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
#'   \item \code{wave_params()}: Retrieves the list of estimated sine wave parameters.
#'   \item \code{get_results()}: Retrieves the list of analysis results.
#' }
#'
#' @return The requested component (for accessors) or the modified
#'   \code{CircadianData} object (for replacement methods).
#'
#' @name CircadianData-accessors
#' @aliases dataset dataset<- metadata metadata<- experiment_info
#'   experiment_info<- wave_params wave_params<-
NULL # A NULL object to hold the main documentation block


## ---- Dataset Accessor/Replacement ----

#' @rdname CircadianData-accessors
#' @export
setGeneric("dataset", function(x) standardGeneric("dataset"))

#' @rdname CircadianData-accessors
setMethod("dataset", "CircadianData", function(x) x@dataset)

#' @rdname CircadianData-accessors
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
#' Extract Results From a CircadianData Object
#'
#' `get_results()` retrieves standardised ("formatted") or original results for
#' one or all rhythm detection methods contained in a `CircadianData` object.
#'
#' @param cd A `CircadianData` object.
#' @param method Character. Either `"all"` (default) to extract results for all
#'   methods, or the name of a specific method.
#' @param type Character. Type of results to return. One of:
#'   \describe{
#'     \item{`"formatted"`}{Return standardised result data frames (default).}
#'     \item{`"original"`}{Return the original method outputs.}
#'     \item{`"both"`}{Return both for each method.}
#'   }
#'
#' @return If a specific method is selected, returns either:
#' \itemize{
#'   \item a data frame (`type = "formatted"`),
#'   \item a method-specific object (`type = "original"`), or
#'   \item a list containing both (`type = "both"`).
#' }
#'
#' If `method = "all"`, returns a named list containing the requested type of
#' results for each method.
#'
#' @examples
#' \dontrun{
#' # Retrieve formatted results for all methods
#' get_results(cd)
#'
#' # Get formatted results for one method
#' get_results(cd, method = "RAIN")
#'
#' # Get original results for all methods
#' get_results(cd, type = "original")
#' }
#'
#' @export
get_results <- function(cd, method = "all", type = c("formatted", "original", "both")) {

  # Match type argument
  type <- match.arg(type)

  # Extract available methods
  methods_available <- names(cd@results)

  # Throw error if no results available
  if (length(methods_available) == 0) {
    stop("No results stored in CircadianData object.")
  }

  # Check method validity
  if (!identical(method, "all") && !method %in% methods_available) {
    stop("Unknown method: ", method,
         ". Available methods: ", paste(methods_available, collapse = ", "))
  }

  # Helper to extract for a single method
  extract_one <- function(m) {
    res <- cd@results[[m]]
    switch(
      type,
      formatted = res$res_formatted,
      original  = res$res_original,
      both      = res
    )
  }

  # If requesting a single method
  if (method != "all") {
    return(extract_one(method))
  }

  # If method = "all"
  if (type == "both") {
    # If only one method, return as data frame instead of list of length 1
    if (length(methods_available) == 1) {
      return(extract_one(methods_available[1]))
    } else {
      # Return the whole slot
      return(cd@results)
    }
  }

  # Otherwise return all formatted or all original results
  out <- lapply(methods_available, extract_one)
  names(out) <- methods_available

  # If only one method, return as data frame
  if (length(out) == 1) out <- out[[1]]

  return(out)
}



## ---- Wave params Accessor/Replacement ----

#' @rdname CircadianData-accessors
#' @export
setGeneric("wave_params", function(x) standardGeneric("wave_params"))

#' @rdname CircadianData-accessors
setMethod("wave_params", "CircadianData", function(x) x@wave_params)

#' @rdname CircadianData-accessors
setGeneric("wave_params<-", function(x, value) standardGeneric("wave_params<-"))

#' @rdname CircadianData-accessors
setReplaceMethod("wave_params", "CircadianData", function(x, value) {
  if (!is.data.frame(value)) stop("'value' must be a data frame.")
  x@wave_params <- value
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
#' synchronized after subsetting. Also updates the experiment info.
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
#' cd_obj <- CircadianData(
#'   dataset = counts,
#'   metadata = meta,
#'   colname_sample = "rownames",
#'   colname_time = "time",
#'   colname_group = "group"
#' )
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

            # Reset levels for group and subject ID to prevent empty levels
            if ("group" %in% colnames(new_metadata)) {
              new_metadata$group = factor(new_metadata$group)
            }
            if ("subject_ID" %in% colnames(new_metadata)) {
              new_metadata$subject_ID = factor(new_metadata$subject_ID)
            }

            # Only keep wave parameters for the selected features (i)

            # TODO: IN THE FILTERING FUNCTION I'M JUST RECALCULATING THE WAVE
            # PARAMETERS IN THE END SO THIS PART IS KIND OF REDUNDANT. I CAN'T
            # RECALCULATE THE WAVE PARAMS IN THIS FUNCTION, BECAUSE THAT
            # FUNCTION CALLS THIS ONE SO AN ENDLESS LOOP IS CREATED. I COULD
            # ALSO JUST EXPAND THE CODE BELOW TO NOT ONLY REMOVE FILTERED
            # FEATURES BUT ALSO REMOVE UNUSED GROUPS.
            new_wave_params <- wave_params(x)
            if (nrow(new_wave_params) > 0) {
              # Note: `i` can be logical, numeric, or character. This works for all.
              # `rownames(x)` provides the names to subset by if `i` is character.
              features_to_keep <- rownames(x)[i]
              new_wave_params <- new_wave_params[new_wave_params$feature %in% features_to_keep, , drop = FALSE]
            }

            # TODO: Only keep results of selected features
            # ...

            # Create the new object
            x_new = new(
              "CircadianData",
              dataset = new_dataset,
              metadata = new_metadata,
              wave_params = new_wave_params,
              results = x@results
            )

            # Recalculate delta t and update replicate numbers, but keep the rest
            exp_info_old = x@experiment_info
            x_new = add_experiment_info(
              cd_obj = x_new,
              period = exp_info_old$period,
              data_type = exp_info_old$data_type,
              log_transformed = exp_info_old$log_transformed,
              log_base = exp_info_old$log_base,
              estimate_delta_t = TRUE
            )

            return(x_new)
          })


# ---- Adding experiment_info ----

#' Add or Update Experiment Parameters for Analysis
#'
#' This function populates or updates the `experiment_info` slot of a
#' `CircadianData` object with parameters required for downstream analysis.
#'
#' @details On the first call, core parameters can be omitted to use sensible
#' defaults (e.g., `period = 24`, `data_type = "norm"`). Subsequent calls will
#' use values already stored in the object unless explicitly provided again to
#' be overwritten.
#'
#' @param cd_obj A \code{CircadianData} object.
#' @param period An optional numeric vector of length 1 or 2. Defaults to 24 if
#'   not already set.
#' @param data_type An optional character string specifying the data type. Must
#'   be one of "count" or "norm". Defaults to "norm" if not already set.
#' @param log_transformed An optional logical value (`TRUE`/`FALSE`). Defaults
#'   to `FALSE` if not already set.
#' @param log_base An optional single numeric value specifying the base of the
#'   logarithm. Defaults to 2 if `log_transformed` is `TRUE` and no base is set.
#' @param estimate_delta_t A logical value. If `TRUE` (default), the sampling
#'   interval (`delta_t`) will be automatically estimated.
#'
#' @return A \code{CircadianData} object with an updated `experiment_info` slot.
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
#' Provides methods for getting and setting elements within the
#' `experiment_info` list slot of a \code{CircadianData} object using standard R
#' operators.
#'
#' @param x A \code{CircadianData} object.
#' @param i,name A single character string specifying the name of the element.
#' @param value The value to assign to the element.
#' @param j Not used in these methods.
#' @param ... Not used
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
#' data("cw_data", package = "clockworks")
#' data("cw_metadata", package = "clockworks")
#' cd_obj <- CircadianData(cw_data,
#'                         cw_metadata,
#'                         colname_sample = "Sample_ID",
#'                         colname_time = "Time")
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


# ---- Estimating Wave Parameters ----

# TODO: Probably export this function in case someone doesn't want to use the
# package for rhythmicity detection but just for plotting
# TODO: Explain stuff about relative amplitude

#' Estimating cosine Wave Parameters
#'
#' This function is used to get an estimate for the cosine wave parameters for
#' each feature using harmonic regression.
#'
#' @param cd_obj A `CircadianData` object
#'
#' @import HarmonicRegression
#'
#' @details
#' The returned parameters correspond to the model \deqn{y = M + A
#' cos(\frac{2\pi}{T} (t - \varphi))} With \eqn{M} the mesor, \eqn{A} the
#' amplitude, \eqn{T} the period, \eqn{t} the time and \eqn{\varphi} the phase
#' in the same units as \eqn{t} (e.g. hours).
#'
#' @returns A data frame with estimated sine wave parameters for every feature.
estimate_wave_params <- function(cd_obj) {
  if (!inherits(cd_obj, "CircadianData")) stop("Input must be a CircadianData object.")

  # Get original meta data
  mdata_orig <- metadata(cd_obj)

  # Get minimum value for time
  t_min <- min(mdata_orig$time)

  # Get period
  per <- mean(cd_obj$period)

  # Add temporary group if there is no group column
  if (is.na(cd_obj$n_groups)){
    mdata_tmp <- mdata_orig
    mdata_tmp[["group"]] <- "tmp"
    metadata(cd_obj) <- mdata_tmp
    add_groups <- FALSE
  } else {
    add_groups <- TRUE
  }

  # Get groups
  groups <- unique(metadata(cd_obj)$group)

  # Run harmonic regression for each group separately
  ls_params <- lapply(groups, function(grp) {
    # Filter cd object

    # Note: Don't use `filter_samples()` here because that function calls this
    # one, so an endless loop would be created
    mdata <- metadata(cd_obj)
    sample_filt <- rownames(mdata[mdata$group == grp, ])
    cd_filt <- cd_obj[, sample_filt]

    # Run harmonic regression
    res_harm <- HarmonicRegression::harmonic.regression(
      inputts = t(dataset(cd_filt)),
      inputtime = metadata(cd_filt)[["time"]],
      Tau = per,
      normalize = FALSE,
      trend.eliminate = FALSE,  # TODO: make this a variable,
      trend.degree = 1  # TODO: make this a variable
    )

    # Get phase estimate in hours.`harmonic.regression()` uses a cosine, but the
    # phase estimate can easily be converted to that of a sine function.
    # Amplitude and mesor stay the same, but the phase is shifted by -pi/2 in
    # radians, so by -period/4 in hours. Using the modulo operator negative
    # phase values "wrap" around and become positive. Note that the resulting
    # phase estimates in hours result in identical curves for the two following
    # models (note the difference in - and +)
    # y = M + A * cos(2 * pi / T * (t - phi))
    # y = M + A * sin(2 * pi / T * (t + phi))
    phase_estimate_rad <- res_harm$pars$phi
    phase_estimate_h_cos <- phase_estimate_rad * per / (2 * pi)
    phase_estimate_h_sin <- (-1 * (phase_estimate_h_cos - per / 4)) %% per

    # Create data frame
    df_res = data.frame(
      feature = row.names(res_harm$pars),
      period = per,
      phase_estimate = phase_estimate_h_cos
    )

    # Add peak time
    # Take phase estimates and shift them upward by repeated additions of the
    # period until the values are greater than t_min. For example, if t_min is 18
    # and one gene has a phase of 21 this remains unchanged, because the peak in
    # the real data is at 21. If it has a phase of 5, the real peak is not at 5
    # but at 29.
    df_res$peak_time_estimate <- df_res$phase_estimate +
      pmax(0, ceiling((t_min - df_res$phase_estimate) / per)) * per

    # Add mesor and amplitude estimates
    if (cd_obj$log_transformed == TRUE) {
      # Get logarithmic base
      b <- cd_obj$log_base

      # Get estimates for mesor and amplitude in log scale
      log_mesors <- res_harm$means
      log_amps <- res_harm$pars$amp

      # Get values for peak and trough
      log_peaks <- b^(log_mesors + log_amps)
      log_troughs <- b^(log_mesors - log_amps)

      # Get mesor and amplitude in linear scale
      lin_mesors <- (log_peaks + log_troughs) / 2
      lin_amplitudes <- (log_peaks - log_troughs) / 2

      # Get relative amplitude
      lin_relative_amplitudes <- lin_amplitudes / lin_mesors
      # lin_relative_amplitudes <- compute_relative_amplitude(log_amps, b)

      # Add to data frame
      # TODO: Be clear in documentation that in this case (i.e. with
      # log-transformed data) the values you get for `mesor_estimate` and
      # `amplitude_estimate` are in the log-scale, i.e. in the scale of the data
      # the user provided, but the `relative_amplitude_estimate` is in the
      # linear scale. We decided to do it this way because it seems appropriate
      # to give the mesor and amplitude for the actual data the user provides.
      # On the other hand, getting the relative amplitude in the linear scale is
      # not a super trivial task (but it might still be of interest), so we
      # provide that. The relative amplitude in the log-scale is easy to get by
      # simply dividing the amplitude estimate by the mesor estimate.
      df_res$mesor_estimate <- log_mesors
      df_res$amplitude_estimate <- log_amps
      df_res$relative_amplitude_estimate <- lin_relative_amplitudes

    } else {
      df_res$mesor_estimate <- res_harm$means
      df_res$amplitude_estimate <- res_harm$pars$amp
      df_res$relative_amplitude_estimate <- df_res$amplitude_estimate / df_res$mesor_estimate
    }

    # Add group if original object had groups
    if (add_groups == TRUE) {
      df_res$group <- grp
    }

    return(df_res)
  })

  # Bind into data frame and return
  df_params <- do.call(rbind, ls_params)

  ## -- Add peak time
  # Take phase estimates and shift them upward by repeated additions of the
  # period until the values are greater than t_min. For example, if t_min is 18
  # and one gene has a phase of 21 this remains unchanged, because the peak in
  # the real data is at 21. If it has a phase of 5, the real peak is not at 5
  # but at 29.
  df_params$peak_time_estimate <- df_params$phase_estimate +
    pmax(0, ceiling((t_min - df_params$phase_estimate) / per)) * per

  # Rearrange columns
  cols <- intersect(
    c(
      "feature",
      "group",
      "period",
      "phase_estimate",
      "peak_time_estimate",
      "mesor_estimate",
      "amplitude_estimate",
      "relative_amplitude_estimate"
    ),
    colnames(df_params)
  )

  df_params <- df_params[, cols]

  return(df_params)
}



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
                wave_params = wave_params(x),
                results = x@results
            )
          }
)


# ---- Filtering ----
#' Filter Samples in a CircadianData Object
#'
#' Subsets a `CircadianData` object by rows of its metadata using a flexible
#' filtering expression, similar to `dplyr::filter()`.
#'
#' @param cd_obj A \code{CircadianData} object.
#' @param filter_expr An expression that evaluates to a logical vector within
#'   the context of the object's metadata. Use column names from
#'   `metadata(cd_obj)` directly in the expression.
#' @param recalc_delta_t A logical value. If `TRUE`, the sampling interval
#'   (`delta_t`) will be re-estimated from the remaining time points after
#'   filtering. Defaults to `TRUE`.
#'
#' @return A new \code{CircadianData} object containing only the samples that
#'   satisfy the `filter_expr`.
#'
#' @importFrom rlang enquo eval_tidy quo_text
#' @export
#' @examples
#' # --- Setup ----
#' counts <- matrix(rpois(80, 50), nrow=10, ncol=8,
#'                  dimnames=list(paste0("Feature", 1:10), paste0("Sample", 1:8)))
#' meta_df <- data.frame(
#'   sample_id = paste0("Sample", 1:8),
#'   time = rep(c(0, 6, 12, 18), each = 2),
#'   group = rep(c("Control", "Treated"), 4)
#' )
#' cd_obj <- CircadianData(
#'   dataset = counts, metadata = meta_df,
#'   colname_sample = "sample_id", colname_time = "time", colname_group = "group"
#' )
#'
#' # --- Filtering Examples ---
#'
#' # Filter for a single group
#' cd_control <- filter_samples(cd_obj, group == "Control")
#' print(metadata(cd_control))
#'
#' # Filter for a time range
#' cd_late <- filter_samples(cd_obj, time >= 12)
#' print(metadata(cd_late))
#'
#' # Combine conditions
#' cd_filtered <- filter_samples(cd_obj, group == "Treated" & time < 12)
#' print(metadata(cd_filtered))
#'
filter_samples <- function(cd_obj, filter_expr, recalc_delta_t = TRUE) {
  # --- 1. Manual Type Check ---
  # Check that cd_obj is a CircadianData object
  if (!inherits(cd_obj, "CircadianData")) {
    stop("'cd_obj' must be an object of class CircadianData.", call. = FALSE)
  }

  mdata <- metadata(cd_obj)
  if (ncol(cd_obj) == 0) return(cd_obj)

  # Check that recalc_delta_t is logical
  if (!is.logical(recalc_delta_t)) {
    stop("'recalc_delta_t' must be of type logcial, not '", class(recalc_delta_t), "'.")
  }

  # --- 2. Capture and Evaluate the Expression (using rlang) ---
  quo_filter <- rlang::enquo(filter_expr)

  logical_vector <- tryCatch({
    rlang::eval_tidy(quo_filter, data = mdata)
  }, error = function(err) {
    stop("Error evaluating 'filter_expr': ", err$message,
         "\n  Expression was: ", rlang::quo_text(quo_filter),
         "\n  Check that variables exist as column names in metadata.", call. = FALSE)
  })

  # --- 3. Validate the Result ---
  if (!is.logical(logical_vector)) {
    stop("'filter_expr' did not evaluate to a logical vector.")
  }
  if (length(logical_vector) != nrow(mdata)) {
    stop("Result of 'filter_expr' does not match the number of samples.")
  }
  logical_vector[is.na(logical_vector)] <- FALSE

  # --- 4. Subset the Object ---
  # Store old experiment info
  exp_info_old <- experiment_info(cd_obj)

  # Use the existing S4 subsetting method `[`
  # Note: This updates the experiment info, including delta t
  filtered_obj <- cd_obj[, logical_vector, drop = FALSE]

  # Optionally restore original delta t
  experiment_info(filtered_obj)$delta_t = exp_info_old$delta_t

  # --- 5. Recalculate wave params ---
  wave_params(filtered_obj) <- estimate_wave_params(filtered_obj)

  return(filtered_obj)
}


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


  # --- Wave Parameters Info ---
  cat("\nWave Parameters:\n")
  w_params <- wave_params(object)
  if (nrow(w_params) > 0) {
    n_features_params <- length(unique(w_params$feature))
    cat(paste(" Calculated for", n_features_params, "of", nrow(object), "features\n"))
  } else {
    cat(" [Not calculated]\n")
  }


  # --- Results Info ---
  cat("\nResults:\n")
  res <- object@results
  if (length(res) > 0) {
    cat(" Stored results for:", paste(names(res), collapse=", "), "\n")
  } else {
    cat(" [No results stored]\n")
  }

  # Add a final newline for spacing
  cat("\n")
})



# ---- Plotting ----

#' Plot Phase Estimates in a Circular Histogram
#'
#' Creates a circular plot showing the distribution of phase estimates for
#' features. The plot is structured as concentric rings, with each ring
#' representing a different group (if groups are present). The density of
#' features peaking at each time point is visualized using color intensity.
#'
#' @details
#' This function visualizes the output of wave-fitting analyses (harmonic
#' regression) stored in a `CircadianData` object. It bins the phase estimates
#' (peak times) for each feature into discrete time intervals and plots their
#' density on a circular axis representing the full period (e.g., 24 hours).
#' Features can be pre-filtered based on statistical significance from an
#' analysis stored in the `results` slot of the object.
#'
#' The plot is built using `ggplot2`, and the returned object can be further
#' customized with standard `ggplot2` functions.
#'
#' @param cd A \code{CircadianData} object that contains wave parameters
#'   generated by `estimate_wave_params()` (ran automatically when the
#'   \code{clockworks} function is used).
#' @param n_bins An integer specifying the number of bins to divide the period
#'   into for the histogram. Defaults to 24.
#' @param n_labels An integer specifying the number of evenly spaced time labels
#'   to display on the circular axis. Defaults to 4 (e.g., 0/24, 6, 12, 18).
#' @param pval_adj_cutoff A numeric value between 0 and 1. Only features with an
#'   adjusted p-value below this cutoff will be included in the plot. Requires a
#'   corresponding analysis result to be stored in the `results` slot. Defaults
#'   to 1 (including all features).
#' @param method An optional character string specifying which analysis result
#'   from the `results` slot to use for p-value filtering. If `NA` (default) and
#'   a `pval_adj_cutoff` is used, the function will use the first available
#'   analysis result.
#' @param initial_offset A numeric value controlling the radius of the innermost
#'   ring from the center of the plot.
#' @param step_size A numeric value controlling the distance between concentric
#'   rings when multiple groups are plotted. The height of the individual rings
#'   is fixed at 1.
#' @param title An optional character string for the plot's main title.
#' @param default_colour A character string specifying the color to use for the
#'   plot when no groups are present.
#'
#' @return A `ggplot` object representing the circular phase plot. This can be
#'   printed to display the plot or modified with additional `ggplot2` layers.
#'
#' @importFrom dplyr mutate count group_by ungroup distinct
#' @importFrom tidyr complete
#' @importFrom magrittr %>%
#' @import ggplot2
#' @export
# TODO: PROVIDE AN EXAMPLE
# TODO: Update documentation
plot_phase_estimates <- function(cd,
                                 n_bins = 24,
                                 n_labels = 4,
                                 pval_adj_cutoff = 1,
                                 method = NA,
                                 initial_offset = 5,
                                 step_size = 1.25,
                                 title = NULL,
                                 add_border = TRUE,
                                 alpha_range = c(0.05, 1),
                                 default_colour = "deepskyblue") {
  # --- 0. Validation ---
  # Check if 'add_border' is a single, non-NA, logical value (TRUE or FALSE)
  if (!is.logical(add_border) || length(add_border) != 1 || is.na(add_border)) {
    stop("The 'add_border' argument must be a single logical value (TRUE or FALSE).")
  }

  # --- 1. Get harmonic regression params and period ---
  # Get params
  df_params <- wave_params(cd)
  per <- mean(experiment_info(cd)$period)

  # Stop and tell user to run wave fit if not done so
  if (ncol(df_params) == 0) {
    stop("No wave parameters detected. Please run ",
    "`wave_parameters(cd) <- estimate_wave_params(cd)` and retry.")
  }

  # --- 2. p-value filtering ---
  res_methods <- names(cd@results)

  if (pval_adj_cutoff < 1 & is.na(method)) {
    if (length(res_methods) == 1) {
      method <- res_methods[1]
    } else if (length(res_methods) == 0) {
      message("Ignoring significance cutoff because no results available.")
    } else {
      method <- res_methods[1]
      message("Multiple results available but none selected. Using ",
              res_methods[1], " results for filtering.")
    }
  }

  # Get results if available
  if (!is.na(method)) {
    if (!method %in% res_methods) {
      message("Method ", method, " not in results. Ignoring significance cutoff.")
    } else {
      # Get results
      df_res <- get_results(cd, method, type = "formatted")
      # Filter by adjusted p-value
      df_res <- df_res[df_res$pval_adj < pval_adj_cutoff, ]
      # Filter wave params data frame
      df_params <- df_params[paste(df_params$feature, df_params$group) %in%
                               paste(df_res$feature, df_res$group), ]
    }
  }

  # --- 3. Data preparation ---
  # Add temporary group to wave params if column doesn't exist yet
  has_group_col <- "group" %in% colnames(df_params)
  if (!has_group_col) {
    df_params$group <- "tmp"
  }

  # Fix tile height at 1
  tile_height = 1

  # Adjust initial offset if necessary to not remove any data
  initial_offset = max(initial_offset, tile_height/2)

  binned_density_df <- df_params %>%
    mutate(
      hour_bin = round(phase_estimate * n_bins / per) %% n_bins
    ) %>%
    count(group, hour_bin, name = "count") %>%
    group_by(group) %>%
    mutate(density = count / sum(count)) %>%
    ungroup() %>%
    complete(group, hour_bin = 0:(n_bins - 1), fill = list(count = 0, density = 0)) %>%
    mutate(group = as.factor(group)) %>%
    mutate(group_numeric = initial_offset + (as.numeric(group) - 1) * step_size)

  # Create a data frame for the background border rings
  ring_background_df <- binned_density_df %>%
    # Get the unique numeric y-position for each group
    distinct(group, group_numeric) %>%
    mutate(
      # The height of our tiles is 1, so the edges are +/- 0.5 from the center
      ymin = group_numeric - tile_height/2,
      ymax = group_numeric + tile_height/2
    )

  # --- 4. Generate axis breaks and labels ---
  # Dynamically generate breaks and labels for the x-axis (hours)
  hour_label_points <- seq(0, per, length.out = n_labels + 1)
  hour_label_points <- head(hour_label_points, - 1) # Remove the last point (e.g., 24) to avoid overlap with 0

  # Convert these hours into the corresponding bin numbers for the 'breaks'
  plot_breaks <- hour_label_points * n_bins / per

  # Create the text labels, making the first one "0/24"
  plot_labels <- as.character(round(hour_label_points, 2))
  plot_labels[1] <- paste0("0/", per)

  # Get the y-axis breaks for positioning the rings
  y_axis_breaks <- unique(binned_density_df$group_numeric)

  # --- 5. Build the base plot ---
  # Start with the base plot object
  plt <- ggplot(binned_density_df, aes(x = hour_bin, y = group_numeric))

  # -- Conditionally add layers and scales --
  if (has_group_col) {
    # If we have groups, MAP fill to the 'group' column
    if (add_border == TRUE) {
      plt <- plt +
        # Add coloured rings
        geom_rect(
          data = ring_background_df,
          aes(ymin = ymin, ymax = ymax, fill = group),
          xmin = -Inf,
          xmax = Inf,
          inherit.aes = FALSE
        ) +
        # Add slightly smaller white rings
        geom_rect(
          data = ring_background_df,
          aes(ymin = ymin + tile_height * 0.025, ymax = ymax - tile_height * 0.025),
          fill = "white",
          xmin = -Inf,
          xmax = Inf,
          inherit.aes = FALSE
        )
    }
    # Add tiles
    plt <- plt +
      geom_tile(aes(fill = group, alpha = density), height = tile_height) +
      scale_fill_discrete(name = "Group")

  } else {
    # If no groups, SET fill to a static, default colour
    if (add_border == TRUE) {
      plt <- plt +
        # Add coloured rings
        geom_rect(
          data = ring_background_df,
          aes(ymin = ymin, ymax = ymax),
          fill = default_colour,
          xmin = -Inf,
          xmax = Inf,
          inherit.aes = FALSE
        ) +
        # Add slightly smaller white rings
        geom_rect(
          data = ring_background_df,
          aes(ymin = ymin + tile_height * 0.025, ymax = ymax - tile_height * 0.025),
          fill = "white",
          xmin = -Inf,
          xmax = Inf,
          inherit.aes = FALSE
        )
    }
    # Add tiles
    plt <- plt +
      geom_tile(aes(alpha = density), fill = default_colour, height = tile_height)
  }

  # --- 6. Add the common plot components that apply to both cases ---
  plt <- plt +
    coord_polar(start = -pi / n_bins) +
    scale_alpha_continuous(range = alpha_range, guide = "none") +
    # Handle axes
    scale_x_continuous(
      breaks = plot_breaks,
      labels = plot_labels
    ) +
    scale_y_continuous(
      breaks = y_axis_breaks,
      labels = NULL,
      limits = c(0, max(y_axis_breaks) + tile_height/2)
    ) +
    labs(
      title = title,
      x = NULL,
      y = NULL
    ) +
    # Theme
    theme_minimal() +
    theme(
      # panel.grid = element_blank(),
      panel.grid.minor.y = element_blank(),
      panel.grid.major.y = element_blank(),
      # panel.grid.minor.x = element_blank(),
      # panel.grid.major.x = element_blank(),
      axis.text.x = element_text(size = 11, face = "bold"), # Make labels stand out
      plot.title = element_text(size = 16, face = "bold", hjust = 0.5)
    )

  return(plt)
}



#' Plot the expression of a feature from a CircadianData object
#'
#' This function generates a ggplot2-based dot plot showing the expression
#' values of a selected feature across time points stored in a `CircadianData`
#' object. A fitted cosine wave can be drawn behind the points, and alternating
#' background shading can be added to indicate day/night or other cyclical
#' intervals.
#'
#' @param cd A `CircadianData` object.
#' @param feature Character or numeric. The name or row number of the feature to
#'   plot.
#' @param plot_type Character. Plot individual points ("points") or means plus
#'   standard deviations ("mean_sd").
#' @param add_wave Logical. If `TRUE`, adds the cosine fit curve.
#' @param point_size Numeric. Size of the points.
#' @param wave_linewidth Numeric. Line width of the cosine fit curve.
#' @param groups Character vector or `NULL`. Restrict the plot to the selected
#'   groups.
#' @param background_cutoffs Numeric vector or `NULL`. Switches for alternating
#'   background shading.
#' @param background_colors Character vector of length 1 or 2. Colors used for
#'   shading.
#' @param background_alpha Numeric. Alpha value of background colours.
#' @param errorbar_linewidth Numeric. Line width of error bars if `plot_type` is
#'   set to "mean_sd".
#' @param errorbar_width Numeric. Width of error bars if `plot_type` is set to
#'   "mean_sd".
#'
#' @return A ggplot object.
#'
#' @examples
#' \dontrun{
#'   plot_feature(cd, "Gene_01", background_cutoffs = c(12, 24))
#' }
#'
#' @import ggplot2
#' @importFrom dplyr n_distinct pull do rowwise filter
#' @importFrom tibble tibble
#'
#' @export
plot_feature <- function(cd,
                         feature,
                         plot_type = c("points", "mean_sd"),
                         add_wave = TRUE,
                         point_size = 3,
                         wave_linewidth = 1,
                         groups = NULL,
                         background_cutoffs = NULL,
                         background_colors = c("white", "grey50"),
                         background_alpha = 0.5,
                         errorbar_linewidth = 0.6,
                         errorbar_width = 0.4) {

  # --- 0. Extract Data ---
  mdata <- cd@metadata
  dat <- cd@dataset
  w_params <- cd@wave_params

  # --- 1. Input Validation ---
  # Validate time column
  # TODO: Probably can remove this part?
  if (!("time" %in% colnames(mdata))) {
    stop("Metadata must contain a 'time' column for plotting.")
  }

  # Validate feature argument
  if (!(is.character(feature) || is.numeric(feature)) || length(feature) != 1) {
    stop("'feature' must be a single character string or a single numeric value.")
  }
  if (is.character(feature)) {
    if (!(feature %in% rownames(cd))) {
      stop("Feature '", feature, "' not found in the dataset.")
    }
  } else {
    if (feature > nrow(cd)) {
      stop("Feature index (", feature, ") out of bounds.")
    }
    feature <- rownames(cd)[feature]
  }

  # Validate plot_type argument
  plot_type <- match.arg(plot_type)

  # Validate groups argument
  has_group_col <- "group" %in% colnames(mdata)
  if (!is.null(groups)) {
    if (!has_group_col) {
      stop("'groups' was provided, but metadata has no 'group' column.")
    }
    if (!is.character(groups)) {
      stop("'groups' must be a character vector.")
    }

    missing_groups <- setdiff(groups, unique(mdata$group))
    if (length(missing_groups) > 0) {
      stop("The following groups were not found in metadata: ",
           paste(missing_groups, collapse = ", "))
    }
  }

  # Validate background_colors
  if (!is.character(background_colors)) {
    stop("'background_colors' must be a character vector.")
  }
  if (length(background_colors) == 1) {
    background_colors <- c("white", background_colors)
  }
  if (length(background_colors) != 2) {
    stop("'background_colors' must be length 2.")
  }

  # --- 2. Prepare Data for Plotting ---
  feature_values <- dat[feature, , drop = TRUE]

  df_plot <- data.frame(
    time  = mdata$time,
    value = feature_values
  )

  if (has_group_col) {
    df_plot$group <- mdata$group
  } else {
    df_plot$group <- factor("all_samples")
  }

  # Filter to group if requested
  if (!is.null(groups)) {
    df_plot <- df_plot[df_plot$group %in% groups, , drop = FALSE]
    if (nrow(df_plot) == 0) {
      stop("No data available for feature '",
           feature,
           "' in the selected groups.")
    }
  }

  multiple_groups <- dplyr::n_distinct(df_plot$group) > 1


  # Get mean and standard deviation
  if (plot_type == "mean_sd") {
    summary_df <- df_plot %>%
      dplyr::group_by(time, group) %>%
      dplyr::summarise(
        mean = mean(value),
        sd   = sd(value),
        .groups = "drop"
      )
  }

  # --- 3. Begin ggplot ---
  p <- ggplot() +
    theme_bw() +
    labs(title = paste0("Expression of ", feature))

  # --- 4. Background Shading ---
  if (!is.null(background_cutoffs)) {

    # x_min <- min(df_plot$time)
    # x_max <- max(df_plot$time)
    x_min <- -Inf
    x_max <- Inf

    cuts <- sort(unique(background_cutoffs))
    boundaries <- c(x_min, cuts, x_max)

    bg_df <- data.frame(
      xmin = boundaries[-length(boundaries)],
      xmax = boundaries[-1],
      fill_bg = rep(background_colors, length.out = length(boundaries) - 1)
    )

    # Use fill as a plain argument
    p <- p +
      geom_rect(
        data = bg_df,
        aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf),
        inherit.aes = FALSE,
        alpha = background_alpha,
        fill = bg_df$fill_bg
      )
  }

  # --- 5. Add Cosine Wave (Behind Points) ---
  if (add_wave && nrow(w_params) > 0) {
    wp <- w_params %>%
      dplyr::filter(feature == !!feature)

    if (!is.null(groups)) {
      wp <- wp %>% dplyr::filter(group %in% groups)
    }

    if (has_group_col == FALSE) {
      wp$group <- factor("all_samples")
    }

    if (nrow(wp) > 0) {
      time_grid <- seq(min(df_plot$time), max(df_plot$time), length.out = 400)

      wave_df <- wp %>%
        rowwise() %>%
        do({
          tibble(
            time  = time_grid,
            value = .$mesor_estimate +
              .$amplitude_estimate *
              cos(2 * pi * (time_grid - .$phase_estimate) / .$period),
            group = .$group
          )
        }) %>%
        ungroup()

      p <- p +
        geom_line(
          data = wave_df,
          aes(x = time, y = value, colour = group),
          linewidth = wave_linewidth
        )

      if (!multiple_groups) {
        p <- p + guides(colour = "none")
      }
    }
  }

  # --- 6. Add Points ---
  if (plot_type == "points") {
    # Plot individual points
    p <- p +
      geom_point(
        data = df_plot,
        aes(x = time, y = value, fill = group),
        shape = 21, size = point_size, colour = "black"
      )

  } else if (plot_type == "mean_sd") {
    # Error bars
    p <- p +
      geom_errorbar(
        data = summary_df,
        aes(x = time, ymin = mean - sd, ymax = mean + sd),
        width = errorbar_width,
        linewidth = errorbar_linewidth
      )

    # Mean points
    p <- p +
      geom_point(
        data = summary_df,
        aes(x = time, y = mean, fill = group),
        shape = 21,
        size = point_size,
        colour = "black"
      )
  }

  p <- p + scale_fill_discrete(name = "group")

  if (!multiple_groups) {
    p <- p + guides(fill = "none")
  }

  return(p)
}




# TODO: Remove this base R plotting function at some point
#' Plot Feature Values Over Time
#'
#' Creates a scatterplot of the values for a specific feature against time. If a
#' 'group' column exists in the metadata, points are colored by group, and the
#' plot can be optionally filtered to a single group.
#'
#' @param x A \code{CircadianData} object.
#' @param feature A single character string specifying the name or a single
#'   numeric value specifying the row number of the feature (from
#'   `rownames(dataset(x))`) to plot.
#' @param group An optional single character string specifying a group to filter
#'   by. This is only applicable if a 'group' column exists in the metadata. If
#'   `NULL` (the default) and a 'group' column exists, data from all groups are
#'   plotted with different colors.
#' @param add_wave An optional logical value (TRUE/FALSE) specifying whether a
#'   sine wave fitted by harmonic regression should be plotted on top of the
#'   data. Only applies when `wave_params` has been filled by running
#'   `estimate_wave_params()` or `clockworks()` on `x`.
#' @param ... Additional arguments passed to the base `plot()` function.
#'
#'
#' @rdname plot_feature_base
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
#' cd_obj_grouped <- CircadianData(
#'   dataset = counts,
#'   metadata = meta,
#'   colname_sample = "rownames",
#'   colname_time = "time",
#'   colname_group = "group"
#' )
#' cd_obj_grouped <- add_experiment_info(cd_obj_grouped)
#'
#' # Plot a feature for all groups (colored by group)
#' plot_feature_base(cd_obj_grouped, feature = "Feature3")
#'
#' # Plot a feature for only the "Treated" group
#' plot_feature_base(cd_obj_grouped, feature = "Feature3", group = "Treated")
#'
#' # --- Without a 'group' column in metadata ---
#' cd_obj_no_group <- CircadianData(
#'   dataset = counts,
#'   metadata = meta,
#'   colname_sample = "rownames",
#'   colname_time = "time"
#' )
#' cd_obj_no_group <- add_experiment_info(cd_obj_no_group)
#'
#' # Plot a feature (all points will have the same color)
#' plot_feature_base(cd_obj_no_group,
#'              feature = "Feature5",
#'              main = "Feature 5 (No Groups Defined)")
#'
setGeneric("plot_feature_base", function(x, feature, group = NULL, add_wave = TRUE, ...) standardGeneric("plot_feature_base"))

#' @rdname plot_feature_base
setMethod("plot_feature_base", "CircadianData",
          function(x, feature, group = NULL, add_wave = TRUE, ...) {

            # --- 1. Input Validation ---
            mdata <- metadata(x)
            if (!("time" %in% colnames(mdata))) {
              stop("Metadata must contain a 'time' column for plotting.")
            }
            if (!(is.character(feature) || is.numeric(feature)) || length(feature) != 1) {
              stop("'feature' must be a single character string or a single numeric value.")
            }

            if (is.character(feature)) {
              if (!(feature %in% rownames(dataset(x)))) {
                stop("Feature '", feature, "' not found in the dataset.")
              }
            } else if (is.numeric(feature)) {
              if (feature > nrow(x)) {
                stop("Feature position (", feature, ") out of bounds.")
              }
              # Get feature name so logic downstream is the same
              feature <- rownames(x)[feature]
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

            if (show_legend == TRUE) {
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

            # --- 4. Create Plot ---
            do.call("plot", final_args)

            # --- 5. Add Fitted Wave ---
            w_params <- wave_params(x)

            if (add_wave == TRUE && ncol(w_params) > 0) {
              # Get wave parameters just for this feature
              w_params <- w_params[w_params$feature == feature, ]

              if (!is.null(group)) {
                w_params <- w_params[w_params$group == group, ]
              }

              # `show_legend` tells us whether we have multiple groups to plot
              if (show_legend == TRUE) {
                # One wave per group
                for (grp in unique_groups) {
                  plot_df_grp <- plot_df[plot_df$group == grp, ]
                  w_params_grp <- w_params[w_params$group == grp, ]

                  # Generate points
                  wave_time <- seq(min(plot_df_grp$time), max(plot_df_grp$time), length.out = 100)

                  # Reconstruct sine wave from harmonic regression results
                  per <- w_params_grp$period
                  phase <- w_params_grp$phase_estimate
                  mesor <- w_params_grp$mesor_estimate
                  amp <- w_params_grp$amplitude_estimate

                  wave_vals <- mesor + amp * cos((2*pi/per) * (wave_time - phase))

                  # Add the line to the plot
                  graphics::lines(wave_time, wave_vals, col = bgs[[grp]])
                }

              } else {
                # One wave
                # Generate points
                wave_time <- seq(min(plot_df$time), max(plot_df$time), length.out = 100)

                # Reconstruct sine wave from harmonic regression results
                per <- w_params$period
                phase <- w_params$phase_estimate
                mesor <- w_params$mesor_estimate
                amp <- w_params$amplitude_estimate

                wave_vals <- mesor + amp * cos((2*pi/per) * (wave_time - phase))

                # Add the line to the plot
                graphics::lines(wave_time, wave_vals, col = final_args$bg)
              }

              # Plot points again so they are on top of the lines
              par(new = TRUE)
              do.call("plot", final_args)
            }

            # --- 6. Add a Legend (if needed) ---
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
  #
}
