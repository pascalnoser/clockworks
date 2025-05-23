% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/CircadianData.R
\name{[,CircadianData,ANY,ANY,ANY-method}
\alias{[,CircadianData,ANY,ANY,ANY-method}
\title{Subset a CircadianData Object}
\usage{
\S4method{[}{CircadianData,ANY,ANY,ANY}(x, i, j, ..., drop = FALSE)
}
\arguments{
\item{x}{A \code{CircadianData} object.}

\item{i}{Row indices or names (features).}

\item{j}{Column indices or names (samples).}

\item{...}{Additional arguments (not used).}

\item{drop}{Logical, currently ignored (always FALSE for dataset/metadata).}
}
\value{
A new, subsetted \code{CircadianData} object.
}
\description{
Subsets the object by features (rows) and/or samples (columns), ensuring
that the dataset and metadata remain synchronized. The experiment_info
slot is carried over unchanged.
}
\examples{
# Sample Data
counts <- matrix(rpois(100, lambda = 10), nrow = 10, ncol = 10,
                 dimnames = list(paste0("Feature", 1:10), paste0("Sample", 1:10)))
meta <- data.frame(
  row.names = paste0("Sample", 1:10),
  group = rep(c("Control", "Treated"), each = 5),
  time = rep(seq(0, 8, by = 2), 2)
)
cd_obj <- CircadianData(dataset = counts, metadata = meta,
                        experiment_info = list(type = "RNA-Seq"))

# Subset features (rows)
cd_obj_sub1 <- cd_obj[c("Feature1", "Feature5"), ]
dim(cd_obj_sub1) # 2 10

# Subset samples (columns) - metadata is subsetted automatically
cd_obj_sub2 <- cd_obj[ , c("Sample1", "Sample3", "Sample8")]
dim(cd_obj_sub2) # 10  3
print(metadata(cd_obj_sub2))
print(experiment_info(cd_obj_sub2)) # Carried over

# Subset both
cd_obj_sub3 <- cd_obj[1:3, c(TRUE, FALSE, TRUE, FALSE, TRUE, FALSE, TRUE, FALSE, TRUE, FALSE)]
dim(cd_obj_sub3) # 3 5
print(colnames(cd_obj_sub3))
print(rownames(metadata(cd_obj_sub3)))

}
