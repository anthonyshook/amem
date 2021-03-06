% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{define_cv_folds}
\alias{define_cv_folds}
\title{Generate folds for cross-validation}
\usage{
define_cv_folds(num_rows, nfolds = 10, seed = as.integer(Sys.time()))
}
\arguments{
\item{num_rows}{Number of rows of target set}

\item{nfolds}{Number of folds to generate (Default = 10)}

\item{seed}{Set to stabilize random sampling (Default is \code{as.integer(Sys.time())})}
}
\description{
Generate folds for cross-validation
}
