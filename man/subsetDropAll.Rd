% Generated by roxygen2 (4.1.1): do not edit by hand
% Please edit documentation in R/subsetDropAll.R
\name{subsetDropAll}
\alias{subsetDropAll}
\title{Drop unused levels after subset}
\usage{
subsetDropAll(x, ...)
}
\arguments{
\item{x}{Object to be subsetted}

\item{...}{further arguments to be passed to or from other methods}
}
\value{
An object similar to \code{x} containing just the selected
    rows and columns (for a data frame). Column factors that have
    one or more levels emptied after the subset, will have this
    levels droped off.
}
\description{
This function is designed to be used in place of
    \code{\link[base]{subset}} when you want to drop off all empty
    levels, of all factor columns in a data frame.
}
\details{
This function works exactly the same way as
    \code{\link[base]{subset}} but it will drop off all empty levels
    of all columns that are factors in \code{x}.
}
\author{
Fernando Mayer
}
\seealso{
\code{\link[base]{subset}}
}

