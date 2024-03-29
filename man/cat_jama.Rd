% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cat_jama.R
\name{cat_jama}
\alias{cat_jama}
\title{JAMA-style summary for a Categorical Row}
\usage{
cat_jama(dt, ...)
}
\arguments{
\item{dt}{the name of the dataframe object.}

\item{...}{Additional arguments supplied within the package row functions.}
}
\value{
A dataframe with summary statistics for a categorical variable.
}
\description{
Summarizes a categorical row using column percentages and the total number in each cell divided by the column total. This is the style used by the Journal of the American Medical Association.
}
\details{
This is an internal function of \code{tangram.pipe}. Additional arguments
should be supplied for this function to work properly.

\code{rowlabel} : the label for the table row name, if different from row_var.

\code{missing} : if TRUE, missing data is considered; FALSE only uses complete cases.

\code{ordering} : Sorts the row variable: options are "ascending" or "descending"

\code{sortvar} : Column to sort row on. Requires \code{ordering} to be \code{ascending} or \code{descending}. By default, will sort based on overall statistics.

\code{digits} : significant digits to use.
}
\seealso{
Additional prewritten summary functions for categorical data: \link[tangram.pipe]{cat_default}, \link[tangram.pipe]{cat_pct}, \link[tangram.pipe]{cat_count}
}
\keyword{tangram.pipe}
