% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/object_oriented.R
\name{print.groupr}
\alias{print.groupr}
\title{Printing groupr objects}
\usage{
\method{print}{groupr}(x, include_colnames = F, ...)
}
\arguments{
\item{x}{A grouping object created with \code{groupr}}

\item{include_colnames}{Boolean for whether or not to print colnames in tree}

\item{...}{Additional arguments}
}
\description{
Print a groupr object as tree.
}
\examples{
groupr <- groupr(
  permits,
  groups = c('type_desc', 'issued_month', 'existing_use')
)
print(groupr)
}
