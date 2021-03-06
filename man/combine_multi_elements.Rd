% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/string_utils.R
\name{combine_multi_elements}
\alias{combine_multi_elements}
\title{Combine multiple elements}
\usage{
combine_multi_elements(
  x,
  sep = ",",
  numeric_FUN = mean,
  logical_FUN = all,
  na.rm = T
)
}
\arguments{
\item{x}{a vector of a class}

\item{sep}{separating character. refer to
\code{\link{concatenate_skip_overlap}}}

\item{numeric_FUN}{the function to handle numeric elements. The default is 
\code{\link{mean}}}

\item{logical_FUN}{the function to aggregate logical elements. The default is
\code{\link{all}}}

\item{na.rm}{if NA is removed}
}
\value{
a vector of same class as \var{x}
}
\description{
Combine multiple elements taking the class into account
}
\author{
Mun-Gwan Hong, \email{mungwan@gmail.com}
}
