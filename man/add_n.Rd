% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/add_n.R
\name{add_n}
\alias{add_n}
\title{add number of cases}
\usage{
add_n(
  y.fun = median,
  vjust = -0.3,
  position = position_dodge(width = 0.75),
  na.rm = T
)
}
\arguments{
\item{y.fun}{function that calculate position in y-axis}

\item{vjust, position, na.rm}{refer to \code{\link{stat_summary}}}
}
\description{
Add number of cases in box plot
}
\examples{
tibble(x = gl(2, 50), y= rnorm(100)) \%>\% 
  ggplot(aes(x, y)) + 
  geom_boxplot() + 
  add_n()

}
\references{
\url{https://stackoverflow.com/questions/28846348/add-number-of-observations-per-group-in-ggplot2-boxplot}
}
