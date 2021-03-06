% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/use.R_template.R
\name{use.R_template}
\alias{use.R_template}
\alias{use.Rmd_template}
\title{Use .R[md] template}
\usage{
use.R_template(file, author)

use.Rmd_template(file, author)
}
\arguments{
\item{file}{The name of the generating file. The ".R" or ".Rmd" extension
will be added if it is missing.}

\item{author}{The author of the script}
}
\description{
Create .R or .Rmd file using a template that includes header lines
}
\examples{
\dontrun{
use.R_template("process_data") 
}

\dontrun{
use.Rmd_template("process_data") 
}

}
\author{
Mun-Gwan Hong, \email{mungwan@gmail.com}
}
