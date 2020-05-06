# -----------------------------------------------------------------------------#
#' Unwhich
#' 
#' Opposite of \code{\link{which}}. Convert to numeric index into a logical
#' vector
#' 
#' @param x a vector of numeric indicies which elements will be marked as TRUE
#' @param length the length of output vector
#' 
#' @return a logical vector
#' @examples
#' unwhich(c(5, 10), 10)
#' 
#' @author Mun-Gwan Hong, \email{mungwan@gmail.com}
#' @export
# -----------------------------------------------------------------------------#
# created  : 2017-09-15 by Mun-Gwan
# -----------------------------------------------------------------------------#

unwhich <- function(x, length) {
  if(is.numeric(x)) x <- as.integer(x)
  stopifnot(is.integer(x),
            max(x) <= length)
  out <- logical(length= length)
  out[x] <- TRUE
  out
}
