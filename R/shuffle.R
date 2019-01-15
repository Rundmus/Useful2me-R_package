# -----------------------------------------------------------------------------#
#' Shuffle
#' 
#' Same as \code{\link{sample}} but it avoids the problem occured by a variable
#' size of \var{x}
#' 
#' This function is almost identical to \code{\link{sample}}. But, if 'x' was 
#' given with a single value, this returns the given value instead of the random
#' sampling of 1:(the value) of the \code{sample}.
#' 
#' @param x,size,replace,prob same as \code{\link{sample}}
#' @return a vector
#' @author Mun-Gwan Hong, \email{mun-gwan.hong@scilifelab.se}
#' @seealso \code{\link{sample}}
#' @export
# -----------------------------------------------------------------------------#
# created  : 2012-11-09 by Mun-Gwan
# modified : 
# -----------------------------------------------------------------------------#

shuffle <- function(x, size, replace = FALSE, prob = NULL) {
	if(length(x) == 1) {
		if(replace) {
			return(rep(x, size)) 
		} else {
			if(!missing(size) && size > 1) {
				stop("'size' shouldn't be larger than 1, ", 
				     "when 'replace' is FALSE and length of 'x' is one!")
			} else return(x)
		}
	}
	base::sample(x, size, replace, prob)
}
