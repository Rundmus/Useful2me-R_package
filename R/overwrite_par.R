# -----------------------------------------------------------------------------#
#' Overwrite parameters
#' 
#' Over-write parameter list with new list
#' 
#' @param prev_par a \code{list} of parameters on which \var{new_par} will 
#'   overwrite.
#' @param new_par a \code{list} of new parameters
#' @param excl the parameter name that will be ignored from the \var{new_par}
#'
#' @return a \code{list} of parameters after overwriting
#'
#' @examples
#' prev_par <- list(x = 1:10, y= 10:1, col= "blue", cex= 0.5)
#' new_par <- list(col= "green", main= "Hello")
#' overwrite_par(prev_par, new_par)
#' 
#' @author Mun-Gwan Hong, \email{mungwan@gmail.com}
#' @export
# -----------------------------------------------------------------------------#
# created  : 2011-09-13 by Mun-Gwan
# modified : 
# -----------------------------------------------------------------------------#

overwrite_par <- function(prev_par, new_par, excl = c()) {
  stopifnot(is.list(prev_par))
  if(missing(new_par) || is.null(new_par)) return(prev_par)
  stopifnot(is.list(new_par))
  if(length(new_par) == 0) return(prev_par)
  
  ## excluding from new parameter list of which name is included in 'excl'
  iExcl <- match(excl, names(new_par), nomatch = 0)
  if(any(iExcl != 0)) new_par <- new_par[ -iExcl ]
  
  # any parameter whose is same as previous one
  same_name_par <- match(names(new_par), names(prev_par), nomatch = 0)
  
  # if nothing is common in both parameter lists
  if(all(same_name_par == 0)) {
    # return combined list
    c(new_par, prev_par)
  } else {					# if some elements are overlapped
    # return overwriting onto previous list
    c(new_par, prev_par[ - same_name_par ])
  }
}
