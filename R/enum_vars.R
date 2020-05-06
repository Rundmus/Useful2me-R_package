# -----------------------------------------------------------------------------#
#' Enumerate
#' 
#' Enumerate `x` in text
#'
#' @param x a vector
#' @param surround with which character each element will be surrounded
#' @param with_oxford_comma whether an oxford comma should be added or not
#'
#' @return a text
#' @export
#'
#' @examples
#' enum_vars(c(LETTERS[1:3]))
# -----------------------------------------------------------------------------#
enum_vars <- function(x,
                      surround = '`',
                      with_oxford_comma = FALSE) {
  if(length(x) <= 1) return(x)
  x <- paste0(surround, x, surround)
  i_last <- length(x)
  
  paste(
    c(paste(x[-i_last], collapse = ", "), x[i_last]),
    collapse= if(with_oxford_comma) ", and " else " and "
  )
}
