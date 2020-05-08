# -----------------------------------------------------------------------------#
#' English and Capitalize 1
#'
#' Read number in English and capitalize the first letter
#' \code{english_} fix the problem of showing digits in Rmd.
#' 
#' @name english_cap1
#'
#' @param x an integer vector
#' @param Zero how to read 0
#'
#' @return a text
#' 
#' @importFrom english english
#' @export
#'
#' @examples
#' english_cap1(c(1, 2))
#' @rdname english_cap1
# -----------------------------------------------------------------------------#
english_cap1 <- function(x, Zero = 'No') {
  x <- english::english(x)
  paste0(
    toupper(substr(x, 1, 1)),
    substring(x, 2)
  ) %>% 
    replace(. == "Zero", Zero)
}


# -----------------------------------------------------------------------------#
#' @importFrom english english
#' @export
#'
#' @examples
#' english(c(1, 2))
#' @rdname english_cap1
# -----------------------------------------------------------------------------#

english <- function(x) {
  as.character(english::english(x))
}
