#' Useful2me : A package of useful functions to me
#' 
#' This package includes functions that simplify works here and there 
#' 
#' @author Mun-Gwan Hong <\email{mungwan@gmail.com}>
#' @keywords package
#' 
#' @docType package
#' 
#' @name Useful2me-package
#' @import dplyr
#' @import ggplot2
#' 
NULL


.onAttach <- function(libname, pkgname){
  #  ref: https://stackoverflow.com/questions/41372146/test-interaction-with-users-in-r-package
  options(mypkg.connection = stdin())
}

# GLOBAL CONSTANTS
