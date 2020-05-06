# -----------------------------------------------------------------------------#
#' Cap 1st Character
#' 
#' Capitalize the first character only
#' 
#' @param x a string vector
#' @return a string vector
#' 
#' @author Mun-Gwan Hong, \email{mungwan@gmail.com}
#' @export
# -----------------------------------------------------------------------------#
# created  : 2016-10-18 by Mun-Gwan
# -----------------------------------------------------------------------------#

toupper_1st_char <- function(x) {
  stopifnot(is.character(x))
  paste0(toupper(substr(x, 1, 1)), substring(x, 2))
}



# -----------------------------------------------------------------------------#
#' Reverse string
#' 
#' Reverse the string. A copy from an example of \code{\link{strsplit}}
#' 
#' @param x a string vector
#' @return a string vector after reversing the order of characters in each
#'   string
#' 
#' @examples 
#' str_rev(c("Sample1", "Sample2"))
#' 
#' @author Mun-Gwan Hong, \email{mungwan@gmail.com}
#' @export
# -----------------------------------------------------------------------------#
# created  : 2017-09-12 by Mun-Gwan
# -----------------------------------------------------------------------------#

str_rev <- function(x) {
  stopifnot(is.character(x))
  strsplit(x, NULL) %>% 
    lapply(., rev) %>% 
    sapply(., paste, collapse = "")  
}



# -----------------------------------------------------------------------------#
#' Concatenate with skip
#' 
#' Concatenate string with "," skipping the overlapped part at the beginning
#' e.g. "Sample1", "Sample2", & "Sample13" -> "Sample1,2,13"
#' 
#' @param x a string vector
#' @param sep separating character
#' @param na.rm if NA is removed
#' @return a string
#' 
#' @examples 
#' concatenate_skip_overlap(c("Sample1", "Sample2", "Sample13"))
#' 
#' @author Mun-Gwan Hong, \email{mungwan@gmail.com}
#' @export
# -----------------------------------------------------------------------------#
#
# created  : 2016-10-19 by Mun-Gwan
# -----------------------------------------------------------------------------#

concatenate_skip_overlap <- function(x,
                                     sep = ",",
                                     na.rm = TRUE) {
  if(na.rm) {
    x <- x[!is.na(x)]
    if(length(x) == 0) return(NA)
  } else {
    x[is.na(x)] <- "NA"
  }
  
  if(all(x == x[1])) return(x[1])
  stopifnot(is.character(x))
  
  sptStr <- strsplit(x, NULL)
  short_str <- which.min(sapply(sptStr, length)) %>% 
    sptStr[[.]]
  
  ## find overlapped letters
  for(ii in seq_along(short_str)) {   # for each letter
    tmp <- sapply(sptStr, function(ex) ex[ii] != short_str[ii])
    if(any(tmp)) break
  }
  prefix <- (if(ii > 1) short_str[1:(ii - 1)] else "") %>% paste(collapse= "")
  
  fixed_str <- lapply(sptStr, function(ea) {
    ea[ii:length(ea)] %>% paste(collapse= "")
  }) %>% 
    unlist()
  
  fixed_str[[1]] <- paste0(prefix, fixed_str[[1]])
  
  paste(fixed_str, collapse= sep)
}


# -----------------------------------------------------------------------------#
#' Combine multiple elements
#' 
#' Combine multiple elements taking the class into account
#' 
#' @param x a vector of a class
#' @param sep separating character. refer to
#'   \code{\link{concatenate_skip_overlap}}
#' @param numeric_FUN the function to handle numeric elements. The default is 
#'   \code{\link{mean}}
#' @param logical_FUN the function to aggregate logical elements. The default is
#'   \code{\link{all}}
#' @param na.rm if NA is removed
#' @return a vector of same class as \var{x}
#' 
#' @author Mun-Gwan Hong, \email{mungwan@gmail.com}
#' @export
# -----------------------------------------------------------------------------#
# required : concatenate_skip_overlap
# created  : 2016-10-19 by Mun-Gwan
# -----------------------------------------------------------------------------#

combine_multi_elements <- function(x,
                                   sep = ",",
                                   numeric_FUN = mean,
                                   logical_FUN = all,
                                   na.rm = T) {
  if(is.factor(x)) {
    as.character(x) %>% 
      concatenate_skip_overlap(sep= sep, na.rm= na.rm) %>% 
      factor()
  } else if(is.character(x)) {
    concatenate_skip_overlap(x, sep= sep, na.rm= na.rm)
  } else if(is.integer(x)) {
    if(all(x == x[1])) x[1] else numeric_FUN(x, na.rm= na.rm)
  } else if(is.numeric(x)) {
    numeric_FUN(x, na.rm= na.rm)
  } else if(is.logical(x)) {
    logical_FUN(x, na.rm= na.rm)
  } else NULL
}
