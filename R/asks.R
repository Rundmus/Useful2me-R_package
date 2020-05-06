# -----------------------------------------------------------------------------#
#' Ask first and save
#' 
#' It works just like \code{save}, but ask first to avoid any unwanted automatic 
#' action in a script. 
#' 
#' @param ...,list,file Please refer to the description in \code{\link{save}}
#' @param yes for running in a script. If 'yes' is TRUE, then 'FUN' will be 
#'   excuted without asking
#' @author Mun-Gwan Hong, \email{mungwan@gmail.com}
#' @seealso \code{\link{save}}
#' 
#' @export
# -----------------------------------------------------------------------------#
# created  : 2012-04-06 by Mun-Gwan
# modified : 
# -----------------------------------------------------------------------------#
ask_save <- function(...,
                     list = character(),
                     file = stop("'file' must be specified"),
                     yes = FALSE) {
  
  # variable names given in ... and list
  var_ns <- match.call(expand.dots = FALSE)$... %>% 
    as.character() %>% 
    c(., list)			# extend if list is given
  
  lth <- length(var_ns)
  if(lth == 0) stop("No variable")
  if(lth > 3) {   # for displayed output only
    var_ns[3:4] <- c("...", paste0(var_ns[lth], " (#", lth, ")"))
    var_ns <- var_ns[1:4]
  }
  
  answer <- if(yes) 'y' else {
    paste(var_ns, collapse=", ") %>% 
      paste("Save [", ., "] to", file, "(y/n)?") %>% 
      readline() %>% 
      tolower()
  }
  if(answer == 'y') {
    save(..., list= list, file= file)
    cat("[saved]\n")
  } else cat("[not saved]\n")
}

# -----------------------------------------------------------------------------#
#' Ask first and write a table
#' 
#' It works just like \code{write.table}, but ask first to avoid any unwanted
#' automatic action in a script.
#' 
#' @param x,file,quote,sep,na,row.names,col.names,... Please refer to the 
#'   description in \code{\link{write.table}}
#' @param sortBy the column by which the rows will be sorted.
#' @param yes for running in a script. If 'yes' is TRUE, then 'FUN' will be 
#'   excuted without asking
#' 
#' @author Mun-Gwan Hong, \email{mungwan@gmail.com}
#' @seealso \code{\link{write.table}}
#' 
#' @importFrom utils write.table
#' @export
# -----------------------------------------------------------------------------#
#
# created  : 2012-04-06 by Mun-Gwan
# modified : 
#   2015-05-22 by Mun-Gwan : add "sortBy" which is useful when subset was given
#     in "x"
# -----------------------------------------------------------------------------#

ask_write.table <- function(x,
                            file = stop("'file' must be specified"),
                            quote = FALSE,
                            sep = "\t",
                            na = "",
                            row.names = FALSE,
                            col.names = if(row.names == TRUE) NA else TRUE,
                            ...,
                            sortBy,
                            yes = FALSE) {

  answer <- if(yes) 'y' else {
    paste("Save [", deparse(substitute(x)), "] to", file, "(y/n)?") %>% 
      readline() %>% 
      tolower()
  }
  
  if(answer == 'y') {
    if(!missing(sortBy)) 
      x <- x[do.call("order", lapply(sortBy, . %>% x[[.]])), ]
    write.table(x, file= file, quote= quote, sep= sep, na= na, 
                row.names= row.names, col.names= col.names, ...)
    cat("[table written]\n")
  } else cat("[not saved]\n")
}



# -----------------------------------------------------------------------------#
#' Ask first and run
#' 
#' Ask first and run the \code{FUN} to avoid any unwanted automatic action in 
#' a script. 
#' 
#' @param what the text description of what \code{FUN} is for
#' @param FUN the script to be excuted
#' @param yes for running in a script. If 'yes' is TRUE, then 'FUN' will be 
#'   excuted without asking
#' @return if 'y' was given for the question, then output of \code{FUN}.
#'   Otherwise, \code{NULL} will be returned.
#'   
#' @author Mun-Gwan Hong, \email{mungwan@gmail.com}
#' @export
# -----------------------------------------------------------------------------#
# created  : 2012-04-25 by Mun-Gwan
# modified : 
# -----------------------------------------------------------------------------#
ask_do <- function(what, FUN, yes= FALSE) {
  stopifnot(!missing(FUN),
            !missing(what))
  
  answer <- if(yes) 'y' else {
    paste0("Do you want to ", what, "(y/n)?") %>% 
      readline() %>% 
      tolower()
  }
  if(answer == 'y') { 
    FUN <- match.fun(FUN)
    return((FUN)())
  } else {
    return(NULL)
  }
}
