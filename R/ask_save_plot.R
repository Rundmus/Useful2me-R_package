# -----------------------------------------------------------------------------#
#' Ask to save or to show plot
#' 
#' Ask interactively if the user wants to save plot into a file or show it on
#' screen
#' 
#' @param filename the name of file to save
#' @param ... arguments to be passed over to a picture generation function such
#'   as \code{\link{png}}, \code{\link{pdf}}. The function is determined by the 
#'   the extension of \code{filename}. Currently "png", "pdf", "tif", and "jpg"
#'   file format can be used.
#' @param FUN the function that make one or more plots
#' 
#' @author Mun-Gwan Hong, \email{mun-gwan.hong@scilifelab.se}
#' @seealso \code{\link{png}}, \code{\link{pdf}}, \code{\link{tiff}}, 
#'          \code{\link{jpeg}}
#' 
#' @import grDevices
#' @import graphics
#' @export
# -----------------------------------------------------------------------------#
# created  : 2012-04-13 by Mun-Gwan
# modified : 
#   2013-02-12 by Mun-Gwan : fix "title"
#   2013-04-08 by Mun-Gwan : change the default process from showing plot on
#     screen to doing nothing
#   2017-03-30 by Mun-Gwan
#     1) remove 'type', yes', and 'title' arguments
#     2) allow 'a' for the answer
# -----------------------------------------------------------------------------#

ask_save_plot <- function(filename= "Rplot.png", ..., FUN) {
  stopifnot(!missing(FUN))
  
  answer <- paste0("Save a plot to ", filename, 
                   "\n(y : save, s : show on screen, others : do nothing)?") %>% 
    readline() %>% 
    tolower()
  
  if(answer == "y") {
    save_plot(filename= filename, ..., FUN= FUN)
  } else if(answer == "s") {
    opar <- par(ask= TRUE)
    on.exit(par(opar))
    # is.function(plot(1:10)) # FALSE
    if(is.function(FUN)) FUN() else invisible(FUN)
  } else {
    NULL
  } %>% 
    invisible()
}


# -----------------------------------------------------------------------------#
#' @describeIn ask_save_plot Same as \code{ask_save_plot} but skip the asking. 
#'     This is a code for swift switch between \code{ask_save_plot} and 
#'     \code{save_plot}.
# -----------------------------------------------------------------------------#
#
# created  : 2017-04-03 by Mun-Gwan
# modified : 
# -----------------------------------------------------------------------------#
#' @export

save_plot <- function(filename= "Rplot.png", ..., FUN) {
  stopifnot(!missing(FUN),
            is.character(filename))
  type <- sub("^.*\\.", "", filename)
  
  switch(type, 
         "png"= png(filename, ...),
         "pdf"= pdf(filename, ...),
         "tif"=, "tiff"= tiff(filename, ...),
         "jpg"=, "jpeg"= jpeg(filename, ...),
         stop("The extension should be 'png', 'pdf', 'tif', or 'jpg'.")
  )
  on.exit(dev.off())
  
  out <- if(is.function(FUN)) FUN() else invisible(FUN)
  
  cat("[ saved ! ]    -", filename, "\n")
  return(invisible(out))
}

