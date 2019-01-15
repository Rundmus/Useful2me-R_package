# -----------------------------------------------------------------------------#
# A collection of utilities for customised plot
# -----------------------------------------------------------------------------#



# -----------------------------------------------------------------------------#
#' Get the positional coordinate from text
#' 
#' Find the coordinate of the position, given by a text, in a plot
#' 
#' @param txt given text indicating position, e.g. \code{'topleft'}
#' @param inset same as \code{inset} of \code{\link{par}}
#' 
#' @return a list having 3 elements, \code{x}, \code{y}, and \code{adj}. The
#'   \code{x} and \code{y} are x- and y-coordinates. The \code{adj} is same as
#'   the one for \code{\link{plot.default}}.
#' 
# -----------------------------------------------------------------------------#
# created  : 2014-03-18 by Mun-Gwan
# -----------------------------------------------------------------------------#

.pos_given_by_text <- function(txt, inset = 0.5) {
  stopifnot(is.character(txt))
  txt <- match.arg(txt, 
                   c("topleft", "top", "topright", 
                     "bottomright", "bottom", "bottomleft", 
                     "left", "right", "center"))
  
  ## copy and modify the code for "legend"
  inset <- rep_len(inset, 2)
  insetx <- inset[1L] * xinch(par("cin")[1L], warn.log= FALSE)
  usr <- par()$usr
  
  posX <- switch(
    txt, 
    bottomright = , topright = , right  = usr[2L] - insetx, 
    bottomleft =  , topleft =  , left   = usr[1L] + insetx, 
    bottom =      , top =      , center = (usr[1L] + usr[2L])/2
  )
  if(par("xlog")) posX <- 10^posX
  
  insety <- inset[2L] * yinch(par("cin")[2L], warn.log= FALSE) 
  
  posY <- switch(
    txt, 
    bottom = , bottomleft = , bottomright = usr[3L] + insety, 
    top = ,       topleft = ,    topright = usr[4L] - insety, 
    center = ,       left = ,       right = (usr[3L] + usr[4L])/2
  )
  if(par("ylog")) posY <- 10^posY
  
  adjX <- switch(
    txt, 
    bottomright = , topright = , right  = 1, 
    bottomleft =  , topleft =  , left   = 0, 
    bottom =      , top =      , center = 0.5
  )
  adjY <- switch(
    txt, 
    bottom = , bottomright = , bottomleft = 0, 
    top =    ,    topright = ,    topleft = 1, 
    center = ,       right = ,       left = 0.5
  )
  
  return(list(x = posX, y = posY, adj = c(adjX, adjY)))
}

