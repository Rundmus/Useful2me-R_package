# -----------------------------------------------------------------------------#
#' show correlation value
#' 
#' This show correlation value whose color and size of font reflect how high the
#' value are. This is designed to use with \code{\link{pairs}} as \cr 
#' \code{pairs(x, upper.panel= panel.cor)}
#' 
#' @param x,y,method same as those for \code{\link{cor}} 
#' @param digits how many digit of correlation values should be displayed.
#' @param cex.cor the magnification factor for correlation values
#' @param ... not used
#' 
#' @author Mun-Gwan Hong, \email{mun-gwan.hong@scilifelab.se}
#' @seealso \code{\link{pairs}}
#' @export
# -----------------------------------------------------------------------------#
# created  : 2017-03-08 by Mun-Gwan
# modified : 
# -----------------------------------------------------------------------------#

panel.cor <- function(x, y, digits= 2, cex.cor, method= "spearman", ...) {
  method <- match.arg(method, eval(formals(stats::cor)$method))
  
  opar <- par(usr = c(0, 1, 0, 1)); on.exit(par(opar))
  corr <- cor(x, y, use= "complete.obs", method= method)
  
  text_x <- c(0.5, 0, 1) %>% {if(par("xlog")) 10^. else .}
  text_y <- c(0.5, 0, 1) %>% {if(par("ylog")) 10^. else .}
  
  rect(text_x[2], text_y[2], text_x[3], text_y[3], 
       col= rainbow(201, end= 2/6)[101 - round(corr * 100)])
  
  if(missing(cex.cor)) cex.cor <- 1/strwidth("0.00")
  
  text(text_x[1], text_y[1], signif(corr, 2), cex= cex.cor * abs(corr))
}
