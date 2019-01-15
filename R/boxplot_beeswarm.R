# -----------------------------------------------------------------------------#
#' Box plot and Beeswarm
#' 
#' Box plot and individual data points added by 'beeswarm'
#' 
#' This display a box plot together with the number of elements of each group
#' and individual data points drawn by 'beeswarm'
#' 
#' @param formula,log same as those for \code{\link{boxplot}} and
#'   \code{\link{boxplot_nEle}}
#' @param ... arguments to be passed to \code{\link{boxplot_nEle}} and
#'   eventually to \code{\link{boxplot}}
#' 
#' @author Mun-Gwan Hong, \email{mun-gwan.hong@scilifelab.se}
#' @seealso \code{\link{boxplot}}, \code{\link{boxplot_nEle}}
#' 
#' @importFrom beeswarm beeswarm
#' @export
# -----------------------------------------------------------------------------#
#
# created  : 2016-10-31 by Mun-Gwan
# modified :
# -----------------------------------------------------------------------------#
boxplot_beeswarm <- function(formula, log= "", ...) {
  dots <- list(...)
  ylog <- grepl("y", log)
  if(ylog) {
    formula <- update(formula, log10(.) ~ .)
    log <- sub("y", "", log)
    if(!is.null(dots$ylim)) dots$ylim <- log10(dots$ylim)
  }
  
  do.call("boxplot_nEle", c(list(formula, test= "", log= log, outpch= NA, yaxt= "n"), dots))
  aT <- axisTicks(par("usr")[3:4], log= ylog)
  axis(2, at= if(ylog) log10(aT) else aT, labels= aT, las= 1)        
  beeswarm(formula, pch= 20, add= T)
  # stripchart(formula, vertical= T, method= "jitter", add= T, pch= 20)
}
