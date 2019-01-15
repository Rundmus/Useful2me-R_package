# -----------------------------------------------------------------------------#
#' Q-Q plot of P-values
#' 
#' Quantile-Quantile plot of observed P values vs. theorical P values in uniform
#' distribution
#' 
#' @param x a vector, matrix, or data frame of P-values 
#' @param main,cex,pch same as those for \code{'plot'} generic function (refer
#'   \code{\link{par}})
#' @param alpha a P-value threshold. Any hit in the vector or the first column
#'   given by \var{x} below this value will be highlighted by showing the ID of
#'   the hit. Note that this threshold will be used after multiple testing
#'   correction.
#' @param adjusted_p The P-values after multiple testing correction. This is
#'   valid only when \var{alpha} was given for the vector or the first column of
#'   \var{x}. If this \code{vector} is not given, the default is Bonferroni
#'   corrected values.
#' @param hit_txt_par a list of parameter for displaying hits
#' @param legend_par a list of parameter for a legend which will be presented
#'   only when multiple columns and the names of them are given in \var{x}.
#' @param ... arguments to be passed to \code{\link{qqplot}}
#' 
#' @return the vector of hit names
#' 
#' @author Mun-Gwan Hong, \email{mun-gwan.hong@scilifelab.se}
#' @seealso \code{\link{qqplot}}
#' @importFrom stats ppoints
#' @export
# -----------------------------------------------------------------------------#
# created  : 2011-09-13 by Mun-Gwan
# modified : 2013-06-20 by Mun-Gwan : show Top Hits' IDs
# -----------------------------------------------------------------------------#

qqplot_log10P <- function(x,
                          main = "Q-Q Plot of P-values",
                          alpha = NULL,
                          adjusted_p = NULL,
                          cex = 0.3,
                          pch = 1,
                          ...,
                          hit_txt_par = list(),
                          legend_par = list()) {
  
  mcD <- match.call(expand.dots = FALSE)$...
  
  # default parameters for plotting device
  opar <- list(mar = c(4, 4, 3, 0.5),
               mgp = c(2.5, 1, 0),
               col.lab = "darkblue") %>% 
    # Those parameters can be modified through ...
    overwrite_par(., mcD[names(mcD) %in% names(.)]) %>% 
    par()
  on.exit(par(opar))
  
  hasCol <- ! is.null( ncol(x) ) 	# if x has column (eg. not a single vector)
  # Y coordinate range : for multiple columns
  y_rg <- range(-log10(x), na.rm = TRUE)
  ##  x_1 = the first column
  x_1 <- if(hasCol) {
    unlist(x[,1]) %>% 
      `names<-`(rownames(x))
  } else x
  # default name is sequential number
  if(is.null(names(x_1))) names(x_1) <- seq_along(x_1)
  
  ## Initialize plot with empty area and the line of identity
  expected_p <- stats::ppoints( length(x_1) )
  qqplot_out <- qqplot(
    x = -log10(expected_p),
    y = -log10(x_1),
    main = main,
    cex = cex,
    pch = pch,
    ylim = y_rg,
    plot.it = T,
    type = "n",
    xlab = expression(paste("-log"[10], "(Expected P-values)")),
    ylab = expression(paste("-log"[10], "(Observed P-values)")),
    ...
  )
  abline(0, 1, col = "royalblue2")
  
  
  # ***   the plot for the first column or given vector   *** #
  #-----------------------------------------------------------#
  points(qqplot_out$x, qqplot_out$y, cex= cex, pch= pch)
  
  
  ###   show names of significant hits 
  #-----------------------------------
  
  if(!is.null(alpha)) {
    
    ##   adjusted_p = a vector of adjusted P-values
    if(is.null(adjusted_p)) {
      # default method for multiple testing problem is Bonferroni correction 
      adjusted_p <- p.adjust(x_1, method = "bonferroni")
    } else if(!is.null(ncol(adjusted_p))) {
      stop("Please, provide a vector of adjusted P-values ", 
           "for the first column in 'x'")
    } 
    
    ##   Hits
    if(length(adjusted_p) != length(x_1))
      stop("the length of 'adjusted_p' should be same as 'x' or 'x[,1]'.")
    
    hits <- x_1[!is.na(adjusted_p) & (adjusted_p < alpha)] %>% 
      sort(.)
    n_hits <- length(hits)
    if(n_hits > 0) {
      hPar <- list(x = -log10(expected_p)[1:n_hits],        # already sorted
                   y = -log10(hits),
                   labels = names(hits),
                   adj = 1.2)
      do.call("text", overwrite_par(hPar, hit_txt_par))
    }
  }
  
  ##   Multiple columns   ##
  #------------------------#
  
  if( hasCol && ncol(x) > 1 ) {				# if 'x' has more than one column
    for( j in 2:ncol(x) ) {
      qq <- qqplot(qqplot_out$x, -log10(unlist(x[,j])), plot.it = FALSE)
      points(qqplot_out$x, qq$y, col = j, cex = cex, pch= pch)
    }
    if(!is.null(colnames(x))) {
      d_par <- list(x = "bottomright",
                    legend = colnames(x),
                    pt.cex = cex,
                    pch = pch,
                    col = 1:ncol(x)
      )
      do.call("legend", overwrite_par(d_par, legend_par))
    }
  }
  
  if(!missing(alpha)) return(invisible(names(hits))) else return(NULL)
}
