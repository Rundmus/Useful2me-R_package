# -----------------------------------------------------------------------------#
#' Plot - linear model
#' 
#' A scatter plot with linear regression results. In addition to a ordinary
#' scatter plot by \code{plot.formula}, a trend line estimated by linear
#' regression is displayed. The slope and significance are presented at the spot
#' of \code{res.pos}. Please note that this function accepts only one
#' independent variable in formula.
#' 
#' @param formula,data,subset same as those of \code{\link{plot.formula}} and
#'   \code{\link{lm}} generic functions
#' @param res.pos a text that indicates where the results of linear model is to
#'   be shown. The possible choices are \code{topleft}(default), \code{top},
#'   \code{topright}, \code{bottomleft}, \code{bottom}, \code{bottomright},
#'   \code{left}, \code{center}, \code{right}
#' @param res.inset the gap between the text of result and boundary. The value
#'   is the ratio of gap to character size
#' @param abline_par the parameters passed to \code{'abline'} for the trend line
#' @param lm_txt_par the parameters passed to \code{'text'} that display the
#'   coefficient and P-value from the linear regression model
#' @param ... arguments to be passed to \code{\link{plot}}
#'   
#' @return Same as output of the function of \code{\link{lm}}.
#'
#' @examples 
#' df <- data.frame(x= 1:20)
#' df$y <- 0.2 * df$x + rnorm(20)
#' plot_lm(y ~ x, df, subset= x != 15, col= rep("blue", 20))
#' 
#' plot_lm(y ~ x, df, subset= x != 15, 
#'         lm_txt_par= list(label= coef(summary(lm(y ~ x, df)))[2, 4]))
#'   
#' @author Mun-Gwan Hong, \email{mun-gwan.hong@scilifelab.se}
#' @seealso \code{\link{plot}}, \code{\link{lm}}
#' @include overwrite_par.R
#' @export
# -----------------------------------------------------------------------------#
# created  : 2014-03-18 by Mun-Gwan
# modified : 
#   2017-08-08 by Mun-Gwan : allow to change the text shown as the results of lm
# -----------------------------------------------------------------------------#

plot_lm <- function(formula,
                    data,
                    ...,
                    subset,
                    res.pos = "topleft",
                    res.inset = 0.5,
                    abline_par = list(col = "cadetblue"),
                    lm_txt_par = list()) {
  
  sub_subset <- substitute(subset)
  
  if(missing(data)) {
    eval(bquote(plot(formula, ..., subset= .(sub_subset))))
    data <- parent.frame()
  } else {
    eval(bquote(plot(formula, data, ..., subset= .(sub_subset))))
  }
  
  # to avoid an error when "response" was a "Date" object
  formula <- update(formula, as.numeric(.) ~ .) 
  
  if(par("xlog")) formula <- update(formula, . ~ log10(.))
  if(par("ylog")) formula <- update(formula, log10(.) ~ .)
  
  ##  Linear regression
  fM <- eval(bquote(lm(formula, data, subset= .(sub_subset))))
  do.call("abline", c(list(reg = fM), abline_par) )
  
  pos <- .pos_given_by_text(txt= res.pos, inset= res.inset)
  est <- list(
    beta= signif(coef(fM)[2], 2), 
    P= signif(coef(summary(fM))[2, "Pr(>|t|)"], 3)
  )
  overwrite_par(
    list(x= pos$x, 
         y= pos$y, 
         adj= pos$adj, 
         col = abline_par$col,
         label= substitute(expression(paste0(beta, "=", estBeta, ", P=", estP)), 
                           list(estBeta= est$beta, estP= est$P))
    ), 
    lm_txt_par
  ) %>% 
    do.call("text", .)
  
  return(fM)
}
