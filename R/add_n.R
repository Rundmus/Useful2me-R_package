# ------------------------------------------------------------------------------
#' add number of cases
#' 
#' Add number of cases in box plot
#'
#' @param y.fun function that calculate position in y-axis
#' @param vjust,position,na.rm refer to \code{\link{stat_summary}}
#'
#' @references \url{https://stackoverflow.com/questions/28846348/add-number-of-observations-per-group-in-ggplot2-boxplot}
#'
#' @examples
#' tibble(x = gl(2, 50), y= rnorm(100)) %>% 
#'   ggplot(aes(x, y)) + 
#'   geom_boxplot() + 
#'   add_n()
#' 
#' @importFrom ggplot2 position_dodge stat_summary
#' @export
# -----------------------------------------------------------------------------#
add_n <- function(
  y.fun= median,
  vjust= -0.3,
  position= position_dodge(width= 0.75),
  na.rm= T
) {
  stat_summary(
    fun.data= function(x) c(y= y.fun(x), label= length(x)),
    geom= 'text',
    vjust= vjust,
    position= position,
    na.rm= na.rm
  )
}
