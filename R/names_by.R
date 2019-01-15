# -----------------------------------------------------------------------------#
#' Get names of 'by' output
#' 
#' To the usual names of \code{\link{by}}-class as a list, the \var{INDICES} are
#' added to make the names clearer. This uses a fairly large part of the code of
#' \code{\link{print.by}} with some modification.
#' 
#' @param x an \code{by} object
#' 
#' @return a character vector with names
#' 
#' @author Mun-Gwan Hong, \email{mun-gwan.hong@scilifelab.se}
#' @seealso \code{\link{by}}, \code{\link{names}}
#' @export
# -----------------------------------------------------------------------------#
# 'names.by' couldn't be used, because it changes the output of 'by' function.
#
# created  : 2017-08-30 by Mun-Gwan
# modified : 
# -----------------------------------------------------------------------------#
names_by <- function(x) {
  d <- dim(x)
  dn <- dimnames(x)
  dnn <- names(dn)
  out <- sapply(seq_along(x), FUN= function(i) {
    ii <- i - 1L
    out1 <- list()
    for (j in seq_along(dn)) {
      iii <- ii%%d[j] + 1L
      ii <- ii%/%d[j]
      out1 <- c(out1, paste0(dnn[j], ": ", dn[[j]][iii]))
    }
    do.call("paste", c(out1, list(sep= ", ")))
  })
  unlist(out)
}
