# -----------------------------------------------------------------------------#
#' Fit a model through
#' 
#' Fit a model changing the dependent variable through a given set of variables
#' and collect results
#'
#' @param dep_var_list the set of variables to be used as the dependent variable
#' @param data a data frame
#' @param rhs right hand side of the formula used in the \code{fitmodel}
#' @param fitmodel a function for fitting a model. 
#' @param ... this will be delivered to the \code{fitmodel} function
#'
#' @return a tibble (or data frame)
#' @export
# -----------------------------------------------------------------------------#
# created  : 2020-01-10 by Mun-Gwan
# -----------------------------------------------------------------------------#
fit_model_thru <- function(dep_var_list, data, rhs, fitmodel, ...) {
  stopifnot(is.vector(dep_var_list),
            is.data.frame(data))
  
  names(data) <- make.names(names(data))
  dep_var_list_mn <- make.names(dep_var_list)
  stopifnot(all(dep_var_list_mn %in% names(data)))
  
  map_dfr(seq(dep_var_list), function(ii) {
    f <- formula( paste(dep_var_list_mn[ii], "~", rhs) )
    
    fM <- fitmodel(f, data, ...)
    
    if(!is.data.frame(fM) || nrow(fM) > 1) {
      fM <- tibble(res = list(fM))
    }
    fM %>% 
      mutate(Dep_var = dep_var_list[ii]) %>% 
      select(Dep_var, everything())
  })
}
