# -----------------------------------------------------------------------------#
#' Lighten a color
#' 
#' Get a slightly brighter color
#' 
#' @param color a vector of color in any of the three available color types,
#'   such as a color name, \code{#rrggbb}, and an integer. refer to
#'   \code{\link{col2rgb}} in \code{\link{grDevices}} package.
#' @param extent the extent of lightening of color, where max value is 255.
#' @param preset previously selected conversion, which is given as a named
#'   vector. The default, \code{c(black= "transparent")}, converts black to
#'   transparent.
#' 
#' @return a vector of lightened color in RGB format
#' 
#' @examples 
#' lighten_color(c("green4", 8, 1, "black", "#6464FF", "blue"))
#' 
#' @author Mun-Gwan Hong, \email{mungwan@gmail.com}
#' @seealso \code{\link{rgb}}
#' @export
# -----------------------------------------------------------------------------#
# created  : 2017-02-07 by Mun-Gwan
# modified : 
# -----------------------------------------------------------------------------#

lighten_color <- function(color,
                          extent = 100,
                          preset = c(black= "transparent")) {
  
  stopifnot(is.numeric(extent),
            extent >= 0 & extent < 256)
  
  # rgb to hex
  rgb2hex <- function(r) {
    grDevices::rgb(r[1, ], r[2, ], r[3, ], maxColorValue= 255)
  }
  
  # matrix with 'red', 'green', and 'blue' in the rows
  oc_rgb <- grDevices::col2rgb(color)
  
  out <- oc_rgb %>% { 
    ifelse(. + extent > 255, 255, . + extent)		# lighten
  } %>% 
    rgb2hex()
  
  ##  'preset' 
  ##  Change color values to hex in order to accept different forms of color
  ##  coding.
  mcol <- match(rgb2hex(oc_rgb), 
              names(preset) %>% grDevices::col2rgb() %>% rgb2hex())
  if(any(ii <- !is.na(mcol))) {
    out[ii] <- preset[mcol][ii]
  }
  
  return(out)
}
