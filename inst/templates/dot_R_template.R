# -----------------------------------------------------------------------------#
# {{{ title }}}
# -----------------------------------------------------------------------------#
# created  : {{{ date }}} by {{{ author }}}
# modified : 
# -----------------------------------------------------------------------------#
rm(list = ls())

library(dplyr)
# library(Useful2me)   # remotes::install_github("Rundmus/Useful2me-R_package")

#----- File names --------------------------------------------------------------

fn <- list(
  i = list(                               #  input
  ),
  o = list(                               #  output
  )
)

#  Check all exists
stopifnot(all(file.exists(c('.', unlist(fn$i)))), 
          all(file.exists(dirname(c('.', unlist(fn$o))))))

#----- MAIN --------------------------------------------------------------------
