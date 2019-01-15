# -----------------------------------------------------------------------------#
#' Use .R template
#' 
#' Create .R file using a template that includes header lines
#' 
#' @param file The name of the generating .R file. The ".R" extension will be
#'   added if it is missing.
#' @param title The title of the generating R script, which is displayed at the
#'   top
#' @param author The author of the script
#' @param date The date written as the creation date
#' @param wddir The directory on which the R script should be run.
#' 
#' @examples
#' \dontrun{
#' use.R_template("process_data") 
#' }
#' 
#' @author Mun-Gwan Hong, \email{mun-gwan.hong@scilifelab.se}
#' @importFrom whisker whisker.render
#' @export
# -----------------------------------------------------------------------------#
# created  : 2017-03-02 by Mun-Gwan
# modified : 
# -----------------------------------------------------------------------------#

use.R_template <- function(file, 
                           title= sub("\\.R$", "", basename(file)), 
                           author= "Mun-Gwan", 
                           date= Sys.Date(), 
                           wddir= getwd()
) {
  #  template_path = path to the template file
  template_path <- system.file("templates", "dot_R_template.R", 
                               package= "Useful4me", 
                               mustWork= TRUE)
  
  #  abbreviate user home directory to "~"
  wddir <- sub(paste0("^", path.expand("~")), "~", wddir)
  
  ##  Substitute the entities in the templates with the given values in this
  ##  function
  data_in <- list(title= title, author= author, date= date, wddir= wddir)
  out_text <- whisker::whisker.render(readLines(template_path), data= data_in)
  
  ##  allow both with or without .R in 'file'
  base_fn <- sub("\\.R$", "", basename(file))
  path <- file.path(dirname(file), paste0(base_fn, ".R"))
  
  #  **  Write the text from the template after modification  **  #
  writeLines(out_text, path)
  
  message('# The file \"', paste0(base_fn, ".R"), '\" has been created.')
  
  invisible(TRUE)
}
