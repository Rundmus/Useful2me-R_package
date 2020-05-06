# -----------------------------------------------------------------------------#
#' Use .R template
#' 
#' Create .R file using a template that includes header lines
#' 
#' @param file The name of the generating .R file. The ".R" extension will be
#'   added if it is missing.
#' @param author The author of the script
#' @param title The title of the generating R script, which is displayed at the
#'   top
#' @param date The date written as the creation date
#' 
#' @examples
#' \dontrun{
#' use.R_template("process_data") 
#' }
#' 
#' @author Mun-Gwan Hong, \email{mungwan@gmail.com}
#' @export
# -----------------------------------------------------------------------------#
# created  : 2017-03-02 by Mun-Gwan
# modified : 
# -----------------------------------------------------------------------------#

use.R_template <- function(file, 
                           author, 
                           title= sub("\\.R$", "", basename(file)), 
                           date= Sys.Date()
) {
  stopifnot(
    !missing(file), 
    !missing(author),
    is.character(title)
  )
  
  #  template_path = path to the template file
  template_path <- system.file("templates", "dot_R_template.R", 
                               package= "Useful2me", 
                               mustWork= TRUE)
  
  ##  Substitute the entities in the templates with the given values in this
  ##  function
  out_text <- readLines(template_path) %>% 
    gsub('\\{\\{\\{ title \\}\\}\\}', title, .) %>% 
    gsub('\\{\\{\\{ author \\}\\}\\}', author, .) %>% 
    gsub('\\{\\{\\{ date \\}\\}\\}', as.character(date), .) 

  ##  allow both with or without .R in 'file'
  base_fn <- sub("\\.R$", "", basename(file))
  path <- file.path(dirname(file), paste0(base_fn, ".R"))
  
  #  **  Write the text from the template after modification  **  #
  writeLines(out_text, path)
  
  message('# The file \"', paste0(base_fn, ".R"), '\" has been created.')
  
  invisible(TRUE)
}
