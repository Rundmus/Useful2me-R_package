use.template <- function(file, 
                         author,
                         extension,
                         template_file
) {
  stopifnot(
    !missing(file), 
    !missing(author)
  )
  
  #  template_path = path to the template file
  template_path <- system.file(
    "templates", template_file, 
    package= "Useful2me", 
    mustWork= TRUE
  )
  
  ext_pattern <- paste0("\\", extension, "$")
  title <- sub(ext_pattern, "", basename(file)) 
  date <- Sys.Date()
  
  ##  Substitute the entities in the templates with the given values in this
  ##  function
  out_text <- readLines(template_path) %>% 
    gsub('\\{\\{\\{ title \\}\\}\\}', title, .) %>% 
    gsub('\\{\\{\\{ author \\}\\}\\}', author, .) %>% 
    gsub('\\{\\{\\{ date \\}\\}\\}', as.character(date), .) 
  
  ##  allow both with or without extension in 'file'
  base_fn <- sub(ext_pattern, "", basename(file))
  path <- file.path(dirname(file), paste0(base_fn, extension))
  
  #  when the file already exists
  if(file.exists(path)) {
    answer <- readline(
      paste0('# The file \"', base_fn, extension, '\" already exists. Overwrite? ')
    )
    if(!tolower(answer) %in% c("y", "yes")) {
      message('# No change.')
      return(invisible(TRUE))
    }
  }

  #  **  Write the text from the template after modification  **  #
  writeLines(out_text, path)
  
  message('# The file \"', paste0(base_fn, extension), '\" has been created.')
  
  invisible(TRUE)
}



# -----------------------------------------------------------------------------#
#' Use .R[md] template
#' 
#' Create .R or .Rmd file using a template that includes header lines
#' 
#' @param file The name of the generating file. The ".R" or ".Rmd" extension
#'   will be added if it is missing.
#' @param author The author of the script
#' 
#' @examples
#' \dontrun{
#' use.R_template("process_data") 
#' }
#' 
#' @author Mun-Gwan Hong, \email{mungwan@gmail.com}
#' @export
#' 
#' @rdname use_template
# -----------------------------------------------------------------------------#
# created  : 2017-03-02 by Mun-Gwan
# modified : 
# -----------------------------------------------------------------------------#

use.R_template <- function(file, author) {
  use.template(
    file = file,
    author = author,
    extension = ".R",
    template_file= "dot_R_template.R"
  )
}



# -----------------------------------------------------------------------------#
#' @examples
#' \dontrun{
#' use.Rmd_template("process_data") 
#' }
#' 
#' @export
#' 
#' @rdname use_template
# -----------------------------------------------------------------------------#
# created  : 2020-05-08 by Mun-Gwan
# modified : 
# -----------------------------------------------------------------------------#

use.Rmd_template <- function(file, author) {
  use.template(
    file = file,
    author = author,
    extension = ".Rmd",
    template_file= "dot_Rmd_template.Rmd"
  )
}




