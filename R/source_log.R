# -----------------------------------------------------------------------------#
#' Source & log
#' 
#' Execute a script while writing a log to keep the history. The log file is
#' created at "../log" as a default. If it is not available, the file can be
#' found where the R script \var{file} is. Automatically, execution date is
#' added into the log file name. Loaded packages in the script are listed in the
#' log.
#' 
#' @param file the name of R file to be run 
#' @param echo same as 'echo' in \code{\link{source}}
#' @param path log output directory relative to the R script
#' 
#' @author Mun-Gwan Hong, \email{mungwan@gmail.com}
#' @seealso \code{\link{source}}
#' 
#' @export
#' @importFrom utils capture.output packageVersion
# -----------------------------------------------------------------------------#
#
# created  : 2012-04-17 by Mun-Gwan
# modified : 
#   2013-08-20 by Mun-Gwan : accept file
#   2015-05-29 by Mun-Gwan : write executing date, processing time
#   2016-10-28 by Mun-Gwan : Save versions
#   2017-08-08 by Mun-Gwan : add dates in the ourput file name
#   2018-05-21 by Mun-Gwan : 
#     1) fix path for the case 'wd' has been changed within R script
#     2) change the date in the log file name that fits to UNIX format
#   2020-05-06 by Mun-Gwan : name change to '.source'
#   2020-11-12 by Mun-Gwan : include the packages in which a function was 
#     directly accessed.
# -----------------------------------------------------------------------------#

.source <- function(file, echo= F, path = "../logs") {
  Rscript <- readLines(file)
  
  fDir <- dirname(file)
  # make it absolute file path
  if(!grepl("^/", fDir)) fDir <- file.path(getwd(), fDir)
  
  logDir <- file.path(fDir, path)    # log output directory
  if(! file.exists(logDir)) logDir <- fDir
  
  # Find the name of log file
  fn <- basename(file) %>% 
    # Remove leading digit followed by character. Such prefix is often used to
    # indicate the order of multiple scripts. But, the order is frequently
    # changed.
    sub("^[[:digit:]][[:alpha:]]?_", "", .) %>% 
    sub("(.*)\\.R$", "\\1", .) %>% 	             # remove the extension ".R"
    paste0(., "--", format(Sys.Date(), "%Y%m%d"), ".log") %>%  # add date and ".log"
    file.path(logDir, .)
  
  loadedPackages <- 
    c(
      #  Find the version of loaded packages by 'library' or 'require'
      Rscript[grep("^[[:blank:]]*(library|require)\\(\\w*\\)", Rscript)] %>% 
        sub("^[[:blank:]]*(library|require)\\((\\w*)\\).*$", "\\2", .),
      #  Find the version of loaded packages by function direct access
      Rscript[grep("[\\:]{2,3}[[:alpha:]\\._]", Rscript)] %>% 
        strsplit("[\\:]{2,3}") %>%      #  multiple occasions of ::
        { 
          if(length(.) == 0) NULL 
          else {
            .[[1]][ -length(.[[1]]) ] %>%     #   remove the last text
              paste(" ", .) %>%       # for the case no character ahead of package name
              sub("^.*[[:punct:] ]([[:alnum:]]+)", "\\1", .)
          }
        }
    ) %>% 
    unique()
  
  #  package versions
  loadedPackages <- loadedPackages %>% 
    sapply(., function(ea) paste0(ea, "-", utils::packageVersion(ea)))

  
  ##  processing time
  ptm <- proc.time()
  utils::capture.output(
    cat("Date :", as.character(Sys.time()), "\n*", R.version.string, "\n"),
    if (length(loadedPackages) == 0)
      cat("\n")
    else
      cat("*", paste(loadedPackages, sep = ", "), "\n\n"),
    source(file, echo = echo),
    cat("\n"),
    print(proc.time() - ptm),
    file = fn
  )
  
  if(!echo) {
    cat("\n\n", rep("=", 45), " R script ", rep("=", 45), "\n\n", sep="", 
        file= fn, append= TRUE)
    cat(Rscript, sep="\n", file= fn, append= TRUE)
  }
  
  return(invisible(NULL))
}
