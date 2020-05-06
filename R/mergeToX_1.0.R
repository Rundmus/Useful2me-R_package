# -----------------------------------------------------------------------------#
#' merge x and y, leaving x intact
#' 
#' @description 
#' merge two data frames, \code{x} and \code{y}, like \code{merge}, but \code{x}
#' is left intact
#' 
#' @param x,y data frames to be merged, in which 'x' will not be changed at all 
#' @param by names of the columns in which the values link two data frames. When 
#' row names is used to match two tables, the function \code{rownames} 
#' can be given here.
#' @param by.x,by.y linking column names in 'x' and 'y' if the names are different
#' @param suffix.y the suffix for the columns in 'y' that have same name as 'x' 
#' excluding 'by.y'
#' 
#' @return A merged data frame 
#' 
#' @examples 
#' tg1 <- data.frame("id"= c("s1", "s2", "s3", "s5"), "height"= c(169, 187, 175, 162))
#' tg2 <- data.frame("id"= c("s1", "s2", "s4", "s5"), "weight"= c(76, 89, 60, 68))
#' mergeToX(tg1, tg2)
#' rownames(tg1)
#' mergeToX(tg1, tg2, by= rownames)
#' 
#' @author Mun-Gwan Hong, \email{mungwan@gmail.com}
#' @seealso \code{\link{merge}}
# -----------------------------------------------------------------------------#
#' @export
#' @importFrom tibble is_tibble
#
# created  : 2012-04-16 by Mun-Gwan
# modified : 
#   2012-08-22 by Mun-Gwan
#   2013-08-02 by Mun-Gwan : allow duplication in the colum of "by.x" in x 
#   2015-10-22 by Mun-Gwan : allow rownames for "by"
#   2017-12-13 by Mun-Gwan : return a 'tibble' object if "x" was a tibble
# -----------------------------------------------------------------------------#

mergeToX <- function(x, 
                     y, 
                     by= intersect(names(x), names(y)), 
                     by.x= by, 
                     by.y= by, 
                     suffix.y= ".y") {
  
	stopifnot( 
	  !missing(x), 
	  !missing(y),
	  is.data.frame(x),
	  is.data.frame(y)
	)

	if(length(by.x) > 1) {
		by.x <- by.x[1]
		warning("More than one column was given in \'by.x\'.", 
		        "Only the first column will be matched.")
	}
	if(length(by.y) > 1) {
		by.y <- by.y[1]
		warning("More than one column was given in \'by.y\'.", 
		        "Only the first column will be matched.")
	}
	
	## check any duplicated entry in the column of 'df1' after matching with 'df2'
	anyDuplicatedIn1 <- function(df1, df2) {
	  df1 <- as.character(df1)
	  df2 <- as.character(df2)
		match(df1, df2) %>% 
		{ .[ !is.na(.) ] } %>%    # skip any NAs 
		  anyDuplicated() %>% 
		  { . != 0 }
	}

	if(!identical(by, rownames)) {      # if 'by' is not the function "rownames"
		stopifnot( by.x %in% names(x) )
		stopifnot( by.y %in% names(y) )
		if(anyDuplicatedIn1(unlist(y[, by.y]), unlist(x[, by.x]))) {
		    stop("Duplicated entries in \'y[, by.y]\' is not allowed.")
		}
		
		i_by <- which(names(y) == by.y)
	}
	
	## add suffix for y
	iinx <- match(names(x), names(y))
	iinx <- iinx[!is.na(iinx)]
	names(y)[iinx] <- paste(names(y)[iinx], suffix.y, sep="")
	
	if(identical(by, rownames)) {
		out <- cbind(x, y[match(rownames(x), rownames(y)), ,drop= F]) 
	} else {
		out <- cbind(x, y[match(unlist(x[, by.x]), unlist(y[, i_by])), -c(i_by), drop= F])
		rownames(out) <- rownames(x)
	}
	
	if(tibble::is_tibble(x)) return(as_tibble(out))
	return(out)
}
