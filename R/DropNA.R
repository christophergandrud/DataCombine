#' Drop rows from a data frame with missing values on a given variable(s).
#'
#' \code{DropNA} drops rows from a data frame when they have missing (NA) values on a given variable(s).
#'
#' @param data a data frame object.
#' @param Var a character vector naming the variables you would like to have only non-missing (NA) values.
#'
#' @examples
#' # Create data frame
#' a <- c(1, 2, 3, 4, NA)
#' b <- c( 1, NA, 3, 4, 5) 
#' ABData <- data.frame(a, b)
#' 
#' # Remove missing values from column a
#' ASubData <- DropNA(ABData, Var = "a")
#' 
#' # Remove missing values in columns a and b
#' ABSubData <- DropNA(ABData, Var = c("a", "b"))
#' 
#' @source Partially based on Stack Overflow answer written by donshikin: http://stackoverflow.com/questions/4862178/remove-rows-with-nas-in-data-frame
#'
#' @export

DropNA <- function(data, Var)
{
	# Find term number
	DataNames <- names(data)
	TestExist <- Var %in% DataNames
	if (!all(TestExist)){
		stop("Variable(s) not found in the data frame.")
	}

	# Drop if NA
	if (length(Var) == 1){
		DataNoNA <- data[!is.na(data[, Var]), ]

		DataVar <- data[, Var]
		DataNA <- DataVar[is.na(DataVar)]
		TotalDropped <- length(DataNA)
	}
	else{
		RowNA <- apply(data[, Var], 1, function(x){any(is.na(x))})
		DataNoNA <- data[!RowNA, ]

		TotalDropped <- sum(RowNA)
	}	

	message(paste(TotalDropped, "rows dropped from the data frame." ))
	return(DataNoNA)
}
