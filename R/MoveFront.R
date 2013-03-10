#' Move a variable to the front of a data frame.
#'
#' \code{MoveFront} moves a specified variable to the front of a data frame. 
#'
#' @param data a data frame object
#' @param Var the variable you would like to move to the front of the data frame.
#'
#' @examples 
#' # Create dummy data
#' A <- B <- C <- 1:50
#' OldOrder <- data.frame(A, B, C)
#' 
#' # Move C to front
#' NewOrder <- MoveFront(OldOrder, "C")
#' names(NewOrder)
#'
#' @source Based on a Stack Overflow answer written by rcs: <http://stackoverflow.com/questions/3369959/moving-columns-within-a-data-frame-without-retyping>
#'
#' @export

MoveFront <- function(data, Var)
{
	col_idx <- grep(Var, names(data))
	MovedData <- data[, c(col_idx, (1:ncol(data))[-col_idx])]
	MovedData
}