#' Remove all objects from a workspace except those specified by the user.
#' 
#' \code{rmExcept} removes all objects from a workspace except those specified by the user.
#' 
#' @param keepers a character vector of the names of object you would like to keep in your workspace.
#' @param envir the \code{\link{environment}} to remove objects from. The default is the global environment (i.e. \code{\link{globalenv}}).
#' @param message logical, whether or not to return a message informing the user of which objects were removed.
#' 
#' @examples
#' # Create objects 
#' A <- 1; B <- 2; C <- 3
#' 
#' # Remove all objects except for A
#' rmExcept("A")
#' 
#' # Show workspace
#' ls()
#' 
#' @export

rmExcept <- function(keepers, envir = globalenv(), message = TRUE){
  DeleteObj <- setdiff(ls(envir = envir), keepers)
  rm(list = DeleteObj, envir = envir)
  if (isTRUE(message)){
    message(paste("Removed the following objects:"))
    message(paste(DeleteObj, collapse = ", "))
  }
} 


#' Subset a data frame if a specified pattern is found in a character string
#'
#' @param data data frame.
#' @param patterns character vector containing a regular expressions to be matched in the given character vector.
#' @param Var character vector of the variables that the pattern should be found in.
#' @param keep.found logical. whether or not to keep observations where the pattern is found (\code{TRUE}) or not found (\code{FALSE}).
#' @param useBytes logical. If TRUE the matching is done byte-by-byte rather than character-by-character. See \code{\link{grep}}.
#'
#' @examples
#' # Create data frame
#' a <- c("London, UK", "Oxford, UK", "Berlin, DE", "Hamburg, DE", "Oslo, NO")
#' b <- c(8, 0.1, 3, 2, 1)
#' ABData <- data.frame(a, b)
#'
#' # Keep only data from Germany (DE)
#' ABGermany <- grepl.sub(data = ABData, patterns = "DE", Var = "a")
#'
#'
#' @export
 
grepl.sub <- function(data, patterns, Var, keep.found = TRUE, useBytes = TRUE){
  data$y <- grepl(pattern = paste0(patterns, collapse="|"), x = data[, Var], useBytes = useBytes)
  subdata <- subset(data, y == keep.found)
  subdata$y <- NULL
  subdata
}