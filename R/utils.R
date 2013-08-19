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