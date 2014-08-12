#' Move variables to the front of a data frame.
#'
#' \code{MoveFront} moves variables to the front of a data frame.
#'
#' @param data a data frame object containing the variable you want to move.
#' @param Var a character vector naming the variables you would like to move to
#' the front of the data frame. The order of the variables should match the
#' order you want them to have in the data frame, i.e. the first variable in the
#' vector will be the first variable in the data frame.
#' @param exact logical. If \code{TRUE} (the default), only exact variable names
#' are matched.
#' @param ignore.case logical. If \code{FALSE}, the variable name matching is
#' case sensitive and if \code{TRUE}, case is ignored during matching. Only
#' available when \code{exact = FALSE}.
#' @param fixed logical. If \code{TRUE}, pattern is a string to be matched as
#' is. Overrides all conflicting arguments. Only available when
#' \code{exact = FALSE}.
#'
#' @examples
#' # Create fake data
#' A <- B <- C <- 1:50
#' OldOrder <- data.frame(A, B, C)
#'
#' # Move C to front
#' NewOrder1 <- MoveFront(OldOrder, "C")
#' names(NewOrder1)
#'
#' # Move B and A to the front
#' NewOrder2 <- MoveFront(OldOrder, c("B", "A"))
#' names(NewOrder2)
#'
#' ## Non-exact matching (example from Felix Hass)
#'  # Create fake data
#'  df <- data.frame(dummy = c(1,0), Name = c("Angola", "Chad"),
#'                  DyadName = c("Government of Angola - UNITA",
#'                  "Government of Chad - FNT"),
#'                  Year = c("2002", "1992"))
#'
#'  df <- MoveFront(df, c("Name", "Year"), exact = FALSE)
#'
#'  names(df)
#'
#'  df <- MoveFront(df, c("Name", "Year"), exact = TRUE)
#'
#'  names(df)
#'
#' @source Based primarily on a Stack Overflow answer written by rcs: \url{http://stackoverflow.com/questions/3369959/moving-columns-within-a-data-frame-without-retyping}.
#'
#' @export

MoveFront <- function(data, Var, exact = TRUE, ignore.case = NULL, fixed = NULL)
{
    if (isTRUE(exact) & !is.null(ignore.case) | !is.null(fixed)){
        warning('When exact = TRUE ignore.case and fixed are ignored.')
    }
    OneMove <- function(data, Var){
        # Determine if Var exists in data
        DataNames <- names(data)
        TestExist <- Var %in% DataNames
        if (!isTRUE(TestExist)){
            stop(paste(Var, "was not found in the data frame."))
        }

        if (isTRUE(exact)){
            col_idx <- which(DataNames %in% Var, arr.ind = TRUE)
        }
        else if (!isTRUE(exact)){
            col_idx <- grep(Var, DataNames, ignore.case = ignore.case, fixed = fixed)
        }
        MovedData <- data[, c(col_idx, (1:ncol(data))[-col_idx])]
        return(MovedData)
    }

    RevVar <- rev(Var)

    for (i in RevVar){
        data <- OneMove(data, i)
    }
    return(data)
}
