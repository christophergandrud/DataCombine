#' Drop rows from a data frame with missing values on a given variable(s).
#'
#' \code{DropNA} drops rows from a data frame when they have missing (\code{NA})
#' values on a given variable(s).
#'
#' @param data a data frame object.
#' @param Var a character vector naming the variables you would like to have
#' only non-missing (\code{NA}) values. If not specified, then all \code{NA}s
#' will be dropped from the data frame.
#' @param message logical. Whether or not to give you a message about the number
#' of rows that are dropped.
#'
#' @examples
#' # Create data frame
#' a <- c(1:4, NA)
#' b <- c(1, NA, 3:5)
#' ABData <- data.frame(a, b)
#'
#' # Remove missing values from column a
#' ASubData <- DropNA(ABData, Var = "a", message = FALSE)
#'
#' # Remove missing values in columns a and b
#' ABSubData <- DropNA(ABData, Var = c("a", "b"))
#'
#' # Remove missing values in all columns of ABDatat
#' AllSubData <- DropNA(ABData)
#'
#' @source Partially based on Stack Overflow answer written by donshikin:
#' \url{http://stackoverflow.com/questions/4862178/remove-rows-with-nas-in-data-frame}
#'
#' @export

DropNA <- function(data, Var, message = TRUE)
{
    # Find term number
    DataNames <- names(data)
    if (missing(Var)) {
        if (isTRUE(message)) {
            message('No Var specified. Dropping all NAs from the data frame.\n')
        }
        Var <- names(data)
    }
    TestExist <- Var %in% DataNames
    if (!all(TestExist)){
        stop("Variable(s) not found in the data frame.", call. = FALSE)
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

  if (isTRUE(message)){
      message(paste(TotalDropped, "rows dropped from the data frame because of missing values." ))
  }
    return(DataNoNA)
}

#' Create new variable(s) indicating if there are missing values in other
#' variable(s)
#'
#' @param data a data frame object.
#' @param Var a character vector naming the variable(s) within which you would
#' like to identify missing values.
#' @param Stub a character string indicating the stub you would like to append
#' to the new variables' name(s).
#' @param reverse logical. If \code{reverse = FALSE} then missing values are
#' coded as \code{1} and non-missing values are coded as \code{0}. If
#' \code{reverse = TRUE} then missing values are coded as \code{0} and
#' non-missing values are coded as \code{1}.
#' @param message logical. Whether or not to give you a message about the names
#' of the new variables that are created.
#'
#' @examples
#' # Create data frame
#' a <- c(1, 2, 3, 4, NA)
#' b <- c( 1, NA, 3, 4, 5)
#' ABData <- data.frame(a, b)
#'
#' # Create varibles indicating missing values in columns a and b
#' ABData1 <- NaVar(ABData, Var = c('a', 'b'))
#'
#' # Create varible indicating missing values in columns a with reversed dummy
#' ABData2 <- NaVar(ABData, Var = 'a', reverse = TRUE, message = FALSE)
#'
#' @export

NaVar <- function(data, Var, Stub = 'Miss_', reverse = FALSE, message = TRUE){
    DataNames <- names(data)
    TestExist <- Var %in% DataNames
    if (!all(TestExist)){
        stop("Variable(s) not found in the data frame.", call. = FALSE)
    }

    MissNames <- vector()
    for (i in Var){
        data[, paste0(Stub, i)] <- 1
        if (!isTRUE(reverse)){
            data[, paste0(Stub, i)][!is.na(data[, i])] <- 0
        } else if (isTRUE(reverse)){
            data[, paste0(Stub, i)][is.na(data[, i])] <- 0
        }
            MissNames <- append(MissNames, paste0(Stub, i))
    }
    if (isTRUE(message)){
        message('The following missingness variable(s) were created:\n')
        message(paste(MissNames, collapse = ', '))
    }
    return(data)
}
