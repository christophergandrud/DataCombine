#' A function for creating lag and lead variables, including for time-series cross-sectional data.
#' 
#' \code{slide} a function for creating lag and lead variables, including for time-series cross-sectional data.
#'
#' @param data a data frame object.
#' @param Var a character string naming the variable you would like to slide (create lag or lead).
#' @param GroupVar a character string naming the variable grouping the units within which \code{Var} will be slid. If \code{GroupVar = NULL} then the whole variable is slid up or down. This is similar to \code{\link{shift}}, though \code{shift} returns the slid data to a new vector rather than the original data frame.
#' @param NewVar a character string specifying the name for the new variable to place the slid data in.
#' @param slideBy numeric value specifying how many rows (time units) to shift the data by. Negative values slide the data down--lag the data. Positive values shift the data up--lead the data.
#' @param reminder logical. Whether or not to remind you to order your data by the \code{GroupVar} and time variable before running \code{slide}.
#'  
#'  @examples
#'  # Create dummy data
#'  A <- B <- C <- 1:20
#'  ID <- sort(rep(seq(1:4), 5))
#'  Data <- data.frame(ID, A, B, C)
#'  
#'  # Lead the variable by two time units 
#'  DataSlid1 <- slide(Data, Var = "A", NewVar = "ALead", slideBy = 2)
#'  
#'  # Lag the variable one time unit by ID group 
#'  DataSlid2 <- slide(data = Data, Var = "B", GroupVar = "ID",
#'                 NewVar = "BLag", slideBy = -1)
#'  
#' @return a data frame
#'  
#' @description The function slides a column up or down to create lag or lead variables. If \code{GroupVar} is specified it will slide \code{Var} for each group. This is important for time-series cross-section data. The slid data is placed in a new variable in the original data frame. 
#' Note: your data needs to be sorted by date. The date should be ascending (i.e. increasing as it moves down the rows). Also, the time difference between rows should be constant, e.g. days, months, years.
#' 
#' @seealso \code{\link{shift}}, \code{\link{ddply}}
#'
#' @source Partially based on TszKin Julian's \code{shift} function: http://ctszkin.com/2012/03/11/generating-a-laglead-variables/
#'
#'
#' @importFrom plyr ddply rename
#' @export

slide <- function(data, Var, GroupVar = NULL, NewVar = NULL, slideBy = -1, reminder = TRUE)
{  
  if (isTRUE(reminder)){
    if (is.null(GroupVar)){
      message(paste('Remember to put', deparse(substitute(data)), 'in time order before running slide.'))      
    }
    if (!is.null(GroupVar)){
      message(paste('Remember to order', deparse(substitute(data)), 'by', GroupVar, 'and the time variable before running slide.'))
    }
  }  
  
  # Determine if Var exists in data
  DataNames <- names(data)
  TestExist <- Var %in% DataNames
  if (!isTRUE(TestExist)){
    stop(paste(Var, "was not found in the data frame."))
  }
  
  VarVect <- data[, Var]
  
  if (is.null(NewVar)){
    NewVar <- paste0(Var, slideBy)
  }
  
  # Create lags/leads
  if (is.null(GroupVar)){
    data[, NewVar] <- shift(VarVect = VarVect, shiftBy = slideBy, reminder = FALSE)
  }
  else if (!is.null(GroupVar)){
    vars <- eval(parse(text = paste0("ddply(data[, c(GroupVar, Var)], GroupVar, transform, NewVarX = shift(", Var, ",", slideBy, ", reminder = FALSE))")))
    data[, NewVar] <- vars$NewVarX
  }
  
  return(data)
}

#' A function for creating lag and lead variables.
#' 
#' \code{shift} a function for creating lag and lead variables, including for time-series cross-sectional data.
#'
#' @param VarVect a vector you would like to shift (create lag or lead).
#' @param shiftBy numeric value specifying how many rows (time units) to shift the data by. Negative values shift the data down--lag the data. Positive values shift the data up--lead the data.
#' @param reminder logical. Whether or not to remind you to order your data by the \code{GroupVar} and time variable before running \code{shift}.
#'  
#' @return a vector
#'  
#' @description The function shifts a vector up or down to create lag or lead variables. 
#' Note: your data needs to be sorted by date. The date should be ascending (i.e. increasing as it moves down the rows). 
#' 
#' @seealso \code{\link{slide}}
#'
#' @source Largely based on TszKin Julian's \code{shift} function: http://ctszkin.com/2012/03/11/generating-a-laglead-variables/
#'
#' @export

shift <- function(VarVect, shiftBy, reminder = TRUE){
  stopifnot(is.numeric(shiftBy))
  #stopifnot(is.numeric(VarVect))
  if (isTRUE(reminder)){
      message(paste('Remember to put', deparse(substitute(data)), 'in time order before running shift.'))      
  }

  if (length(shiftBy) > 1)
    return(sapply(shiftBy, shift, Var = VarVect))

  out <- NULL
  abs_shiftBy = abs(shiftBy)
  
  if (shiftBy > 0){
    out <- c(tail(VarVect, -abs_shiftBy), rep(NA, abs_shiftBy))
  } else if (shiftBy < 0) {
    out <- c(rep(NA, abs_shiftBy), head(VarVect, -abs_shiftBy))
  } else {
    out <- VarVect
  }
  out
}

#' Create a moving average for a period before or after each time point for a given variable
#' 
#' @param data a data frame object.
#' @param Var a character string naming the variable you would like to create the lag/lead moving averages from.
#' @param GroupVar a character string naming the variable grouping the units within which \code{Var} will be turned into slid moving averages. If \code{GroupVar = NULL} then the whole variable is slid up or down and moving averages will be created. This is similar to \code{\link{shift}}, though \code{shift} returns the slid data to a new vector rather than the original data frame.
#' @param periodBound numeric. The time point for the outer bound of the time period overwhich to create the moving averages. The default is \code{-3}, i.e. the period begins three time periods before a given time period. Can also be positive for leading moving averages.
#' @param offset numeric. How many time increments away from a given increment to begin the moving average time period. The default is \code{1}. Note: must be positive. 
#' @param NewVar a character string specifying the name for the new variable to place the slid data in.
#' @param reminder logical. Whether or not to remind you to order your data by the \code{GroupVar} and time variable before running \code{slideMA}.
#' 
#' @return a data frame
#' 
#' @examples
#'  # Create dummy data
#'  A <- B <- C <- 1:20
#'  ID <- sort(rep(seq(1:4), 5))
#'  Data <- data.frame(ID, A, B, C)
#'  
#'  # Lead the variable by two time units 
#'  DataSlidMA1 <- slideMA(Data, Var = "A", NewVar = "ALead_MA", 
#'                  periodBound = 3)
#'  
#'  # Lag the variable one time unit by ID group 
#'  DataSlidMA2 <- slideMA(data = Data, Var = "B", GroupVar = "ID",
#'                 NewVar = "BLag_MA", periodBound = -3, offset = 2)
#' 
#' @seealso \code{\link{shift}}, \code{\link{slide}}, \code{\link{ddply}}
#' @importFrom plyr ddply
#' @importFrom forecast ma
#' @export

slideMA <- function(data, Var, GroupVar = NULL, periodBound = -3, offset = 1, NewVar = NULL, reminder = TRUE){
  slideBy <- NULL
  if (isTRUE(reminder)){
    if (is.null(GroupVar)){
      message(paste('Remember to put', deparse(substitute(data)), 'in time order before running slide.'))      
    }
    if (!is.null(GroupVar)){
      message(paste('Remember to order', deparse(substitute(data)), 'by', GroupVar, 'and the time variable before running slide.'))
    }
  }  
  # Determine if Var exists in data
  DataNames <- names(data)
  TestExist <- Var %in% DataNames
  if (!isTRUE(TestExist)){
    stop(paste(Var, "was not found in the data frame."))
  }
  
  VarVect <- data[, Var]
  
  if (is.null(NewVar)){
    NewVar <- paste0(Var, 'MA', periodBound, '_', offset)
  }
  
  Abs = abs(periodBound)
  if (periodBound < 0){
    slideBy = periodBound + abs(offset)
  }
  else if (periodBound > 0){
    slideBy = periodBound - abs(offset)
  }
  
  shiftMA <- function(x, shiftBy = slideBy, Abs = Abs, reminder = reminder){
    x <- shift(x, shiftBy, reminder = reminder)
    ma(x, Abs, centre = FALSE)
  }
  
  # Create lags/leads moving averages
  if (is.null(GroupVar)){
    data[, NewVar] <- shiftMA(data[, Var], shiftBy = slideBy, Abs = Abs, reminder = FALSE)
  }
  else if (!is.null(GroupVar)){
    vars <- eval(parse(text = paste0("ddply(data[, c(GroupVar, Var)], GroupVar, transform, NewVarX = DataCombine:::shiftMA(", Var, ", shiftBy =", slideBy, ", Abs = ", Abs, ", reminder = FALSE))"))) 
    data[, NewVar] <- vars$NewVarX
  }
  return(data)
}

#' Internal function for slideMA
#'
#' @noRd
#' @keywords internals
#' @noRd

shiftMA <- function(x, shiftBy = slideBy, Abs = Abs, reminder = reminder){
  slideBy <- NULL
  x <- shift(x, shiftBy, reminder = reminder)
  ma(x, Abs, centre = FALSE)
}