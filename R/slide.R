#' A function for creating lag and lead variables, including for time-series cross-sectional data.
#' 
#' \code{slide} a function for creating lag and lead variables, including for time-series cross-sectional data.
#'
#' @param data a data frame object.
#' @param Var a character string naming the variable you would like to slide (create lag or lead).
#' @param GroupVar a character string naming the variable grouping the units within which \code{Var} will be slid. If \code{GroupVar = NULL} then the whole variable is slid up or down. This is similar to \code{\link{shift}}, though \code{shift} returns the slid data to a new vector rather than the original data frame.
#' @param NewVar a character string specifying the name for the new variable to place the slid data in.
#' @param shiftBy numeric value specifying how many rows (time units) to shift the data by. Negative values slide the data down--lag the data. Positive values shift the data up--lead the data.
#'  
#'  @examples
#'  # Create dummy data
#'  A <- B <- C <- 1:20
#'  ID <- sort(rep(seq(1:4), 5))
#'  Data <- data.frame(ID, A, B, C)
#'  
#'  # Lead the variable by two time units 
#'  DataSlid1 <- slide(Data, Var = "A", NewVar = "ALag", shiftBy = 2)
#'  
#'  # Lag the variable one time unit by ID group 
#'  DataSlid2 <- slide(data = Data, Var = "B", GroupVar = "ID",
#'                 NewVar = "BLag", shiftBy = -1)
#'  
#' @return a data frame
#'  
#' @description The function slides a column up or down to create lag or lead variables. If \code{GroupVar} is specified it will slide \code{Var} for each group. This is important for time-series cross-section data where  The slid data is placed in a new variable. in the original data frame. 
#' Note: your data needs to be sorted by date. The date should be ascending (i.e. increasing as it moves down the rows). Also, the time difference between rows should be constant, e.g. days, months, years.
#' 
#' @seealso \code{\link{shift}}, \code{\link{ddply}}
#'
#' @source Partially based on TszKin Julian's \code{shift} function: http://ctszkin.com/2012/03/11/generating-a-laglead-variables/
#'
#'
#' @importFrom plyr ddply
#' @export

slide <- function(data, Var, GroupVar = NULL, NewVar = NULL, shiftBy = -1){

  VarVect <- data[, Var]

  if (is.null(NULL)){
    NewVar <- paste0(Var, shiftBy)
  }

  # Create lags/leads
  if (is.null(GroupVar)){
    data[, NewVar] <- shift(VarVect = VarVect, shiftBy = shiftBy)
  }
  else if (!is.null(GroupVar)){
    data <- eval(parse(text = paste0("ddply(data, GroupVar, transform, NewVarX = shift(", Var, ",", shiftBy, "))")))
  }

  return(data)
}

#' A function for creating lag and lead variables.
#' 
#' \code{shift} a function for creating lag and lead variables, including for time-series cross-sectional data.
#'
#' @param VarVect a vector you would like to shift (create lag or lead).
#' @param shiftBy numeric value specifying how many rows (time units) to shift the data by. Negative values shift the data down--lag the data. Positive values shift the data up--lead the data.
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

shift <- function(VarVect, shiftBy){
  stopifnot(is.numeric(shiftBy))
  #stopifnot(is.numeric(VarVect))

  if (length(shiftBy) > 1)
    return(sapply(shiftBy, shift, Var = VarVect))

  out <- NULL
  abs_shiftBy = abs(shiftBy)
  
  if (shiftBy > 0){
    out <- c(tail(VarVect, -abs_shiftBy),rep(NA, abs_shiftBy))
  } else if (shiftBy < 0) {
    out <- c(rep(NA, abs_shiftBy), head(VarVect, -abs_shiftBy))
  } else {
    out <- VarVect
  }
  out
}
