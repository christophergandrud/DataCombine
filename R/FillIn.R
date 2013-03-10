#' An R function for filling in missing values of a variable from one data frame with the values from another variable.
#'
#' \code{FillIn} uses values of a variable from one data set to fill in missing values in another.
#' 
#' @param D1 the data frame with the variable you would like to fill in.
#' @param D2 the data frame with the variable you would like to use to fill in \code{D1}.
#' @param Var1 a character string of the name of the variable in \code{D1} you want to fill in.
#' @param Var2 an optional character string of variable name in \code{D2} that you would like to use to fill in.
#' @param KeyVar a character vector of variable names that are shared by \code{D1} and \code{D2} that can be used to join the data frames.
#' 
#' @examples 
#' # Create data set with missing values
#' naDF <- data.frame(a = sample(c(1,2), 100, rep=TRUE), 
#'                    b = sample(c(3,4), 100, rep=TRUE), 
#'                    fNA = sample(c(100, 200, 300, 400, NA), 100, rep=TRUE))
#'
#' # Created full data set
#' fillDF <- data.frame(a = c(1,2,1,2), 
#'                      b = c(3,3,4,4),
#'                      fFull = c(100, 200, 300, 400))
#'
#' # Fill in missing f's from naDF with values from fillDF
#' FilledInData <- FillIn(naDF, fillDF, Var1 = "fNA", Var2 = "fFull", KeyVar = c("a", "b"))
#'

FillIn <- function(D1, D2, Var1, Var2 = NULL, KeyVar = c("iso2c", "year"))
{
  # Give Var2 the same name as var1 if Var2 is NULL
  if (is.null(Var2)){
    Var2 <- Var1
  } else {
    Var2 <- Var2
  }
  
  # Give var a generic name
  names(D1)[match(Var1, names(D1))] <- "VarGen"
  names(D2)[match(Var2, names(D2))] <- "VarGen.1"
  
  # Convert data frames to data.table type objects
  D1Temp <- data.table::data.table(D1, key = KeyVar)
  D2Temp <- data.table::data.table(D2, key = KeyVar)
  
  # Merge data.tables
  OutDT <- D2Temp[D1Temp]
  
  # Tell the user how many values will be filled in
  SubNA <- OutDT[, list(VarGen, VarGen.1)]
  SubNA <- subset(SubNA, is.na(VarGen) & !is.na(VarGen.1))
  print(paste(nrow(SubNA), "NAs were replaced."))
  
  # Fill in missing values from D1 with values from D2
  OutDT <- OutDT[is.na(VarGen), VarGen := VarGen.1]
  
  # Convert back to data frame
  OutDF <- data.frame(OutDT)
  
  # Tell the user what the correlation coefficient is between the variables
  SubNoNA <- subset(OutDF, !is.na(VarGen) & !is.na(VarGen.1))
  HowMany <- nrow(SubNoNA)
  CORR <- cor(SubNoNA$VarGen, SubNoNA$VarGen.1, use = "complete.obs")
  print(paste("The correlation between", Var1, "and", Var2, "is", round(CORR, digits = 3), "based on", HowMany, "shared observations." ))
  
  # Remove uncombined variable and return main variable's name
  names(OutDF)[match("VarGen", names(OutDF))] <- Var1
  Keepers <- setdiff(names(OutDF), "VarGen.1")
  OutDF <- OutDF[, Keepers]
  OutDF
}