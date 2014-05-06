#' A function for creating lag and lead variables, including for time-series
#' cross-sectional data.
#'
#' \code{slide} a function for creating lag and lead variables, including for
#' time-series cross-sectional data.
#'
#' @param data a data frame object.
#' @param Var a character string naming the variable you would like to slide
#' (create lag or lead).
#' @param GroupVar a character string naming the variable grouping the units
#' within which \code{Var} will be slid. If \code{GroupVar = NULL} then the
#' whole variable is slid up or down. This is similar to \code{\link{shift}},
#' though \code{shift} returns the slid data to a new vector rather than the
#' original data frame.
#' @param NewVar a character string specifying the name for the new variable to
#' place the slid data in.
#' @param slideBy numeric value specifying how many rows (time units) to shift
#' the data by. Negative values slide the data down--lag the data. Positive
#' values shift the data up--lead the data.
#' @param reminder logical. Whether or not to remind you to order your data by
#' the \code{GroupVar} and time variable before running \code{slide}, plus other
#' messages.
#'
#'  @examples
#'  # Create dummy data
#'  A <- B <- C <- sample(1:20, size = 20, replace = TRUE)
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
#' @description The function slides a column up or down to create lag or lead
#' variables. If \code{GroupVar} is specified it will slide \code{Var} for each
#' group. This is important for time-series cross-section data. The slid data is
#' placed in a new variable in the original data frame.
#' Note: your data needs to be sorted by date. The date should be ascending
#' (i.e. increasing as it moves down the rows). Also, the time difference
#' between rows should be constant, e.g. days, months, years.
#'
#' @seealso \code{\link{shift}}, \code{\link{dplyr}}
#'
#' @source Partially based on TszKin Julian's \code{shift} function:
#' \url{http://ctszkin.com/2012/03/11/generating-a-laglead-variables/}
#'
#'
#' @importFrom dplyr group_by summarize mutate
#' @export

slide <- function(data, Var, GroupVar = NULL, NewVar = NULL, slideBy = -1,
                  reminder = TRUE)
{
    fake <- total <- NULL

    # Determine if Var exists in data
    DataNames <- names(data)
    TestExist <- Var %in% DataNames

    if (!isTRUE(TestExist)){
        stop(paste(Var, "was not found in the data frame."), call. = FALSE)
    }

    VarVect <- data[, Var]

    # Variable Name
    if (is.null(NewVar)){
        NewVar <- paste0(Var, slideBy)
    }

    # Give messages
    if (isTRUE(reminder)){
        if (is.null(GroupVar)){
          message(paste('\nRemember to put', deparse(substitute(data)),
                        'in time order before running.\n'))
        }
        if (!is.null(GroupVar)){
          message(paste('\nRemember to order', deparse(substitute(data)),
                        'by', GroupVar,
                        'and the time variable before running.\n'))
        }
        if (slideBy < 0) {
            message(paste('Lagging', Var, 'by', abs(slideBy), 'time units.\n'))
        }
        else if (slideBy > 0) {
            message(paste('Leading', Var, 'by', abs(slideBy), 'time units.\n'))
        }
    }

    # Drop if there are not enough observations per group to slide
    if (!is.null(GroupVar)){
        data <- eval(parse(text = paste0('group_by(data, ', GroupVar, ')')))
        data$fake <- 1
        Minimum <- abs(slideBy) - 1
        Summed <- dplyr::mutate(data, total = sum(fake))
        SubSummed <- subset(Summed, total <= Minimum)
        if (nrow(SubSummed) > 0){
            Dropping <- unique(SubSummed[, GroupVar])
            data <- data[!(data[, GroupVar] %in% Dropping), ]
            message(paste0('\nWarning: the following groups have ', Minimum,
                          ' or fewer observations.\nNo reasonable lag/lead can be created, so they are dropped:\n'))
            message(paste(Dropping, collapse = "\n"))
        }
    data <- VarDrop(data, 'fake')
    }

    # Create lags/leads
    if (is.null(GroupVar)){
        data[, NewVar] <- shift(VarVect = VarVect, shiftBy = slideBy,
                                reminder = FALSE)
    }
    else if (!is.null(GroupVar)){
        DataSub <- eval(parse(text = paste0('group_by(data[, c(GroupVar, Var)], ',
                                            GroupVar,')')))
        vars <- eval(parse(text = paste0("dplyr::mutate(DataSub, NewVarX = shift(",
                                 Var, ",", slideBy, ", reminder = FALSE))")))
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
  if(!is.numeric(shiftBy)){
    stop(paste(shiftBy, 'must be numeric.'), call. = FALSE)
  }
  if (isTRUE(reminder)){
      message(paste('Remember to put', deparse(substitute(data)),
                    'in time order before running shift.'))
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
  return(out)
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
#'  A <- B <- C <- sample(1:20, size = 20, replace = TRUE)
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
#' @seealso \code{\link{shift}}, \code{\link{slide}}, \code{\link{dplyr}}
#' @importFrom dplyr group_by mutate
#' @export

slideMA <- function(data, Var, GroupVar = NULL, periodBound = -3, offset = 1,
                    NewVar = NULL, reminder = TRUE){
  slideBy <- NULL
  if (isTRUE(reminder)){
    if (is.null(GroupVar)){
      message(paste('\nRemember to put', deparse(substitute(data)),
                    'in time order before running slide.\n'))
    }
    if (!is.null(GroupVar)){
      message(paste('\nRemember to order', deparse(substitute(data)), 'by',
                    GroupVar, 'and the time variable before running slide.\n'))
    }
  }
  # Determine if Var exists in data
  DataNames <- names(data)
  TestExist <- Var %in% DataNames
  if (!isTRUE(TestExist)){
    stop(paste(Var, "was not found in the data frame.\n"), call. = FALSE)
  }

  VarVect <- data[, Var]

  if (is.null(NewVar)){
    NewVar <- paste0(Var, 'MA', periodBound, '_', offset)
  }

  Abs = abs(periodBound)
  if (periodBound < 0){
    slideBy = periodBound + abs(offset)
  } else if (periodBound > 0){
    slideBy = periodBound - abs(offset)
  }

  # Create lags/leads moving averages
  if (is.null(GroupVar)){
    data[, NewVar] <- shiftMA(data[, Var], shiftBy = slideBy,
                              Abs = Abs, reminder = FALSE)
  } else if (!is.null(GroupVar)){
    DataSub <- eval(parse(text = paste0('group_by(data[, c(GroupVar, Var)], ',
                                        GroupVar,')')))
    vars <- eval(parse(text = paste0("dplyr::mutate(DataSub, NewVarX = shiftMA(",
                                    Var, ", shiftBy =", slideBy, ", Abs = ",
                                    Abs, ", reminder = FALSE))")))
    data[, NewVar] <- vars$NewVarX
    data <- data.frame(data)
  }
  return(data)
}

#' Internal function for slideMA
#' @param x vector
#' @param shiftBy numeric
#' @param Abs numeric
#' @param reminder logical
#'
#' @keywords internals
#' @export

shiftMA <- function(x, shiftBy, Abs, reminder){
  x <- shift(x, shiftBy, reminder = reminder)
  ma(x, Abs, centre = FALSE)
}

#' Spread a dummy variable (1's and 0') over a specified time period and for specified groups
#'
#' @param data a data frame object.
#' @param Var a character string naming the numeric dummy variable with values 0 and 1 that you would like to spread. Can be either spread as a lag or lead.
#' @param GroupVar a character string naming the variable grouping the units within which \code{Var} will be spread If \code{GroupVar = NULL} then the whole variable is spread up or down. This is similar to \code{\link{shift}}, though \code{shift} slides the data and returns it to a new vector rather than the original data frame.
#' @param NewVar a character string specifying the name for the new variable to place the spread dummy data in.
#' @param spreadBy numeric value specifying how many rows (time units) to spread the data over. Negative values spread the data down--lag the data. Positive values spread the data up--lead the data.
#' @param reminder logical. Whether or not to remind you to order your data by the \code{GroupVar} and time variable before running \code{SpreadDummy}.
#'
#' @examples
#' # Create dummy data
#' ID <- sort(rep(seq(1:4), 5))
#' NotVar <- rep(1:5, 4)
#' Dummy <-  sample(c(0, 1), size = 20, replace = TRUE)
#' Data <- data.frame(ID, NotVar, Dummy)
#'
#' # Spread
#' DataSpread1 <- SpreadDummy(data = Data, Var = 'Dummy',
#'                            spreadBy = 2, reminder = FALSE)
#'
#' DataSpread2 <- SpreadDummy(data = Data, Var = 'Dummy', GroupVar = 'ID',
#'                            spreadBy = -2)
#'
#' @seealso \code{\link{slide}}
#' @export

SpreadDummy <- function(data, Var, GroupVar = NULL, NewVar = NULL,
                        spreadBy = -2, reminder = TRUE){
    # Check if variable is numeric dummy
    if (class(data[, Var]) != 'numeric'){
        stop(paste(Var, 'must be a numeric dummy variable.'), call. = FALSE)
    }

    if (is.null(NewVar)){
        NewVar <- paste0(Var, 'Spread', spreadBy)
    }

    if (spreadBy < 0){
        start = -1
    } else if (spreadBy > 0){
        start = 1
    }

    for (i in start:spreadBy){
        NewTemp <- paste0(NewVar, i)
        if (isTRUE(reminder) & abs(i) == 1){
          if (!is.null(GroupVar)){
            temp <- slide(data, Var = Var, GroupVar = GroupVar,
                          NewVar = NewTemp, slideBy = i)
          }
          else if (is.null(GroupVar)){
            temp <- slide(data, Var = Var, NewVar = NewTemp,
                          slideBy = i)
          }
        }
        else if (!is.null(GroupVar)){
        temp <- slide(data, Var = Var, GroupVar = GroupVar,
            NewVar = NewTemp, slideBy = i, reminder = FALSE)
        }
        else if (is.null(GroupVar)){
        temp <- slide(data, Var = Var, NewVar = NewTemp,
            slideBy = i, reminder = FALSE)
        }

      MainNames <- names(data)
      if (nrow(temp) != nrow(data)){
        data <- data[(data[, GroupVar] %in% temp[, GroupVar]), ]
      }
      temp <- temp[, NewTemp]

      data <- data.frame(data, temp)
      names(data) <- c(MainNames, NewTemp)
    }

    tempNames <- Var
    for (i in start:spreadBy){
      temp <- paste0(NewVar, i)
      tempNames <- append(tempNames, temp)
    }
    data[, NewVar] <- data[, Var]
    for (i in tempNames){
      data[, NewVar][data[, i] == 1] <- 1
    }
    data <- VarDrop(data, tempNames[-1])
    return(data)
}

#' Find the starting and ending time points of a spell
#'
#' \code{StartEnd} finds the starting and ending time points of a spell, including for time-series cross-sectional data. Note: your data needs to be sorted by date. The date should be ascending (i.e. increasing as it moves down the rows).
#'
#' @param data a data frame object.
#' @param SpellVar a character string naming the variable you would like to slide (create lag or lead).
#' @param GroupVar a character string naming the variable grouping the units experiencing the spells. If \code{GroupVar = NULL} then .
#' @param SpellValue a value indicating when a unit is in a spell. If \code{SpellValue = NULL} then any change in \code{Var}'s value will be treated as the start/end of a spell.
#'
#' @examples
#' # Create fake data
#' ID <- sort(rep(seq(1:4), 5))
#' Time <- rep(1:5, 4)
#' Dummy <-  c(1, sample(c(0, 1), size = 19, replace = TRUE))
#' Data <- data.frame(ID, Time, Dummy)
#'
#' # Find start/end of spells denoted by Dummy = 1
#' DataSpell <- StartEnd(Data, SpellVar = 'Dummy', GroupVar = 'ID',
#'                      SpellValue = 1)
#'
#' head(DataSpell)
#'
#' @return a data frame with two new variables:
#' \itemize{
#'    \item{Spell_Start: }{The time period year of a given spell.}
#'    \item{Spell_End: }{The end time period of a given spell.}
#' }
#' @seealso \code{\link{slide}}
#' @export

StartEnd <- function(data, SpellVar, GroupVar, SpellValue = NULL){
  # Find Start
  Temp <- slide(data = data, Var = SpellVar, GroupVar = GroupVar,
                slideBy = -1, NewVar = 'TempStart')
  Temp <- data.frame(Temp)
  Temp$Spell_Start <- 0
  Temp$Spell_Start[is.na(Temp[, SpellVar])] <- NA
  if (is.null(SpellValue)){
    Temp$Spell_Start[Temp[, SpellVar] != Temp[, 'TempStart']] <- 1
  }
  else if (!is.null(SpellValue)){
    Temp$Spell_Start[Temp[, SpellVar] == SpellValue &
          Temp[, 'TempStart'] != SpellValue] <- 1
  }

  # Find End
  Temp <- slide(data = Temp, Var = SpellVar, GroupVar = GroupVar,
                slideBy = 1, NewVar = 'TempEnd', reminder = FALSE)
  Temp <- data.frame(Temp)
  Temp$Spell_End <- 0
  Temp$Spell_End[is.na(Temp[, SpellVar])] <- NA
  if (is.null(SpellValue)){
    Temp$Spell_End[Temp[, SpellVar] != Temp[, 'TempEnd']] <- 1
  }
  else if (!is.null(SpellValue)){
    Temp$Spell_End[Temp[, SpellVar] == SpellValue &
          Temp[, 'TempEnd'] != SpellValue] <- 1
  }
  # Final clean
  Temp <- VarDrop(Temp, c('TempStart', 'TempEnd'))
  return(Temp)
}
