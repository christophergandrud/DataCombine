#' A function for creating lag and lead variables, including for time-series
#' cross-sectional data.
#'
#' \code{slide} a function for creating lag and lead variables, including for
#' time-series cross-sectional data.
#'
#' @param data a data frame object.
#' @param Var a character string naming the variable you would like to slide
#' (create lag or lead).
#' @param TimeVar optional character string naming the time variable. If
#' specified then the data is ordered by Var-TimeVar before sliding.
#' @param GroupVar a character string naming the variable grouping the units
#' within which \code{Var} will be slid. If \code{GroupVar} is missing then the
#' whole variable is slid up or down. This is similar to \code{\link{shift}},
#' though \code{shift} returns the slid data to a new vector rather than the
#' original data frame.
#' @param NewVar a character string specifying the name for the new variable to
#' place the slid data in.
#' @param slideBy numeric value specifying how many rows (time units) to shift
#' the data by. Negative values slide the data down--lag the data. Positive
#' values shift the data up--lead the data.
#' @param keepInvalid logical. Whether or not to keep observations for groups for
#' which no valid lag/lead can be created due to an insufficient number of time
#' period observations. If \code{TRUE} then these groups are returned to the
#' bottom of the data frame and \code{NA} is given for their new lag/lead
#' variable value.
#' @param reminder logical. Whether or not to remind you to order your data by
#' the \code{GroupVar} and time variable before running \code{slide}, plus other
#' messages.
#'
#' @examples
#' # Create dummy data
#' A <- B <- C <- sample(1:20, size = 20, replace = TRUE)
#' ID <- sort(rep(seq(1:4), 5))
#' Data <- data.frame(ID, A, B, C)
#'
#' # Lead the variable by two time units
#' DataSlid1 <- slide(Data, Var = 'A', NewVar = 'ALead', slideBy = 2)
#'
#' # Lag the variable one time unit by ID group
#' DataSlid2 <- slide(data = Data, Var = 'B', GroupVar = 'ID',
#'                 NewVar = 'BLag', slideBy = -1)
#'
#' # Lag the variable one time unit by ID group, with invalid lags
#' Data <- Data[1:16, ]
#'
#' DataSlid3 <- slide(data = Data, Var = 'B', GroupVar = 'ID',
#'                  NewVar = 'BLag', slideBy = -2, keepInvalid = TRUE)
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
#' @importFrom dplyr group_by group_by_ summarize mutate ungroup %>% bind_rows
#' @export

slide <- function(data, Var, TimeVar, GroupVar, NewVar, slideBy = -1,
    keepInvalid = FALSE, reminder = TRUE) {
    fake <- total <- FullData <- NULL
    if (!missing(GroupVar) & "data.table" %in% class(data))
        stop(paste("slide does not support data.tables with Grouped variables.\n",
            "Convert to data.frame and try again."), call. = F)

    if (!missing(GroupVar) & "tbl_df" %in% class(data)) {
        message("Converting to plain data frame from tbl_df.")
        data <- as.data.frame(data)
    }

    # Determine if Var exists in data
    DataNames <- names(data)
    TestExist <- Var %in% DataNames

    if (!isTRUE(TestExist)) {
        stop(paste(Var, "was not found in the data frame."),
            call. = FALSE)
    }

    if (!missing(TimeVar))
        data[order(data[, Var], data[, TimeVar]), ]

    VarVect <- data[, Var]

    # Variable Name
    if (missing(NewVar)) {
        NewVar <- paste0(Var, slideBy)
    }

    # Logically valid argument pairs
    if (isTRUE(keepInvalid) & missing(GroupVar)) {
        warning("keepInvalid set to FALSE when GroupVar is missing.")
        keepInvalid <- FALSE
    }

    # Give messages
    if (isTRUE(reminder)) {
        if (missing(TimeVar)) {
            if (missing(GroupVar)) {
                message(paste("\nRemember to put", deparse(substitute(data)),
                  "in time order before running."))
            }
            if (!missing(GroupVar)) {
                message(paste("\nRemember to order", deparse(substitute(data)),
                  "by", GroupVar, "and the time variable before running."))
            }
        }
        if (slideBy < 0) {
            message(paste("\nLagging", Var, "by", abs(slideBy),
                "time units.\n"))
        } else if (slideBy > 0) {
            message(paste("\nLeading", Var, "by", abs(slideBy),
                "time units.\n"))
        }
    }

    # Drop if there are not enough observations per group to
    # slide
    if (!missing(GroupVar)) {
        data <- group_by_(data, .dots = GroupVar)
        data$fake <- 1
        Minimum <- abs(slideBy) - 1
        Summed <- dplyr::mutate(data, total = sum(fake))
        SubSummed <- subset(Summed, total <= Minimum) %>% data.frame()
        data <- VarDrop(data, "fake")
        if (nrow(SubSummed) == 0) {
            FullData <- NULL
        } else if (nrow(SubSummed) > 0) {
            ## Hack
            FullData <- data
            class(FullData) <- "data.frame"
            ## End Hack

            Dropping <- unique(SubSummed[, GroupVar])

            ## Hack
            class(data) <- "data.frame"
            data <- data[!(data[, GroupVar] %in% Dropping), ]
            data <- group_by_(data, .dots = GroupVar)
            ## End Hack

            if (!isTRUE(keepInvalid)) {
                message(paste0("\nWarning: the following groups have ",
                  Minimum, " or fewer observations.", "\nNo valid lag/lead can be created, so they are dropped:\n"))
                message(paste(Dropping, collapse = "\n"))
                message("\n")
            } else if (isTRUE(keepInvalid)) {
                message(paste0("\nWarning: the following groups have ",
                  Minimum, " or fewer observations.", "\n  No valid lag/lead can be created.",
                  "\n  NA will be returned for these observations in the new lag/lead variable.",
                  "\n  They will be returned at the bottom of the data frame.\n"))
                message(paste(Dropping, collapse = "\n"))
                message("\n")
            }
        }
    }

    # Create lags/leads
    if (missing(GroupVar)) {
        data[, NewVar] <- shift(VarVect = VarVect, shiftBy = slideBy,
            reminder = FALSE)
    } else if (!missing(GroupVar)) {
        DataSub <- eval(parse(text = paste0("group_by(data[, c(GroupVar, Var)], ",
            GroupVar, ")")))
        vars <- eval(parse(text = paste0("dplyr::mutate(DataSub, NewVarX = shift(",
            Var, ",", slideBy, ", reminder = FALSE))")))
        data[, NewVar] <- vars$NewVarX
    }
    if (isTRUE(keepInvalid) & !is.null(FullData)) {
        invalid <- FullData[(FullData[, GroupVar] %in% Dropping), ]
        invalid[, NewVar] <- NA
        data <- bind_rows(data, invalid)
    }
    data <- ungroup(data)
    class(data) <- "data.frame"
    return(data)
}

#' A function for creating lag and lead variables.
#'
#' \code{shift} a function for creating lag and lead variables, including for
#' time-series cross-sectional data.
#'
#' @param VarVect a vector you would like to shift (create lag or lead).
#' @param shiftBy numeric value specifying how many rows (time units) to shift
#' the data by. Negative values shift the data down--lag the data. Positive
#' values shift the data up--lead the data.
#' @param reminder logical. Whether or not to remind you to order your data by
#' the \code{GroupVar} and time variable before running \code{shift}.
#'
#' @return a vector
#'
#' @description The function shifts a vector up or down to create lag or lead
#' variables.
#' Note: your data needs to be sorted by date. The date should be ascending
#' (i.e. increasing as it moves down the rows).
#'
#' @seealso \code{\link{slide}}
#'
#' @source Largely based on TszKin Julian's \code{shift} function:
#' \url{http://ctszkin.com/2012/03/11/generating-a-laglead-variables/}.
#'
#' @importFrom utils tail head
#'
#' @export

shift <- function(VarVect, shiftBy, reminder = TRUE) {
    if (!is.numeric(shiftBy)) {
        stop(paste(shiftBy, "must be numeric."), call. = FALSE)
    }
    if (isTRUE(reminder)) {
        message(paste("Remember to put", deparse(substitute(data)),
            "in time order before running shift."))
    }

    if (length(shiftBy) > 1)
        return(sapply(shiftBy, shift, Var = VarVect))

    out <- NULL
    abs_shiftBy = abs(shiftBy)

    if (shiftBy > 0) {
        out <- c(tail(VarVect, -abs_shiftBy), rep(NA, abs_shiftBy))
    } else if (shiftBy < 0) {
        out <- c(rep(NA, abs_shiftBy), head(VarVect, -abs_shiftBy))
    } else {
        out <- VarVect
    }
    return(out)
}

#' Create a moving average for a period before or after each time point for a
#' given variable
#'
#' @param data a data frame object.
#' @param Var a character string naming the variable you would like to create
#' the lag/lead moving averages from.
#' @param GroupVar a character string naming the variable grouping the units
#' within which \code{Var} will be turned into slid moving averages. If
#' \code{GroupVar} is missing then the whole variable is slid up or down and
#' moving averages will be created. This is similar to \code{\link{shift}},
#' though \code{shift} returns the slid data to a new vector rather than the
#' original data frame.
#' @param periodBound integer. The time point for the outer bound of the time
#' period over which to create the moving averages. The default is \code{-3},
#' i.e. the moving average period begins three time points before a given time
#' point. Can also be positive for leading moving averages.
#' @param offset integer. How many time increments away from a given time point
#' to begin the moving average time period. The default is \code{1}. Effectively
#' controls how wide the moving average window is in the other direction of
#' \code{periodBound}. Note: must be positive.
#' @param NewVar a character string specifying the name for the new variable to
#' place the slid data in.
#' @param reminder logical. Whether or not to remind you to order your data by
#' the \code{GroupVar} and time variable before running \code{slideMA}.
#'
#' @details \code{slideMA} is designed to give you more control over the window
#' for creating the moving average. Think of the \code{periodBound} and
#' \code{offset} arguments working together. If for example,
#' \code{periodBound = -3} and \code{offset = 1} then the variable of interest
#' will be lagged by 2 then a moving average window of three time increments
#' around the lagged variable is found.
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
#'  DataSlidMA1 <- slideMA(Data, Var = 'A', NewVar = 'ALead_MA',
#'                  periodBound = 3)
#'
#'  # Lag the variable one time unit by ID group
#'  DataSlidMA2 <- slideMA(data = Data, Var = 'B', GroupVar = 'ID',
#'                 NewVar = 'BLag_MA', periodBound = -3, offset = 2)
#'
#' @seealso \code{\link{shift}}, \code{\link{slide}}, \code{\link{dplyr}}
#' @importFrom dplyr group_by mutate
#' @export

slideMA <- function(data, Var, GroupVar, periodBound = -3, offset = 1,
    NewVar, reminder = TRUE) {
    slideBy <- NULL
    if (isTRUE(reminder)) {
        if (missing(GroupVar)) {
            message(paste("\nRemember to put", deparse(substitute(data)),
                "in time order before running slide.\n"))
        }
        if (!missing(GroupVar)) {
            message(paste("\nRemember to order", deparse(substitute(data)),
                "by", GroupVar, "and the time variable before running slide.\n"))
        }
    }
    # Determine if Var exists in data
    DataNames <- names(data)
    TestExist <- Var %in% DataNames
    if (!isTRUE(TestExist)) {
        stop(paste(Var, "was not found in the data frame.\n"),
            call. = FALSE)
    }

    VarVect <- data[, Var]

    if (missing(NewVar)) {
        NewVar <- paste0(Var, "MA", periodBound, "_", offset)
    }

    Abs = abs(periodBound)
    if (periodBound < 0) {
        slideBy = periodBound + abs(offset)
    } else if (periodBound > 0) {
        slideBy = periodBound - abs(offset)
    }

    # Create lags/leads moving averages
    if (missing(GroupVar)) {
        data[, NewVar] <- shiftMA(data[, Var], shiftBy = slideBy,
            Abs = Abs, reminder = FALSE)
    } else if (!missing(GroupVar)) {
        DataSub <- eval(parse(text = paste0("group_by(data[, c(GroupVar, Var)], ",
            GroupVar, ")")))
        vars <- eval(parse(text = paste0("dplyr::mutate(DataSub, NewVarX = shiftMA(",
            Var, ", shiftBy =", slideBy, ", Abs = ", Abs, ", reminder = FALSE))")))
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

shiftMA <- function(x, shiftBy, Abs, reminder) {
    x <- shift(x, shiftBy, reminder = reminder)
    ma(x, Abs, centre = FALSE)
}

#' Spread a dummy variable (1's and 0') over a specified time period and for
#' specified groups
#'
#' @param data a data frame object.
#' @param Var a character string naming the numeric dummy variable with values
#' 0 and 1 that you would like to spread. Can be either spread as a lag or lead.
#' @param GroupVar a character string naming the variable grouping the units
#' within which \code{Var} will be spread If \code{GroupVar} is missing then the
#' whole variable is spread up or down. This is similar to \code{\link{shift}},
#' though \code{shift} slides the data and returns it to a new vector rather
#' than the original data frame.
#' @param NewVar a character string specifying the name for the new variable to
#' place the spread dummy data in.
#' @param spreadBy numeric value specifying how many rows (time units) to spread
#' the data over. Negative values spread the data down--lag the data. Positive
#' values spread the data up--lead the data.
#' @param reminder logical. Whether or not to remind you to order your data by
#' the \code{GroupVar} and time variable before running \code{SpreadDummy}.
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

SpreadDummy <- function(data, Var, GroupVar, NewVar, spreadBy = -2,
    reminder = TRUE) {
    # Coerce to data.frame
    data <- as.data.frame(data)

    # Check if variable is numeric dummy
    if (class(data[, Var]) != "numeric") {
        stop(paste(Var, "must be a numeric dummy variable."),
            call. = FALSE)
    }

    if (missing(NewVar)) {
        NewVar <- paste0(Var, "Spread", spreadBy)
    }

    if (spreadBy < 0) {
        start = -1
    } else if (spreadBy > 0) {
        start = 1
    }

    for (i in start:spreadBy) {
        NewTemp <- paste0(NewVar, i)
        if (isTRUE(reminder) & abs(i) == 1) {
            if (!missing(GroupVar)) {
                temp <- suppressMessages(slide(data, Var = Var, GroupVar = GroupVar,
                  NewVar = NewTemp, slideBy = i))
            } else if (missing(GroupVar)) {
                temp <- suppressMessages(slide(data, Var = Var, NewVar = NewTemp,
                  slideBy = i))
            }
        } else if (!missing(GroupVar)) {
            temp <- suppressMessages(slide(data, Var = Var, GroupVar = GroupVar,
                NewVar = NewTemp, slideBy = i, reminder = FALSE))
        } else if (missing(GroupVar)) {
            temp <- suppressMessages(slide(data, Var = Var, NewVar = NewTemp,
                slideBy = i, reminder = FALSE))
        }

        MainNames <- names(data)
        if (nrow(temp) != nrow(data)) {
            data <- data[(data[, GroupVar] %in% temp[, GroupVar]),
                ]
        }
        temp <- temp[, NewTemp]

        data <- data.frame(data, temp)
        names(data) <- c(MainNames, NewTemp)
    }

    tempNames <- Var
    for (i in start:spreadBy) {
        temp <- paste0(NewVar, i)
        tempNames <- append(tempNames, temp)
    }
    data[, NewVar] <- data[, Var]
    for (i in tempNames) {
        data[, NewVar][data[, i] == 1] <- 1
    }
    data <- VarDrop(data, tempNames[-1])
    return(data)
}

#' Find the starting and ending time points of a spell
#'
#' \code{StartEnd} finds the starting and ending time points of a spell,
#' including for time-series cross-sectional data. Note: your data needs to be
#' sorted by date. The date should be ascending (i.e. increasing as it moves
#' down the rows).
#'
#' @param data a data frame object.
#' @param SpellVar a character string naming the variable with information on
#' when each spell starts.
#' @param GroupVar a character string naming the variable grouping the units
#' experiencing the spells. If \code{GroupVar} is missing then .
#' @param SpellValue a value indicating when a unit is in a spell. If
#' \code{SpellValue} is missing then any change in \code{Var}'s value will be
#' treated as the start/end of a spell. Must specify if \code{OnlyStart = TRUE}.
#' @param OnlyStart logical for whether or not to only add a new
#' \code{Spell_Start} variable. Please see the details.
#' @param ... Aguments to pass to \code{\link{slide}}.
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
#'                      TimeVar = 'Time', SpellValue = 1)
#'
#' head(DataSpell)
#'
#' @return a data frame. If \code{OnlyStart = FALSE} then two new variables are
#' returned:
#' \itemize{
#'    \item{Spell_Start: }{The time period year of a given spell.}
#'    \item{Spell_End: }{The end time period of a given spell.}
#' }
#' If \code{OnlyStart = TRUE} then only \code{Spell_Start} is added.
#' This variable includes both \code{1}'s for the start of a new spell and for
#' the start of a 'gap spell', i.e. a spell after \code{Spell_End}.
#' @seealso \code{\link{slide}}
#' @importFrom dplyr ungroup
#' @export

StartEnd <- function(data, SpellVar, GroupVar, SpellValue, OnlyStart = FALSE,
    ...) {
    if (missing(SpellVar)) {
        stop("You must specify the SpellVar", call. = FALSE)
    }
    if (!missing(GroupVar)) {
        message(paste("\nRemember to order", deparse(substitute(data)),
            "by", GroupVar, "and the time variable before running slide.\n"))
    }
    # Find Start
    Temp <- slide(data = data, Var = SpellVar, GroupVar = GroupVar,
        slideBy = -1, NewVar = "TempStart", reminder = FALSE,
        ...)
    Temp <- data.frame(Temp)
    Temp$Spell_Start <- 0
    Temp$Spell_Start[is.na(Temp[, SpellVar])] <- NA
    if (missing(SpellValue)) {
        Temp$Spell_Start[Temp[, SpellVar] != Temp[, "TempStart"]] <- 1
    } else if (!missing(SpellValue)) {
        Temp$Spell_Start[Temp[, SpellVar] == SpellValue & Temp[,
            "TempStart"] != SpellValue] <- 1
    }

    # Find End
    Temp <- slide(data = Temp, Var = SpellVar, GroupVar = GroupVar,
        slideBy = 1, NewVar = "TempEnd", reminder = FALSE, ...)
    Temp <- data.frame(Temp)
    Temp$Spell_End <- 0
    Temp$Spell_End[is.na(Temp[, SpellVar])] <- NA
    if (missing(SpellValue)) {
        Temp$Spell_End[Temp[, SpellVar] != Temp[, "TempEnd"]] <- 1
    } else if (!missing(SpellValue)) {
        Temp$Spell_End[Temp[, SpellVar] == SpellValue & Temp[,
            "TempEnd"] != SpellValue] <- 1
    }
    Temp <- ungroup(Temp)

    if (isTRUE(OnlyStart)) {
        if (missing(SpellValue)) {
            stop("Must specify SpellValue if OnlyStart = TRUE.",
                call. = FALSE)
        }
        Temp <- slide(data = Temp, Var = "Spell_End", GroupVar = GroupVar,
            slideBy = -1, NewVar = "TempNewStart", reminder = FALSE,
            ...)
        Temp <- ungroup(Temp)
        Temp$Spell_Start[Temp$Spell_Start != SpellValue & Temp$TempNewStart ==
            SpellValue] <- 1
        Temp <- VarDrop(Temp, c("TempStart", "TempEnd", "TempNewStart",
            "Spell_End"))
    } else if (!isTRUE(OnlyStart)) {
        Temp <- VarDrop(Temp, c("TempStart", "TempEnd"))
    }
    return(Temp)
}

#' Count spells, including for grouped data
#'
#' \code{CountSpell} is a function that returns a variable counting the spell
#' number for an observation. Works with grouped data.
#' @param data a data frame object.
#' @param TimeVar a character string naming the time variable.
#' @param SpellVar a character string naming the variable with information on
#' when each spell starts.
#' @param GroupVar a character string naming the variable grouping the units
#' experiencing the spells.
#' @param NewVar NewVar a character string naming the new variable to place the
#' spell counts in.
#' @param SpellValue a value indicating when a unit is in a spell. Must match
#' the class of the \code{SpellVar}.
#'
#' @examples
#' # Create fake data
#' ID <- sort(rep(seq(1:4), 5))
#' Time <- rep(1:20)
#' Dummy <-  c(1, sample(c(0, 1), size = 19, replace = TRUE))
#' Data <- data.frame(ID, Time, Dummy)
#'
#' # Find spell for whole data frame
#' DataSpell1 <- CountSpell(Data, TimeVar = 'Time', SpellVar = 'Dummy',
#'                          SpellValue = 1)
#'
#' head(DataSpell1)
#'
#' # Find spell for each ID group
#' DataSpell2 <- CountSpell(Data, TimeVar = 'Time', SpellVar = 'Dummy',
#'                          GroupVar = 'ID', SpellValue = 1)
#'
#' head(DataSpell2)
#'
#' @export

CountSpell <- function(data, TimeVar, SpellVar, GroupVar, NewVar,
    SpellValue) {
    if (missing(NewVar)) {
        NewVar <- paste0(SpellVar, "_", "SpellCount")
        message(paste0("\nSpell count placed in new variable: ",
            NewVar, ".\n"))
    }
    
    if (!missing(GroupVar)) {
        tempMain <- data.frame()
        for (i in unique(data[, GroupVar])) {
            tempSub <- data[data[, GroupVar] == i, ]
            tempSub[, NewVar] <- CountSpellOne(data = tempSub,
                TimeVar = TimeVar, SpellVar = SpellVar, SpellValue = SpellValue)
            tempMain <- rbind(tempMain, tempSub)
        }
        data <- tempMain
    } else if (missing(GroupVar)) {
        data[, NewVar] <- CountSpellOne(data = data, TimeVar = TimeVar,
            SpellVar = SpellVar, SpellValue = SpellValue)
    }
    return(data)
}

#' Internal function for finding spells for one unit
#'
#' @importFrom dplyr arrange_
#' @noRd

CountSpellOne <- function(data, TimeVar, SpellVar, SpellValue) {
    Spell_Start <- NULL
    if (missing(TimeVar))
        stop("You must specify the TimeVar", call. = FALSE)

    if (missing(SpellValue))
        stop("You must specify the SpellValue", call. = FALSE)

    if (class(SpellValue) != class(data[, SpellVar]))
        stop("SpellValue must be the same class as SpellVar",
            call. = FALSE)
    
    data <- arrange_(data, .dots = TimeVar)

    if (all(data[, SpellVar] == SpellValue)) {
        dataSpell <- data
        dataSpell$Spell_Count <- 1:nrow(dataSpell)
    } else if (!any(data[, SpellVar] == SpellValue)) {
        dataSpell <- data
        dataSpell$Spell_Count <- 0
    } else if (any(data[, SpellVar] != SpellValue)) {
        dataSpell <- StartEnd(data = data, SpellVar = SpellVar,
            SpellValue = SpellValue, OnlyStart = TRUE)

        temp <- subset(dataSpell, Spell_Start == 1)
        temp$spell_ID <- 1:nrow(temp)

        dataSpell$Spell_Count <- 0
        for (u in 1:max(temp$spell_ID)) {
            dataSpell$Spell_Count[dataSpell[, TimeVar] >= temp[temp$spell_ID ==
                u, TimeVar]] <- u
        }
    }
    return(dataSpell$Spell_Count)
}
