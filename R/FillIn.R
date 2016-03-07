#' A function for filling in missing values of a variable from one data frame
#' with the values from another variable.
#'
#' \code{FillIn} uses values of a variable from one data set to fill in missing
#' values in another.
#'
#' @param D1 the data frame with the variable you would like to fill in.
#' @param D2 the data frame with the variable you would like to use to fill in
#' \code{D1}.
#' @param Var1 a character string of the name of the variable in \code{D1} you
#' want to fill in.
#' @param Var2 an optional character string of variable name in \code{D2} that
#' you would like to use to fill in. Note: must be of the same class as
#' \code{Var1}.
#' @param KeyVar a character vector of variable names that are shared by
#' \code{D1} and \code{D2} that can be used to join the data frames.
#' @param allow.cartesian logical. See the \code{\link{data.table}}
#' documentation for more details.
#' @param KeepD2Vars logical, indicating whether or not to keep the variables
#' from D2 in the output data frame. The default is \code{KeepD2Vars = FALSE}.
#' Hint: avoid having variables in your \code{D2} data frame that share names
#' with variables in \code{D1} other than the \code{KeyVar}s
#'
#' @examples
#' # Create data set with missing values
#' naDF <- data.frame(a = sample(c(1,2), 100, rep = TRUE),
#'                    b = sample(c(3,4), 100, rep = TRUE),
#'                    fNA = sample(c(100, 200, 300, 400, NA), 100, rep = TRUE))
#'
#' # Created full data set
#' fillDF <- data.frame(a = c(1, 2, 1, 2),
#'                      b = c(3, 3, 4, 4),
#'                      j = c(5, 5, 5, 5),
#'                      fFull = c(100, 200, 300, 400))
#'
#' # Fill in missing f's from naDF with values from fillDF
#' FilledInData <- FillIn(naDF, fillDF, Var1 = "fNA",
#'                        Var2 = "fFull", KeyVar = c("a", "b"))
#'
#' @import data.table
#' @export

FillIn <- function(D1, D2, Var1, Var2,
                    KeyVar = c("iso2c", "year"), allow.cartesian = FALSE,
                    KeepD2Vars = FALSE)
{
    VarGen = VarGen.1 = NULL

    # Give Var2 the same name as var1 if Var2 is NULL
    if (missing(Var2)){
        Var2 <- Var1
    } else {
        Var2 <- Var2
    }
    
    # Ensure that Var1 and Var2 exist in D1 and D2
    if (!(Var1 %in% names(D1))) stop('Var1 must be a variable in D1.', 
                                     call. = FALSE)
    if (!(Var2 %in% names(D2))) stop('Var2 must be a variable in D2.', 
                                     call. = FALSE)

    # Ensure that Var1 and Var2 are of the same class
    if (class(D1[, Var1]) != class(D2[, Var2])){
        stop(paste('Var1 and Var2 need to have the same class.'), call. = FALSE)
    }

    # Give var a generic name
    names(D1)[match(Var1, names(D1))] <- "VarGen"
    names(D2)[match(Var2, names(D2))] <- "VarGen.1"

    # Convert data frames to data.table type objects
    D1Temp <- data.table::data.table(D1, key = KeyVar)
    D2Temp <- data.table::data.table(D2, key = KeyVar)

    # Merge data.tables
    OutDT <- D2Temp[D1Temp, allow.cartesian = allow.cartesian]

    # Tell the user how many values will be filled in
    SubNA <- OutDT[, list(VarGen, VarGen.1)]
    SubNA <- subset(SubNA, is.na(VarGen) & !is.na(VarGen.1))
    message(paste(nrow(SubNA), "NAs were replaced."))

    # Fill in missing values from D1 with values from D2
    OutDT <- OutDT[is.na(VarGen), VarGen := VarGen.1]

    # Convert back to data frame
    OutDF <- data.frame(OutDT)

    # Tell the user what the correlation coefficient is between the variables
    if (is.numeric(OutDT$VarGen) & is.numeric(VarGen.1)){
        SubNoNA <- subset(OutDF, !is.na(VarGen) & !is.na(VarGen.1))
        HowMany <- nrow(SubNoNA)
        CORR <- cor(SubNoNA$VarGen, SubNoNA$VarGen.1, use = "complete.obs")
        print(paste("The correlation between", Var1, "and", Var2, "is",
        round(CORR, digits = 3), "based on", HowMany, "shared observations." ))
    }

    names(OutDF)[match("VarGen", names(OutDF))] <- Var1

    # Remove uncombined variable and return main variable's name
    if (!isTRUE(KeepD2Vars)){
        D2Vars <- names(D2)
        Droppers <- setdiff(D2Vars, names(D1))
        Droppers <- c(VarGen.1, Droppers)
        Keepers <- setdiff(names(OutDF), Droppers)
        OutDF <- OutDF[, Keepers]
    }
    OutDF
}

#' Fills in missing (NA) values with the previous non-missing value
#'
#' @param data a data frame. Optional as you can simply specify a vector with
#' \code{Var},
#' @param Var the variable in \code{data} or a vector you would like to fill 
#' down missing (\code{NA}) values.
#'
#' @return data frame
#'
#' @examples
#' # Create fake data
#' id <- c('Algeria', NA, NA, NA, 'Mexico', NA, NA)
#' score <- rnorm(7)
#' Data <- data.frame(id, score)
#'
#' # FillDown id
#' DataOut <- FillDown(Data, 'id')
#' 
#' \dontrun{
#' # Use group_by and mutate from dplyr to FillDown grouped data, e.g.:
#' Example <- Example %>% group_by(grouping) %>% 
#'              mutate(NewFilled = FillDown(Var = VarToFill))
#' }
#' @export

FillDown <- function(data, Var) {
    if (!missing(data)) {
        # Determine if Var exists in data
        TestExist <- Var %in% names(data)
        
        if (!isTRUE(TestExist)) {
            stop(paste(Var, "was not found in the data frame."), call. = FALSE)
        }
        if (is.na(data[1, Var])) fill <- NA
        for (i in 1:nrow(data)) {
            if (!is.na(data[i, Var])) {
                fill <- data[i, Var]
            }
            else if (is.na(data[i, Var])) {
                data[i, Var] <- fill
            }
        }
        return(data)
    }
    else if (missing(data)) {
        if (is.na(Var[[1]])) fill <- NA
            for (i in 1:length(Var)) {
                if (!is.na(Var[[i]])) {
                    fill <- Var[[i]]
                }
                else if (is.na(Var[[i]])) {
                    Var[[i]] <- fill
                }
            }
        return(Var)
    }
}

#' Expands a data set so that it includes an observation for each time point in
#' a sequence. Works with grouped data.
#'
#' @param data a data frame.
#' @param GroupVar the variable in \code{data} that signifies the group
#' variable.
#' @param TimeVar the variable in \code{data} that signifies the time variable.
#' The sequence will be expanded between its minimum and maximum value if 
#' \code{begin} and \code{end} are not specified.
#' @param begin numeric of length 1. Specifies beginning time point. 
#' Only relevant if \code{end} is specified.
#' @param end numeric of length 1. Specifies ending time point. 
#' Only relevant if \code{begin} is specified.
#' @param by numeric or character string specifying the steps in the
#' \code{TimeVar} sequence. Can use \code{"month"}, \code{"year"} etc for
#' POSIXt data.
#'
#' @examples
#' Data <- data.frame(country = c("Cambodia", "Camnodia", "Japan", "Japan"),
#'                    year = c(1990, 2001, 1994, 2012))
#'
#' ExpandedData <- TimeExpand(Data, GroupVar = 'country', TimeVar = 'year')
#'
#' @importFrom dplyr select inner_join left_join
#' @export

TimeExpand <- function(data, GroupVar, TimeVar, begin, end, by = 1) {
    fake <- NULL
    if (!is.data.frame(data)) stop('data must be a data frame',
                                            call. = F)
    if (!missing(TimeVar)){
        if (!(TimeVar %in% names(data))) stop(paste0(
            TimeVar, 'not found in',
            deparse(substitute(data)), '.'),
            call. = F)
    }
    if (!missing(begin) & !missing(end)) {
        minimum <- begin
        maximum <- end
    }
    else if (missing(begin) & missing(end)){
        minimum <- min(data[, TimeVar])
        maximum <- max(data[, TimeVar])
    }
    
    # Create sequence of times
    times <- seq(minimum, maximum, by = by)
    times_df <- data.frame(fake = 1, temp = times)
    names(times_df) <- c('fake', TimeVar)

    if (!missing(GroupVar)) {
        group <- unique(data[, GroupVar])
        groups_df <- data.frame(fake = 1, temp = group)
        names(groups_df) <- c('fake', GroupVar)

        times_df <- suppressMessages(inner_join(times_df, groups_df))
        times_df <- times_df[order(times_df[, GroupVar], times_df[, TimeVar]), ]
    }
    times_df <- select(times_df, -fake)

    data <- suppressWarnings(suppressMessages(left_join(times_df, data)))
    class(data) <- 'data.frame'
    return(data)
}
