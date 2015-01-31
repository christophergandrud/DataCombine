#' Creates a continuous Unit-Time-Dummy data frame from a data frame with Unit-Start-End times
#'
#' @param data a data frame with a Group, Start, and End variables.
#' @param GroupVar a character string naming the variable grouping the units within which the new dummy variable will be found.
#' @param StartVar a character string indicating the variable with the starting times of some series.
#' @param EndVar a character string indicating the variable with the ending times of some series.
#' @param NewVar a character string specifying the name of the new dummy variable for the series. The default is \code{TimeFilled}.
#' @param NewTimeVar a character string specifying the name of the new time variable. The default is \code{Time}.
#' @param KeepStartStop logical indicating whether or not to keep the \code{StartVar} and \code{EndVar} variables in the output data frame.
#'
#' @return Returns a data frame with at least three columns, with the \code{GroupVar}, \code{NewTimeVar}, and a new dummy variable with the name specified by \code{NewVar}. This variable is \code{1} for every time increment between and including \code{StartVar} and \code{EndVar}. It is \code{0} otherwise.
#'
#' @examples
#' # Create fake data
#'
#' Country = c('Panama', 'Korea', 'Korea', 'Germany', 'Finland')
#' Start = c(1995, 1980, 2004, 2000, 2012)
#' End = c(1995, 2001, 2010, 2002, 2014)
#'
#' Data <- data.frame(Country, Start, End)
#'
#' # TimeFill
#' FilledData <- TimeFill(Data, GroupVar = 'Country',
#'                  StartVar = 'Start', EndVar = 'End')
#'
#' # Show selection from TimeFill-ed data
#' FilledData[90:100, ]
#'
#' @export

TimeFill <- function(data, GroupVar, StartVar, EndVar, NewVar = 'TimeFilled',
                     NewTimeVar = 'Time', KeepStartStop = FALSE)
{
  # Warnings
  if (class(data[, StartVar]) != 'numeric'){
    message(paste0('Converting ', deparse(substitute(StartVar)), ' to numeric. Things might get wacky. Please check.'))
    data[, StartVar] <- as.character(data[, StartVar])
    data[, StartVar] <- as.numeric(data[, StartVar])
  }
  if (class(data[, EndVar]) != 'numeric'){
    message(paste0('Converting ', deparse(substitute(EndVar)), ' to numeric. Things might get wacky. Please check.'))
    data[, EndVar] <- as.character(data[, EndVar])
    data[, EndVar] <- as.numeric(data[, EndVar])
  }

  # Keep only basic data
  Sub <- data[, c(GroupVar, StartVar, EndVar)]

  # Find minimum and maximum times and create full extent of time
  Min <- min(Sub[, StartVar])
  Max <- max(Sub[, EndVar])
  Time <- Min:Max

  UniGroup <- unique(Sub[, GroupVar])

  Full <- merge(UniGroup, Time)
  names(Full) <- c(GroupVar, NewTimeVar)
  Full <- Full[order(Full[, GroupVar], Full[, NewTimeVar]), ]
  Full <- merge(Full, Sub, by = GroupVar)

  # Create dummy variable
  Full[, NewVar] <- 0
  Full[, NewVar][Full[, StartVar] == Full[, NewTimeVar]] <- 1
  Full[, NewVar][Full[, EndVar] == Full[, NewTimeVar]] <- 1
  Full[, NewVar][Full[, StartVar] < Full[, NewTimeVar] & Full[, NewTimeVar] < Full[, EndVar]] <- 1

  # Drop unneeded duplicates
  Full <- Full[order(Full[, GroupVar], Full[, NewTimeVar], Full[, NewVar]), ]
  Full <- Full[!duplicated(Full[, c(GroupVar, NewTimeVar)], fromLast = TRUE), ]

  if (!isTRUE(KeepStartStop)){
    Full <- VarDrop(Full, c(StartVar, EndVar))
  }

  return(Full)
}
