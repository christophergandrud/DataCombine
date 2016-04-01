#' Create reports cases after listwise deletion of missing values for 
#' time-series cross-sectional data.
#' 
#' @param data a data frame with the full sample.
#' @param GroupVar a character string specifying the variable in \code{data} 
#' which contains the group IDs.
#' @param TimeVar an optional character string specifying the variable in 
#' \code{data} which contains the time variable.
#' @param Vars a character vector with variables names from \code{data}
#' for which you would like to listwise delete observations with missing values.
#' 
#' @return If \code{TimeVar} is specified then a data frame is returned with 
#' three colums. One identifying the \code{GroupVar} and two others specifying 
#' each unique value of \code{GroupVar}'s first and last observation time 
#' post-listwise deletion of missing values. 
#' 
#' If \code{TimeVar} is not specified, then a vector of unique \code{GroupVar}
#' post-listwise deletion of missing values is returned.
#' 
#' @examples
#' # Create dummy data
#' ID <- rep(1:4, 4)
#' time <- rep(2000:2003, 4)
#' a <- rep(c(1:3, NA), 4)
#' b <- rep(c(1, NA, 3:4), 4)
#' Data <- data.frame(ID, time, a, b)
#' 
#' # Find cases that have not been listwise deleted
#' CasesTable(Data, GroupVar = 'ID')
#' CasesTable(Data, GroupVar = 'ID', Vars = 'a')
#' CasesTable(Data, GroupVar = 'ID', TimeVar = 'time', Vars = 'a')
#' 
#' 
#' @importFrom dplyr %>% group_by_ summarise
#' @export

CasesTable <- function(data, GroupVar, TimeVar, Vars) {

    if (missing(GroupVar)) stop('GroupVar must be specified.', call. = FALSE)
    
    if ("data.table" %in% class(data))
        stop(paste("CasesTable does not support data.tables with Grouped variables.\n",
                   "Convert to data.frame and try again."), call. = FALSE)
    
    if (!missing(GroupVar) & "tbl_df" %in% class(data)) {
        message("Converting to plain data frame from tbl_df.")
        data <- as.data.frame(data)
    }
    
    if (!missing(Vars)) {
        # Determine if Vars exists in data
        DataNames <- names(data)
        TestExist <- Vars %in% DataNames
        
        if (!isTRUE(TestExist)) {
            stop(paste(Vars, "was not found in the data frame."),
                 call. = FALSE)
        }
        if (missing(TimeVar)) {
            keep_vars <- c(GroupVar, Vars)
        }
        else if (!missing(TimeVar)) {
            keep_vars <- c(GroupVar, TimeVar, Vars)
        } 
        data <- data[, keep_vars]
    }
    
    complete <- DropNA(data, message = FALSE)
    
    if (missing(TimeVar)) {
        the_sample <- unique(complete[, GroupVar])
    }
    else if (!(missing(TimeVar))) {
        complete <- complete %>% group_by_(.dots = GroupVar)
        the_sample <- eval(parse(text = 
            paste('summarise(complete, 
                  begin = min(', TimeVar, '),
                  end = max(', TimeVar, '))'
            )
        ))
        the_sample <- as.data.frame(the_sample)
    }
    return(the_sample)
}