#' Remove all objects from a workspace except those specified by the user.
#' 
#' \code{rmExcept} removes all objects from a workspace except those specified by the user.
#' 
#' @param keepers a character vector of the names of object you would like to keep in your workspace.
#' @param envir the \code{\link{environment}} to remove objects from. The default is the global environment (i.e. \code{\link{globalenv}}).
#' @param message logical, whether or not to return a message informing the user of which objects were removed.
#' 
#' @examples
#' # Create objects 
#' A <- 1; B <- 2; C <- 3
#' 
#' # Remove all objects except for A
#' rmExcept("A")
#' 
#' # Show workspace
#' ls()
#' 
#' @export

rmExcept <- function(keepers, envir = globalenv(), message = TRUE){
  DeleteObj <- setdiff(ls(envir = envir), keepers)
  rm(list = DeleteObj, envir = envir)
  if (isTRUE(message)){
    message(paste("Removed the following objects:"))
    message(paste(DeleteObj, collapse = ", "))
  }
} 


#' Subset a data frame if a specified pattern is found in a character string
#'
#' @param data data frame.
#' @param patterns character vector containing a regular expressions to be matched in the given character vector.
#' @param Var character vector of the variables that the pattern should be found in.
#' @param keep.found logical. whether or not to keep observations where the pattern is found (\code{TRUE}) or not found (\code{FALSE}).
#' @param useBytes logical. If TRUE the matching is done byte-by-byte rather than character-by-character. See \code{\link{grep}}.
#'
#' @examples
#' # Create data frame
#' ABData <- data.frame(a = c("London, UK", "Oxford, UK", "Berlin, DE", "Hamburg, DE", "Oslo, NO"), 
#'                      b = c(8, 0.1, 3, 2, 1))
#'
#' # Keep only data from Germany (DE)
#' ABGermany <- grepl.sub(data = ABData, patterns = "DE", Var = "a")
#'
#' @export
 
grepl.sub <- function(data, patterns, Var, keep.found = TRUE, useBytes = TRUE){
  y <- NULL
  data$y <- grepl(pattern = paste0(patterns, collapse="|"), x = data[, Var], useBytes = useBytes)
  subdata <- subset(data, y == keep.found)
  subdata$y <- NULL
  subdata
}

#' Replace multiple patterns found in a character string column of a data frame
#' 
#' \code{FindReplace} allows you to find and replace multiple character string patterns in a data frame's column.
#' 
#' @param data data frame with the column you would like to replace string patterns.
#' @param Var character string naming the column you would like to replace string patterns. The column must be of class \code{character} or \code{factor}.
#' @param replaceData a data frame with at least two columns. One contains the patterns to replace and the other contains their replacement. Note: the pattern and its replacement must be in the same row.
#' @param from character string naming the column with the patterns you would like to replace.
#' @param to character string naming the column with the the pattern replacements.
#' @param exact logical. Indicates whether to only replace exact pattern matches (\code{TRUE}) or not (\code{FALSE}).
#' @param vector logical. If \code{TRUE} then the replacement is returned as a single vector. If \code{FALSE} then the whole data frame is returned.
#' 
#' @examples
#' # Create original data
#' ABData <- data.frame(a = c("London, UK", "Oxford, UK", "Berlin, DE", "Hamburg, DE", "Oslo, NO"), 
#'                      b = c(8, 0.1, 3, 2, 1))
#' 
#' # Create replacements data frame                     
#' Replaces <- data.frame(from = c("UK", "DE"), to = c("England", "Germany"))
#' 
#' # Replace patterns and return full data frame
#' ABNewDF <- FindReplace(data = ABData, Var = "a", replaceData = Replaces, 
#'                      from = "from", to = "to", exact = FALSE)
#'                      
#' # Replace patterns and return the Var as a vector
#' ABNewVector <- FindReplace(data = ABData, Var = "a", replaceData = Replaces, 
#'                      from = "from", to = "to", vector = TRUE)
#' 
#' @export

FindReplace <- function(data, Var, replaceData, from, to, exact = TRUE, vector = FALSE){
  if(!(class(data[, Var]) %in% c('character', 'factor'))){
    stop(paste(Var, 'is not a character string or factor. Please convert to a character string or factor and then rerun.'))
  }
  ReplaceNRows <- nrow(replaceData)
  
  for (i in 1:ReplaceNRows){
    if(isTRUE(exact)){
      data[, Var] <- gsub(pattern = paste0("^", replaceData[i, from], "$"), replacement = replaceData[i, to], data[, Var])
    }
    if(!isTRUE(exact)){
      data[, Var] <- gsub(pattern = replaceData[i, from], replacement = replaceData[i, to], data[, Var])
    }
  }
  if(isTRUE(vector)){
    data <- data[, Var]
  }
  return(data)
}

#' Drop one or more variables from a data frame.
#' 
#' \code{VarDrop} drops one or more variables from a data frame.
#' 
#' @param data a data frame.
#' @param Var character vector containing the names of the variables to drop.
#' 
#' @examples
#' # Create dummy data
#' a <- c(1, 2, 3, 4, NA)
#' b <- c( 1, NA, 3, 4, 5) 
#' c <- c(1:5)
#' d <- c(1:5)
#' ABCData <- data.frame(a, b, c, d)
#'  
#' # Drop a and b
#' DroppedData <- VarDrop(ABCData, c('b', 'c'))  
#' 
#' @export

VarDrop <- function(data, Var){
  if (!all(Var %in% names(data))){
    message('At least one of the specified variables to drop is not found.')
  }
  data <- data[, !(names(data) %in% Var)]
  return(data)
}

#' Merges 2 data frames and report/drop/keeps only duplicates.
#' 
#' \code{dMerge} merges 2 data frames and reports/drops/keeps only duplicates.
#' 
#' @param data1 a data frame. The first data frame to merge.
#' @param data2 a data frame. The second data frame to merge.
#' @param Var character vector containing the names of the variables merge by. See \code{\link{merge}}.
#' @param dropDups logical. Whether or not to drop duplicated rows based on \code{Var}. If \code{dropDups = FALSE} then it gives a count of the duplicated rows.
#' @param dupsOut logical. If \code{TRUE} then a data frame only containing duplicated values is returned and \code{dropDups} is ignored.
#' @param fromLast logical indicating if duplication should be considered from the reverse side, i.e., the last identical elements would correspond to \code{duplicated = FALSE}. Only relevant if \code{dropDups = TRUE}.
#' @param all logical; all = L is shorthand for all.x = L and all.y = L, where L is either TRUE or FALSE.
#' @param all.x logical; if TRUE, then extra rows will be added to the output, one for each row in x that has no matching row in y. These rows will have NAs in those columns that are usually filled with values from y. The default is FALSE, so that only rows with data from both x and y are included in the output.
#' @param all.y logical; analogous to all.x.
#' @param sort logical. Should the result be sorted on the by columns?
#' @param suffixes	a character vector of length 2 specifying the suffixes to be used for making unique the names of columns in the result which not used for merging (appearing in by etc).
#' @param incomparables	values which cannot be matched. See \code{\link{match}}.
#'  
#' 
#' @seealso \code{\link{duplicated}}, \code{\link{merge}}
#' 
#' @export

dMerge <- function(data1, data2, Var, dropDups = TRUE, dupsOut = FALSE, fromLast = FALSE, all = FALSE, all.x = all, all.y = all,
      sort = TRUE, suffixes = c(".x",".y"),
      incomparables = NULL){
  if (isTRUE(dropDups) & isTRUE(dupsOut)){
    message("dropDups ignored")
    dropDups = FALSE
  }

  # Perform basic merge
  Comb <- merge(data1, data2, by = Var, all = all, all.x = all.x, all.y = all.y,
      sort = TRUE, suffixes = c(".x",".y"),
      incomparables = incomparables)

  # Find duplicated 
  DupDF <- Comb[duplicated(Comb[, Var]), ]

  # Inform user of duplicates and drop if requested
  if (is.null(DupDF)){
    message('There are no duplicated rows.')
    if (isTRUE(dupsOut)){
      message('Full data set returned.')
    }
  } 
  else if (!is.null(DupDF)){
    Count <- nrow(DupDF)
    if (!isTRUE(dropDups) & !isTRUE(dupsOut)){
      message(paste('There are', Count, 'duplicated rows.'))
    }
    else if (isTRUE(dropDups) & !isTRUE(dupsOut)){
      Comb <- Comb[!duplicated(Comb[, Var], fromLast = fromLast), ]
      message(paste(Count, 'duplicated rows were dropped.'))
    }
    else if (!isTRUE(dropDups) & isTRUE(dupsOut)){
      message(paste('There are', Count, 'duplicated rows.'))
      Comb <- DupDF
    }
  }
  return(Comb)
}

