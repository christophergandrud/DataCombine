\name{FindReplace}
\alias{FindReplace}
\title{Replace multiple patterns found in a character string column of a data frame}
\usage{
  FindReplace(data, Var, replaceData, from, to)
}
\arguments{
  \item{data}{data frame with the column you would like to
  replace string patterns.}

  \item{Var}{character string naming the column you would
  like to replace string patterns.}

  \item{replaceData}{a data frame with at least two
  columns. One contains the patterns to replace and the
  other contains their replacement. Note: the pattern and
  its replacement must be in the same row.}

  \item{from}{character string naming the column with the
  patterns you would like to replace.}

  \item{to}{character string naming the column with the the
  pattern replacements.}
}
\description{
  \code{FindReplace} allows you to find and replace
  multiple character string patterns in a data frame's
  column.
}
\examples{
# Create original data
ABData <- data.frame(a = c("London, UK", "Oxford, UK", "Berlin, DE", "Hamburg, DE", "Oslo, NO"),
                     b = c(8, 0.1, 3, 2, 1))

# Create replacements data frame
Replaces <- data.frame(from = c("UK", "DE"), to = c("England", "Germany"))

# Replace patterns
ABNew <- FindReplace(data = ABData, Var = "a", replaceData = Replaces,
                     from = "from", to = "to")
}

