\name{DropNA}
\alias{DropNA}
\title{Drop rows from a data frame with missing values on a given variable(s).}
\source{
Partially based on Stack Overflow answer written by
donshikin:
http://stackoverflow.com/questions/4862178/remove-rows-with-nas-in-data-frame
}
\usage{
DropNA(data, Var, message = TRUE)
}
\arguments{
  \item{data}{a data frame object.}

  \item{Var}{a character vector naming the variables you
  would like to have only non-missing (NA) values.}

  \item{message}{logical. Whether or not to give you a
  message about the number of rows that are dropped.}
}
\description{
\code{DropNA} drops rows from a data frame when they have
missing (NA) values on a given variable(s).
}
\examples{
# Create data frame
a <- c(1, 2, 3, 4, NA)
b <- c( 1, NA, 3, 4, 5)
ABData <- data.frame(a, b)

# Remove missing values from column a
ASubData <- DropNA(ABData, Var = "a", message = FALSE)

# Remove missing values in columns a and b
ABSubData <- DropNA(ABData, Var = c("a", "b"))
}

