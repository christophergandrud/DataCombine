\name{FillIn}
\alias{FillIn}
\title{A function for filling in missing values of a variable from one data frame with the values from another variable.}
\usage{
  FillIn(D1, D2, Var1, Var2 = NULL,
    KeyVar = c("iso2c", "year"), allow.cartesian = FALSE)
}
\arguments{
  \item{D1}{the data frame with the variable you would like
  to fill in.}

  \item{D2}{the data frame with the variable you would like
  to use to fill in \code{D1}.}

  \item{Var1}{a character string of the name of the
  variable in \code{D1} you want to fill in.}

  \item{Var2}{an optional character string of variable name
  in \code{D2} that you would like to use to fill in.}

  \item{KeyVar}{a character vector of variable names that
  are shared by \code{D1} and \code{D2} that can be used to
  join the data frames.}

  \item{allow.cartesian}{logical. See the
  \code{\link{data.table}} documentation for more details.}
}
\description{
  \code{FillIn} uses values of a variable from one data set
  to fill in missing values in another.
}
\examples{
# Create data set with missing values
naDF <- data.frame(a = sample(c(1,2), 100, rep=TRUE),
                   b = sample(c(3,4), 100, rep=TRUE),
                   fNA = sample(c(100, 200, 300, 400, NA), 100, rep=TRUE))

# Created full data set
fillDF <- data.frame(a = c(1,2,1,2),
                     b = c(3,3,4,4),
                     fFull = c(100, 200, 300, 400))

# Fill in missing f's from naDF with values from fillDF
FilledInData <- FillIn(naDF, fillDF, Var1 = "fNA", Var2 = "fFull", KeyVar = c("a", "b"))
}

