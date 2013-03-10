\name{MoveFront}
\alias{MoveFront}
\title{Move a variable to the front of a data frame.}
\source{
  Based on a Stack Overflow answer written by rcs:
  <http://stackoverflow.com/questions/3369959/moving-columns-within-a-data-frame-without-retyping>
}
\usage{
  MoveFront(data, Var)
}
\arguments{
  \item{data}{a data frame object}

  \item{Var}{the variable you would like to move to the
  front of the data frame.}
}
\description{
  \code{MoveFront} moves a specified variable to the front
  of a data frame.
}
\examples{
# Create dummy data
A <- B <- C <- 1:50
OldOrder <- data.frame(A, B, C)

# Move C to front
NewOrder <- MoveFront(OldOrder, "C")
names(NewOrder)
}

