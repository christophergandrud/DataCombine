\name{MoveFront}
\alias{MoveFront}
\title{Move variables to the front of a data frame.}
\source{
Based on a Stack Overflow answer written by rcs:
<http://stackoverflow.com/questions/3369959/moving-columns-within-a-data-frame-without-retyping>
}
\usage{
MoveFront(data, Var)
}
\arguments{
  \item{data}{a data frame object containing the variable
  you want to move.}

  \item{Var}{a character vector naming the variables you
  would like to move to the front of the data frame. The
  order of the variables should match the order you want
  them to have in the data frame, i.e. the first variable
  in the vector will be the first variable in the data
  frame.}
}
\description{
\code{MoveFront} moves variables to the front of a data
frame.
}
\examples{
# Create dummy data
A <- B <- C <- 1:50
OldOrder <- data.frame(A, B, C)

# Move C to front
NewOrder1 <- MoveFront(OldOrder, "C")
names(NewOrder1)

# Move B and A to the front
NewOrder2 <- MoveFront(OldOrder, c("B", "A"))
names(NewOrder2)
}

