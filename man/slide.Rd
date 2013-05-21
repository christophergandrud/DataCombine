\name{slide}
\alias{slide}
\title{A function for creating lag and lead variables, including for time-series cross-sectional data.}
\source{
  Partially based on TszKin Julian's \code{shift} function:
  http://ctszkin.com/2012/03/11/generating-a-laglead-variables/
}
\usage{
  slide(data, Var, GroupVar = NULL, NewVar = NULL,
    shiftBy = -1)
}
\arguments{
  \item{data}{a data frame object.}

  \item{Var}{a character string naming the variable you
  would like to slide (create lag or lead).}

  \item{GroupVar}{a character string naming the variable
  grouping the units within which \code{Var} will be slid.
  If \code{GroupVar = NULL} then the whole variable is slid
  up or down.}

  \item{NewVar}{a character string specifying the name for
  the new variable to place the slid data in.}

  \item{shiftBy}{numeric value specifying how many rows
  (time units) to shift the data by. Negative values slide
  the data down--lag the data. Positive values shift the
  data up--lead the data.}
}
\description{
  The function slides a column up or down to create lag or
  lead variables. If \code{GroupVar} is specified it will
  slide \code{Var} for each group. This is important for
  time-series cross-section data where The slid data is
  placed in a new variable. in the original data frame.
  Note: your data needs to be sorted by date. The date
  should be ascending (i.e. increasing as it moves down the
  rows). Also, the time difference between rows should be
  constant, e.g. days, months, years.
}
\details{
  \code{slide} a function for creating lag and lead
  variables, including for time-series cross-sectional
  data.
}
\examples{
# Create dummy data
 A <- B <- C <- 1:20
 ID <- sort(rep(seq(1:4), 5))
 Data <- data.frame(ID, A, B, C)

 # Lead the variable by two time units
 DataSlid1 <- slide(Data, Var = "A", NewVar = "ALag", shiftBy = 2)

 # Lag the variable one time unit by ID group
 DataSlid2 <- slide(data = Data, Var = "B", GroupVar = "ID",
                NewVar = "BLag", shiftBy = -1)
}
\seealso{
  \code{\link{shift}}, \code{\link{ddply}}
}

