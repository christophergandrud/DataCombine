\name{StartEnd}
\alias{StartEnd}
\title{Find the starting and ending time points of a spell}
\usage{
StartEnd(data, SpellVar, GroupVar, SpellValue = NULL)
}
\arguments{
  \item{data}{a data frame object.}

  \item{SpellVar}{a character string naming the variable
  you would like to slide (create lag or lead).}

  \item{GroupVar}{a character string naming the variable
  grouping the units experiencing the spells. If
  \code{GroupVar = NULL} then .}

  \item{SpellValue}{a value indicating when a unit is in a
  spell. If \code{SpellValue = NULL} then any change in
  \code{Var}'s value will be treated as the start/end of a
  spell.}
}
\value{
a data frame with two new variables: \itemize{
\item{Spell_Start: }{The time period year of a given
spell.} \item{Spell_End: }{The end time period of a given
spell.} }
}
\description{
\code{StartEnd} finds the starting and ending time points
of a spell, including for time-series cross-sectional data.
Note: your data needs to be sorted by date. The date should
be ascending (i.e. increasing as it moves down the rows).
}
\examples{
# Create fake data
ID <- sort(rep(seq(1:4), 5))
Time <- rep(1:5, 4)
Dummy <-  sample(c(0, 1), size = 20, replace = TRUE)
Data <- data.frame(ID, Time, Dummy)

# Find start/end of spells denoted by Dummy = 1
DataSpell <- StartEnd(Data, SpellVar = 'Dummy', GroupVar = 'ID',
                     SpellValue = 1)

head(DataSpell)
}
\seealso{
\code{\link{slide}}
}

