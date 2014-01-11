\name{rmExcept}
\alias{rmExcept}
\title{Remove all objects from a workspace except those specified by the user.}
\usage{
rmExcept(keepers, envir = globalenv(), message = TRUE)
}
\arguments{
  \item{keepers}{a character vector of the names of object
  you would like to keep in your workspace.}

  \item{envir}{the \code{\link{environment}} to remove
  objects from. The default is the global environment (i.e.
  \code{\link{globalenv}}).}

  \item{message}{logical, whether or not to return a
  message informing the user of which objects were
  removed.}
}
\description{
\code{rmExcept} removes all objects from a workspace except
those specified by the user.
}
\examples{
# Create objects
A <- 1; B <- 2; C <- 3

# Remove all objects except for A
rmExcept("A")

# Show workspace
ls()
}

