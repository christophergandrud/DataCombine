\name{grepl.sub}
\alias{grepl.sub}
\title{Subset a data frame if a specified pattern is found in a character string}
\usage{
  grepl.sub(data, patterns, Var, keep.found = TRUE,
    useBytes = TRUE)
}
\arguments{
  \item{data}{data frame.}

  \item{patterns}{character vector containing a regular
  expressions to be matched in the given character vector.}

  \item{Var}{character vector of the variables that the
  pattern should be found in.}

  \item{keep.found}{logical. whether or not to keep
  observations where the pattern is found (\code{TRUE}) or
  not found (\code{FALSE}).}

  \item{useBytes}{logical. If TRUE the matching is done
  byte-by-byte rather than character-by-character. See
  \code{\link{grep}}.}
}
\description{
  Subset a data frame if a specified pattern is found in a
  character string
}
\examples{
# Create data frame
a <- c("London, UK", "Oxford, UK", "Berlin, DE", "Hamburg, DE", "Oslo, NO")
b <- c(8, 0.1, 3, 2, 1)
ABData <- data.frame(a, b)

# Keep only data from Germany (DE)
ABGermany <- grepl.sub(data = ABData, patterns = "DE", Var = "a")
}

