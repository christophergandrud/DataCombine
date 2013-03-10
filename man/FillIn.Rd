\name{FillIn}
\alias{FillIn}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
FillIn(D1, D2, Var1, Var2 = NULL, KeyVar = c("iso2c", "year"))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{D1}{
%%     ~~Describe \code{D1} here~~
}
  \item{D2}{
%%     ~~Describe \code{D2} here~~
}
  \item{Var1}{
%%     ~~Describe \code{Var1} here~~
}
  \item{Var2}{
%%     ~~Describe \code{Var2} here~~
}
  \item{KeyVar}{
%%     ~~Describe \code{KeyVar} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (D1, D2, Var1, Var2 = NULL, KeyVar = c("iso2c", "year")) 
{
    if (is.null(Var2)) {
        Var2 <- Var1
    }
    else {
        Var2 <- Var2
    }
    names(D1)[match(Var1, names(D1))] <- "VarGen"
    names(D2)[match(Var2, names(D2))] <- "VarGen.1"
    D1Temp <- data.table::data.table(D1, key = KeyVar)
    D2Temp <- data.table::data.table(D2, key = KeyVar)
    OutDT <- D2Temp[D1Temp]
    SubNA <- OutDT[, list(VarGen, VarGen.1)]
    SubNA <- subset(SubNA, is.na(VarGen) & !is.na(VarGen.1))
    print(paste(nrow(SubNA), "NAs were replaced."))
    OutDT <- OutDT[is.na(VarGen), `:=`(VarGen, VarGen.1)]
    OutDF <- data.frame(OutDT)
    SubNoNA <- subset(OutDF, !is.na(VarGen) & !is.na(VarGen.1))
    HowMany <- nrow(SubNoNA)
    CORR <- cor(SubNoNA$VarGen, SubNoNA$VarGen.1, use = "complete.obs")
    print(paste("The correlation between", Var1, "and", Var2, 
        "is", round(CORR, digits = 3), "based on", HowMany, "shared observations."))
    names(OutDF)[match("VarGen", names(OutDF))] <- Var1
    Keepers <- setdiff(names(OutDF), "VarGen.1")
    OutDF <- OutDF[, Keepers]
    OutDF
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
