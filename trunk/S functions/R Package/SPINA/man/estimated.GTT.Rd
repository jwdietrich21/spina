\name{estimated.GTT}
\alias{estimated.GTT}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
estimated.GTT(TSH, T4)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{TSH}{
%%     ~~Describe \code{TSH} here~~
}
  \item{T4}{
%%     ~~Describe \code{T4} here~~
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
function (TSH, T4) 
{
    pico.factor <- 1e+12
    nano.factor <- 1e+09
    alphaT <- 0.1
    betaT <- 1.1e-06
    DT <- 2.75
    TSH[which(TSH == 0)] = NA
    T4 <- T4/nano.factor
    GT <- betaT * (DT + TSH) * T4/(alphaT * TSH) * pico.factor
    return(GT)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
