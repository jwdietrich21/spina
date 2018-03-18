\name{estimated.sTSHI}
\alias{estimated.sTSHI}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
estimated.sTSHI(TSH, FT4, mean = 2.7, sd = 0.676)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{TSH}{
%%     ~~Describe \code{TSH} here~~
}
  \item{FT4}{
%%     ~~Describe \code{FT4} here~~
}
  \item{mean}{
%%     ~~Describe \code{mean} here~~
}
  \item{sd}{
%%     ~~Describe \code{sd} here~~
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
function (TSH, FT4, mean = 2.7, sd = 0.676) 
{
    stshi <- (estimated.TSHI(TSH, FT4) - mean)/sd
    return(stshi)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
