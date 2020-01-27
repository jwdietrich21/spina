\name{SPINA.GTT}
\alias{SPINA.GTT}
\title{Calculated Thyroid's Secretory Capacity (SPINA-GT)}
\description{Calculate thyroid's secretory capacity (SPINA-GT) from equilibrium TSH and total T4 concentrations. This is an alias for \code{estimated.GTT}.  }
\usage{
SPINA.GTT(TSH, T4)
}
\arguments{
  \item{TSH}{Thyrotropin (TSH) concentration in mIU/l}
  \item{T4}{Total thyroxine (TT4) concentration in nmol/L}
}
\details{This function is able to do vectorised calculations.  }
\value{This function returns thyroid's secretory capacity as a numeric result representing a single value or a vector, depending on the vector length of the arguments. Results are in pmol/s.  }
\references{
Dietrich J. W., Landgrafe G., Fotiadou E. H. 2012 TSH and Thyrotropic Agonists: Key Actors in Thyroid Homeostasis. \emph{J Thyroid Res}. 2012;2012:351864. doi: 10.1155/2012/351864. PMID: 23365787; PMCID: PMC3544290.

Dietrich J. W., Landgrafe-Mende G., Wiora E., Chatzitomaris A., Klein H. H., Midgley J. E., Hoermann R. 2016 Calculated Parameters of Thyroid Homeostasis: Emerging Tools for  Differential Diagnosis and Clinical Research. \emph{Front Endocrinol (Lausanne)}. 2016 Jun 9;7:57. doi: 10.3389/fendo.2016.00057. PMID: 27375554; PMCID: PMC4899439.
}
\author{Johannes W. Dietrich}
\note{The software functions described in this document are intended for research
use only. Hormone levels should have been obtained simultaneously in order to avoid
bias by transition effects.}

\seealso{
\code{\link{SPINA.GT}}, \code{\link{estimated.GT}}, \code{\link{estimated.GTT}}, \code{\link{SPINA.GD}}, \code{\link{estimated.GD}}, \code{\link{SPINA.GDTT}}, \code{\link{estimated.GDTT}}, \code{\link{SPINA.sGD}}, \code{\link{estimated.sGD}}, \code{\link{estimated.TSHI}}, \code{\link{estimated.sTSHI}}, \code{\link{estimated.TTSI}}
}
\examples{
TSH <- c(1, 3.24, 0.7);
FT4 <- c(16.5, 7.7, 9);
FT3 <- c(4.5, 28, 6.2);
print(paste("GT^:", SPINA.GT(TSH, FT4)));
print(paste("GD^:", SPINA.GD(FT4, FT3)));
print(paste("sGD^:", SPINA.sGD(FT4, FT3)));
}
\keyword{SPINA}