\name{estimated.TTSI}
\alias{estimated.TTSI}
\title{Thyrotroph Thyroid Hormone Sensitivity Index (TTSI)}
\description{Calculate thyrotroph thyroid hormone sensitivity index (TTSI) from equilibrium TSH and free T4 concentrations.  }
\usage{
estimated.TTSI(TSH, FT4, lu)
}
\arguments{
  \item{TSH}{Thyrotropin (TSH) concentration in mIU/l}
  \item{FT4}{Free thyroxine (FT4) concentration in pmol/L}
  \item{lu}{upper limit of FT4 reference range (should be in the same unit of measurement as the FT4 concentration)}
}
\details{This function is able to do vectorised calculations.  }
\value{This function returns thyrotroph thyroid hormone sensitivity index (TTSI) as a numeric result representing a single value or a vector, depending on the vector length of the arguments. Results are without unit of measurement.  }
\references{
Pohlenz J, Weiss RE, Macchia PE, Pannain S, Lau IT, Ho H, Refetoff S. Five new families with resistance to thyroid hormone not caused by mutations in the thyroid hormone receptor beta gene. J Clin Endocrinol Metab. 1999 Nov;84(11):3919-28. PMID: 10566629.

Dietrich J. W., Landgrafe G., Fotiadou E. H. 2012 TSH and Thyrotropic Agonists: Key Actors in Thyroid Homeostasis. \emph{J Thyroid Res}. 2012;2012:351864. doi: 10.1155/2012/351864. PMID: 23365787; PMCID: PMC3544290.

Dietrich J. W., Landgrafe-Mende G., Wiora E., Chatzitomaris A., Klein H. H., Midgley J. E., Hoermann R. 2016 Calculated Parameters of Thyroid Homeostasis: Emerging Tools for  Differential Diagnosis and Clinical Research. \emph{Front Endocrinol (Lausanne)}. 2016 Jun 9;7:57. doi: 10.3389/fendo.2016.00057. PMID: 27375554; PMCID: PMC4899439.
}
\author{Johannes W. Dietrich}
\note{The software functions described in this document are intended for research
use only. Hormone levels should have been obtained simultaneously in order to avoid
bias by transition effects.}

\seealso{
\code{\link{SPINA.GT}}, \code{\link{estimated.GT}}, \code{\link{SPINA.GTT}}, \code{\link{estimated.GTT}}, \code{\link{SPINA.GD}}, \code{\link{estimated.GD}}, \code{\link{SPINA.GDTT}}, \code{\link{estimated.GDTT}}, \code{\link{SPINA.sGD}}, \code{\link{estimated.sGD}}, \code{\link{estimated.TSHI}}, \code{\link{estimated.sTSHI}}
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