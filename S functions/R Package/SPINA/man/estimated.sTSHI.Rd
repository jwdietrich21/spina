\name{estimated.sTSHI}
\alias{estimated.sTSHI}
\title{Jostels TSH index in standardised form}
\description{Calculate Jostels TSH index in standardised form resulting from z-transformation.  }
\usage{
estimated.sTSHI(TSH, FT4, mean = 2.7, sd = 0.676)
}
\arguments{
  \item{TSH}{Thyrotropin (TSH) concentration in mIU/l}
  \item{FT4}{Free thyroxine (FT4) concentration in pmol/L}
  \item{mean}{mean value of population sample for standardised (z-transformed)
tests}
  \item{sd}{standard deviation of population sample for standardised (ztransformed)
tests}
}
\details{This function is able to do vectorised calculations.  }
\value{his function returns Jostel's TSH index as a numeric result in standardised form representing a single value or a vector, depending on the vector length of the arguments. Results are z-transformed and therefore without unit of measurement.  }
\references{
Jostel A, Ryder WD, Shalet SM. The use of thyroid function tests in the diagnosis of hypopituitarism: definition and evaluation of the TSH Index. Clin Endocrinol (Oxf). 2009 Oct;71(4):529-34. doi: 10.1111/j.1365-2265.2009.03534.x. PMID: 19226261.

Dietrich J. W., Landgrafe G., Fotiadou E. H. 2012 TSH and Thyrotropic Agonists: Key Actors in Thyroid Homeostasis. \emph{J Thyroid Res}. 2012;2012:351864. doi: 10.1155/2012/351864. PMID: 23365787; PMCID: PMC3544290.

Dietrich J. W., Landgrafe-Mende G., Wiora E., Chatzitomaris A., Klein H. H., Midgley J. E., Hoermann R. 2016 Calculated Parameters of Thyroid Homeostasis: Emerging Tools for  Differential Diagnosis and Clinical Research. \emph{Front Endocrinol (Lausanne)}. 2016 Jun 9;7:57. doi: 10.3389/fendo.2016.00057. PMID: 27375554; PMCID: PMC4899439.
}
\author{Johannes W. Dietrich}
\note{The software functions described in this document are intended for research
use only. Hormone levels should have been obtained simultaneously in order to avoid
bias by transition effects.}

\seealso{
\code{\link{SPINA.GT}}, \code{\link{estimated.GT}}, \code{\link{SPINA.GTT}}, \code{\link{estimated.GTT}}, \code{\link{SPINA.GD}}, \code{\link{estimated.GD}}, \code{\link{SPINA.GDTT}}, \code{\link{estimated.GDTT}}, \code{\link{SPINA.sGD}}, \code{\link{estimated.sGD}}, \code{\link{estimated.TSHI}}, \code{\link{estimated.TTSI}}
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