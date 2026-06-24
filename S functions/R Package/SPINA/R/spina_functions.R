##########################################
# SPINA functions
# Calculate structure parameters of
# thyroid and insulin-glucose homeostasis
# in S, including the implementation in R
# Version 5.1.0 (Cyclone)
# Last Change 20260621 by J. W. D.
##########################################

Insulin.conversion.factor <- 6;  # A Voelund 1993, L. Heinemann 2010
Glucose.conversion.factor <- 18; # derived from molar mass

#' Data for SPINA
#'
#' Pilo: Data from Pilo et al. (1990)
#'
#' @name Pilo
#' @docType data
#' @format ## "Pilo"
#' A data frame with 14 rows and 33 columns
#' \describe{
#'  \item{Sex}{Sex (f: female; m: male)}
#'  \item{Age}{Age in years}
#' }
#'
#' @source
#' Dietrich JW, Landgrafe-Mende G, Wiora E, Chatzitomaris A, Klein HH, Midgley JE, Hoermann R. Calculated Parameters of Thyroid Homeostasis: Emerging Tools for Differential Diagnosis and Clinical Research. Front Endocrinol (Lausanne). 2016 Jun 9;7:57. doi 10.3389/fendo.2016.00057. PMID 27375554; PMCID PMC4899439.
#'
#' Pilo A, Iervasi G, Vitek F, Ferdeghini M, Cazzuola F, Bianchi R. Thyroidal and
#' peripheral production of 3,5,3'-triiodothyronine in humans by multicompartmental
#' analysis. Am J Physiol. 1990 Apr;258(4 Pt 1):E715-26. PMID 2333963.
NULL

#' Data for SPINA
#'
#' Vellore: Data from Dietrich et al. (2022 and 2024)
#'
#' @name Vellore
#' @docType data
#' @format ## "Vellore"
#' A data frame with 117 rows and 14 columns
#' \describe{
#'  \item{Age}{Age in years}
#' }
#'
#' @source
#' Dietrich JW, Dasgupta R, Anoop S, Jebasingh F, Kurian ME, Inbakumari M, Boehm BO,
#' Thomas N. SPINA Carb: a simple mathematical model supporting fast in-vivo estimation
#' of insulin sensitivity and beta cell function. Sci Rep. 2022 Oct 21;12(1):17659.
#' doi 10.1038/s41598-022-22531-3. PMID 36271244; PMCID PMC9587026.
#'
#' Dietrich JW, Abood A, Dasgupta R, Anoop S, Jebasingh FK, Spurgeon R, Thomas N,
#' Boehm BO. A novel simple disposition index (SPINA-DI) from fasting insulin and
#' glucose concentration as a robust measure of carbohydrate homeostasis. J Diabetes.
#' 2024 Jan 2. doi: 10.1111/1753-0407.13525. PMID: 38169110.
NULL

#' Calculated secretory capacity of pancreatic beta cells (SPINA-GBeta)
#'
#' @param Insulin Insulin concentration in pmol/L
#' @param Glucose Glucose concentration in mmol/L
#'
#' @returns Returns the secretory capacity of pancreatic beta cells
#' (SPINA-GBeta) as a numeric result representing a single value or a vector,
#' depending on the vector length of the arguments. Results are in pmol/s.
#' @export
#'
#' @examples
#' SPINA.GBeta(63.01, 4.34)
#' @author Johannes W. Dietrich, Bernhard O. Boehm
#' @details This function is able to do vectorised calculations.
#' @references
#' Dietrich JW, Dasgupta R, Anoop S, Jebasingh F, Kurian ME, Inbakumari M, Boehm BO, Thomas N. SPINA Carb: a simple mathematical model supporting fast in-vivo estimation of insulin sensitivity and beta cell function. Sci Rep. 2022 Oct 21;12(1):17659. doi: 10.1038/s41598-022-22531-3. PMID: 36271244; PMCID: PMC9587026.
#'
#' Dietrich JW, Abood A, Dasgupta R, Anoop S, Jebasingh FK, Spurgeon R, Thomas N, Boehm BO. A novel simple disposition index (SPINA-DI) from fasting insulin and glucose concentration as a robust measure of carbohydrate homeostasis. J Diabetes. 2024 Sep;16(9):e13525. doi: 10.1111/1753-0407.13525. Epub 2024 Jan 2. PMID: 38169110; PMCID: PMC11418405.
#' @note
#' The software functions described in this document are intended for research use only.
#' Hormone or metabolite concentrations should have been obtained simultaneously in order to
#' avoid bias by transition effects.

SPINA.GBeta <- function(Insulin, Glucose)
  # Insulin expected in pmol/l, Glucose in mmol/l
{
  pico.factor <- 1e12;
  mili.factor <- 1e3
  betaI <- 3.4e-3;
  alphaI <- 0.2;
  dBeta <- 7e-3;
  GBeta <- pico.factor * betaI * Insulin / pico.factor * (dBeta + Glucose /
                                                            mili.factor) / (alphaI * Glucose / mili.factor);
  return(GBeta);
}

#' Calculated insulin receptor gain (SPINA-GR)
#'
#' @param Insulin Insulin concentration in pmol/L
#' @param Glucose Glucose concentration in mmol/L
#'
#' @returns Returns the insulin receptor gain (SPINA-GR), a
#' calculated biomarker of insulin sensitivity as a numeric result representing
#' a single value or a vector, depending on the vector length of the arguments.
#' Results are in mol/s.
#' @export
#'
#' @examples
#' SPINA.GR(63.01, 4.34)
#' @author Johannes W. Dietrich, Bernhard O. Boehm
#' @details This function is able to do vectorised calculations.
#' @references
#' Dietrich JW, Dasgupta R, Anoop S, Jebasingh F, Kurian ME, Inbakumari M, Boehm BO, Thomas N. SPINA Carb: a simple mathematical model supporting fast in-vivo estimation of insulin sensitivity and beta cell function. Sci Rep. 2022 Oct 21;12(1):17659. doi: 10.1038/s41598-022-22531-3. PMID: 36271244; PMCID: PMC9587026.
#'
#' Dietrich JW, Abood A, Dasgupta R, Anoop S, Jebasingh FK, Spurgeon R, Thomas N, Boehm BO. A novel simple disposition index (SPINA-DI) from fasting insulin and glucose concentration as a robust measure of carbohydrate homeostasis. J Diabetes. 2024 Sep;16(9):e13525. doi: 10.1111/1753-0407.13525. Epub 2024 Jan 2. PMID: 38169110; PMCID: PMC11418405.
#' @note
#' The software functions described in this document are intended for research use only.
#' Hormone or metabolite concentrations should have been obtained simultaneously in order to
#' avoid bias by transition effects.

SPINA.GR <- function(Insulin, Glucose)
  # Insulin in pmol/l, Glucose in mmol/l
{
  pico.factor <- 1e12;
  mili.factor <- 1e3;
  alphaG <- 0.11;
  betaG <- 7.1e-4;
  P0 <- 150e-6;
  DR <- 1.6e-9;
  GE <- 50;
  GR <- alphaG * P0 * (DR + Insulin / pico.factor) / (betaG * GE * Insulin /
                                                        pico.factor * Glucose / mili.factor) - DR / (GE * Insulin / pico.factor)
  - 1 / GE;
  return(GR);
}

#' Static disposition index (SPINA-DI)
#'
#' @param Insulin Insulin concentration in pmol/L
#' @param Glucose Glucose concentration in mmol/L
#'
#' @returns Returns the static disposition index (SPINA-DI), a
#' representation of the loop gain of glucose homeostasis as a numeric result representing
#' a single value or a vector, depending on the vector length of the arguments.
#' @export
#'
#' @examples
#' SPINA.DI(63.01, 4.34)
#' @author Johannes W. Dietrich, Bernhard O. Boehm
#' @details This function is able to do vectorised calculations.
#' @references
#' Dietrich JW, Dasgupta R, Anoop S, Jebasingh F, Kurian ME, Inbakumari M, Boehm BO, Thomas N. SPINA Carb: a simple mathematical model supporting fast in-vivo estimation of insulin sensitivity and beta cell function. Sci Rep. 2022 Oct 21;12(1):17659. doi: 10.1038/s41598-022-22531-3. PMID: 36271244; PMCID: PMC9587026.
#'
#' Dietrich JW, Abood A, Dasgupta R, Anoop S, Jebasingh FK, Spurgeon R, Thomas N, Boehm BO. A novel simple disposition index (SPINA-DI) from fasting insulin and glucose concentration as a robust measure of carbohydrate homeostasis. J Diabetes. 2024 Sep;16(9):e13525. doi: 10.1111/1753-0407.13525. Epub 2024 Jan 2. PMID: 38169110; PMCID: PMC11418405.
#' @note
#' The software functions described in this document are intended for research use only.
#' Hormone or metabolite concentrations should have been obtained simultaneously in order to
#' avoid bias by transition effects.

SPINA.DI <- function(Insulin, Glucose) # Insulin in pmol/l, Glucose in mmol/l
{
  DI <- SPINA.GBeta(Insulin, Glucose) * SPINA.GR(Insulin, Glucose);
  return(DI);
}

#' Homeostasis model assessment: insulin resistance (HOMA-IR)
#'
#' @param Insulin Insulin concentration in pmol/L
#' @param Glucose Glucose concentration in mmol/L
#'
#' @returns Returns the HOMA-IR index, a calculated biomarker for insulin resistance.
#' @export
#'
#' @examples
#' HOMA.IR(63.01, 4.34)
#' @author Johannes W. Dietrich, Bernhard O. Boehm
#' @details This function is able to do vectorised calculations.
#' @note
#' The software functions described in this document are intended for research use only.
#' Hormone or metabolite concentrations should have been obtained simultaneously in order to
#' avoid bias by transition effects.

HOMA.IR <- function(Insulin, Glucose) # Insulin in pmol/l, Glucose in mmol/l
{
  IR <- Glucose * Insulin / Insulin.conversion.factor / 22.5;
  return(IR);
}

#' Homeostasis model assessment: beta-cell function (HOMA-Beta)
#'
#' @param Insulin Insulin concentration in pmol/L
#' @param Glucose Glucose concentration in mmol/L
#'
#' @returns Returns the HOMA-Beta index, a calculated biomarker for beta-cell function
#' @export
#'
#' @examples
#' HOMA.Beta(63.01, 4.34)
#' @author Johannes W. Dietrich, Bernhard O. Boehm
#' @details This function is able to do vectorised calculations.
#' @note
#' The software functions described in this document are intended for research use only.
#' Hormone or metabolite concentrations should have been obtained simultaneously in order to
#' avoid bias by transition effects.

HOMA.Beta <- function(Insulin, Glucose) # Insulin in pmol/l, Glucose in mmol/l
{
  Beta <- rep(NA, times = length(Insulin));
  Beta[which(Glucose > 3.5)] <- 20 * Insulin[which(Glucose > 3.5)] / Insulin.conversion.factor / (Glucose[which(Glucose > 3.5)] - 3.5);
  return(Beta);
}

#' Homeostasis model assessment: insulin sensitivity (HOMA-IS)
#'
#' @param Insulin Insulin concentration in pmol/L
#' @param Glucose Glucose concentration in mmol/L
#'
#' @returns Returns the HOMA-IS index, a calculated biomarker for insulin sensitivity
#' @export
#'
#' @examples
#' HOMA.IS(63.01, 4.34)
#' @author Johannes W. Dietrich, Bernhard O. Boehm
#' @details This function is able to do vectorised calculations.
#' @note
#' The software functions described in this document are intended for research use only.
#' Hormone or metabolite concentrations should have been obtained simultaneously in order to
#' avoid bias by transition effects.

HOMA.IS <- function(Insulin, Glucose) # Insulin in pmol/l, Glucose in mmol/l
{
  IS <- 1 / HOMA.IR(Insulin, Glucose);
  return(IS);
}

#' Quantitative insulin-sensitivity check index (QUICKI)
#'
#' @param Insulin Insulin concentration in pmol/L
#' @param Glucose Glucose concentration in mmol/L
#'
#' @returns Returns the QUICKI index, a calculated biomarker for insulin sensitivity
#' @export
#'
#' @examples
#' QUICKI(63.01, 4.34)
#' @author Johannes W. Dietrich, Bernhard O. Boehm
#' @details This function is able to do vectorised calculations.
#' @note
#' The software functions described in this document are intended for research use only.
#' Hormone or metabolite concentrations should have been obtained simultaneously in order to
#' avoid bias by transition effects.

QUICKI <- function(Insulin, Glucose) # Insulin in pmol/l, Glucose in mmol/l
{
  QUICKI <- 1 / (log10(Insulin / Insulin.conversion.factor) + log10(Glucose * Glucose.conversion.factor));
  return(QUICKI);
}

#' Calculated thyroid's secretory capacity (SPINA-GT)
#'
#' @param TSH thyrotropin concentration in mIU/l
#' @param FT4 free T4 concentration in pmol/l
#'
#' @returns Returns SPINA-GT, a calculated biomarker for thyroid's secretory capacity (aka thyroid output)
#' @export
#'
#' @examples
#' estimated.GT(1.0, 16.4)
#' @author Johannes W. Dietrich
#' @details This function is able to do vectorised calculations.
#' @references
#' Dietrich JW, Landgrafe G, Fotiadou EH. TSH and Thyrotropic Agonists: Key Actors in Thyroid Homeostasis. J Thyroid Res. 2012;2012:351864. doi: 10.1155/2012/351864. Epub 2012 Dec 30. PMID: 23365787; PMCID: PMC3544290.
#'
#' Dietrich JW, Landgrafe-Mende G, Wiora E, Chatzitomaris A, Klein HH, Midgley JE, Hoermann R. Calculated Parameters of Thyroid Homeostasis: Emerging Tools for Differential Diagnosis and Clinical Research. Front Endocrinol (Lausanne). 2016 Jun 9;7:57. doi: 10.3389/fendo.2016.00057. PMID: 27375554; PMCID: PMC4899439.
#' @note
#' The software functions described in this document are intended for research use only.
#' Hormone concentrations should have been obtained simultaneously in order to
#' avoid bias by transition effects.

estimated.GT <- function(TSH, FT4)
  # TSH in mU/l, FT4 in pmol/l
{
  pico.factor <- 1e12;
  alphaT <- 0.1;
  betaT <- 1.1e-6;
  DT <- 2.75;
  TBG <- 3e-7;
  TBPA <- 4.5e-6;
  k41 <- 2e10;
  k42 <- 2e8;
  TSH[which(TSH == 0)] = NA;
  FT4 <- FT4 / pico.factor;
  TT4 <- (1 + k41 * TBG + k42 * TBPA) * FT4;
  GT <- betaT * (DT + TSH) * TT4 / (alphaT * TSH) * pico.factor;
  return(GT);
}

estimated.GD <- function(FT4, FT3)
  # FT4 and FT3 in pmol/l
{
  pico.factor <- 1e12;
  nano.factor <- 1e9;
  TBG <- 3e-7;
  k30 <- 2e9;
  alpha31 <- 0.026;
  beta31 <- 8e-6;
  kM1 <- 5e-7;
  FT4[which(FT4 == 0)] = NA;
  FT3 <- FT3 / pico.factor;
  FT4 <- FT4 / pico.factor;
  TT3 <- (1 + k30 * TBG) * FT3;
  GD <- beta31 * (kM1 + FT4) * TT3 / (alpha31 * FT4) * nano.factor;
  return(GD);
}

estimated.GTT <- function(TSH, T4)
  # TSH in mU/l, T4 in nmol/l
{
  pico.factor <- 1e12;
  nano.factor <- 1e9;
  alphaT <- 0.1;
  betaT <- 1.1e-6;
  DT <- 2.75;
  TSH[which(TSH == 0)] = NA;
  T4 <- T4 / nano.factor;
  GT <- betaT * (DT + TSH) * T4 / (alphaT * TSH) * pico.factor;
  return(GT);
}

estimated.GDTT <- function(T4, T3)
  # T4 and T3 in nmol/l
{
  pico.factor <- 1e12;
  nano.factor <- 1e9;
  TBG <- 3e-7;
  TBPA <- 4.5e-6;
  k30 <- 2e9;
  k41 <- 2e10;
  k42 <- 2e8;
  alpha31 <- 0.026;
  beta31 <- 8e-6;
  kM1 <- 5e-7;
  T4[which(T4 == 0)] = NA;
  T3 <- T3 / pico.factor;
  T4 <- T4 / pico.factor;
  FT4 <- T4 / (1 + k41 * TBG + k42 * TBPA);
  GD <- beta31 * (kM1 + FT4) * T3 / (alpha31 * FT4) * nano.factor;
  return(GD);
}

estimated.TTSI <- function(TSH, FT4, lu)
  # TSH in mU/l, FT4 in arbitrary unit
  # lu: upper limit of FT4 reference range, same unit as FT4
{
  TSH[which(TSH == 0)] = NA;
  ttsi <- 100 * TSH * FT4 / lu;
  return(ttsi);
}

estimated.TSHI <- function(TSH, FT4)
  # TSH in mU/l, FT4 in pmol/l
{
  beta <- -0.1345;
  TSH[which(TSH == 0)] = NA;
  tshi <- log(TSH) - beta * FT4;
  return(tshi);
}

estimated.sTSHI <- function(TSH, FT4, mean = 2.7, sd = 0.676)
  # TSH in mU/l, FT4 in pmol/l
{
  stshi <- (estimated.TSHI(TSH, FT4) - mean) / sd;
  return(stshi);
}

estimated.sGD <- function(FT4, FT3, mean = 30, sd = 5)
  # T4 and T3 in nmol/l
{
  sgd <- (estimated.GD(FT4, FT3) - mean) / sd;
  return(sgd);
}

# Alias functions renamed according to new nomenclature (SPINA-GT and SPINA-GD):

SPINA.GT <- function(TSH, FT4) estimated.GT(TSH, FT4);
SPINA.GD <- function(FT4, FT3) estimated.GD(FT4, FT3);
SPINA.GTT <- function(TSH, T4) estimated.GTT(TSH, T4);
SPINA.GDTT <- function(T4, T3) estimated.GDTT(T4, T3);
SPINA.sGD <- function(FT4, FT3, mean = 30, sd = 5) estimated.sGD(FT4, FT3, mean, sd);

# References:

# 1. Dietrich JW, Landgrafe-Mende G, Wiora E, Chatzitomaris A, Klein HH, Midgley JE,
#    Hoermann R. Calculated Parameters of Thyroid Homeostasis: Emerging Tools for
#    Differential Diagnosis and Clinical Research. Front Endocrinol (Lausanne). 2016 Jun
#    9;7:57. doi 10.3389/fendo.2016.00057. PMID 27375554; PMCID PMC4899439.

# 2. Dietrich JW, Dasgupta R, Anoop S, Jebasingh F, Kurian ME, Inbakumari M, Boehm BO,
#    Thomas N. SPINA Carb: a simple mathematical model supporting fast in-vivo estimation
#    of insulin sensitivity and beta cell function. Sci Rep. 2022 Oct 21;12(1):17659.
#    doi 10.1038/s41598-022-22531-3. PMID 36271244; PMCID PMC9587026.

# 3. Heinemann L. Insulin assay standardization: leading to measures of insulin
#    sensitivity and secretion for practical clinical care: response to Staten et al.
#    Diabetes Care. 2010 Jun;33(6):e83; author reply e84. doi: 10.2337/dc10-0034. PMID:
#    20508228.

# 4. Pilo A, Iervasi G, Vitek F, Ferdeghini M, Cazzuola F, Bianchi R. Thyroidal and
#    peripheral production of 3,5,3'-triiodothyronine in humans by multicompartmental
#    analysis. Am J Physiol. 1990 Apr;258(4 Pt 1):E715-26. PMID 2333963.

# 5. Voelund A. Conversion of insulin units to SI units. Am J Clin Nutr. 1993
#    Nov;58(5):714-5. doi: 10.1093/ajcn/58.5.714. PMID: 8237884.

# 6. Dietrich JW, Abood A, Dasgupta R, Anoop S, Jebasingh FK, Spurgeon R, Thomas N,
#    Boehm BO. A novel simple disposition index (SPINA-DI) from fasting insulin and
#    glucose concentration as a robust measure of carbohydrate homeostasis. J Diabetes.
#    2024 Jan 2. doi: 10.1111/1753-0407.13525. PMID: 38169110.
