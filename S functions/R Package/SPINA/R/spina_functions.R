##########################################
# SPINA functions
# Calculate structure parameters of
# thyroid and insulin-glucose homeostasis
# in S, including the implementation in R
# Version 5.1.0 (Cyclone)
# Last Change 20260705 by J. W. D.
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
#'  \item{Body.Mass}{Body mass in kg}
#'  \item{BSA}{Body surface area in m^2}
#'  \item{IDV.T4}{initial distribution volume of thyroxine in mL}
#'  \item{TT4}{Total T4 concentration in µg/dL}
#'  \item{TT4.SI}{Total T4 concentration in nmol/L}
#'  \item{TT3}{Total T3 concentration in ng/mL}
#'  \item{TT3.SI}{Total T3 concentration in nmol/L}
#'  \item{TSH}{Thyrotropin concentration in mIU/L}
#'  \item{SR.T4}{Thyroidal secretion rate for T4 in µg/day/m^2}
#'  \item{SR.T3}{Thyroidal secretion rate for T3 in µg/day/m^2}
#'  \item{CR.F.mean}{Rate of conversion from T4 to T3 in fast tissue pools in µg/day/m^2}
#'  \item{CR.S.mean}{Rate of conversion from T4 to T3 in slow tissue pools in µg/day/m^2}
#'  \item{CR.T.mean}{Rate of total conversion from T4 to T3 in µg/day/m^2}
#'  \item{PAR.T3}{Plasma appearance rate for T3 in µg/day/m^2}
#'  \item{PR.T3.mean}{Mean production rate for T3 in µg/day/m^2}
#'  \item{PR.T3}{Production rate for T3 in µg/day/m^2, calculated with alternative method (DiStefano et al. 1982)}
#'  \item{CR.S}{Rate of conversion from T4 to T3 in µg/day/m^2, calculated with alternative method (DiStefano et al. 1982)}
#'  \item{CR.6}{Ratio of conversion from T4 to T3 in in %, computed using six-compartment model}
#'  \item{CR.2}{Ratio of conversion from T4 to T3 in in %, computed using two-compartment model}
#'  \item{CR.0}{Ratio of conversion from T4 to T3 in in %, computed using noncompartmental approach}
#'  \item{QP}{Mass of the plasma pool for T3 in in µg/m^2}
#'  \item{QF}{Mass of the fast tissue pool for T3 in in µg/m^2}
#'  \item{QS}{Mass of the slow tissue pool for T3 in in µg/m^2}
#'  \item{QT}{Mass of the total tissue pool for T3 in in µg/m^2}
#'  \item{SPINA_GT}{Thyroid's secretory capacity (SPINA-GT) in pmol/s}
#'  \item{SPINA_GD}{Sum activity of step-up deiodinases (SPINA-GD) in nmol/s}
#' }
#'
#' @source
#' Pilo A, Iervasi G, Vitek F, Ferdeghini M, Cazzuola F, Bianchi R. Thyroidal and
#' peripheral production of 3,5,3'-triiodothyronine in humans by multicompartmental
#' analysis. Am J Physiol. 1990 Apr;258(4 Pt 1):E715-26. PMID 2333963.
#'
#' DiStefano JJ 3rd, Jang M, Malone TK, Broutman M. Comprehensive kinetics of
#' triiodothyronine production, distribution, and metabolism in blood and tissue
#' pools of the rat using optimized blood-sampling protocols. Endocrinology. 1982
#' Jan;110(1):198-213. doi: 10.1210/endo-110-1-198. PMID: 7053984.
#'
#' Dietrich JW, Landgrafe-Mende G, Wiora E, Chatzitomaris A, Klein HH, Midgley JE,
#' Hoermann R. Calculated Parameters of Thyroid Homeostasis: Emerging Tools for
#' Differential Diagnosis and Clinical Research. Front Endocrinol (Lausanne).
#' 2016 Jun 9;7:57. doi 10.3389/fendo.2016.00057. PMID 27375554; PMCID PMC4899439.
#'
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
#'  \item{Height}{Body height in cm}
#'  \item{Body.Mass}{Body mass in kg}
#'  \item{BMI}{Body mass index in kg/m^2}
#'  \item{Fasting.Glucose}{Fasting glucose concentration in mg/dL}
#'  \item{Fasting.Insulin}{Fasting insulin concentration in mIU/L}
#'  \item{SPINA_GR}{Insulin receptor gain (SPINA-GR) in mol/s}
#'  \item{SPINA_GBeta}{Secretory capacity of pancreatic beta-cells (SPINA-GBeta) in pmol/s}
#'  \item{SPINA_DI}{Static disposition index (SPINA-DI)}
#'  \item{HOMA_Beta}{Estimate of beta-cell function based on the Homeostasis Model Assessment (HOMA-Beta)}
#'  \item{HOMA_IR}{Estimate of insulin resistance based on the Homeostasis Model Assessment (HOMA-IR)}
#'  \item{HOMA_IS}{Estimate of insulin sensitivity based on the Homeostasis Model Assessment (HOMA-IS)}
#'  \item{QUICKI_Score}{Quantitative Insulin Sensitivity Check Index (QUICKI)}
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
#' This function calculates the secretory capacity of pancreatic beta cells
#' (SPINA-GBeta) from steady-state fasting concentrations of insulin and glucose.
#' SPINA-GBeta is an inferred representation of GBeta, the maximum amount of insulin
#' that can be produced by beta-cells under stimulated conditions.
#' This parameter is an implementation of the structure parameter inference approach (SPINA).
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
  # Insulin expected in pmol/L, Glucose in mmol/L
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
#' This function calculates the insulin receptor gain (SPINA-GR) from steady-state
#' fasting concentrations of insulin and glucose. SPINA-GR is an inferred
#' representation of GR, the maximum insulin signaling under stimulated conditions.
#' This parameter is an implementation of the structure parameter inference approach (SPINA).
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
  # Insulin in pmol/L, Glucose in mmol/L
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
#' This function calculates the static disposition index (SPINA-DI) from steady-state
#' fasting concentrations of insulin and glucose. SPINA-DI is an inferred
#' representation of the loop gain of the homeostatic feedback control system.
#' This parameter is an implementation of the structure parameter inference approach (SPINA).
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

SPINA.DI <- function(Insulin, Glucose) # Insulin in pmol/L, Glucose in mmol/L
{
  DI <- SPINA.GBeta(Insulin, Glucose) * SPINA.GR(Insulin, Glucose);
  return(DI);
}

#' Homeostasis model assessment: insulin resistance (HOMA-IR)
#'
#' This function calculates the insulin resistance index (HOMA-IR) from steady-state
#' fasting concentrations of insulin and glucose. HOMA-IR is one of the parameters
#' provided by the homeostasis model assessment.
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

HOMA.IR <- function(Insulin, Glucose) # Insulin in pmol/L, Glucose in mmol/L
{
  IR <- Glucose * Insulin / Insulin.conversion.factor / 22.5;
  return(IR);
}

#' Homeostasis model assessment: beta-cell function (HOMA-Beta)
#'
#' This function calculates the beta-cell function (HOMA-Beta) from steady-state
#' fasting concentrations of insulin and glucose. HOMA-Beta is one of the parameters
#' provided by the homeostasis model assessment.
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

HOMA.Beta <- function(Insulin, Glucose) # Insulin in pmol/L, Glucose in mmol/L
{
  Beta <- rep(NA, times = length(Insulin));
  Beta[which(Glucose > 3.5)] <- 20 * Insulin[which(Glucose > 3.5)] / Insulin.conversion.factor / (Glucose[which(Glucose > 3.5)] - 3.5);
  return(Beta);
}

#' Homeostasis model assessment: insulin sensitivity (HOMA-IS)
#'
#' This function calculates the insulin sensitivity index (HOMA-IS) from steady-state
#' fasting concentrations of insulin and glucose. HOMA-IS is one of the parameters
#' provided by the homeostasis model assessment.
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

HOMA.IS <- function(Insulin, Glucose) # Insulin in pmol/L, Glucose in mmol/L
{
  IS <- 1 / HOMA.IR(Insulin, Glucose);
  return(IS);
}

#' Quantitative insulin-sensitivity check index (QUICKI)
#'
#' This function calculates the quantitative insulin sensitivity check index (QUICKI)
#' from steady-state fasting concentrations of insulin and glucose.
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

QUICKI <- function(Insulin, Glucose) # Insulin in pmol/L, Glucose in mmol/L
{
  QUICKI <- 1 / (log10(Insulin / Insulin.conversion.factor) + log10(Glucose * Glucose.conversion.factor));
  return(QUICKI);
}

#' Calculated thyroid's secretory capacity (SPINA-GT)
#'
#' This function calculates the secretory capacity of the thyroid gland for T4
#' (SPINA-GT) from steady-state concentrations of TSH and free T4.
#' SPINA-GT is an inferred representation of GT, the maximum amount of thyroxine
#' that can be produced by the thyroid under stimulated conditions.
#' This parameter is an implementation of the structure parameter inference approach (SPINA).
#'
#' @param TSH thyrotropin concentration in mIU/L
#' @param FT4 free T4 concentration in pmol/L
#'
#' @returns Returns SPINA-GT, a calculated biomarker for thyroid's secretory capacity (aka thyroid output)
#' @export
#'
#' @examples
#' estimated.GT(1.0, 16.5)
#' @author Johannes W. Dietrich
#' @details This function is able to do vectorised calculations. See [SPINA.GT] for a an implementation
#' with a more modern nomenclature.
#' @references
#' Dietrich JW, Landgrafe G, Fotiadou EH. TSH and Thyrotropic Agonists: Key Actors in Thyroid Homeostasis. J Thyroid Res. 2012;2012:351864. doi: 10.1155/2012/351864. Epub 2012 Dec 30. PMID: 23365787; PMCID: PMC3544290.
#'
#' Dietrich JW, Landgrafe-Mende G, Wiora E, Chatzitomaris A, Klein HH, Midgley JE, Hoermann R. Calculated Parameters of Thyroid Homeostasis: Emerging Tools for Differential Diagnosis and Clinical Research. Front Endocrinol (Lausanne). 2016 Jun 9;7:57. doi: 10.3389/fendo.2016.00057. PMID: 27375554; PMCID: PMC4899439.
#' @note
#' The software functions described in this document are intended for research use only.
#' Hormone concentrations should have been obtained simultaneously in order to
#' avoid bias by transition effects.

estimated.GT <- function(TSH, FT4)
  # TSH in mU/L, FT4 in pmol/L
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

#' Calculated step-up deiodinase activity (SPINA-GD)
#'
#' This function calculates the sum activity of peripheral step-up-deiodinases
#' (SPINA-GD) from steady-state concentrations of free T4 and free T3.
#' SPINA-GD is an inferred representation of GD, the total activity of the type 1
#' deiodinase under stimulated conditions.
#' This parameter is an implementation of the structure parameter inference approach (SPINA).
#'
#' @param FT4 free T4 concentration in pmol/L
#' @param FT3 free T3 concentration in pmol/L
#'
#' @returns Returns SPINA-GD, a calculated biomarker for the sum activity of peripheral step-up deiodinases
#' @export
#'
#' @examples
#' estimated.GD(16.5, 4.5)
#' @author Johannes W. Dietrich
#' @details This function is able to do vectorised calculations. See [SPINA.GD] for a an implementation
#' with a more modern nomenclature.
#' @references
#' Dietrich JW, Landgrafe G, Fotiadou EH. TSH and Thyrotropic Agonists: Key Actors in Thyroid Homeostasis. J Thyroid Res. 2012;2012:351864. doi: 10.1155/2012/351864. Epub 2012 Dec 30. PMID: 23365787; PMCID: PMC3544290.
#'
#' Dietrich JW, Landgrafe-Mende G, Wiora E, Chatzitomaris A, Klein HH, Midgley JE, Hoermann R. Calculated Parameters of Thyroid Homeostasis: Emerging Tools for Differential Diagnosis and Clinical Research. Front Endocrinol (Lausanne). 2016 Jun 9;7:57. doi: 10.3389/fendo.2016.00057. PMID: 27375554; PMCID: PMC4899439.
#' @note
#' The software functions described in this document are intended for research use only.
#' Hormone concentrations should have been obtained simultaneously in order to
#' avoid bias by transition effects.

estimated.GD <- function(FT4, FT3)
  # FT4 and FT3 in pmol/L
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

#' Calculated thyroid's secretory capacity, based on total T4 (SPINA-GTT)
#'
#' This function calculates the secretory capacity of the thyroid gland for T4
#' (SPINA-GT) from steady-state concentrations of TSH and total T4.
#' SPINA-GT is an inferred representation of GT, the maximum amount of thyroxine
#' that can be produced by the thyroid under stimulated conditions.
#' This parameter is an implementation of the structure parameter inference approach (SPINA).
#'
#' @param TSH thyrotropin concentration in mIU/L
#' @param T4 total T4 concentration in nmol/L
#'
#' @returns Returns SPINA-GTT, a calculated biomarker for thyroid's secretory capacity (aka thyroid output)
#' @export
#'
#' @examples
#' estimated.GTT(1.0, 90)
#' @author Johannes W. Dietrich
#' @details This function is able to do vectorised calculations. See [SPINA.GTT] for a an implementation
#' with a more modern nomenclature.
#' @references
#' Dietrich JW, Landgrafe G, Fotiadou EH. TSH and Thyrotropic Agonists: Key Actors in Thyroid Homeostasis. J Thyroid Res. 2012;2012:351864. doi: 10.1155/2012/351864. Epub 2012 Dec 30. PMID: 23365787; PMCID: PMC3544290.
#'
#' Dietrich JW, Landgrafe-Mende G, Wiora E, Chatzitomaris A, Klein HH, Midgley JE, Hoermann R. Calculated Parameters of Thyroid Homeostasis: Emerging Tools for Differential Diagnosis and Clinical Research. Front Endocrinol (Lausanne). 2016 Jun 9;7:57. doi: 10.3389/fendo.2016.00057. PMID: 27375554; PMCID: PMC4899439.
#' @note
#' The software functions described in this document are intended for research use only.
#' Hormone concentrations should have been obtained simultaneously in order to
#' avoid bias by transition effects.

estimated.GTT <- function(TSH, T4)
  # TSH in mU/L, T4 in nmol/L
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

#' Calculated step-up deiodinase activity, based on total T4 and T3 (SPINA-GDTT)
#'
#' This function calculates the sum activity of peripheral step-up-deiodinases
#' (SPINA-GD) from steady-state concentrations of total T4 and total T3.
#' SPINA-GD is an inferred representation of GD, the total activity of the type 1
#' deiodinase under stimulated conditions.
#' This parameter is an implementation of the structure parameter inference approach (SPINA).
#'
#' @param T4 total T4 concentration in nmol/L
#' @param T3 total T3 concentration in nmol/L
#'
#' @returns Returns SPINA-GD, a calculated biomarker for the sum activity of peripheral step-up deiodinases
#' @export
#'
#' @examples
#' estimated.GDTT(90, 2.5)
#' @author Johannes W. Dietrich
#' @details This function is able to do vectorised calculations. See [SPINA.GDTT] for a an implementation
#' with a more modern nomenclature.
#' @references
#' Dietrich JW, Landgrafe G, Fotiadou EH. TSH and Thyrotropic Agonists: Key Actors in Thyroid Homeostasis. J Thyroid Res. 2012;2012:351864. doi: 10.1155/2012/351864. Epub 2012 Dec 30. PMID: 23365787; PMCID: PMC3544290.
#'
#' Dietrich JW, Landgrafe-Mende G, Wiora E, Chatzitomaris A, Klein HH, Midgley JE, Hoermann R. Calculated Parameters of Thyroid Homeostasis: Emerging Tools for Differential Diagnosis and Clinical Research. Front Endocrinol (Lausanne). 2016 Jun 9;7:57. doi: 10.3389/fendo.2016.00057. PMID: 27375554; PMCID: PMC4899439.
#' @note
#' The software functions described in this document are intended for research use only.
#' Hormone concentrations should have been obtained simultaneously in order to
#' avoid bias by transition effects.

estimated.GDTT <- function(T4, T3)
  # T4 and T3 in nmol/L
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

#' Thyrotroph thyroid hormone sensitivity index (TTSI)
#'
#' This function calculates the thyrotroph thyroid hormone sensitivity index
#' (TTSI) from steady-state concentrations of TSH and free T4. This parameter is also
#' referred to as Thyrotroph T4 Resistance Index or TT4RI.
#'
#' @param TSH thyrotropin concentration in mIU/L
#' @param FT4 free T4 concentration in pmol/L
#' @param lu upper limit of the reference interval of FT4 in pmol/L
#'
#' @returns Returns the TTSI, a calculated biomarker for the sensitivity of pituitary thyrotrophic cells to thyroxine
#' @export
#'
#' @examples
#' estimated.TTSI(1.0, 16.5, 18)
#' @author Johannes W. Dietrich
#' @details This function is able to do vectorised calculations.
#' @references
#' Dietrich JW, Landgrafe G, Fotiadou EH. TSH and Thyrotropic Agonists: Key Actors in Thyroid Homeostasis. J Thyroid Res. 2012;2012:351864. doi: 10.1155/2012/351864. Epub 2012 Dec 30. PMID: 23365787; PMCID: PMC3544290.
#'
#' Dietrich JW, Landgrafe-Mende G, Wiora E, Chatzitomaris A, Klein HH, Midgley JE, Hoermann R. Calculated Parameters of Thyroid Homeostasis: Emerging Tools for Differential Diagnosis and Clinical Research. Front Endocrinol (Lausanne). 2016 Jun 9;7:57. doi: 10.3389/fendo.2016.00057. PMID: 27375554; PMCID: PMC4899439.
#'
#' Pohlenz J, Weiss RE, Macchia PE, Pannain S, Lau IT, Ho H, Refetoff S. Five new families with resistance to thyroid hormone not caused by mutations in the thyroid hormone receptor beta gene. J Clin Endocrinol Metab. 1999 Nov;84(11):3919-28. doi: 10.1210/jcem.84.11.6080. PMID: 10566629.
#'
#' @note
#' The software functions described in this document are intended for research use only.
#' Hormone concentrations should have been obtained simultaneously in order to
#' avoid bias by transition effects.

estimated.TTSI <- function(TSH, FT4, lu)
  # TSH in mU/L, FT4 in arbitrary unit
  # lu: upper limit of FT4 reference range, same unit as FT4
{
  TSH[which(TSH == 0)] = NA;
  ttsi <- 100 * TSH * FT4 / lu;
  return(ttsi);
}

#' Jostel's TSH index (TSHI, JTI)
#'
#' This function calculates Jostel's TSH index (TSHI, JTI) from steady-state
#' concentrations of TSH and free T4.
#'
#' @param TSH thyrotropin concentration in mIU/L
#' @param FT4 free T4 concentration in pmol/L
#'
#' @returns Returns the TSH index, a calculated biomarker for the thyrotropic function of the anterior pituitary gland
#' @export
#'
#' @examples
#' estimated.TSHI(1.0, 16.5)
#' @author Johannes W. Dietrich
#' @details This function is able to do vectorised calculations.
#' @references
#' Dietrich JW, Landgrafe G, Fotiadou EH. TSH and Thyrotropic Agonists: Key Actors in Thyroid Homeostasis. J Thyroid Res. 2012;2012:351864. doi: 10.1155/2012/351864. Epub 2012 Dec 30. PMID: 23365787; PMCID: PMC3544290.
#'
#' Dietrich JW, Landgrafe-Mende G, Wiora E, Chatzitomaris A, Klein HH, Midgley JE, Hoermann R. Calculated Parameters of Thyroid Homeostasis: Emerging Tools for Differential Diagnosis and Clinical Research. Front Endocrinol (Lausanne). 2016 Jun 9;7:57. doi: 10.3389/fendo.2016.00057. PMID: 27375554; PMCID: PMC4899439.
#'
#' Jostel A, Ryder WD, Shalet SM. The use of thyroid function tests in the diagnosis of hypopituitarism: definition and evaluation of the TSH Index. Clin Endocrinol (Oxf). 2009 Oct;71(4):529-34. doi: 10.1111/j.1365-2265.2009.03534.x. Epub 2009 Feb 18. PMID: 19226261.
#'
#' @note
#' The software functions described in this document are intended for research use only.
#' Hormone concentrations should have been obtained simultaneously in order to
#' avoid bias by transition effects.

estimated.TSHI <- function(TSH, FT4)
  # TSH in mU/L, FT4 in pmol/L
{
  beta <- -0.1345;
  TSH[which(TSH == 0)] = NA;
  tshi <- log(TSH) - beta * FT4;
  return(tshi);
}

#' Standardised Jostel's TSH index (sTSHI, sJTI)
#'
#' This function calculates Jostel's TSH index (TSHI, JTI) from steady-state
#' concentrations of TSH and free T4. The result is delivered in standardised form (z-transformed).
#'
#' @param TSH thyrotropin concentration in mIU/L
#' @param FT4 free T4 concentration in pmol/L
#' @param mean mean TSH index (optional, standard value 2.7)
#' @param sd standard deviation of TSH index (optional, standard value 0.676)
#'
#' @returns Returns the the standardised TSH index, a calculated biomarker for the thyrotropic function of the anterior pituitary gland (z-transformed variant)
#' @export
#'
#' @examples
#' estimated.sTSHI(1.0, 16.5)
#' @author Johannes W. Dietrich
#' @details This function is able to do vectorised calculations.
#' @references
#' Dietrich JW, Landgrafe G, Fotiadou EH. TSH and Thyrotropic Agonists: Key Actors in Thyroid Homeostasis. J Thyroid Res. 2012;2012:351864. doi: 10.1155/2012/351864. Epub 2012 Dec 30. PMID: 23365787; PMCID: PMC3544290.
#'
#' Dietrich JW, Landgrafe-Mende G, Wiora E, Chatzitomaris A, Klein HH, Midgley JE, Hoermann R. Calculated Parameters of Thyroid Homeostasis: Emerging Tools for Differential Diagnosis and Clinical Research. Front Endocrinol (Lausanne). 2016 Jun 9;7:57. doi: 10.3389/fendo.2016.00057. PMID: 27375554; PMCID: PMC4899439.
#'
#' Jostel A, Ryder WD, Shalet SM. The use of thyroid function tests in the diagnosis of hypopituitarism: definition and evaluation of the TSH Index. Clin Endocrinol (Oxf). 2009 Oct;71(4):529-34. doi: 10.1111/j.1365-2265.2009.03534.x. Epub 2009 Feb 18. PMID: 19226261.
#'
#' @note
#' The software functions described in this document are intended for research use only.
#' Hormone concentrations should have been obtained simultaneously in order to
#' avoid bias by transition effects.

estimated.sTSHI <- function(TSH, FT4, mean = 2.7, sd = 0.676)
  # TSH in mU/L, FT4 in pmol/L
{
  stshi <- (estimated.TSHI(TSH, FT4) - mean) / sd;
  return(stshi);
}

#' Calculated standardised step-up deiodinase activity (SPINA-sGD)
#'
#' This function calculates the sum activity of peripheral step-up-deiodinases
#' (SPINA-GD) from steady-state concentrations of free T4 and free T3.
#' SPINA-GD is an inferred representation of GD, the total activity of the type 1
#' deiodinase under stimulated conditions.
#' This parameter is an implementation of the structure parameter inference approach (SPINA).
#' The result is delivered in standardised form (z-transformed).
#'
#' @param FT4 free T4 concentration in pmol/L
#' @param FT3 free T3 concentration in pmol/L
#' @param mean mean SPINA-GD (optional, default value 30)
#' @param sd standard deviation of SPINA-GD (optional, default value 5)
#'
#' @returns Returns standardised SPINA-GD, a calculated biomarker for the sum activity of peripheral step-up deiodinases (z-transformed variant)
#' @export
#'
#' @examples
#' estimated.sGD(16.5, 4.5)
#' @author Johannes W. Dietrich
#' @details This function is able to do vectorised calculations. See [SPINA.sGD] for a an implementation
#' with a more modern nomenclature.
#' @references
#' Dietrich JW, Landgrafe G, Fotiadou EH. TSH and Thyrotropic Agonists: Key Actors in Thyroid Homeostasis. J Thyroid Res. 2012;2012:351864. doi: 10.1155/2012/351864. Epub 2012 Dec 30. PMID: 23365787; PMCID: PMC3544290.
#'
#' Dietrich JW, Landgrafe-Mende G, Wiora E, Chatzitomaris A, Klein HH, Midgley JE, Hoermann R. Calculated Parameters of Thyroid Homeostasis: Emerging Tools for Differential Diagnosis and Clinical Research. Front Endocrinol (Lausanne). 2016 Jun 9;7:57. doi: 10.3389/fendo.2016.00057. PMID: 27375554; PMCID: PMC4899439.
#' @note
#' The software functions described in this document are intended for research use only.
#' Hormone concentrations should have been obtained simultaneously in order to
#' avoid bias by transition effects.

estimated.sGD <- function(FT4, FT3, mean = 30, sd = 5)
  # T4 and T3 in nmol/L
{
  sgd <- (estimated.GD(FT4, FT3) - mean) / sd;
  return(sgd);
}

# Alias functions renamed according to new nomenclature (SPINA-GT and SPINA-GD):

#' Calculated thyroid's secretory capacity (SPINA-GT)
#'
#' This function calculates the secretory capacity of the thyroid gland for T4
#' (SPINA-GT) from steady-state concentrations of TSH and free T4.
#' SPINA-GT is an inferred representation of GT, the maximum amount of thyroxine
#' that can be produced by the thyroid under stimulated conditions.
#' This parameter is an implementation of the structure parameter inference approach (SPINA).
#'
#' @param TSH thyrotropin concentration in mIU/L
#' @param FT4 free T4 concentration in pmol/L
#'
#' @returns Returns SPINA-GT, a calculated biomarker for thyroid's secretory capacity (aka thyroid output)
#' @export
#'
#' @examples
#' SPINA.GT(1.0, 16.5)
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

SPINA.GT <- function(TSH, FT4) estimated.GT(TSH, FT4);

#' Calculated step-up deiodinase activity (SPINA-GD)
#'
#' This function calculates the sum activity of peripheral step-up-deiodinases
#' (SPINA-GD) from steady-state concentrations of free T4 and free T3.
#' SPINA-GD is an inferred representation of GD, the total activity of the type 1
#' deiodinase under stimulated conditions.
#' This parameter is an implementation of the structure parameter inference approach (SPINA).
#'
#' @param FT4 free T4 concentration in pmol/L
#' @param FT3 free T3 concentration in pmol/L
#'
#' @returns Returns SPINA-GD, a calculated biomarker for the sum activity of peripheral step-up deiodinases
#' @export
#'
#' @examples
#' SPINA.GD(16.5, 4.5)
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

SPINA.GD <- function(FT4, FT3) estimated.GD(FT4, FT3);

#' Calculated thyroid's secretory capacity, based on total T4 (SPINA-GTT)
#'
#' This function calculates the secretory capacity of the thyroid gland for T4
#' (SPINA-GT) from steady-state concentrations of TSH and total T4.
#' SPINA-GTT is an inferred representation of GT, the maximum amount of thyroxine
#' that can be produced by the thyroid under stimulated conditions.
#' This parameter is an implementation of the structure parameter inference approach (SPINA).

#' @param TSH thyrotropin concentration in mIU/L
#' @param T4 total T4 concentration in nmol/L
#'
#' @returns Returns SPINA-GTT, a calculated biomarker for thyroid's secretory capacity (aka thyroid output)
#' @export
#'
#' @examples
#' SPINA.GTT(1.0, 90)
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

SPINA.GTT <- function(TSH, T4) estimated.GTT(TSH, T4);

#' Calculated step-up deiodinase activity, based on total T4 and T3 (SPINA-GDTT)
#'
#' This function calculates the sum activity of peripheral step-up-deiodinases
#' (SPINA-GD) from steady-state concentrations of total T4 and total T3.
#' SPINA-GD is an inferred representation of GD, the total activity of the type 1
#' deiodinase under stimulated conditions.
#' This parameter is an implementation of the structure parameter inference approach (SPINA).
#'
#' @param T4 total T4 concentration in nmol/L
#' @param T3 tptaö T3 concentration in nmol/L
#'
#' @returns Returns SPINA-GD, a calculated biomarker for the sum activity of peripheral step-up deiodinases
#' @export
#'
#' @examples
#' SPINA.GDTT(90, 2.5)
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

SPINA.GDTT <- function(T4, T3) estimated.GDTT(T4, T3);

#' Calculated standardised step-up deiodinase activity (SPINA-sGD)
#'
#' This function calculates the sum activity of peripheral step-up-deiodinases
#' (SPINA-GD) from steady-state concentrations of free T4 and free T3.
#' SPINA-GD is an inferred representation of GD, the total activity of the type 1
#' deiodinase under stimulated conditions.
#' This parameter is an implementation of the structure parameter inference approach (SPINA).
#' The result is delivered in standardised form (z-transformed).
#'
#' @param FT4 free T4 concentration in pmol/L
#' @param FT3 free T3 concentration in pmol/L
#' @param mean mean SPINA-GD (optional, default value 30)
#' @param sd standard deviation of SPINA-GD (optional, default value 5)
#'
#' @returns Returns standardised SPINA-GD, a calculated biomarker for the sum activity of peripheral step-up deiodinases (z-transformed variant)
#' @export
#'
#' @examples
#' SPINA.sGD(16.5, 4.5)
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
