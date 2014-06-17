##########################################
# SPINA-Thyr functions
# Calculate GT and GD in S
# Version 3.4.1
# Last Change 20140617 by J. W. D.
##########################################

estimated.GT <- function(TSH, FT4) # TSH in mU/l, FT4 in pmol/l
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

estimated.GD <- function(FT4, FT3) # FT4 and FT3 in pmol/l
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

estimated.GTT <- function(TSH, T4) # TSH in mU/l, T4 in nmol/l
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

estimated.GDTT <- function(T4, T3) # T4 and T3 in nmol/l
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

estimated.TTSI <- function(TSH, FT4, lu) # TSH in mU/l, FT4 in arbitrary unit
# lu: upper limit of FT4 reference range, same unit as FT4
{
  TSH[which(TSH == 0)] = NA;
  ttsi <- 100 * TSH * FT4 / lu;
  return(ttsi);
}

estimated.TSHI <- function(TSH, FT4) # TSH in mU/l, FT4 in pmol/l
{
  beta <- -0.1345;
  TSH[which(TSH == 0)] = NA;
  tshi <- log(TSH) - beta * FT4;
  return(tshi);
}

estimated.sTSHI <- function(TSH, FT4) # TSH in mU/l, FT4 in pmol/l
{
  stshi <- (estimated.TSHI(TSH, FT4) - 2.7) / 0.676;
  return(stshi);
}

# Alias functions renamed according to new nomenclature (SPINA-GT and SPINA-GD):

SPINA.GT <- function(TSH, FT4) estimated.GT(TSH, FT4);
SPINA.GD <- function(FT4, FT3) estimated.GD(FT4, FT3);
SPINA.GTT <- function(TSH, T4) estimated.GTT(TSH, T4);
SPINA.GDTT <- function(T4, T3) estimated.GDTT(T4, T3);

# Test scenarios:

cat("\nScenario 9: GT = 4.7 pmol/s, GD = 25.22 nmol/s:\n");
TSH <- 1;
FT4 <- 16.5;
FT3 <- 4.5;

print(paste("GT^:", SPINA.GT(TSH, FT4)));
print(paste("GD^:", SPINA.GD(FT4, FT3)));

cat("\nScenario 10: GT = 1.08 pmol/s, GD = 336.2 nmol/s:\n");
TSH <- 3.24;
FT4 <- 7.7;
FT3 <- 28;

print(paste("GT^:", SPINA.GT(TSH, FT4)));
print(paste("GD^:", SPINA.GD(FT4, FT3)));

cat("\nScenario 11: GT = 3.37 pmol/s, GD = 63.7 nmol/s:\n");
TSH <- 0.7;
FT4 <- 9;
FT3 <- 6.2;

print(paste("GT^:", SPINA.GT(TSH, FT4)));
print(paste("GD^:", SPINA.GD(FT4, FT3)));

cat("\nScenarios 9 to 11 in vector form:\n");
TSH <- c(1, 3.24, 0.7);
FT4 <- c(16.5, 7.7, 9);
FT3 <- c(4.5, 28, 6.2);

print(paste("GT^:", SPINA.GT(TSH, FT4)));
print(paste("GD^:", SPINA.GD(FT4, FT3)));

cat("\nEvaluate pituitary function with TTSI, TSHI and sTSHI:\n");
TSH <- c(21.97, 1.82, 0.29);
FT4 <- c(6, 8, 8); # ng/l, upper limit of reference range: 20 ng/l
print(paste("TTSI:", estimated.TTSI(TSH, FT4, 20)));
print(paste("Jostel's TSHI:", estimated.TSHI(TSH, FT4 * 1.287)));
print(paste("sTSHI:", estimated.sTSHI(TSH, FT4 * 1.287)));

# Example data
# Source:
# Pilo A, Iervasi G, Vitek F, Ferdeghini M, Cazzuola F, Bianchi R. Thyroidal and
# peripheral production of 3,5,3'-triiodothyronine in humans by multicompartmental 
# analysis. Am J Physiol. 1990 Apr;258(4 Pt 1):E715-26. PMID 2333963.

t3.mc <- data.frame(
Sex = c("m", "f", "m", "m", "m", "m", "m", "m", "f", "m", "f", "m", "f", "f"), 
Age = c(54, 43, 31, 65, 44, 26, 27, 19, 53, 36, 48, 20, 44, 59),
Body.Mass = c(83, 68.5, 83, 69, 75, 73, 82, 63, 66.5, 72, 53, 65.5, 63, 60),
BSA = c(2.02, 1.7, 2.02, 1.73, 1.8, 1.9, 1.98, 1.8, 1.69, 1.81, 1.49, 1.79, 1.75, 1.57),
IDV.T4 = c(3801, 2272, 2686, 2726, 2632, 2804, 2770, 3119, 2749, 2860, 2467, 3624, 2965, 2327),
TT4 = c(8, 10.4, 7.9, 8, 8.8, 6.5, 7.7, 7.2, 6.6, 8.3, 9.5, 6.5, 7.9, 9.6),
TT4.SI = c(8, 10.4, 7.9, 8, 8.8, 6.5, 7.7, 7.2, 6.6, 8.3, 9.5, 6.5, 7.9, 9.6) * 12.87,
TT3 = c(1.23, 1.1, 1.32, 1.03, 1.33, 1.07, 1.4, 1.2, 1.36, 1.08, 1.21, 1.26, 1.21, 1.02),
TT3.SI = c(1.23, 1.1, 1.32, 1.03, 1.33, 1.07, 1.4, 1.2, 1.36, 1.08, 1.21, 1.26, 1.21, 1.02) * 1.54,
FT4 = c(10.1, 10.5, 8.8, 13.1, 11.1, 8.8, 10.2, 8.9, 6.9, 9, 10.5, 10.4, 11.8, 8.4),
FT4.SI = c(10.1, 10.5, 8.8, 13.1, 11.1, 8.8, 10.2, 8.9, 6.9, 9, 10.5, 10.4, 11.8, 8.4) * 1.287,
FT3 = c(4.3, 5.7, 4.3, 4.4, 3.5, 3.4, 4.1, 4.1, 3.6, 2.6, 3.8, 4.1, 5, 4.3),
FT3.SI = c(4.3, 5.7, 4.3, 4.4, 3.5, 3.4, 4.1, 4.1, 3.6, 2.6, 3.8, 4.1, 5, 4.3) * 1.54,
TSH = c(1.2, 1.4, 1, 1.8, 1.5, 1.5, 2, 1.9, 1.4, 1.8, 1.1, 1.3, 2, 1.5),
GT = SPINA.GT(c(1.2, 1.4, 1, 1.8, 1.5, 1.5, 2, 1.9, 1.4, 1.8, 1.1, 1.3, 2, 1.5), c(10.1, 10.5, 8.8, 13.1, 11.1, 8.8, 10.2, 8.9, 6.9, 9, 10.5, 10.4, 11.8, 8.4) * 1.287),
GD = SPINA.GD(c(10.1, 10.5, 8.8, 13.1, 11.1, 8.8, 10.2, 8.9, 6.9, 9, 10.5, 10.4, 11.8, 8.4) * 1.287, c(4.3, 5.7, 4.3, 4.4, 3.5, 3.4, 4.1, 4.1, 3.6, 2.6, 3.8, 4.1, 5, 4.3) * 1.54),
SR.T4 = c(44.5, 55.5, 45.3, 59.3, 59.2, 41.5, 50.9, 57.7, 54.3, 51.8, 76.5, 61.1, 65.7, 62.9), 
SR.T3 = c(1.98, 4.45, 7.05, 3.14, 5.95, 5.31, 3.51, 4.15, 0.91, 1.4, 2.89, 2.47, 2.64, 0.88),
CR.F = c(12.3, 6.71, 8.73, 9.32, 8.75, 9.14, 10.6, 9.35, 7.9, 12.6, 13.1, 17.3, 14.5, 9.07),
CR.S = c(3.7, 1.11, 1.06, 0.18, 1.2, 0.52, 0.4, 6.74, 3.39, 0.38, 1.55, 2.02, 2.37, 3.64),
CR.T = c(12.3, 6.71, 8.73, 9.32, 8.75, 9.14, 10.6, 9.35, 7.9, 12.6, 13.1, 17.3, 14.5, 9.07) + c(3.7, 1.11, 1.06, 0.18, 1.2, 0.52, 0.4, 6.74, 3.39, 0.38, 1.55, 2.02, 2.37, 3.64)
)
