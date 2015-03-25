##########################################
# SPINA-Thyr functions
# Calculate structure parameters of
# thyroid homeostasis in S
# Version 4.0.0
# Last Change 20150325 by J. W. D.
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

estimated.sTSHI <- function(TSH, FT4, mean = 2.7, sd = 0.676) # TSH in mU/l, FT4 in pmol/l
{
  stshi <- (estimated.TSHI(TSH, FT4) - mean) / sd;
  return(stshi);
}

estimated.sGD <- function(FT4, FT3, mean = 30, sd = 5) # T4 and T3 in nmol/l
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

# Test scenarios:

cat("\nScenario 9: GT = 4.7 pmol/s, GD = 25.22 nmol/s:\n");
TSH <- 1;
FT4 <- 16.5;
FT3 <- 4.5;

print(paste("GT^:", SPINA.GT(TSH, FT4)));
print(paste("GD^:", SPINA.GD(FT4, FT3)));
print(paste("sGD^:", SPINA.sGD(FT4, FT3)));

cat("\nScenario 10: GT = 1.08 pmol/s, GD = 336.2 nmol/s:\n");
TSH <- 3.24;
FT4 <- 7.7;
FT3 <- 28;

print(paste("GT^:", SPINA.GT(TSH, FT4)));
print(paste("GD^:", SPINA.GD(FT4, FT3)));
print(paste("sGD^:", SPINA.sGD(FT4, FT3)));

cat("\nScenario 11: GT = 3.37 pmol/s, GD = 63.7 nmol/s:\n");
TSH <- 0.7;
FT4 <- 9;
FT3 <- 6.2;

print(paste("GT^:", SPINA.GT(TSH, FT4)));
print(paste("GD^:", SPINA.GD(FT4, FT3)));
print(paste("sGD^:", SPINA.sGD(FT4, FT3)));

cat("\nScenarios 9 to 11 in vector form:\n");
TSH <- c(1, 3.24, 0.7);
FT4 <- c(16.5, 7.7, 9);
FT3 <- c(4.5, 28, 6.2);

print(paste("GT^:", SPINA.GT(TSH, FT4)));
print(paste("GD^:", SPINA.GD(FT4, FT3)));
print(paste("sGD^:", SPINA.sGD(FT4, FT3)));

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

# BSA: body surface area in m^2
# IDV: initial distribution volume
# TT4: total T4 in mcg/dl
# TT3: total T3 in ng/ml
# FT4: free T4 in pg/ml
# FT3: free T3 in pg/ml
# TT4.SI: total T4 in nmol/l
# TT3.SI: total T3 in nmol/l
# FT4.SI: free T4 in pmol/l
# FT3.SI: free T3 in pmol/l
# SR: secretion rate
# CR.F, CR.S and CR.T: conversion rate (fast pool, slow pool and total)
# PAR: plasma apperance rate
# PR: production rate
# CR.6, CR.2, CR.0: conversion ratio from 6-compartment, 2-compartment and noncompartmental model

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
SR.T4 = c(44.5, 55.5, 45.3, 59.3, 59.2, 41.5, 50.9, 57.7, 54.3, 51.8, 76.5, 61.1, 65.7, 62.9), 
SR.T3 = c(1.98, 4.45, 7.05, 3.14, 5.95, 5.31, 3.51, 4.15, 0.91, 1.4, 2.89, 2.47, 2.64, 0.88),
CR.F.mean = c(12.3, 6.71, 8.73, 9.32, 8.75, 9.14, 10.6, 9.35, 7.9, 12.6, 13.1, 17.3, 14.5, 9.07),
CR.S.mean = c(3.7, 1.11, 1.06, 0.18, 1.2, 0.52, 0.4, 6.74, 3.39, 0.38, 1.55, 2.02, 2.37, 3.64),
CR.T.mean = c(12.3, 6.71, 8.73, 9.32, 8.75, 9.14, 10.6, 9.35, 7.9, 12.6, 13.1, 17.3, 14.5, 9.07) + c(3.7, 1.11, 1.06, 0.18, 1.2, 0.52, 0.4, 6.74, 3.39, 0.38, 1.55, 2.02, 2.37, 3.64),
PAR.T3 = c(16.7, 11.8, 16.4, 12.4, 15.5, 14.7, 14.2, 18.8, 11.4, 14.1, 16.9, 21.1, 18.5, 12.6),
PR.T3.mean = c(18, 12.3, 16.8, 12.6, 15.9, 15, 14.5, 20.2, 12.2, 14.4, 17.5, 21.8, 19.5, 13.6),
PR.T3 = c(20, 14.4, 18.9, 15.2, 17.6, 17.3, 16.1, 21.2, 12.9, 16.3, 19.8, 24.1, 22.4, 14.6),
CR.S = c(12.3, 9.02, 11.7, 9.51, 10.6, 10.5, 9.5, 12.8, 7.74, 9.78, 12.2, 14.4, 13.9, 8.87),
CR.6 = c(42.9, 16.9, 25.8, 19.2, 20.1, 27.8, 25.8, 33.4, 24.8, 30, 22.8, 37.9, 30.7, 24.1),
CR.2 = c(41, 16.5, 25.7, 19.4, 19.9, 27.8, 25.8, 31.3, 23.9, 30.1, 22.7, 37.6, 30.2, 23.1),
CR.0 = c(39.3, 15.6, 24.2, 18.5, 19.1, 26.9, 25.2, 30.8, 22.5, 29.1, 21.5, 35, 28.5, 22.5),
QP = c(2.31, 1.47, 1.76, 1.62, 1.95, 1.58, 1.96, 2.08, 2.21, 1.71, 2, 2.55, 2.06, 1.51),
QF = c(3.22, 2.85, 2.86, 3.55, 2.55, 3.04, 2.87, 3.66, 3.31, 2.9, 3.18, 3.94, 4.35, 2.82),
QS = c(25.8, 18.3, 22.6, 19, 15.4, 14.2, 14.8, 19.4, 18, 18.7, 15.4, 30.1, 25.7, 17.8),
QT = c(31.3, 22.6, 27.3, 24, 19.9, 18.8, 19.6, 25.2, 23.5, 23.3, 20.6, 36.6, 32.2, 22.2)
)

t3.mc$GT <- SPINA.GT(t3.mc$TSH, t3.mc$FT4.SI);
t3.mc$GD <- SPINA.GD(t3.mc$FT4.SI, t3.mc$FT3.SI);

