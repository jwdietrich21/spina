##########################################
# SPINA-Thyr functions
# Calculate structure parameters of
# thyroid homeostasis in S
# Version 4.1.0
# Last Change 20180319 by J. W. D.
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

