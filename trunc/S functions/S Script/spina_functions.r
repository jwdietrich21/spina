##########################################
# SPINA-Thyr functions
# Calculate GT and GD in S
# Version 3.1.2
# Last Change 20120825 by J. W. D.
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
  TBG <- 3e-7;
  TBPA <- 4.5e-6;
  k41 <- 2e10;
  k42 <- 2e8;
  TSH[which(TSH == 0)] = NA;
  T4 <- T4 / nano.factor;
  GT <- betaT * (DT + TSH) * T4 / (alphaT * TSH) * pico.factor;
  return(GT);
}

estimated.GDTT <- function(T4, T3) # T4 in nmol/l and FT3 in pmol/l
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

# Test scenarios:

cat("\nScenario 9: GT = 4.7 pmol/s, GD = 25.22 nmol/s:\n");
TSH <- 1;
FT4 <- 16.5;
FT3 <- 4.5;

print(paste("GT^:", estimated.GT(TSH, FT4)));
print(paste("GD^:", estimated.GD(FT4, FT3)));

cat("\nScenario 10: GT = 1.08 pmol/s, GD = 336.2 nmol/s:\n");
TSH <- 3.24;
FT4 <- 7.7;
FT3 <- 28;

print(paste("GT^:", estimated.GT(TSH, FT4)));
print(paste("GD^:", estimated.GD(FT4, FT3)));

cat("\nScenario 11: GT = 3.37 pmol/s, GD = 63.7 nmol/s:\n");
TSH <- 0.7;
FT4 <- 9;
FT3 <- 6.2;

print(paste("GT^:", estimated.GT(TSH, FT4)));
print(paste("GD^:", estimated.GD(FT4, FT3)));

cat("\nScenarios 9 to 11 in vector form:\n");
TSH <- c(1, 3.24, 0.7);
FT4 <- c(16.5, 7.7, 9);
FT3 <- c(4.5, 28, 6.2);

print(paste("GT^:", estimated.GT(TSH, FT4)));
print(paste("GD^:", estimated.GD(FT4, FT3)));
