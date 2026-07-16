##########################################
# Testing suite for:
# SPINA S functions, version for R package
# Calculate structure parameters of
# thyroid and insulin-glucose homeostasis
# in S, including the implementation in R
# Version 5.1.0 (Cyclone)
# Last Change 20260716 by J. W. D.
##########################################

test_that("Scenario 9: SPINA-GT = 4.7 pmol/s, SPINA-GD = 25.22 nmol/s",
{
  TSH <- 1;
  FT4 <- 16.5;
  FT3 <- 4.5;
  expect_lt(abs(SPINA.GT(TSH, FT4) - 4.7), 0.05);   # Result rounded
  expect_lt(abs(SPINA.GD(FT4, FT3) - 25.22), 0.05); # Difference should be less than 0.05
  expect_lt(abs(estimated.TSHI(TSH, FT4) - 2.21), 0.05);
  expect_lt(abs(estimated.TTSI(TSH, FT4, 18) - 91.7), 0.05);
})

test_that("Scenario 10: SPINA-GT = 1.08 pmol/s, SPINA-GD = 336.2 nmol/s",
{
  TSH <- 3.24;
  FT4 <- 7.7;
  FT3 <- 28;
  expect_lt(abs(SPINA.GT(TSH, FT4) - 1.08), 0.05);   # Result rounded
  expect_lt(abs(SPINA.GD(FT4, FT3) - 336.2), 0.05);  # Difference should be less than 0.05
})

test_that("Scenario 11: SPINA-GT = 3.37 pmol/s, SPINA-GD = 63.7 nmol/s",
{
  TSH <- 0.7;
  FT4 <- 9;
  FT3 <- 6.2;
  expect_lt(abs(SPINA.GT(TSH, FT4) - 3.37), 0.05);   # Result rounded
  expect_lt(abs(SPINA.GD(FT4, FT3) - 63.7), 0.05);   # Difference should be less than 0.05
})

test_that("Scenarios 9 to 11 in vector form, length 3",
{
  TSH <- c(1, 3.24, 0.7);
  FT4 <- c(16.5, 7.7, 9);
  FT3 <- c(4.5, 28, 6.2);
  expect_length(SPINA.GT(TSH, FT4), 3);
  expect_length(SPINA.GD(FT4, FT3), 3)
})

test_that("Scenario 13: SPINA-GT = 5.73 pmol/s, SPINA-GD = 36.12 nmol/s, TSHI = -0.91, TTSI = 4",
{
  TSH <- 0.2;
  FT4 <- 5.12;
  FT3 <- 2;
  expect_lt(abs(SPINA.GT(TSH, FT4) - 5.73), 0.05);   # Result rounded
  expect_lt(abs(SPINA.GD(FT4, FT3) - 36.12), 0.05);  # Difference should be less than 0.05
  expect_lt(abs(estimated.TSHI(TSH, FT4) - -0.91), 0.05);
  expect_lt(abs(estimated.TTSI(TSH, FT4, 18) - 5.7), 0.05);
})

test_that("Scenario 100: SPINA-GBeta = 2.8 pmol/s, SPINA-GR = 2.3 mol/s",
{
  Insulin <- 63.01;
  Glucose <- 4.34;
  expect_lt(abs(SPINA.GBeta(Insulin, Glucose) - 2.8), 0.05);   # Result rounded
  expect_lt(abs(SPINA.GR(Insulin, Glucose) - 2.3), 0.05);  # Difference should be less than 0.05
  expect_lt(abs(SPINA.DI(Insulin, Glucose) - 2.8 * 2.3), 0.09);
})

test_that("Scenario 101: SPINA-GBeta = 2.8 pmol/s, SPINA-GR = 0.7 mol/s",
{
  Insulin <- 88.77;
  Glucose <- 8.18;
  expect_lt(abs(SPINA.GBeta(Insulin, Glucose) - 2.8), 0.05);   # Result rounded
  expect_lt(abs(SPINA.GR(Insulin, Glucose) - 0.7), 0.05);  # Difference should be less than 0.05
  expect_lt(abs(SPINA.DI(Insulin, Glucose) - 2.8 * 0.7), 0.09);
})

test_that("Scenario 102: SPINA-GBeta = 0.6 pmol/s, SPINA-GR = 2.3 mol/s",
{
  Insulin <- 20.33;
  Glucose <- 9.51;
  expect_lt(abs(SPINA.GBeta(Insulin, Glucose) - 0.6), 0.05);   # Result rounded
  expect_lt(abs(SPINA.GR(Insulin, Glucose) - 2.3), 0.05);  # Difference should be less than 0.05
  expect_lt(abs(SPINA.DI(Insulin, Glucose) - 0.6 * 2.3), 0.09);
})

test_that("Scenarios 100 to 102 in vector form, length 3",
{
  Insulin <- c(63.01, 88.77, 20.33);
  Glucose <- c(4.34, 8.18, 9.51);
  expect_length(SPINA.GBeta(Insulin, Glucose), 3);
  expect_length(SPINA.GR(Insulin, Glucose), 3);
  expect_length(SPINA.DI(Insulin, Glucose), 3);
})
