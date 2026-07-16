
<!-- README.md is generated from README.Rmd. Please edit that file -->

# SPINA <img src="man/figures/logo.png" align="right" height="139" alt="" />

<!-- badges: start -->

<!-- badges: end -->

SPINA (structure parameter inference approach) is a cybernetic method
for advanced interpretation of laboratory results. It allows for
calculating constant structure parameters of endocrine feedback control
systems in vivo from hormone or metabolite concentrations that have been
obtained from serum or plasma specimens. The method is based on
mathematical and cybernetic modelling of processing structures

## Installation

You can install the development version of SPINA by visting
<https://spina.sf.net> and following the directions given there.

## Examples

These basic examples show how to use SPINA in your own S scripts with R:

``` r
library(SPINA)

TSH <- c(1, 3.24, 0.7);
FT4 <- c(16.5, 7.7, 9);
FT3 <- c(4.5, 28, 6.2);

print(paste("SPINA-GT:", SPINA.GT(TSH, FT4)));
#> [1] "SPINA-GT: 4.696993125"      "SPINA-GT: 1.08063057191358"
#> [3] "SPINA-GT: 3.36719507142857"
print(paste("SPINA-GD:", SPINA.GD(FT4, FT3)));
#> [1] "SPINA-GD: 25.2176153706294" "SPINA-GD: 336.22895406993" 
#> [3] "SPINA-GD: 63.6968730188034"
print(paste("SPINA-sGD:", SPINA.sGD(FT4, FT3)));
#> [1] "SPINA-sGD: -0.956476925874126" "SPINA-sGD: 61.245790813986"   
#> [3] "SPINA-sGD: 6.73937460376069"
```

``` r
library(SPINA)

Insulin <- 63.01;
Glucose <- 4.34;

print(paste("SPINA-GBeta:", SPINA.GBeta(Insulin, Glucose)));
#> [1] "SPINA-GBeta: 2.7988635483871"
print(paste("SPINA-GR:", SPINA.GR(Insulin, Glucose)));
#> [1] "SPINA-GR: 2.31865876698364"
print(paste("SPINA-DI:", SPINA.DI(Insulin, Glucose)));
#> [1] "SPINA-DI: 6.48960950405869"
```
