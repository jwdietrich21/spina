#!/usr/bin/env python3

# SPINA Carb

# Software for calculating structure parameters
# of insulin-glucose feedback control

# Version 5.1.0 (Cyclone)

# (c) J. W. Dietrich, 1994 - 2024 
# (c) Ludwig Maximilian University of Munich 1995 - 2002 
# (c) University of Ulm Hospitals 2002 - 2004 
# (c) Ruhr University of Bochum 2005 - 2024 

# This unit implements the calculation engine 

# Source code released under the BSD License 
# See http://spina.medical-cybernetics.de for details  

import math

INSULINCONVERSIONFACTOR = 6;
GLUCOSECONVERSIONFACTOR = 18;
PICOFACTOR = 1E12;
MILLIFACTOR = 1E3;
BETAI = 3.4E-3;
ALPHAI = 0.2;
DBETA = 7E-3;
ALPHAG = 0.11;
BETAG = 7.1E-4; 
P0 = 150e-6;
DR = 1.6e-9;
GE = 50;
P1 = 22.5;
P2 = 20;
P3 = 3.5;
kError101 = 'Runtime error: Negative parameters';
kError102 = 'Runtime error: Parameter out of range';


def SPINA_GBeta(Insulin, Glucose):
  result = float("NaN")
  if math.isnan(Insulin) or math.isnan(Glucose):
    result = float("NaN")
  else:
    result = PICOFACTOR * BETAI * Insulin / PICOFACTOR * (DBETA + Glucose / MILLIFACTOR) / (ALPHAI * Glucose / MILLIFACTOR)
  if (Insulin == 0) and not (math.isnan(Glucose)):
    result = 0
  return result

def SPINA_GR(Insulin, Glucose):
  result = float("NaN")
  if math.isnan(Insulin) or math.isnan(Glucose) or (Insulin == 0) or (Glucose == 0):
    result = float("NaN")
  else:
    result = ALPHAG * P0 * (DR + Insulin / PICOFACTOR) / (BETAG * GE * Insulin / PICOFACTOR * Glucose / MILLIFACTOR) - DR / (GE * Insulin / PICOFACTOR) - 1 / GE
  if result < 0: result = 0
  return result

def SPINA_DI(Insulin, Glucose):
  result = float("NaN")
  if math.isnan(Insulin) or math.isnan(Glucose):
    result = float("NaN")
  else:
    result = SPINA_GBeta(Insulin, Glucose) * SPINA_GR(Insulin, Glucose)
  return result

def HOMA_Beta(Insulin, Glucose):
  result = float("NaN")
  if math.isnan(Insulin) or math.isnan(Glucose) or (Glucose <= P3):
    result = float("NaN")
  else:
    result = P2 * Insulin / INSULINCONVERSIONFACTOR / (Glucose - P3)
  return result

def HOMA_IR(Insulin, Glucose):
  result = float("NaN")
  if math.isnan(Insulin) or math.isnan(Glucose):
    result = float("NaN")
  else:
    result = Glucose * Insulin / INSULINCONVERSIONFACTOR / P1
  return result

def HOMA_IS(Insulin, Glucose):
  result = float("NaN")
  if math.isnan(Insulin) or math.isnan(Glucose) or (HOMA_IR(Insulin, Glucose) == 0):
    result = float("NaN")
  else:
    result = 1 / HOMA_IR(Insulin, Glucose)
  return result

def QUICKI(Insulin, Glucose):
  result = float("NaN")
  if math.isnan(Insulin) or math.isnan(Glucose) or (Insulin == 0) or (Glucose == 0):
    result = float("NaN")
  else:
    result = 1 / (math.log10(Insulin / INSULINCONVERSIONFACTOR) + math.log10(Glucose * GLUCOSECONVERSIONFACTOR))
  return result


