#!/usr/bin/env python3

# SPINA Carb

# Software for calculating structure parameters
# of insulin-glucose feedback control

# Version 5.1.0 (Cyclone)

# (c) J. W. Dietrich, 1994 - 2025 
# (c) Ludwig Maximilian University of Munich 1995 - 2002 
# (c) University of Ulm Hospitals 2002 - 2004 
# (c) Ruhr University of Bochum 2005 - 2025 

# This unit implements test cases

# Source code released under the BSD License 
# See http://spina.medical-cybernetics.de for details  

from spina_functions import *

print('Test Cases for SPINA Carb');
print('-------------------------');

print('Test Case V1:')

GlucoseC = 92
InsulinC = 1.2
Glucose = GlucoseC / GLUCOSECONVERSIONFACTOR
Insulin = InsulinC * INSULINCONVERSIONFACTOR
print('Glucose:', GlucoseC, 'mg/dL')
print('Insulin:', InsulinC, 'mIU/L')
print('SPINA-GBeta:', SPINA_GBeta(Insulin, Glucose), 'pmol/s')  
print('SPINA-GR:', SPINA_GR(Insulin, Glucose), 'mol/s')  
print('SPINA-DI:', SPINA_DI(Insulin, Glucose))  
print('HOMA-Beta:', HOMA_Beta(Insulin, Glucose), '%')  
print('HOMA-IR:', HOMA_IR(Insulin, Glucose))  
print('HOMA-IS:', HOMA_IS(Insulin, Glucose))  
print('QUICKI', QUICKI(Insulin, Glucose))  

print()

print('Scenario E1: Extreme values for insulin and glucose:')

GlucoseC = 700
InsulinC = 200
Glucose = GlucoseC / GLUCOSECONVERSIONFACTOR
Insulin = InsulinC * INSULINCONVERSIONFACTOR
print('Glucose:', GlucoseC, 'mg/dL')
print('Insulin:', InsulinC, 'mIU/L')
print('SPINA-GBeta:', SPINA_GBeta(Insulin, Glucose), 'pmol/s')  
print('SPINA-GR:', SPINA_GR(Insulin, Glucose), 'mol/s')  
print('SPINA-DI:', SPINA_DI(Insulin, Glucose))  
print('HOMA-Beta:', HOMA_Beta(Insulin, Glucose), '%')  
print('HOMA-IR:', HOMA_IR(Insulin, Glucose))  
print('HOMA-IS:', HOMA_IS(Insulin, Glucose))  
print('QUICKI', QUICKI(Insulin, Glucose))  

print()

print('Scenario E2: Extreme values for insulin and glucose:')

GlucoseC = 800
InsulinC = 0.1
Glucose = GlucoseC / GLUCOSECONVERSIONFACTOR
Insulin = InsulinC * INSULINCONVERSIONFACTOR
print('Glucose:', GlucoseC, 'mg/dL')
print('Insulin:', InsulinC, 'mIU/L')
print('SPINA-GBeta:', SPINA_GBeta(Insulin, Glucose), 'pmol/s')  
print('SPINA-GR:', SPINA_GR(Insulin, Glucose), 'mol/s')  
print('SPINA-DI:', SPINA_DI(Insulin, Glucose))  
print('HOMA-Beta:', HOMA_Beta(Insulin, Glucose), '%')  
print('HOMA-IR:', HOMA_IR(Insulin, Glucose))  
print('HOMA-IS:', HOMA_IS(Insulin, Glucose))  
print('QUICKI', QUICKI(Insulin, Glucose))  

print()

print('Scenario E3 Extreme values for insulin and glucose:')

GlucoseC = 800
InsulinC = 0
Glucose = GlucoseC / GLUCOSECONVERSIONFACTOR
Insulin = InsulinC * INSULINCONVERSIONFACTOR
print('Glucose:', GlucoseC, 'mg/dL')
print('Insulin:', InsulinC, 'mIU/L')
print('SPINA-GBeta:', SPINA_GBeta(Insulin, Glucose), 'pmol/s')  
print('SPINA-GR:', SPINA_GR(Insulin, Glucose), 'mol/s')  
print('SPINA-DI:', SPINA_DI(Insulin, Glucose))  
print('HOMA-Beta:', HOMA_Beta(Insulin, Glucose), '%')  
print('HOMA-IR:', HOMA_IR(Insulin, Glucose))  
print('HOMA-IS:', HOMA_IS(Insulin, Glucose))  
print('QUICKI', QUICKI(Insulin, Glucose))  

print()

print('Scenario E4: Extreme values for insulin and glucose:')

GlucoseC = 30
InsulinC = 1
Glucose = GlucoseC / GLUCOSECONVERSIONFACTOR
Insulin = InsulinC * INSULINCONVERSIONFACTOR
print('Glucose:', GlucoseC, 'mg/dL')
print('Insulin:', InsulinC, 'mIU/L')
print('SPINA-GBeta:', SPINA_GBeta(Insulin, Glucose), 'pmol/s')  
print('SPINA-GR:', SPINA_GR(Insulin, Glucose), 'mol/s')  
print('SPINA-DI:', SPINA_DI(Insulin, Glucose))  
print('HOMA-Beta:', HOMA_Beta(Insulin, Glucose), '%')  
print('HOMA-IR:', HOMA_IR(Insulin, Glucose))  
print('HOMA-IS:', HOMA_IS(Insulin, Glucose))  
print('QUICKI', QUICKI(Insulin, Glucose))  

print()