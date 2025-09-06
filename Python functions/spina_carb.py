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

print()
print('SPINA Carb, Python version');
print('--------------------------');
print()

GlucoseC = input("Glucose concentration (mg/dL): ")
InsulinC = input("Insulin concentration (mIU/L): ")
Glucose = float(GlucoseC) / GLUCOSECONVERSIONFACTOR
Insulin = float(InsulinC) * INSULINCONVERSIONFACTOR
print()
print('Glucose:', GlucoseC, 'mg/dL')
print('Insulin:', InsulinC, 'mIU/L')
print('SPINA-GBeta:', round(SPINA_GBeta(Insulin, Glucose), 2), 'pmol/s')  
print('SPINA-GR:', round(SPINA_GR(Insulin, Glucose), 2), 'mol/s')  
print('SPINA-DI:', round(SPINA_DI(Insulin, Glucose), 2))  
print('HOMA-Beta:', round(HOMA_Beta(Insulin, Glucose), 1), '%')  
print('HOMA-IR:', round(HOMA_IR(Insulin, Glucose), 1))  
print('HOMA-IS:', round(HOMA_IS(Insulin, Glucose), 1))  
print('QUICKI', round(QUICKI(Insulin, Glucose), 1))  

print()
