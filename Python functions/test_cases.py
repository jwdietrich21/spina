#!/usr/bin/env python3

# SPINA Carb

# Software for calculating structure parameters
# of insulin-glucose feedback control

# Version 5.1.0 (Cyclone)

# (c) J. W. Dietrich, 1994 - 2024 
# (c) Ludwig Maximilian University of Munich 1995 - 2002 
# (c) University of Ulm Hospitals 2002 - 2004 
# (c) Ruhr University of Bochum 2005 - 2024 

# This unit implements test cases

# Source code released under the BSD License 
# See http://spina.medical-cybernetics.de for details  

from spina_functions import *

print('Test Cases for SPINA Carb');
print('-------------------------');

print('Test Case V1:')

Glucose = 92 / GLUCOSECONVERSIONFACTOR
Insulin = 1.2 * INSULINCONVERSIONFACTOR
print('SPINA-GBeta:', SPINA_GBeta(Insulin, Glucose), 'pmol/s')  
print('SPINA-GR:', SPINA_GR(Insulin, Glucose), 'mol/s')  
print('SPINA-DI:', SPINA_DI(Insulin, Glucose))  
print('HOMA-Beta:', HOMA_Beta(Insulin, Glucose), '%')  
print('HOMA-IR:', HOMA_IR(Insulin, Glucose))  
print('HOMA-IS:', HOMA_IS(Insulin, Glucose))  
print('QUICKI', QUICKI(Insulin, Glucose))  
