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

print();

print('Additional test cases derived from simulations:');      

print('Scenario 100: GBeta = 2.8 pmol/s. GR = 2.3 mol/s:')

Glucose = 4.34
Insulin = 63.01
print('SPINA-GBeta:', SPINA_GBeta(Insulin, Glucose), 'pmol/s')  
print('SPINA-GR:', SPINA_GR(Insulin, Glucose), 'mol/s')  
print('SPINA-DI:', SPINA_DI(Insulin, Glucose))  
print('HOMA-Beta:', HOMA_Beta(Insulin, Glucose), '%')  
print('HOMA-IR:', HOMA_IR(Insulin, Glucose))  
print('HOMA-IS:', HOMA_IS(Insulin, Glucose))  
print('QUICKI', QUICKI(Insulin, Glucose))  

print();

print('Scenario 101: GBeta = 2.8 pmol/s. GR = 0.7 mol/s:')

Glucose = 8.18
Insulin = 88.77
print('SPINA-GBeta:', SPINA_GBeta(Insulin, Glucose), 'pmol/s')  
print('SPINA-GR:', SPINA_GR(Insulin, Glucose), 'mol/s')  
print('SPINA-DI:', SPINA_DI(Insulin, Glucose))  
print('HOMA-Beta:', HOMA_Beta(Insulin, Glucose), '%')  
print('HOMA-IR:', HOMA_IR(Insulin, Glucose))  
print('HOMA-IS:', HOMA_IS(Insulin, Glucose))  
print('QUICKI', QUICKI(Insulin, Glucose))  

print();

print('Scenario 102: GBeta = 0.6 pmol/s. GR = 2.3 mol/s:')

Glucose = 9.51
Insulin = 20.33
print('SPINA-GBeta:', SPINA_GBeta(Insulin, Glucose), 'pmol/s')  
print('SPINA-GR:', SPINA_GR(Insulin, Glucose), 'mol/s')  
print('SPINA-DI:', SPINA_DI(Insulin, Glucose))  
print('HOMA-Beta:', HOMA_Beta(Insulin, Glucose), '%')  
print('HOMA-IR:', HOMA_IR(Insulin, Glucose))  
print('HOMA-IS:', HOMA_IS(Insulin, Glucose))  
print('QUICKI', QUICKI(Insulin, Glucose))  

print();

print('Scenario 103: GBeta = 0.6 pmol/s. GR = 2.3 mol/s:')

Glucose = 9.51
Insulin = 20.33
print('SPINA-GBeta:', SPINA_GBeta(Insulin, Glucose), 'pmol/s')  
print('SPINA-GR:', SPINA_GR(Insulin, Glucose), 'mol/s')  
print('SPINA-DI:', SPINA_DI(Insulin, Glucose))  
print('HOMA-Beta:', HOMA_Beta(Insulin, Glucose), '%')  
print('HOMA-IR:', HOMA_IR(Insulin, Glucose))  
print('HOMA-IS:', HOMA_IS(Insulin, Glucose))  
print('QUICKI', QUICKI(Insulin, Glucose))  

print();

print('Scenario 104: GBeta = 13.0 pmol/s. GR = 2.3 mol/s:')

Glucose = 1.96
Insulin = 167.09
print('SPINA-GBeta:', SPINA_GBeta(Insulin, Glucose), 'pmol/s')  
print('SPINA-GR:', SPINA_GR(Insulin, Glucose), 'mol/s')  
print('SPINA-DI:', SPINA_DI(Insulin, Glucose))  
print('HOMA-Beta:', HOMA_Beta(Insulin, Glucose), '%')  
print('HOMA-IR:', HOMA_IR(Insulin, Glucose))  
print('HOMA-IS:', HOMA_IS(Insulin, Glucose))  
print('QUICKI', QUICKI(Insulin, Glucose))  

print();
