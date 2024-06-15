unit SPINATypes;

{ SPINA Carb }

{ Application for calculating structure parameters }
{ of insulin-glucose feedback control }

{ Programm zur Berechnung von Strukturparametern }
{ des Insulin-Glukose-Regelkreises }

{ Version 5.1.0 (Cyclone) }

{ (c) J. W. Dietrich, 1994 - 2024 }
{ (c) Ludwig Maximilian University of Munich 1995 - 2002 }
{ (c) University of Ulm Hospitals 2002 - 2004 }
{ (c) Ruhr University of Bochum 2005 - 2024 }

{ Global types and constants }

{ Source code released under the BSD License }
{ See http://spina.medical-cybernetics.de for details }

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils;

const
  kNUL = char(0);           {Special ASCII characters}
  kENTER = char(3);
  kTAB = char(9);
  kLF = char(10);
  kRETURN = char(13);
  kESCAPE = char(27);
  kPERIOD = '.';
  kSPACE = ' ';
  kSLASH = '/';
  kCOLON = ':';
  kOMIT = '•';
  kCRLF = #13#10;

  kBPars = 'Behavioural parameters:';
  kSPars = 'Structural parameters:';
  kGluc = 'Glucose';
  kIns = 'Insulin';
  kSPINA_GBeta = 'SPINA-GBeta';
  kSPINA_GR = 'SPINA-GR';
  kSPINA_DI = 'SPINA-DI';
  kHOMA_Beta = 'HOMA-Beta';
  kHOMA_IR = 'HOMA-IR';
  kHOMA_IS = 'HOMA-IS';
  kQUICKI = 'QUICKI';
  GBetaUoM = 'pmol/s';
  GRUoM = 'mol/s';
  HOMABetaUoM = '%';

implementation

end.

