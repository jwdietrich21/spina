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
  Classes, SysUtils, Graphics;

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
  kCpt = 'C-Peptide';
  kSPINA_GBeta = 'SPINA-GBeta';
  kSPINA_GR = 'SPINA-GR';
  kSPINA_DI = 'SPINA-DI';
  kHOMA_Beta = 'HOMA-Beta';
  kHOMA_IR = 'HOMA-IR';
  kHOMA_IS = 'HOMA-IS';
  kQUICKI = 'QUICKI';
  kCGR = 'CGR';
  GBetaUoM = 'pmol/s';
  GRUoM = 'mol/s';
  HOMABetaUoM = '%';

  kPID2 = 'PID: ';
  kName2 = 'Patient name: ';
  kPlacer2 = 'Placer: ';
  kExamDate2 = 'Examination Date: ';
  kDOB2 = 'Date of Birth: ';
  kCaseNum2 = 'Case / Admission Number: ';
  kPrintingDate = 'Printing Date: ';
  kUserName = 'User name: ';

  BASE_URL = 'http://spina.medical-cybernetics.de';
  SPINA_GLOBAL_ID = 'net.sf.spina';
  SPINA_CARB_GLOBAL_ID = 'net.sf.spina.carb';
  HELP_URL = 'http://spina.sourceforge.net/manual.html';
  PORTAL_URL = 'http://www.ruhr-uni-bochum.de/spina-portal';

  IMPLEMENTATION_MESSAGE = 'This function is not implemented in this version of SPINA Thyr.';
  FORMAT_MESSAGE = 'Please check your input.';
  FILE_FORMAT_MESSAGE = 'File format error.';
  SAVE_ERROR_MESSAGE = 'Error saving the file.';
  FILE_EMPTY_MESSAGE = 'No lab results available.';
  RR_FORMAT_ERROR_MESSAGE = 'The CDISC Lab model XML file is malformatted.';
  RR_SPINA_ERROR_MESSAGE = 'Definitions for structure parameters in CDISC Lab model XML file are missing.';
  PREFERENCES_READ_ERROR_MESSAGE = 'Preferences could not be read. Please check access rights of your user or home folder';
  PREFERENCES_SAVE_ERROR_MESSAGE = 'The preferences could not be saved permanently, however, they are valid for this session.';
  FOLDER_NOT_SUPPORTED_MESSAGE = 'Folders are not supported in this version of SPINA Carb.';
  URLS_NOT_SUPPORTED_MESSAGE = 'URLs are not supported in this version of SPINA Carb.';

  ISO_8601_DATE_FORMAT = 'YYYY-MM-DD"T"hh:nn:ss'; {Date/time format in XML representation}
  STANDARD_NUM_FORMAT = '###,##0.0000';
  SHORT_NUM_FORMAT = '###,###.00';
  STANDARD_TIME_FORMAT = '"d"D hh:nn:ss';

  clLtYellow = TColor($66FFFF);
  clLtOrange = TColor($89E9FF);

type
  tPreferences = record
       new, rememberUsedUnits, colouriseMandatoryFields, exportLOINC: boolean;
       MandatoryColor: TColor;
       MSH_ID, Placer_ID: String;
       PrintFont: String;
  end;

var
  gPreferences: tPreferences;
  gNumberFormat, gDateTimeFormat: String;
  gStandardMandatoryColor: TColor;

implementation

initialization
  gNumberFormat := STANDARD_NUM_FORMAT;
  gDateTimeFormat := STANDARD_TIME_FORMAT;

end.

