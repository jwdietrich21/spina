unit SPINA_Types;

{ SPINA-Thyr }

{ Application for calculating structure parameters }
{ of thyrotropic feedback control }

{ Programm zur Berechnung von Strukturparametern }
{ des thyreotropen Regelkreises }

{ Version 4.2.2 (Kontinuum) }

{ (c) J. W. Dietrich, 1994 - 2025 }
{ (c) Ludwig Maximilian University of Munich 1995 - 2002 }
{ (c) University of Ulm Hospitals 2002 - 2004 }
{ (c) Ruhr University of Bochum 2005 - 2025 }

{ Common globals and types that are used by multiple units }

{ Source code released under the BSD License }
{ See http://spina.medical-cybernetics.de for details }

{$mode objfpc}

interface

uses
  Classes, SysUtils, Forms, Graphics;

const
  sOK = 0;
  sNegative = 101;
  sOutOfRange = 102;

  T4_MOLAR_MASS = 776.87; {molar mass of T4}
  T3_MOLAR_MASS = 650.97; {molar mass of T3}
  kTAB = chr(9);
  kLF = chr(10);
  kCR = chr(13);
  DEC_POINT = '.';
  DEC_COMMA = ',';
  NA_MARK = 'N/A';
  TEXT_WIDTH = 10;
  TSH_UNIT = 'mU/l';   {standard initial unit}
  FT4_UNIT = 'ng/dl';  {standard initial unit}
  FT3_UNIT = 'pg/ml';  {standard initial unit}
  kMarginSpaces = '                                    ';

  BASE_URL = 'http://spina.medical-cybernetics.de';
  SPINA_GLOBAL_ID = 'net.sf.spina';
  SPINA_THYR_GLOBAL_ID = 'net.sf.spina.thyr';
  HELP_URL = 'http://spina.sourceforge.net/manual.html';
  PORTAL_URL = 'http://www.ruhr-uni-bochum.de/spina-portal';

  IMPLEMENTATION_MESSAGE = 'This function is not implemented in this version of SPINA Thyr.';
  FORMAT_MESSAGE = 'Please check your input.';
  FILE_FORMAT_MESSAGE = 'File format error.';
  SAVE_ERROR_MESSAGE = 'Error saving the file.';
  FILE_EMPTY_MESSAGE = 'No lab results available.';
  RR_FORMAT_ERROR_MESSAGE = 'The CDISC Lab model XML file is malformatted.';
  RR_SPINA_ERROR_MESSAGE = 'Definitions for structure parameters in CDISC Lab model XML file are missing.';
  PREFERENCES_SAVE_ERROR_MESSAGE = 'The preferences could not be saved permanently, however, they are valid for this session.';
  FOLDER_NOT_SUPPORTED_MESSAGE = 'Folders are not supported in this version of SPINA Thyr.';
  URLS_NOT_SUPPORTED_MESSAGE = 'URLs are not supported in this version of SPINA Thyr.';

  MAXFACTORS = 10; {for measurement units and preferences}

  REF_RANGE_FLAG = '*'; {flag sign for marking results outside the reference range}

  clLtYellow = TColor($66FFFF);
  clLtOrange = TColor($89E9FF);

type
Str3 = string[3];
Str255 = String[255];
tCodeList = set of 0..255;
tLabMethod = (freeHormone, totalHormone);
tParameterSettings = record
       Method: tLabMethod;
       isSI: boolean;
       UOM: string;
       UnitFactor: real;
       PopUpItem: integer;
       MethodPopUpItem: integer;
end;
tPreferences = record
       new, rememberUsedUnits, colouriseMandatoryFields, exportLOINC: boolean;
       TSH, T4, T3: tParameterSettings;
       MandatoryColor: TColor;
       MSH_ID, Placer_ID: String;
       PrintFont: String;
end;
tReferenceAlerts = record
       ln, hn, lt, ht, lp, hp: real;
       UOM: string;
end;
tReferenceExDefinitions = record
       Sex: char;
       AgeL, AgeH: integer;
       UOMS, UOMC: string;
       LXS, HXS, LXC, HXC: real;
       startDateTime: string
end;
tReferenceNormDefinitions = record
       Sex: char;
       AgeL, AgeH: integer;
       UOMS, UOMC: string;
       LS, HS, LTS, HTS, LPS, HPS, LC, HC, LTC, HTC, LPC, HPC: real;
       startDateTime: string
end;
tReferenceValues = record
       TSH, FT4, FT3, TT4, TT3, GT, GD, sGD, TSHI, sTSHI, TTSI: tReferenceAlerts;
       meanTSHI, sdTSHI: real;
end;

tPrefsFile = file of tPreferences;
tInterfaceLanguage = (English, German);

var
  gStartup, gCEcertified: boolean;
  T4UnitLabel, T3UnitLabel: array[0..MAXFACTORS - 1] of Str3;
  T4UnitFactor, T3UnitFactor: array[0..MAXFACTORS - 1] of real;
  gUseReferenceRanges: boolean;
  gHostName, gPrefsDir, gPrefsFileName: String;
  gTSHRR, gFT4RR, gTT4RR, gFT3RR, gTT3RR, gGTRR, gGDRR, gsGDRR, gTSHIRR,
    gsTSHIRR, gTTSIRR: string;
  gPreferences: tPreferences;
  gPrefsFile: tPrefsFile;
  gAnleitung1, gAnleitung2, gVerhaltensparameter, gStrukturparameter,
    gReferenzbereiche: string;
  gResultHint, gHintCaption, gTherapyHint, gBenutzername: string;
  gPatientenname, gGeburtsdatum, gUntersuchungsdatum, gEinsender,
    gDruckdatum: string;
  gUncertified1, gUncertified2, gUncertified3, gUncertified4: string;
  gPreferencesHint: string;
  gNegativeError, gReferenceRangeError: string;
  gcalcTitle, gcalcString, gNotCalculableString: Str255;
  gExplanationString: Str255;
  gRefExp, gGTRef, gGDRef, gSignalString, gParameterString: Str255;
  gResultString: string;
  gTSHUnit, gT4Unit, gT3Unit: Str255;
  gInterfaceLanguage: tInterfaceLanguage;
  gResultDialogString1, gResultDialogString2, gReferenceValueString1, gReferenceValueString2: Str255;
  gReferenceRanges, gSIReferenceRanges, gConvReferenceRanges: tReferenceValues;
  gLastActiveCustomForm: TForm;
  gStandardMandatoryColor: TColor;

implementation

end.
