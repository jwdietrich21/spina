unit SPINA_Types;

{ SPINA-Thyr }

{ Application for calculating structure parameters }
{ of thyrotropic feedback control }

{ Programm zur Berechnung von Strukturparametern }
{ des thyreotropen Regelkreises }

{ Version 3.3 }

{ (c) J. W. Dietrich, 1994 - 2013 }
{ (c) Ludwig Maximilian University of Munich 1995 - 2002 }
{ (c) University of Ulm Hospitals 2002-2004 }
{ (c) Ruhr University of Bochum 2005 - 2013 }

{ Common globals and types that are used by multiple units }

{ Source code released under the BSD License }
{ See http://spina.medical-cybernetics.de for details }

{$mode objfpc}

interface

uses
  Classes, SysUtils, Forms;

const
  UFT4 = 1.28e-11; {Conversion factor ng/dl -> mol/l (T4)}
  UFT3 = 1.54e-12; {Conversion factor pg/ml -> mol/l (T3)}
  kTAB = chr(9);
  kLF = chr(10);
  kCR = chr(13);
  DEC_POINT = '.';
  DEC_COMMA = ',';
  TEXT_WIDTH = 10;
  TSH_UNIT = 'mU/l';
  FT4_UNIT = 'ng/dl';
  FT3_UNIT = 'pg/ml';
  kAnleitung01 = '';
  kAnleitung02 = '';
  kAnleitung11 =
    'Bitte geben Sie die gemessenen Werte für TSH, T4 (oder FT4) und T3 (oder FT3) ein und klicken Sie dann auf "Berechnen".'#13#10#13#10'Im Falle einer Substitutionstherapie klicken Sie bitte auf die Boxen rechts in diesem Fenster unter "Therapie".';
  kAnleitung12 =
    'Please enter simultaneously obtained values for TSH, T4 (or FT4) and T3 (or FT3), and click on "Calculate".'#13#10#13#10'In case of substitution therapy please click the corresponding boxes in the right of this window under the captionn "Therapy".';
  kAnleitung21 = 'Sie können das Ergebnis nun ausdrucken oder in die Zwischenablage kopieren.';
  kAnleitung22 = 'You may want to print or copy the result now.';
  kVerhaltensparameter1 = 'Verhaltensparameter:';
  kVerhaltensparameter2 = 'Behavioural parameters:';
  kStrukturparameter1 = 'Strukturparameter:';
  kStrukturparameter2 = 'Structural parameters:';
  kReferenzbereiche1 = 'Referenzbereiche:';
  kReferenzbereiche2 = 'Reference ranges:';
  kNotCalculatable1 = '<Nicht berechenbar>';
  kNotCalculatable2 = '<Not computable>';
  kPatientenname1 = 'Patientenname: ';
  kPatientenname2 = 'Patient name: ';
  kUntersuchungsdatum1 = 'Untersuchungsdatum: ';
  kUntersuchungsdatum2 = 'Examination Date: ';
  kGeburtsdatum1 = 'Geburtsdatum: ';
  kGeburtsdatum2 = 'Birth date: ';
  kEinsender1 = 'Einsender: ';
  kEinsender2 = 'Sender: ';
  kDruckdatum1 = 'Druckdatum: ';
  kDruckdatum2 = 'Printing Date: ';
  kBenutzername1 = 'Benutzerkennung: ';
  kBenutzername2 = 'User name: ';
  kResultHint1 = 'Ergebnis:';
  kResultHint2 = 'Result:';
  kTherapyHint1 = 'Therapie:';
  kTherapyHint2 = 'Therapy:';
  kHintCaption1 = 'Hinweis:';
  kHintCaption2 = 'Hint:';
  kPreferencesHint1 = 'Die Voreinstellungsdatei wurde neu angelegt. Bitte überprüfen Sie Parameter und Maßeinheiten.';
  kPreferencesHint2 = 'A new preferences file has been created. Please check parameters and measurement units.';
  kPreferences1 = 'Einstellungen';
  kPreferences2 = 'Preferences';
  kMethodLabel1 = 'Methode';
  kMethodLabel2 = 'Method';
  kUnitLabel1 = 'Einheit';
  kUnitLabel2 = 'Unit';
  kUnitsGroupCaption1 = 'Methoden und Maßeinheiten:';
  kUnitsGroupCaption2 = 'Methods and measurement units:';
  kRemember1 = 'Zuletzt verwendete Maßeinheiten merken';
  kRemember2 = 'Remember last used unit';
  kCDISCCaption1 = 'Referenzbereiche:';
  kCDISCCaption2 = 'Reference values:';
  kCancel1 = 'Abbrechen';
  kCancel2 = 'Cancel';
  kReadCDISC1 = 'Aus Datei lesen ...';
  kReadCDISC2 = 'Read from file ...';
  kMarginSpaces = '                                    ';

  BASE_URL = 'http://spina.medical-cybernetics.de';
  SPINA_GLOBAL_ID = 'net.sf.spina';

  IMPLEMENTATION_MESSAGE = 'This function is not implemented in this version of SPINA Thyr.';
  FORMAT_MESSAGE = 'Please check your input.';
  SAVE_ERROR_MESSAGE = 'Error saving the file';
  RR_FORMAT_ERROR_MESSAGE = 'The CDISC Lab model XML file is malformatted.';
  RR_SPINA_ERROR_MESSAGE = 'Definitions for structure parameters in CDISC Lab model XML file are missing.';
  PREFERENCES_SAVE_ERROR_MESSAGE = 'The preferences could not be saved permanently, however, they are valid for this session.';

  MAXFACTORS = 10; {for measurement units and preferences}

type
Str3 = string[3];
Str255 = String[255];
tCodeList = set of 0..255;
tUnitElements = record
       MassPrefix, MassUnit, VolumePrefix, VolumeUnit: String;
end;
tLabMethod = (freeHormone, totalHormone);
tParameterSettings = record
       Method: tLabMethod;
       isSI: boolean;
       measurementUnit: string;
       UnitFactor: real;
       PopUpItem: integer;
       MethodPopUpItem: integer;
end;
tPreferences = record
       new, rememberUsedUnits: boolean;
       TSH, T4, T3: tParameterSettings;
       end;
tReferenceAlerts = record
       ln, hn, lt, ht, lp, hp: real;
       measurementUnit: string;
       end;
tReferenceValues = record
       TSH, FT4, FT3, TT4, TT3, GT, GD: tReferenceAlerts;
end;

tPrefsFile = file of tPreferences;
tInterfaceLanguage = (English, German);

var
  gStartup: boolean;
  PrefixLabel, T4UnitLabel, T3UnitLabel: array[0..MAXFACTORS - 1] of Str3;
  PrefixFactor, T4UnitFactor, T3UnitFactor: array[0..MAXFACTORS - 1] of real;
  gTSHTherapy, gT4Therapy, gT3Therapy, gUseReferenceRanges: boolean;
  gPrefsDir, gPrefsFileName: String;
  gTSHRR, gFT4RR, gTT4RR, gFT3RR, gTT3RR, gGTRR, gGDRR: string;
  gCDISC_RR: TStrings;
  gPreferences: tPreferences;
  gPrefsFile: tPrefsFile;
  gAnleitung1, gAnleitung2, gVerhaltensparameter, gStrukturparameter, gReferenzbereiche: string;
  gResultHint, gHintCaption, gTherapyHint, gBenutzername: string;
  gPatientenname, gGeburtsdatum, gUntersuchungsdatum, gEinsender, gDruckdatum: string;
  gPreferencesHint: string;
  gcalcTitle, gcalcString, gNotCalculableString: Str255;
  gExplanationString, gMessageString, TSH_String, T4_String, T3_String: Str255;
  gRefExp, gGTRef, gGDRef, gSignalString, gParameterString: Str255;
  gTSHUnit, gT4Unit, gT3Unit, gResultString: Str255;
  gInterfaceLanguage: tInterfaceLanguage;
  gResultDialogString1, gResultDialogString2, gReferenceValueString1, gReferenceValueString2: Str255;
  gReferenceRanges, gSIReferenceRanges, gConvReferenceRanges: tReferenceValues;
  gLastActiveCustomForm: TForm;

implementation

end.
