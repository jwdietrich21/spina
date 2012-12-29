unit SPINA_Types;

{ SPINA-Thyr }

{ Application for calculating structure parameters }
{ of thyrotropic feedback control }

{ Programm zur Berechnung von Strukturparametern }
{ des thyreotropen Regelkreises }

{ Version 3.3 }

{ (c) J. W. Dietrich, 1994 - 2012 }
{ (c) Ludwig Maximilian University of Munich 1995 - 2002 }
{ (c) University of Ulm Hospitals 2002-2004 }
{ (c) Ruhr University of Bochum 2005 - 2012 }

{ Common globals and types that are used by multiple units }

{ Source code released under the BSD License }
{ See http://spina.medical-cybernetics.de for details }

{$mode objfpc}

interface

uses
  Classes, SysUtils;

const
  kTAB = chr(9);
  kLF = chr(10);
  kCR = chr(13);
  DEC_POINT = '.';
  DEC_COMMA = ',';
  TEXT_WIDTH = 10;
  TSH_UNIT = ' mU/l';
  FT4_UNIT = ' ng/dl';
  FT3_UNIT = ' pg/ml';
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
  kMarginSpaces = '                                    ';

  BASE_URL = 'http://spina.medical-cybernetics.de';
  SPINA_GLOBAL_ID = 'net.sf.spina';

  IMPLEMENTATION_MESSAGE = 'This function is not implemented in this version of SPINA Thyr.';
  FORMAT_MESSAGE = 'Please check your input.';
  SAVE_ERROR_MESSAGE = 'Error saving the file';
  PREFERENCES_SAVE_ERROR_MESSAGE = 'The preferences could not be saved permanently, however, they are valid for this session';


type
Str255 = String[255];
tCodeList = set of 0..255;
tLabMethod = (freeHormone, totalHormone);
tPreferences = record
       T4Method, T3Method: tLabMethod;
       TSHUnitFactor, T4UnitFactor, T3UnitFactor: real;
       TSHPopUpItem, T4PopUpItem, T3PopUpItem: integer;
       T4MethodPopUpItem, T3MethodPopUpItem: integer;
       end;
tReferenceAlerts = record
       ln, hn, lt, ht, lp, hp: real;
       end;
tReferenceValues = record
       TSH, FT4, FT3, TT4, TT3, GT, GD: tReferenceAlerts;
end;

tPrefsFile = file of tPreferences;
tInterfaceLanguage = (English, German);

var
  gStartup: boolean;
  gTSHTherapy, gT4Therapy, gT3Therapy, gUseReferenceRanges: boolean;
  gPrefsDir, gPrefsFileName: String;
  gTSHRR, gFT4RR, gTT4RR, gFT3RR, gTT3RR, gGTRR, gGDRR: string;
  gCDISC_RR: TStrings;
  gPreferences: tPreferences;
  gPrefsFile: tPrefsFile;
  gAnleitung1, gAnleitung2, gVerhaltensparameter, gStrukturparameter, gReferenzbereiche: string;
  gResultHint, gHintCaption, gTherapyHint, gBenutzername: string;
  gPatientenname, gGeburtsdatum, gUntersuchungsdatum, gEinsender, gDruckdatum: string;
  gcalcTitle, gcalcString, gNotCalculableString: Str255;
  gExplanationString, gMessageString, TSH_String, T4_String, T3_String: Str255;
  gRefExp, gGTRef, gGDRef, gSignalString, gParameterString: Str255;
  gTSHUnit, gT4Unit, gT3Unit, gResultString: Str255;
  gInterfaceLanguage: tInterfaceLanguage;
  gResultDialogString1, gResultDialogString2, gReferenceValueString1, gReferenceValueString2: Str255;
  gReferenceRanges: tReferenceValues;

implementation

end.

