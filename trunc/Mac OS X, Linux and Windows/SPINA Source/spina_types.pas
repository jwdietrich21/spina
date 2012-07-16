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

unit SPINA_Types;

{$mode objfpc}

interface

uses
  Classes, SysUtils;

const
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
tPrefsFile = file of tPreferences;

var
  gStartup: boolean;
  gTSHTherapy, gT4Therapy, gT3Therapy: boolean;
  gPrefsDir, gPrefsFileName: String;
  gPreferences: tPreferences;
  gPrefsFile: tPrefsFile;


implementation

end.

