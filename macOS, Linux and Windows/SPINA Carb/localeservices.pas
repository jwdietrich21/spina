unit LocaleServices;

{ SPINA Carb }

{ Application for calculating structure parameters }
{ of insulin-glucose feedback control }

{ Programm zur Berechnung von Strukturparametern }
{ des Insulin-Glukose-Regelkreises }

{ Version 5.1.0 (Cyclone) }

{ (c) J. W. Dietrich, 1994 - 2025 }
{ (c) Ludwig Maximilian University of Munich 1995 - 2002 }
{ (c) University of Ulm Hospitals 2002 - 2004 }
{ (c) Ruhr University of Bochum 2005 - 2025 }

{ This unit provides services for localisation and internationalisation }

{ Source code released under the BSD License }
{ See http://spina.medical-cybernetics.de for details }

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils,
  {$IFDEF Windows}
  Windows,
  {$ELSE}
  {$IFDEF LCLCarbon}
  MacOSAll, CarbonProc,
  {$ELSE}
  {$IFDEF LCLCocoa}
  CocoaAll, MacOSAll, CocoaUtils,
  {$ENDIF}
  {$ENDIF}
  {$ENDIF}
  SPINATypes
;

procedure GetMacDateFormats;
function StrToFloatDefL(const S: string; const Default: Extended): Extended;

implementation

procedure GetMacDateFormats;
var
  theFormatString: string;
  {$IFDEF Darwin}
  theFormatter: CFDateFormatterRef;
  {$ENDIF}
begin
  {$IFDEF Darwin}
  theFormatter := CFDateFormatterCreate(kCFAllocatorDefault,
    CFLocaleCopyCurrent, kCFDateFormatterMediumStyle, kCFDateFormatterNoStyle);
  theFormatString := CFStringToStr(CFDateFormatterGetFormat(theFormatter));
  if pos('yyyy', theFormatString) = 0 then // year format shorter than 4 digits
  begin
    theFormatString := StringReplace(theFormatString, 'y', 'yyyy', [rfIgnoreCase]);
  end;
  if pos('.', theFormatString) > 0 then
    DefaultFormatSettings.DateSeparator := '.'
  else if pos('/', theFormatString) > 0 then
    DefaultFormatSettings.DateSeparator := '/'
  else if pos('-', theFormatString) > 0 then
    DefaultFormatSettings.DateSeparator := '-';
  DefaultFormatSettings.ShortDateFormat := theFormatString;
  CFRelease(theFormatter);
  theFormatter := CFDateFormatterCreate(kCFAllocatorDefault,
    CFLocaleCopyCurrent, kCFDateFormatterLongStyle, kCFDateFormatterNoStyle);
  theFormatString := CFStringToStr(CFDateFormatterGetFormat(theFormatter));
  if pos('yyyy', theFormatString) = 0 then // year format shorter than 4 digits
  begin
    theFormatString := StringReplace(theFormatString, 'y', 'yyyy', [rfIgnoreCase]);
  end;
  DefaultFormatSettings.LongDateFormat := theFormatString;
  CFRelease(theFormatter);
  {$ENDIF}
end;

function StrToFloatDefL(const S: string; const Default: Extended): Extended;
{ Localised version of StrToFloatDef }
var
  oldSeparator: char;
begin
  oldSeparator := DefaultFormatSettings.decimalSeparator;
  if pos(DEC_COMMA, S) > 0 then
    DefaultFormatSettings.decimalSeparator := DEC_COMMA
  else
    DefaultFormatSettings.decimalSeparator := DEC_POINT;
  Result := StrToFloatDef(S, Default);
  DefaultFormatSettings.decimalSeparator := oldSeparator;
end;

end.

