unit SPINA_GUIServices;

{ SPINA-Thyr }

{ Application for calculating structure parameters }
{ of thyrotropic feedback control }

{ Programm zur Berechnung von Strukturparametern }
{ des thyreotropen Regelkreises }

{ Version 4.1.2 (Bonfire) }

{ (c) J. W. Dietrich, 1994 - 2020 }
{ (c) Ludwig Maximilian University of Munich 1995 - 2002 }
{ (c) University of Ulm Hospitals 2002 - 2004 }
{ (c) Ruhr University of Bochum 2005 - 2020 }

{ This unit implements universal services for the GUI }

{ Source code released under the BSD License }
{ See http://spina.medical-cybernetics.de for details }

{$mode objfpc}{$H+}
{$IFDEF LCLCocoa}
  {$modeswitch objectivec1}
{$ENDIF}

interface

uses
  Classes, SysUtils, gettext
  {$IFDEF LCLCarbon}
  , MacOSAll
  {$ELSE}
  {$IFDEF LCLCocoa}
  , MacOSAll, CocoaAll, CocoaUtils
  {$ENDIF}
  {$ENDIF}
  ;

function GetOSLanguage: string;
function DarkTheme: boolean;

implementation

function GetOSLanguage: string;
  {platform-independent method to read the language of the user interface}
var
  l, fbl: string;
  {$IFDEF Darwin}
  theLocaleRef: CFLocaleRef;
  locale: CFStringRef;
  buffer: StringPtr;
  bufferSize: CFIndex;
  encoding: CFStringEncoding;
  success: boolean;
  {$ENDIF}
begin
  {$IFDEF Darwin}
  theLocaleRef := CFLocaleCopyCurrent;
  locale := CFLocaleGetIdentifier(theLocaleRef);
  encoding := 0;
  bufferSize := 256;
  buffer := new(StringPtr);
  success := CFStringGetPascalString(locale, buffer, bufferSize, encoding);
  if success then
    l := string(buffer^)
  else
    l := '';
  fbl := Copy(l, 1, 2);
  dispose(buffer);
  {$ELSE}
  {$IFDEF UNIX}
  fbl := Copy(GetEnvironmentVariable('LC_CTYPE'), 1, 2);
    {$ELSE}
  GetLanguageIDs(l, fbl);
    {$ENDIF}
  {$ENDIF}
  Result := fbl;
end;

{$IFDEF LCLCocoa}
{The following two functions were suggested by Hansaplast at https://forum.lazarus.freepascal.org/index.php/topic,43111.msg304366.html}

// Retrieve key's string value from user preferences. Result is encoded using NSStrToStr's default encoding.
function GetPrefString(const KeyName : string) : string;
begin
  Result := NSStringToString(NSUserDefaults.standardUserDefaults.stringForKey(NSStr(@KeyName[1])));
end;
{$ENDIF}

// DarkTheme: Detects if the Dark Theme (true) has been enabled or not (false)
function DarkTheme: boolean;
begin
  {$IFDEF LCLCocoa}
  Result := pos('DARK',UpperCase(GetPrefString('AppleInterfaceStyle'))) > 0;
  {$ELSE}
  Result := false;
  {$ENDIF}
end;

end.

