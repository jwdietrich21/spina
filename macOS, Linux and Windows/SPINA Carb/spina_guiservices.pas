unit SPINA_GUIServices;

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

{ This unit implements universal services for the GUI }

{ Source code released under the BSD License }
{ See http://spina.medical-cybernetics.de for details }

{$mode ObjFPC}{$H+}
{$IFDEF LCLCocoa}
  {$modeswitch objectivec1}
{$ENDIF}

interface

uses
  Classes, SysUtils, Types, StrUtils
  {$IFDEF WINDOWS}
  , Windows, Win32Proc, registry
  {$ENDIF}
  {$IFDEF DARWIN}
  , MacOSAll
  {$ENDIF}
  {$IFDEF LCLCocoa}
  , CocoaAll, CocoaUtils
  {$ENDIF}
  {$IFDEF UNIX}
  , Unix, clocale
  {$ENDIF}
  , SPINATypes, EnvironmentInfo;

function DarkTheme: boolean;
function CombString(theArray: TStringDynArray; theDelim: String): String;
function WithReferenceRanges(RefMessage: String): String;

implementation

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
  {$IFDEF Windows}
  const
    KEYPATH = '\Software\Microsoft\Windows\CurrentVersion\Themes\Personalize';
    KEYNAME = 'AppsUseLightTheme';
    WindowsDarkModeSupported: boolean = false; // may be set to true in future versions
  var
    LightKey: boolean;
    Registry: TRegistry;
  {$ENDIF}
begin
  Result := False;
  {$IFDEF Windows}
    if WindowsDarkModeSupported then
    begin
      Registry := TRegistry.Create;
      try
        Registry.RootKey := HKEY_CURRENT_USER;
        if Registry.OpenKeyReadOnly(KEYPATH) then
          begin
            if Registry.ValueExists(KEYNAME) then
              LightKey := Registry.ReadBool(KEYNAME)
            else
              LightKey := true;
          end
        else
          LightKey := true;
          Result := not LightKey
      finally
        Registry.Free;
      end;
    end
    else
    Result := false;
  {$ELSE}
  {$IFDEF LCLCocoa}
    if MojaveOrNewer then
      Result := pos('DARK',UpperCase(GetPrefString('AppleInterfaceStyle'))) > 0
    else
      Result := false;
  {$ELSE}
  Result := False;
  {$ENDIF}
  {$ENDIF}
end;

function CombString(theArray: TStringDynArray; theDelim: String): String;
var
  i, l: integer;
begin
  l := length(theArray);
  result := '';
  for i := 0 to l - 1 do
  begin
    result := result + theArray[i] + theDelim;
  end;

end;

function WithReferenceRanges(RefMessage: String): String;
var
  BParArray, SParArray, RefArray, ResArray: TStringDynArray;
  i, j, k, l: integer;
begin
  BParArray := SplitString(BParLabels, LineEnding);
  SParArray := SplitString(SParLabels, LineEnding);
  RefArray := SplitString(RefMessage, LineEnding);
  result := '';
  l := length(RefArray);
  j := length(BParArray);
  k := 0;
  SetLength(ResArray, l);
  ResArray[0] := RefArray[0];
  for i := 1 to l - 1 do
  begin
    if RefArray[i] <> ' ' then
    begin
      if i <= j then
        ResArray[i] := BParArray[i - 1] + RefArray[i]
      else
        begin
        ResArray[i] := SParArray[k] + RefArray[i];
        inc(k);
        end;
    end
    else
      ResArray[i] := RefArray[i];
  end;
  result := CombString(ResArray, LineEnding);
end;

end.
