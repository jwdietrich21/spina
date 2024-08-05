unit SPINA_GUIServices;

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

{ This unit implements universal services for the GUI }

{ Source code released under the BSD License }
{ See http://spina.medical-cybernetics.de for details }

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils
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
  {$ENDIF};

function DarkTheme: boolean;

implementation

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

end.
