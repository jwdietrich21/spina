unit SPINA_AboutBox;

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

{ This unit displays an information dialog }

{ Source code released under the BSD License }
{ See http://spina.medical-cybernetics.de for details }

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, StdCtrls, LCLIntf, ComCtrls, DOS, SPINA_Types, VersionSupport,
  HandlePreferences
  {$IFDEF win32}
  , Windows, Win32Proc
  {$ELSE}
    {$IFDEF LCLCarbon}
  , MacOSAll
    {$ENDIF}
  , Unix
  {$ENDIF}  ;

type

  { TAboutBox }

  TAboutBox = class(TForm)
    Button1: TButton;
    Image1: TImage;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    Memo1: TMemo;
    Tabs: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    TabSheet3: TTabSheet;
    procedure ClickURL(Sender: TObject);
    procedure CloseAboutBox(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure Label5Click(Sender: TObject);
    procedure Label7Click(Sender: TObject);
    procedure Label9Click(Sender: TObject);
    procedure TabsChange(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  AboutBox: TAboutBox;
  gExtendedInfo: boolean;

procedure ShowAboutBox;

implementation

function OSVersion: Str255; {returns the major version of the operating system}
begin
  {$IFDEF LCLcarbon}
  OSVersion := 'Mac OS X 10.';
  {$ELSE}
  {$IFDEF Linux}
  OSVersion := 'Linux Kernel ';
  {$ELSE}
  {$IFDEF UNIX}
  OSVersion := 'Unix ';
  {$ELSE}
  {$IFDEF WINDOWS}
  if WindowsVersion = wv95 then
    OSVersion := 'Windows 95 '
  else if WindowsVersion = wvNT4 then
    OSVersion := 'Windows NT v.4 '
  else if WindowsVersion = wv98 then
    OSVersion := 'Windows 98 '
  else if WindowsVersion = wvMe then
    OSVersion := 'Windows ME '
  else if WindowsVersion = wv2000 then
    OSVersion := 'Windows 2000 '
  else if WindowsVersion = wvXP then
    OSVersion := 'Windows XP '
  else if WindowsVersion = wvServer2003 then
    OSVersion := 'Windows Server 2003 '
  else if WindowsVersion = wvVista then
    OSVersion := 'Windows Vista '
  else if WindowsVersion = wv7 then
    OSVersion := 'Windows 7 '
  else
    OSVersion := 'Windows ';
  {$ENDIF}
  {$ENDIF}
  {$ENDIF}
  {$ENDIF}
end;

procedure ShowAboutBox;
var
  SystemStem, MajVer, MinVer: Str255;
begin
  if gStartup then
    AboutBox.Hide
  else
    begin
      gExtendedInfo := false;
      SystemStem := OSVersion;
      AboutBox.FormStyle := fsStayOnTop;
      AboutBox.AlphaBlend := false;
      {The following lines provide additional information}
      {on the software installation}
      AboutBox.Memo1.Lines.Clear;
      AboutBox.Memo1.Lines.Add('SPINA Thyr ' + GetFileVersion);
      AboutBox.Memo1.Lines.Add('');
      AboutBox.Memo1.Lines.Add('Licence: BSD');
      AboutBox.Memo1.Lines.Add('');
      AboutBox.Memo1.Lines.Add('File version: ' + GetFileVersion);
      AboutBox.Memo1.Lines.Add('');
      AboutBox.Memo1.Lines.Add('Build Date: ' + {$I %DATE%} + ', ' + {$I %TIME%});
      AboutBox.Memo1.Lines.Add('');
      AboutBox.Memo1.Lines.Add('Developed with Lazarus / Free Pascal');
      AboutBox.Memo1.Lines.Add('Built for '+ GetTargetInfo);
      AboutBox.Memo1.Lines.Add('with '+ GetCompilerInfo + ' on '+ GetCompiledDate);
      AboutBox.Memo1.Lines.Add('and using '+ GetLCLVersion + ' and ' + GetWidgetset);
      AboutBox.Memo1.Lines.Add('');
      {$IFDEF WINDOWS}
      MajVer := IntToStr(Win32MajorVersion);
      MinVer := IntToStr(Win32MinorVersion);
      {$ELSE}
      MajVer := IntToStr(Lo(DosVersion) - 4);
      MinVer := IntToStr(Hi(DosVersion));
      {$ENDIF}
      AboutBox.Memo1.Lines.Add('Operating system: ' + GetOS + ' (' + SystemStem + MajVer + '.' + MinVer + ')');
      AboutBox.AlphaBlendValue := 255;
      AboutBox.ShowModal;
    end;
end;

{ TAboutBox }

procedure TAboutBox.CloseAboutBox(Sender: TObject);
begin
  Close;
end;

procedure TAboutBox.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
{provide additional information, if option or alt key is pressed}
begin
  if (key = 18) and not gExtendedInfo then
  begin
    AboutBox.Memo1.Lines.Add('');
    AboutBox.Memo1.Lines.Add('Preferences file: ' + GetPreferencesFile);
    gExtendedInfo := true;
  end;
end;

procedure TAboutBox.Label5Click(Sender: TObject);
begin
  OpenURL('http://www.famfamfam.com/lab/icons/silk/');
end;

procedure TAboutBox.Label7Click(Sender: TObject);
begin
  OpenURL('http://tango.freedesktop.org/Tango_Icon_Library');
end;

procedure TAboutBox.Label9Click(Sender: TObject);
begin
  OpenURL('http://www.lazarus.freepascal.org/index.php/topic,13957');
end;

procedure TAboutBox.TabsChange(Sender: TObject);
begin

end;

procedure TAboutBox.ClickURL(Sender: TObject);
begin
  OpenURL('http://spina.medical-cybernetics.de');
end;

initialization
  {$I spina_aboutbox.lrs}


end.
