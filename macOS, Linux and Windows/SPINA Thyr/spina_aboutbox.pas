unit SPINA_AboutBox;

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

{ This unit displays an information dialog }

{ Source code released under the BSD License }
{ See http://spina.medical-cybernetics.de for details }

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, StdCtrls, LCLIntf, ComCtrls, DOS, SPINA_Types, EnvironmentInfo,
  HandlePreferences, SPINA_GUIServices, LocaleServices
  {$IFDEF Windows}
  , Windows, Win32Proc
  {$ELSE}
    {$IFDEF Darwin}
  , MacOSAll
    {$ENDIF}
  , Unix
  {$ENDIF}  ;

type

  { TAboutBox }

  TAboutBox = class(TForm)
    CopyrightLabel8: TLabel;
    Logo: TImage;
    Memo2: TMemo;
    OKButton: TButton;
    CopyrightLabel1: TLabel;
    CopyrightLabel2: TLabel;
    CopyrightLabel3: TLabel;
    CopyrightLabel4: TLabel;
    CopyrightLabel5: TLabel;
    CopyrightLabel6: TLabel;
    CopyrightLabel7: TLabel;
    Divider: TLabel;
    SPINALabel: TImage;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    Label15: TLabel;
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
    TabSheet4: TTabSheet;
    URL1: TLabel;
    URL2: TLabel;
    VersionLabel: TLabel;
    procedure ClickURL(Sender: TObject);
    procedure CloseAboutBox(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormPaint(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure Label15Click(Sender: TObject);
    procedure Label5Click(Sender: TObject);
    procedure Label7Click(Sender: TObject);
    procedure Label9Click(Sender: TObject);
    procedure Memo2Change(Sender: TObject);
    procedure TabsChange(Sender: TObject);
    procedure URL1Click(Sender: TObject);
    procedure URL2Click(Sender: TObject);
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

uses
  SPINA_Userinterface, SPINA_SplashScreen;

procedure ShowAboutBox;
begin
  if gStartup then
    AboutBox.Hide
  else
    begin
      gExtendedInfo := false;
      AboutBox.FormStyle := fsStayOnTop;
      AboutBox.AlphaBlend := false;
      {The following lines provide additional information}
      {on the software installation}
      AboutBox.Memo1.Lines.Clear;
      AboutBox.Memo1.Lines.Add('SPINA Thyr ' + FileVersion);
      AboutBox.Memo1.Lines.Add('');
      AboutBox.Memo1.Lines.Add('Licence: BSD');
      AboutBox.Memo1.Lines.Add('');
      AboutBox.Memo1.Lines.Add('File version: ' + FileVersion);
      AboutBox.Memo1.Lines.Add('');
      AboutBox.Memo1.Lines.Add('Build Date: ' + {$I %DATE%} + ', ' + {$I %TIME%});
      AboutBox.Memo1.Lines.Add('');
      AboutBox.Memo1.Lines.Add('Developed with Lazarus / Free Pascal');
      AboutBox.Memo1.Lines.Add('Built for ' + PlatformInfo.OS + ' (' + PlatformInfo.CPU + ')');
      AboutBox.Memo1.Lines.Add('with ' + CompilerVersion + ' on '+ DateOfCompilingAsString);
      AboutBox.Memo1.Lines.Add('and using ' + EnvironmentInfo.LCLVersion + ' with ' + CurrentWidgetSet);
      AboutBox.Memo1.Lines.Add('');
      AboutBox.Memo1.Lines.Add('Operating system: ' + PlatformInfo.OS + ' (' + SystemVersion + ')');
      AboutBox.Memo1.Lines.Add('');
      AboutBox.Memo1.Lines.Add('User language: ' + gSysLanguage);
      AboutBox.AlphaBlendValue := 255;
      AboutBox.ShowModal;
    end;
end;

{ TAboutBox }

procedure TAboutBox.CloseAboutBox(Sender: TObject);
begin
  Close;
end;

procedure TAboutBox.FormCreate(Sender: TObject);
begin
  if YosemiteOrNewer then
    OKButton.Height := 22;
  VersionLabel.Caption := 'Version ' + FileVersion;
end;

procedure TAboutBox.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
{provide additional information, if option or alt key is pressed}
begin
  if (key = 18) and not gExtendedInfo then
  begin
    AboutBox.Memo1.Lines.Add('');
    AboutBox.Memo1.Lines.Add('Preferences file: ' + GetPreferencesFile);
    AboutBox.Memo1.Lines.Add('Definition file for reference values: ' + RRFile);
    gExtendedInfo := true;
  end
  else if (key = 87) and ((ssMeta in Shift) or (ssCtrl in Shift)) then
    self.Close;
end;

procedure TAboutBox.FormPaint(Sender: TObject);
begin
  if DarkTheme = true then
  begin
    Color := clDefault;
    Memo1.Font.Color := clWhite;
    Memo2.Font.Color := clWhite;
    SPINALabel.Picture := Hauptschirm.ImageContainerDark.Picture;
    Logo.Picture := SplashScreen.LogoDark.Picture;
    URL1.Font.Color := clSkyBlue;
    URL2.Font.Color := clSkyBlue;
    Label5.Font.Color := clSkyBlue;
    Label7.Font.Color := clSkyBlue;
    Label9.Font.Color := clSkyBlue;
    Label15.Font.Color := clSkyBlue;
  end
  else
  begin
    Color := clWhite;
    Memo1.Font.Color := clBlack;
    Memo2.Font.Color := clBlack;
    SPINALabel.Picture := Hauptschirm.ImageContainerDark.Picture;
    Logo.Picture := SplashScreen.LogoLight.Picture;
    URL1.Font.Color := clNavy;
    URL2.Font.Color := clNavy;
     Label5.Font.Color := clNavy;
    Label7.Font.Color := clNavy;
    Label9.Font.Color := clNavy;
    Label15.Font.Color := clNavy;
 end;
end;

procedure TAboutBox.FormShow(Sender: TObject);
begin
  Memo2.SelStart := 0;
  Memo2.SelLength := 0;
  Memo2.VertScrollBar.Position := 0;
end;

procedure TAboutBox.Label15Click(Sender: TObject);
begin
  OpenURL('http://puma-repository.sf.net');
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

procedure TAboutBox.Memo2Change(Sender: TObject);
begin

end;

procedure TAboutBox.TabsChange(Sender: TObject);
begin

end;

procedure TAboutBox.URL1Click(Sender: TObject);
begin
  OpenURL('http://spina.medical-cybernetics.de')
end;

procedure TAboutBox.URL2Click(Sender: TObject);
begin
  OpenURL('http://spina.sourceforge.net')
end;

procedure TAboutBox.ClickURL(Sender: TObject);
begin

end;

initialization
  {$I spina_aboutbox.lrs}


end.
