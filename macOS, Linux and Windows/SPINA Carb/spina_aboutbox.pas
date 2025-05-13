unit SPINA_Aboutbox;

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

{ About box }

{ Source code released under the BSD License }
{ See http://spina.medical-cybernetics.de for details }

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ComCtrls,
  ExtCtrls, LCLIntf, EnvironmentInfo, SPINA_GUIServices, HandlePreferences,
  LocaleServices;

type

  { TAboutBox }

  TAboutBox = class(TForm)
    CopyrightLabel1: TLabel;
    CopyrightLabel2: TLabel;
    CopyrightLabel3: TLabel;
    CopyrightLabel4: TLabel;
    CopyrightLabel5: TLabel;
    CopyrightLabel6: TLabel;
    CopyrightLabel7: TLabel;
    CopyrightLabel8: TLabel;
    Divider: TLabel;
    ImageContainer1: TImageList;
    ImageContainer2: TImageList;
    AdviseLabel: TLabel;
    AdviseContent: TLabel;
    PUMALabel: TLabel;
    PUMAURL: TLabel;
    ResLabel: TLabel;
    VersionSuppLabel: TLabel;
    VersionSuppURL: TLabel;
    Logo: TImage;
    Memo1: TMemo;
    Memo2: TMemo;
    MoreInfoTabSheet: TTabSheet;
    AcknowledgementTabSheet: TTabSheet;
    ImprintTabSheet: TTabSheet;
    URL1: TLabel;
    URL2: TLabel;
    VersionLabel: TLabel;
    OKButton: TButton;
    PageControl1: TPageControl;
    SPINALabel: TImage;
    VersionTabSheet: TTabSheet;
    procedure CloseAboutBox(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
    procedure FormPaint(Sender: TObject);
    procedure PUMAURLClick(Sender: TObject);
    procedure URL1Click(Sender: TObject);
    procedure URL2Click(Sender: TObject);
    procedure VersionSuppURLClick(Sender: TObject);
  private

  public
    ExtendedInfo: boolean;
    finished: boolean;
    procedure ShowAbout;
  end;

var
  AboutBox: TAboutBox;

implementation

{$R *.lfm}

{ TAboutBox }


procedure TAboutBox.CloseAboutBox(Sender: TObject);
begin
  Close;
end;

procedure TAboutBox.FormCreate(Sender: TObject);
begin
  VersionLabel.Caption := 'Version ' + FileVersion;
  finished := true;
end;

procedure TAboutBox.FormKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
{provide additional information, if option or alt key is pressed}
begin
  if (key = 18) and not ExtendedInfo then
  begin
    AboutBox.Memo1.Lines.Add('');
    AboutBox.Memo1.Lines.Add('Preferences file: ' + PreferencesFile);
    AboutBox.Memo1.Lines.Add('Definition file for reference values: ' + RefRangeFile);
    ExtendedInfo := true;
  end
  else if (key = 87) and ((ssMeta in Shift) or (ssCtrl in Shift)) then
    self.Close;
end;

procedure TAboutBox.FormPaint(Sender: TObject);
begin
  if DarkTheme then
  begin
    URL1.Font.Color := clSkyBlue;
    URL2.Font.Color := clSkyBlue;
    VersionSuppURL.Font.Color := clSkyBlue;
    PUMAURL.Font.Color := clSkyBlue;
    ImageContainer1.GetBitmap(1, SPINALabel.Picture.Bitmap);
    ImageContainer2.GetBitmap(1, Logo.Picture.Bitmap);
  end
  else
  begin
    URL1.Font.Color := clNavy;
    URL2.Font.Color := clNavy;
    VersionSuppURL.Font.Color := clNavy;
    PUMAURL.Font.Color := clNavy;
    ImageContainer1.GetBitmap(0, SPINALabel.Picture.Bitmap);
    ImageContainer2.GetBitmap(0, Logo.Picture.Bitmap);
  end;
  application.ProcessMessages;
end;

procedure TAboutBox.PUMAURLClick(Sender: TObject);
begin
  OpenURL(PUMAURL.Caption);
end;

procedure TAboutBox.URL1Click(Sender: TObject);
begin
  OpenURL('http://' + URL1.Caption);
end;

procedure TAboutBox.URL2Click(Sender: TObject);
begin
  OpenURL('http://' + URL2.Caption);
end;

procedure TAboutBox.VersionSuppURLClick(Sender: TObject);
begin
  OpenURL(VersionSuppURL.Caption);
end;

procedure TAboutBox.ShowAbout;
begin
  ExtendedInfo := False;
  AboutBox.FormStyle := fsStayOnTop;
  AboutBox.AlphaBlend := False;
  AboutBox.Memo1.Lines.Clear;
  AboutBox.Memo1.Lines.Add('SPINA Carb ' + FileVersion);
  AboutBox.Memo1.Lines.Add('');
  AboutBox.Memo1.Lines.Add('Licence: BSD');
  AboutBox.Memo1.Lines.Add('');
  AboutBox.Memo1.Lines.Add('File version: ' + FileVersion);
  AboutBox.Memo1.Lines.Add('');
  AboutBox.Memo1.Lines.Add('Build Date: ' + {$I %DATE%} + ', ' + {$I %TIME%});
  AboutBox.Memo1.Lines.Add('');
  AboutBox.Memo1.Lines.Add('Developed with Lazarus / Free Pascal');
  AboutBox.Memo1.Lines.Add('Built for ' + PlatformInfo.OS + ' (' +
    PlatformInfo.CPU + ')');
  AboutBox.Memo1.Lines.Add('with ' + CompilerVersion + ' on ' +
    DateOfCompilingAsString);
  AboutBox.Memo1.Lines.Add('and using ' + EnvironmentInfo.LCLVersion +
    ' with ' + CurrentWidgetSet);
  AboutBox.Memo1.Lines.Add('');
  AboutBox.Memo1.Lines.Add('Operating system: ' + PlatformInfo.OS +
    ' (' + SystemVersion + ')');
  AboutBox.Memo1.Lines.Add('');
  AboutBox.Memo1.Lines.Add('User language: ' + gPreferredLanguage);
  AboutBox.AlphaBlendValue := 255;
  PageControl1.ActivePage := VersionTabSheet;
  AboutBox.ShowModal;
end;

end.
