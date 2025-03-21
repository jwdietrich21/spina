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
  ExtCtrls, LCLIntf, EnvironmentInfo, SPINA_GUIServices;

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
    Logo: TImage;
    URL1: TLabel;
    URL2: TLabel;
    VersionLabel: TLabel;
    OKButton: TButton;
    PageControl1: TPageControl;
    SPINALabel: TImage;
    TabSheet1: TTabSheet;
    procedure CloseAboutBox(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure URL1Click(Sender: TObject);
    procedure URL2Click(Sender: TObject);
  private

  public

  end;

var
  AboutBox: TAboutBox;

implementation

{$R *.lfm}

{ TAboutBox }


procedure TAboutBox.CloseAboutBox(Sender: TObject);
begin
  close;
end;

procedure TAboutBox.FormCreate(Sender: TObject);
begin
  VersionLabel.Caption := 'Version ' + FileVersion;
end;

procedure TAboutBox.FormPaint(Sender: TObject);
begin
  if DarkTheme = true then
  begin
    URL1.Font.Color := clSkyBlue;
    URL2.Font.Color := clSkyBlue;
  end
  else
  begin
    URL1.Font.Color := clNavy;
    URL2.Font.Color := clNavy;
  end;
end;

procedure TAboutBox.URL1Click(Sender: TObject);
begin
  OpenURL('http://spina.medical-cybernetics.de');
end;

procedure TAboutBox.URL2Click(Sender: TObject);
begin
  OpenURL('http://spina.sourceforge.net');
end;

end.

