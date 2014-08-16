unit spina_help;

{ SPINA-Thyr }

{ Application for calculating structure parameters }
{ of thyrotropic feedback control }

{ Programm zur Berechnung von Strukturparametern }
{ des thyreotropen Regelkreises }

{ Version 4.0.0 }

{ (c) J. W. Dietrich, 1994 - 2014 }
{ (c) Ludwig Maximilian University of Munich 1995 - 2002 }
{ (c) University of Ulm Hospitals 2002-2004 }
{ (c) Ruhr University of Bochum 2005 - 2014 }

{ This unit implements a basic help system }

{ Source code released under the BSD License }
{ See http://spina.medical-cybernetics.de for details }

{$mode objfpc}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ExtCtrls, LCLIntf, ComCtrls, SPINA_Types, SPINA_AboutBox;

const
  HELP_TITLE1 = 'SPINA-Hilfe';
  HELP_TITLE2 = 'SPINA Help';
  WINDOW_TITLE1 = HELP_TITLE1;
  WINDOW_TITLE2 = HELP_TITLE2;
  HELP_ITEM1_TITLE1 = 'Online-Hilfe';
  HELP_ITEM1_TITLE2 = 'Online Help';
  HELP_ITEM2_TITLE1 = 'Über die Methodik';
  HELP_ITEM2_TITLE2 = 'Information about the method';
  HELP_ITEM3_TITLE1 = 'Versionsinformation';
  HELP_ITEM3_TITLE2 = 'Version info';
  HELP_ITEM1_ENTRY1 = 'Beginnen Sie hier für eine kurze Einführung, ein Handbuch und Online-Tutorials.';
  HELP_ITEM1_ENTRY2 = 'Get a short introduction, a full handbook and tutorials online.';
  HELP_ITEM2_ENTRY1 = 'Informieren Sie sich über SPINA und die Schilddrüsen-Homöostase.';
  HELP_ITEM2_ENTRY2 = 'References and background information about SPINA.';
  HELP_ITEM3_ENTRY1 = 'Lassen Sie sich Informationen über die Programmversion und die Laufzeitumgebung anzeigen.';
  HELP_ITEM3_ENTRY2 = 'Get information about the version of this application and information on the runtime environment.';

type

  { THelpWindow }

  THelpWindow = class(TForm)
    Image1: TImage;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure Image1Click(Sender: TObject);
    procedure Label3Click(Sender: TObject);
    procedure Label4Click(Sender: TObject);
    procedure Label6Click(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  HelpWindow: THelpWindow;

implementation

{ THelpWindow }

procedure THelpWindow.FormCreate(Sender: TObject);
{i18n}
begin
  {$IFDEF LCLCarbon}
  Label3.Font.Height := 13;
  Label4.Font.Height := 13;
  Label6.Font.Height := 13;
  Label2.Font.Height := 10;
  Label5.Font.Height := 10;
  Label7.Font.Height := 10;
  {$ELSE}
  Label3.Font.Height := 15;
  Label4.Font.Height := 15;
  Label6.Font.Height := 15;
  Label2.Font.Height := 13;
  Label5.Font.Height := 13;
  Label7.Font.Height := 13;
  {$ENDIF}
  if gInterfaceLanguage <> German then
  begin
    HelpWindow.Caption := WINDOW_TITLE2;
    Label1.Caption := HELP_TITLE2;
    Label4.Caption := HELP_ITEM1_TITLE2;
    Label3.Caption := HELP_ITEM2_TITLE2;
    Label6.Caption := HELP_ITEM3_TITLE2;
    Label2.Caption := HELP_ITEM1_ENTRY2;
    Label5.Caption := HELP_ITEM2_ENTRY2;
    Label7.Caption := HELP_ITEM3_ENTRY2;
  end
  else
  begin
    HelpWindow.Caption := WINDOW_TITLE1;
    Label1.Caption := HELP_TITLE1;
    Label4.Caption := HELP_ITEM1_TITLE1;
    Label3.Caption := HELP_ITEM2_TITLE1;
    Label6.Caption := HELP_ITEM3_TITLE1;
    Label2.Caption := HELP_ITEM1_ENTRY1;
    Label5.Caption := HELP_ITEM2_ENTRY1;
    Label7.Caption := HELP_ITEM3_ENTRY1;
  end;
end;

procedure THelpWindow.Image1Click(Sender: TObject);
begin

end;

procedure THelpWindow.Label3Click(Sender: TObject);
begin
  OpenURL(PORTAL_URL);
end;

procedure THelpWindow.Label4Click(Sender: TObject);
begin
  OpenURL(HELP_URL);
end;

procedure THelpWindow.Label6Click(Sender: TObject);
begin
  HelpWindow.Close;
  ShowAboutBox;
end;

initialization
  {$I spina_help.lrs}

end.

