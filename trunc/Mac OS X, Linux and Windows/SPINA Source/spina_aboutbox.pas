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

{ This unit displays an information dialog }

{ Source code released under the BSD License }
{ See http://spina.medical-cybernetics.de for details }

unit SPINA_AboutBox;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, StdCtrls, LCLIntf, SPINA_Types;

type

  { TAboutBox }

  TAboutBox = class(TForm)
    Button1: TButton;
    Image1: TImage;
    procedure ClickURL(Sender: TObject);
    procedure CloseAboutBox(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  AboutBox: TAboutBox;

procedure ShowAboutBox;

implementation

procedure ShowAboutBox;
begin
  if gStartup then
    AboutBox.Hide
  else
    AboutBox.ShowModal;
end;

{ TAboutBox }

procedure TAboutBox.CloseAboutBox(Sender: TObject);
begin
  Close;
end;

procedure TAboutBox.ClickURL(Sender: TObject);
begin
  OpenURL('http://spina.medical-cybernetics.de');
end;

initialization
  {$I spina_aboutbox.lrs}


end.

