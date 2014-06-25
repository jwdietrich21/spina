unit Help;

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

{ This unit draws aad handles a global help window }

{ Source code released under the BSD License }
{ See http://spina.medical-cybernetics.de for details }

{$mode objfpc}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ExtCtrls, LCLIntf, SPINA_Types, SPINA_AboutBox;

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

procedure THelpWindow.Label4Click(Sender: TObject);
begin
  OpenURL(HELP_URL);
end;

procedure THelpWindow.Label6Click(Sender: TObject);
begin
  HelpWindow.Close;
  ShowAboutBox;
end;

procedure THelpWindow.Label3Click(Sender: TObject);
begin
  OpenURL(METHOD_URL);
end;

initialization
  {$I help.lrs}

end.

