{ SPINA-Thyr }
{ Programm zur Berechnung von Strukturparametern }
{ des thyreotropen Regelkreises }
{ Version 3.2 }

{ J. W. Dietrich, Klinikum der LMU München 1997-2001 }
{ J. W. Dietrich, Universitätsklinikum Ulm 2002-2004 }
{ J. W. Dietrich, Universitätsklinikum Bergmannsheil 2005-2010 }

{ This software is provided via a BSD licence }
{ See http://spina.medical-cybernetics.de for details }

unit SPINA_ResultDialog;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, StdCtrls;

type

  { TResultForm }

  TResultForm = class(TForm)
    Button1: TButton;
    Image1: TImage;
    Label1: TLabel;
    MessageField2: TMemo;
    MessageField1: TMemo;
    procedure Button1Click(Sender: TObject);
    procedure UpdateResultDisplay(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  ResultForm: TResultForm;

implementation

uses SPINA_UserInterface;

{ TResultForm }

procedure TResultForm.UpdateResultDisplay(Sender: TObject);
begin
  ResultForm.MessageField1.text := gResultDialogString1;
  ResultForm.messageField2.text := gResultDialogString2;
end;

procedure TResultForm.Button1Click(Sender: TObject);
begin
  Close;
end;

initialization
  {$I spina_resultdialog.lrs}

end.

