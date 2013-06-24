unit SPINA_ResultDialog;

{ SPINA-Thyr }

{ Application for calculating structure parameters }
{ of thyrotropic feedback control }

{ Programm zur Berechnung von Strukturparametern }
{ des thyreotropen Regelkreises }

{ Version 3.3.1 }

{ (c) J. W. Dietrich, 1994 - 2013 }
{ (c) Ludwig Maximilian University of Munich 1995 - 2002 }
{ (c) University of Ulm Hospitals 2002-2004 }
{ (c) Ruhr University of Bochum 2005 - 2013 }

{ This unit delivers a dialog box with calculation results }

{ Source code released under the BSD License }
{ See http://spina.medical-cybernetics.de for details }

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, StdCtrls, SPINA_Types;

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
  {Adjustments for small windows:}
    if Screen.Width < ResultForm.Width then
    begin
      ResultForm.Width := Screen.Width - 13;
      ResultForm.Left := (Screen.Width - ResultForm.Width) div 2;
    end;
  if gInterfaceLanguage = German then
    Label1.Caption := kResultHint1
  else
    Label1.Caption := kResultHint2;
  ResultForm.MessageField1.text := gResultDialogString1;
  ResultForm.MessageField2.text := gResultDialogString2;
  ResultForm.messageField1.Hint := gReferenceValueString2;
  ResultForm.messageField2.Hint := gReferenceValueString2;
end;

procedure TResultForm.Button1Click(Sender: TObject);
begin
  Close;
end;

initialization
  {$I spina_resultdialog.lrs}

end.
