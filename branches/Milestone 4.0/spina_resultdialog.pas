unit SPINA_ResultDialog;

{ SPINA-Thyr }

{ Application for calculating structure parameters }
{ of thyrotropic feedback control }

{ Programm zur Berechnung von Strukturparametern }
{ des thyreotropen Regelkreises }

{ Version 4.0.0 (Mercator) }

{ (c) J. W. Dietrich, 1994 - 2015 }
{ (c) Ludwig Maximilian University of Munich 1995 - 2002 }
{ (c) University of Ulm Hospitals 2002-2004 }
{ (c) Ruhr University of Bochum 2005 - 2015 }

{ This unit delivers a dialog box with calculation results }

{ Source code released under the BSD License }
{ See http://spina.medical-cybernetics.de for details }

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, StdCtrls, SPINA_Types, SPINA_Resources, SPINA_AboutBox;

type

  { TResultForm }

  TResultForm = class(TForm)
    OKButton: TButton;
    Image1: TImage;
    Label1: TLabel;
    MessageField2: TMemo;
    MessageField1: TMemo;
    procedure FormCreate(Sender: TObject);
    procedure OKButtonClick(Sender: TObject);
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
const
  LINE_NUMBER = 7;
  BOTTOM_MARGIN = 51;
begin
  {Adjustments for small windows:}
  if Screen.Width < Width then
  begin
    Width := Screen.Width - 13;
    Left := (Screen.Width - Width) div 2;
  end;
  {Adjustments for large fonts:}
    Height := trunc(MessageField2.Top + LINE_NUMBER * 1.6 * abs(MessageField2.Font.Height) + BOTTOM_MARGIN);
  if gInterfaceLanguage = German then
    Label1.Caption := kResultHint1
  else
    Label1.Caption := kResultHint2;
  MessageField1.text := gResultDialogString1;
  MessageField2.text := gResultDialogString2;
  messageField1.Hint := gReferenceValueString2;
  messageField2.Hint := gReferenceValueString2;
end;

procedure TResultForm.OKButtonClick(Sender: TObject);
begin
  Close;
end;

procedure TResultForm.FormCreate(Sender: TObject);
begin
  { adapt button height on newer versions of Mac OS X: }
  if YosemiteORNewer then
    OKButton.Height := 22;
  if gInterfaceLanguage = German then
    Caption := kResult1
  else
    Caption := kResult2;
end;

initialization
  {$I spina_resultdialog.lrs}

end.
