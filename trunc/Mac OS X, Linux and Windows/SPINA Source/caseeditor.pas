unit CaseEditor;

{ SPINA-Thyr }

{ Application for calculating structure parameters }
{ of thyrotropic feedback control }

{ Programm zur Berechnung von Strukturparametern }
{ des thyreotropen Regelkreises }

{ Version 3.5.0 }

{ (c) J. W. Dietrich, 1994 - 2014 }
{ (c) Ludwig Maximilian University of Munich 1995 - 2002 }
{ (c) University of Ulm Hospitals 2002-2004 }
{ (c) Ruhr University of Bochum 2005 - 2014 }

{ This unit implements an editor for case records }

{ Source code released under the BSD License }
{ See http://spina.medical-cybernetics.de for details }

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, EditBtn, Math, SPINA_Engine;

type

  { TCaseEditorForm }

  TCaseEditorForm = class(TForm)
    CaseIDLabel: TLabel;
    DoBLabel: TLabel;
    DoBEdit: TDateEdit;
    GivenNameEdit: TEdit;
    GivenNameLabel: TLabel;
    OBDateLabel: TLabel;
    CancelButton: TButton;
    PlacerLabel: TLabel;
    NameLabel: TLabel;
    PIDLabel: TLabel;
    OKButton: TButton;
    CaseIDEdit: TEdit;
    OBDateEdit: TDateEdit;
    PlacerEdit: TEdit;
    PIDEdit: TEdit;
    NameEdit: TEdit;
    procedure FillCaseRecord(var theCase: TCaseRecord);
    procedure CancelButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure OKButtonClick(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  CaseEditorForm: TCaseEditorForm;

implementation

uses
  SPINA_UserInterface;

{ TCaseEditorForm }

procedure TCaseEditorForm.FillCaseRecord(var theCase: TCaseRecord);
begin
  theCase.CaseID := CaseIDEdit.Text;
  theCase.PID := PIDEdit.Text;
  theCase.Name := NameEdit.Text;
  theCase.GivenNames := GivenNameEdit.Text;
  if DOBEdit.Text = '' then
    theCase.DoBDate := NaN
  else
    theCase.DoBDate := DoBEdit.Date;
  theCase.Placer := PlacerEdit.Text;
  if OBDateEdit.Text = '' then
    theCase.OBDate := NaN
  else
    theCase.OBDate := OBDateEdit.Date;
end;

procedure TCaseEditorForm.OKButtonClick(Sender: TObject);
begin
  FillCaseRecord(Hauptschirm.caseRecord);
  Close;
end;

procedure TCaseEditorForm.CancelButtonClick(Sender: TObject);
begin
  Close;
end;

procedure TCaseEditorForm.FormCreate(Sender: TObject);
begin

end;

initialization
  {$I caseeditor.lrs}

end.
