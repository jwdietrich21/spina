unit CaseEditor;

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

{ This unit implements an editor for case records }

{ Source code released under the BSD License }
{ See http://spina.medical-cybernetics.de for details }

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, EditBtn, ExtCtrls, Math, SPINA_Types, SPINA_Resources,
  SPINA_Engine, HandleImpEx;

type

  { TCaseEditorForm }

  TCaseEditorForm = class(TForm)
    Bevel1: TBevel;
    Bevel2: TBevel;
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
    procedure FillFromCaseRecord(var theCase: TCaseRecord);
    procedure SetCaseRecord(var theCase: TCaseRecord);
    procedure CancelButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDropFiles(Sender: TObject; const FileNames: array of String);
    procedure FormShow(Sender: TObject);
    procedure OKButtonClick(Sender: TObject);
    procedure PIDLabelClick(Sender: TObject);
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

procedure TCaseEditorForm.FillFromCaseRecord(var theCase: TCaseRecord);
{ fills edit fields of dialog according with the contents of the case record }
begin
  CaseIDEdit.Text := theCase.CaseID;
  PIDEdit.Text := theCase.PID;
  NameEdit.Text := theCase.Name;
  GivenNameEdit.Text := theCase.GivenNames;
  DoBEdit.Date := theCase.DoBDate;
  PlacerEdit.Text := theCase.Placer;
  OBDateEdit.Date := theCase.OBDate;
end;

procedure TCaseEditorForm.SetCaseRecord(var theCase: TCaseRecord);
{ reads contents of dialog box and sets the case record appropriately }
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
  SetCaseRecord(Hauptschirm.caseRecord);
  Close;
end;

procedure TCaseEditorForm.PIDLabelClick(Sender: TObject);
begin

end;

procedure TCaseEditorForm.CancelButtonClick(Sender: TObject);
begin
  Close;
end;

procedure TCaseEditorForm.FormCreate(Sender: TObject);
begin
  if gInterfaceLanguage = German then
  begin
    Caption := kCaseEditor1;
    CancelButton.Caption := kCancel1;
    CaseIDLabel.Caption := kCaseID1;
    NameLabel.Caption := kName1;
    GivenNameLabel.Caption := kGivenName1;
    DoBLabel.Caption := kDOB1;
    PlacerLabel.Caption := kEinsender1;
    OBDateLabel.Caption := kOB1;
  end
  else
  begin
    Caption := kCaseEditor2;
    CancelButton.Caption := kCancel2;
    CaseIDLabel.Caption := kCaseID2;
    NameLabel.Caption := kName2;
    GivenNameLabel.Caption := kGivenName2;
    DoBLabel.Caption := kDOB2;
    PlacerLabel.Caption := kEinsender2;
    OBDateLabel.Caption := kOB2;
  end;
end;

procedure TCaseEditorForm.FormDropFiles(Sender: TObject;
  const FileNames: array of String);
begin
  ReadHL7Message(FileNames[0], Hauptschirm.caseRecord);
  FormShow(Sender);
end;

procedure TCaseEditorForm.FormShow(Sender: TObject);
begin
  ActiveControl := CaseIDEdit;
  DoBEdit.DateFormatChanged; // adapts display style to system settings
  OBDateEdit.DateFormatChanged;
  CaseIDEdit.Text := Hauptschirm.caseRecord.CaseID;
  PIDEdit.Text := Hauptschirm.caseRecord.PID;
  NameEdit.Text := Hauptschirm.caseRecord.Name;
  GivenNameEdit.Text := Hauptschirm.caseRecord.GivenNames;
  if not isNaN(Hauptschirm.caseRecord.DoBDate) then
    DoBEdit.Date := Hauptschirm.caseRecord.DoBDate
  else
    DOBEdit.Text := '';
  PlacerEdit.Text := Hauptschirm.caseRecord.Placer;
  if not isNaN(Hauptschirm.caseRecord.OBDate) then
    OBDateEdit.Date := Hauptschirm.caseRecord.OBDate
  else
    OBDateEdit.Text := '';
end;

initialization
  {$I caseeditor.lrs}

end.

