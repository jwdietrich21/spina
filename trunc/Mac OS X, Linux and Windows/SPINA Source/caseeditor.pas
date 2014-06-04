unit CaseEditor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, EditBtn;

type

  { TCaseEditorForm }

  TCaseEditorForm = class(TForm)
    CaseIDLabel: TLabel;
    DoBLabel: TLabel;
    DoBEdit: TDateEdit;
    GivenNameEdit: TEdit;
    GivenNameLabel: TLabel;
    OBDateLabel: TLabel;
    PlacerLabel: TLabel;
    NameLabel: TLabel;
    PIDLabel: TLabel;
    OKButton: TButton;
    CaseIDEdit: TEdit;
    OBDateEdit: TDateEdit;
    PlacerEdit: TEdit;
    PIDEdit: TEdit;
    NameEdit: TEdit;
    procedure OKButtonClick(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  CaseEditorForm: TCaseEditorForm;

implementation

{ TCaseEditorForm }

procedure TCaseEditorForm.OKButtonClick(Sender: TObject);
begin
  Close;
end;

initialization
  {$I caseeditor.lrs}

end.

