unit Setpreferences;

{ SPINA Carb }

{ Application for calculating structure parameters }
{ of insulin-glucose feedback control }

{ Programm zur Berechnung von Strukturparametern }
{ des Insulin-Glukose-Regelkreises }

{ Version 5.1.0 (Cyclone) }

{ (c) J. W. Dietrich, 1994 - 2024 }
{ (c) Ludwig Maximilian University of Munich 1995 - 2002 }
{ (c) University of Ulm Hospitals 2002 - 2004 }
{ (c) Ruhr University of Bochum 2005 - 2024 }

{ This unit draws a dialog box for setting global application preferences }

{ Source code released under the BSD License }
{ See http://spina.medical-cybernetics.de for details }

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ComCtrls,
  SPINATypes, HandlePreferences, SPINA_GUIServices, Grids;

type

  { TPreferencesForm }

  TPreferencesForm = class(TForm)
    CancelButton: TButton;
    CDISCGroupBox: TGroupBox;
    CPeptideUnitsCombo: TComboBox;
    DashLabel2: TLabel;
    DashLabel3: TLabel;
    FontsCombobox: TComboBox;
    GlucoseLabel: TLabel;
    GlucoseUnitsCombo: TComboBox;
    GluoseRRLEdit: TEdit;
    GluoseRRHEdit: TEdit;
    GUIGroupBox: TGroupBox;
    DashLabel1: TLabel;
    InsulinLabel: TLabel;
    CPeptideLabel: TLabel;
    InsulinRRHEdit: TEdit;
    CPeptideRRHEdit: TEdit;
    InsulinRRLEdit: TEdit;
    CPeptideRRLEdit: TEdit;
    InsulinUnitsCombo: TComboBox;
    MandatoryFieldsGrid: TStringGrid;
    MarkMandatoryCheck: TCheckBox;
    PrintingGroupbox: TGroupBox;
    SendingFacEdit: TEdit;
    IDGroupBox: TGroupBox;
    PlacerEdit: TEdit;
    SendingFacLabel: TLabel;
    OKButton: TButton;
    PageControl1: TPageControl;
    PlacerLabel: TLabel;
    TabSheet2: TTabSheet;
    TabSheet3: TTabSheet;
    TabSheet1: TTabSheet;
    procedure CancelButtonClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure OKButtonClick(Sender: TObject);
    procedure PageControl1Change(Sender: TObject);
  private

  public

  end;

var
  PreferencesForm: TPreferencesForm;

implementation

{$R *.lfm}

{ TPreferencesForm }

procedure TPreferencesForm.PageControl1Change(Sender: TObject);
begin

end;

procedure TPreferencesForm.CancelButtonClick(Sender: TObject);
begin
  Close;
end;

procedure TPreferencesForm.FormActivate(Sender: TObject);
begin
  SendingFacEdit.Text := gPreferences.MSH_ID;
  PlacerEdit.Text := gPreferences.Placer_ID;
  FontsCombobox.Items.Assign(Screen.Fonts);
end;

procedure TPreferencesForm.FormPaint(Sender: TObject);
begin
  if DarkTheme then
    Color := clDefault
  else
    Color := clWhite;
end;

procedure TPreferencesForm.OKButtonClick(Sender: TObject);
begin
  gPreferences.MSH_ID := SendingFacEdit.Text;
  gPreferences.Placer_ID := PlacerEdit.Text;
  if FontsCombobox.ItemIndex > 0 then
    gPreferences.PrintFont := FontsCombobox.Items[FontsCombobox.ItemIndex];
  SavePreferences;
  Close;
end;

end.

