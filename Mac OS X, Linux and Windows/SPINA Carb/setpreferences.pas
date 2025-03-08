unit Setpreferences;

{ SPINA Carb }

{ Application for calculating structure parameters }
{ of insulin-glucose feedback control }

{ Programm zur Berechnung von Strukturparametern }
{ des Insulin-Glukose-Regelkreises }

{ Version 5.1.0 (Cyclone) }

{ (c) J. W. Dietrich, 1994 - 2025 }
{ (c) Ludwig Maximilian University of Munich 1995 - 2002 }
{ (c) University of Ulm Hospitals 2002 - 2004 }
{ (c) Ruhr University of Bochum 2005 - 2025 }

{ This unit draws a dialog box for setting global application preferences }

{ Source code released under the BSD License }
{ See http://spina.medical-cybernetics.de for details }

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ComCtrls,
  Math, SPINATypes, HandlePreferences, SPINA_GUIServices, Grids;

type

  { TPreferencesForm }

  TPreferencesForm = class(TForm)
    CancelButton: TButton;
    CDISCGroupBox: TGroupBox;
    CGRLabel: TLabel;
    CGRRRHEdit: TEdit;
    CGRRRLEdit: TEdit;
    DashLabel10: TLabel;
    DashLabel5: TLabel;
    DashLabel6: TLabel;
    DashLabel7: TLabel;
    DashLabel8: TLabel;
    DashLabel9: TLabel;
    GRUnitLabel: TLabel;
    HOMAIRLabel: TLabel;
    HOMABetaRRHEdit: TEdit;
    HOMAISLabel: TLabel;
    HOMAIRRRHEdit: TEdit;
    HOMABetaRRLEdit: TEdit;
    QUICKILabel: TLabel;
    HOMAISRRHEdit: TEdit;
    HOMAIRRRLEdit: TEdit;
    HOMABetaUnitLabel: TLabel;
    GRRRHEdit: TEdit;
    DIRRHEdit: TEdit;
    GRRRLEdit: TEdit;
    DIRRLEdit: TEdit;
    QUICKIRRHEdit: TEdit;
    HOMAISRRLEdit: TEdit;
    QUICKIRRLEdit: TEdit;
    SPINAGBetaLabel: TLabel;
    GBetaRRHEdit: TEdit;
    GBetaRRLEdit: TEdit;
    CPeptideUnitsCombo: TComboBox;
    DashLabel2: TLabel;
    DashLabel3: TLabel;
    DashLabel4: TLabel;
    FontsCombobox: TComboBox;
    GlucoseLabel: TLabel;
    GlucoseUnitsCombo: TComboBox;
    GlucoseRRLEdit: TEdit;
    GlucoseRRHEdit: TEdit;
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
    GBetaUnitLabel: TLabel;
    HOMABetaLabel: TLabel;
    SPINAGRLabel: TLabel;
    SPINADILabel: TLabel;
    TabSheet2: TTabSheet;
    TabSheet3: TTabSheet;
    TabSheet1: TTabSheet;
    procedure CancelButtonClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
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
var
  preferredFontPos, sansSerifPos: integer;
begin
  SendingFacEdit.Text := gPreferences.MSH_ID;
  PlacerEdit.Text := gPreferences.Placer_ID;
  preferredFontPos := -1;
  sansSerifPos := -1;
  FontsCombobox.Items.Assign(Screen.Fonts);
  if gPreferences.PrintFont <> '' then
    preferredFontPos := FontsCombobox.Items.IndexOf(gPreferences.PrintFont);
  sansSerifPos := FontsCombobox.Items.IndexOf('Helvetica');
  if sansSerifPos = -1 then
    sansSerifPos := FontsCombobox.Items.IndexOf('Arial');
  if preferredFontPos >= 0 then
    FontsCombobox.ItemIndex := preferredFontPos
  else if sansSerifPos >= 0 then
    FontsCombobox.ItemIndex := sansSerifPos;
  if FontsCombobox.ItemIndex >= 0 then
    gPreferences.PrintFont := FontsCombobox.Items[FontsCombobox.ItemIndex];
end;

procedure TPreferencesForm.FormCreate(Sender: TObject);
begin
  GlucoseRRLEdit.Text := FloatToStr(gPreferences.ReferenceValues.Glucose.ln);
  GlucoseRRHEdit.Text := FloatToStr(gPreferences.ReferenceValues.Glucose.hn);
  GlucoseUnitsCombo.Text := gPreferences.ReferenceValues.Glucose.UoM;
  InsulinRRLEdit.Text := FloatToStr(gPreferences.ReferenceValues.Insulin.ln);
  InsulinRRHEdit.Text := FloatToStr(gPreferences.ReferenceValues.Insulin.hn);
  InsulinUnitsCombo.Text := gPreferences.ReferenceValues.Insulin.UoM;
  CPeptideRRLEdit.Text := FloatToStr(gPreferences.ReferenceValues.CPeptide.ln);
  CPeptideRRHEdit.Text := FloatToStr(gPreferences.ReferenceValues.CPeptide.hn);
  CPeptideUnitsCombo.Text := gPreferences.ReferenceValues.CPeptide.UoM;
  GBetaRRLEdit.Text := FloatToStr(gPreferences.ReferenceValues.SPINA_GBeta.ln);
  GBetaRRHEdit.Text := FloatToStr(gPreferences.ReferenceValues.SPINA_GBeta.hn);
  GRRRLEdit.Text := FloatToStr(gPreferences.ReferenceValues.SPINA_GR.ln);
  GRRRHEdit.Text := FloatToStr(gPreferences.ReferenceValues.SPINA_GR.hn);
  DIRRLEdit.Text := FloatToStr(gPreferences.ReferenceValues.SPINA_DI.ln);
  DIRRHEdit.Text := FloatToStr(gPreferences.ReferenceValues.SPINA_DI.hn);
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
  gPreferences.ReferenceValues.Glucose.ln := StrToFloatDef(GlucoseRRLEdit.Text, Math.Nan);
  gPreferences.ReferenceValues.Glucose.hn := StrToFloatDef(GlucoseRRHEdit.Text, Math.Nan);
  gPreferences.ReferenceValues.Glucose.UoM := GlucoseUnitsCombo.Text;
  gPreferences.ReferenceValues.Insulin.ln := StrToFloatDef(InsulinRRLEdit.Text, Math.Nan);
  gPreferences.ReferenceValues.Insulin.hn := StrToFloatDef(InsulinRRHEdit.Text, Math.Nan);
  gPreferences.ReferenceValues.Insulin.uom := InsulinUnitsCombo.Text;
  gPreferences.ReferenceValues.CPeptide.ln := StrToFloatDef(CPeptideRRLEdit.Text, Math.Nan);
  gPreferences.ReferenceValues.CPeptide.hn := StrToFloatDef(CPeptideRRHEdit.Text, Math.Nan);
  gPreferences.ReferenceValues.CPeptide.uom := CPeptideUnitsCombo.Text;
  gPreferences.ReferenceValues.SPINA_GBeta.ln := StrToFloatDef(GBetaRRLEdit.Text, Math.Nan);
  gPreferences.ReferenceValues.SPINA_GBeta.hn := StrToFloatDef(GBetaRRHEdit.Text, Math.Nan);
  gPreferences.ReferenceValues.SPINA_GBeta.UoM := GBetaUnitLabel.Caption;
  gPreferences.ReferenceValues.SPINA_GR.ln := StrToFloatDef(GRRRLEdit.Text, Math.Nan);
  gPreferences.ReferenceValues.SPINA_GR.hn := StrToFloatDef(GRRRHEdit.Text, Math.Nan);
  gPreferences.ReferenceValues.SPINA_GR.UoM := GRUnitLabel.Caption;
  gPreferences.ReferenceValues.SPINA_DI.ln := StrToFloatDef(DIRRLEdit.Text, Math.Nan);
  gPreferences.ReferenceValues.SPINA_DI.hn := StrToFloatDef(DIRRHEdit.Text, Math.Nan);
  gPreferences.ReferenceValues.HOMA_Beta.ln := StrToFloatDef(HOMABetaRRLEdit.Text, Math.Nan);
  gPreferences.ReferenceValues.HOMA_Beta.hn := StrToFloatDef(HOMABetaRRHEdit.Text, Math.Nan);
  gPreferences.ReferenceValues.HOMA_Beta.UoM := HOMABetaUnitLabel.Caption;
  gPreferences.ReferenceValues.HOMA_IR.ln := StrToFloatDef(HOMAIRRRLEdit.Text, Math.Nan);
  gPreferences.ReferenceValues.HOMA_IR.hn := StrToFloatDef(HOMAIRRRHEdit.Text, Math.Nan);
  gPreferences.ReferenceValues.HOMA_IS.ln := StrToFloatDef(HOMAISRRLEdit.Text, Math.Nan);
  gPreferences.ReferenceValues.HOMA_IS.hn := StrToFloatDef(HOMAISRRHEdit.Text, Math.Nan);
  gPreferences.ReferenceValues.QUICKI.ln := StrToFloatDef(QUICKIRRLEdit.Text, Math.Nan);
  gPreferences.ReferenceValues.QUICKI.hn := StrToFloatDef(QUICKIRRHEdit.Text, Math.Nan);
  gPreferences.ReferenceValues.CGR.ln := StrToFloatDef(CGRRRLEdit.Text, Math.Nan);
  gPreferences.ReferenceValues.CGR.hn := StrToFloatDef(CGRRRHEdit.Text, Math.Nan);
  if FontsCombobox.ItemIndex > 0 then
    gPreferences.PrintFont := FontsCombobox.Items[FontsCombobox.ItemIndex];
  SavePreferences;
  Close;
end;

end.
