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
  Math, Grids, UnitConverter, SPINATypes, SPINA_Resources, SPINA_Engine,
  HandlePreferences, SPINA_GUIServices, LocaleServices, Types;

type

  { TPreferencesForm }

  TPreferencesForm = class(TForm)
    CancelButton: TButton;
    CDISCGroupBox: TGroupBox;
    CGRLabel: TLabel;
    AIGRLabel: TLabel;
    CGRRRHEdit: TEdit;
    AIGRRRHEdit: TEdit;
    CGRRRLEdit: TEdit;
    AIGRRRLEdit: TEdit;
    CGRUnitLabel: TLabel;
    LOINCCheck: TCheckBox;
    DashLabel10: TLabel;
    DashLabel11: TLabel;
    DashLabel12: TLabel;
    DashLabel5: TLabel;
    DashLabel6: TLabel;
    DashLabel7: TLabel;
    DashLabel8: TLabel;
    DashLabel9: TLabel;
    VariableGroupBox: TGroupBox;
    GRUnitLabel: TLabel;
    AIGRUnitLabel: TLabel;
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
    HL7Tab: TTabSheet;
    OtherTab: TTabSheet;
    RangesTab: TTabSheet;
    procedure CancelButtonClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure MandatoryFieldsGridDrawCell(Sender: TObject; aCol, aRow: Integer;
      aRect: TRect; aState: TGridDrawState);
    procedure OKButtonClick(Sender: TObject);
    procedure PageControl1Change(Sender: TObject);
  private
    procedure CheckMandatoryColourising(Sender: TObject);
  public
    procedure PopulateEdits(Sender: TObject);
  end;

var
  PreferencesForm: TPreferencesForm;

implementation

{$R *.lfm}

{ TPreferencesForm }

procedure TPreferencesForm.PageControl1Change(Sender: TObject);
begin

end;

procedure TPreferencesForm.CheckMandatoryColourising(Sender: TObject);
begin
  if MarkMandatoryCheck.Checked then
    gPreferences.colouriseMandatoryFields := true
  else
    gPreferences.colouriseMandatoryFields := false;
end;

procedure TPreferencesForm.PopulateEdits(Sender: TObject);
var
  preferredFontPos, sansSerifPos: integer;
begin
  GlucoseRRLEdit.Text := FloatToStrF(
    ConvertedValue(gPreferences.ReferenceValues.Glucose.ln, kGlucoseMolarMass,
    gPreferences.ReferenceValues.Glucose.UoM, gPreferences.PreferredUoMs.Glucose), ffFixed, 4, 1);
  GlucoseRRHEdit.Text := FloatToStrF(
    ConvertedValue(gPreferences.ReferenceValues.Glucose.hn, kGlucoseMolarMass,
    gPreferences.ReferenceValues.Glucose.UoM, gPreferences.PreferredUoMs.Glucose), ffFixed, 4, 1);
  GlucoseUnitsCombo.Text := gPreferences.PreferredUoMs.Glucose;

  InsulinRRLEdit.Text := FloatToStrF(
    ConvertedValue(gPreferences.ReferenceValues.Insulin.ln, kInsulinConversionFactor,
    gPreferences.ReferenceValues.Insulin.UoM, gPreferences.PreferredUoMs.Insulin), ffFixed, 4, 1);
  InsulinRRHEdit.Text := FloatToStrF(
    ConvertedValue(gPreferences.ReferenceValues.Insulin.hn, kInsulinConversionFactor,
    gPreferences.ReferenceValues.Insulin.UoM, gPreferences.PreferredUoMs.Insulin), ffFixed, 4, 1);
  InsulinUnitsCombo.Text := gPreferences.PreferredUoMs.Insulin;

  CPeptideRRLEdit.Text := FloatToStrF(
    ConvertedValue(gPreferences.ReferenceValues.CPeptide.ln, kCPeptideMolarMass,
    gPreferences.ReferenceValues.CPeptide.UoM, gPreferences.PreferredUoMs.CPeptide), ffFixed, 4, 1);
  CPeptideRRHEdit.Text := FloatToStrF(
    ConvertedValue(gPreferences.ReferenceValues.CPeptide.hn, kCPeptideMolarMass,
    gPreferences.ReferenceValues.CPeptide.UoM, gPreferences.PreferredUoMs.CPeptide), ffFixed, 4, 1);
  CPeptideUnitsCombo.Text := gPreferences.PreferredUoMs.CPeptide;

  GBetaRRLEdit.Text := FloatToStrF(gPreferences.ReferenceValues.SPINA_GBeta.ln, ffFixed, 4, 2);
  GBetaRRHEdit.Text := FloatToStrF(gPreferences.ReferenceValues.SPINA_GBeta.hn, ffFixed, 4, 2);

  GRRRLEdit.Text := FloatToStrF(gPreferences.ReferenceValues.SPINA_GR.ln, ffFixed, 4, 2);
  GRRRHEdit.Text := FloatToStrF(gPreferences.ReferenceValues.SPINA_GR.hn, ffFixed, 4, 2);

  DIRRLEdit.Text := FloatToStrF(gPreferences.ReferenceValues.SPINA_DI.ln, ffFixed, 4, 2);
  DIRRHEdit.Text := FloatToStrF(gPreferences.ReferenceValues.SPINA_DI.hn, ffFixed, 4, 2);

  HOMABetaRRLEdit.Text := FloatToStrF(gPreferences.ReferenceValues.HOMA_Beta.ln, ffFixed, 4, 1);
  HOMABetaRRHEdit.Text := FloatToStrF(gPreferences.ReferenceValues.HOMA_Beta.hn, ffFixed, 4, 1);

  HOMAIRRRLEdit.Text := FloatToStrF(gPreferences.ReferenceValues.HOMA_IR.ln, ffFixed, 4, 1);
  HOMAIRRRHEdit.Text := FloatToStrF(gPreferences.ReferenceValues.HOMA_IR.hn, ffFixed, 4, 1);

  HOMAISRRLEdit.Text := FloatToStrF(gPreferences.ReferenceValues.HOMA_IS.ln, ffFixed, 4, 1);
  HOMAISRRHEdit.Text := FloatToStrF(gPreferences.ReferenceValues.HOMA_IS.hn, ffFixed, 4, 1);

  QUICKIRRLEdit.Text := FloatToStrF(gPreferences.ReferenceValues.QUICKI.ln, ffFixed, 4, 1);
  QUICKIRRHEdit.Text := FloatToStrF(gPreferences.ReferenceValues.QUICKI.hn, ffFixed, 4, 1);

  AIGRRRLEdit.Text := FloatToStrF(gPreferences.ReferenceValues.AIGR.ln, ffFixed, 4, 1);
  AIGRRRHEdit.Text := FloatToStrF(gPreferences.ReferenceValues.AIGR.hn, ffFixed, 4, 1);

  CGRRRLEdit.Text := FloatToStrF(gPreferences.ReferenceValues.CGR.ln, ffFixed, 4, 1);
  CGRRRHEdit.Text := FloatToStrF(gPreferences.ReferenceValues.CGR.hn, ffFixed, 4, 1);

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

procedure TPreferencesForm.CancelButtonClick(Sender: TObject);
begin
  Close;
end;

procedure TPreferencesForm.FormActivate(Sender: TObject);
begin
  PopulateEdits(Sender);
  {$IFDEF Darwin}
  MarkMandatoryCheck.Checked := false;
  {$ELSE}
  if gPreferences.colouriseMandatoryFields then
    MarkMandatoryCheck.Checked := true
  else
    MarkMandatoryCheck.Checked := false;
  {$ENDIF}
  if gPreferences.exportLOINC then
    LOINCCheck.Checked := true
  else
    LOINCCheck.Checked := false
end;

procedure TPreferencesForm.FormCreate(Sender: TObject);
begin
  if gPreferredLanguage = 'de' then
    begin
      CancelButton.Caption := kPrefLabels_de.Cancel;
      RangesTab.Caption := kPrefLabels_de.RangesTab;
      HL7Tab.Caption := kPrefLabels_de.HL7Tab;
      OtherTab.Caption := kPrefLabels_de.OtherTab;
      CDISCGroupBox.Caption := kPrefLabels_de.rtRanges;
      GlucoseLabel.Caption := kPrefLabels_de.rtGlucose;
      InsulinLabel.Caption := kPrefLabels_de.rtInsulin;
      CPeptideLabel.Caption := kPrefLabels_de.rtCPep;
      IDGroupBox.Caption := kPrefLabels_de.hlIDGroup;
      SendingFacLabel.Caption := kPrefLabels_de.hlSender;
      PlacerLabel.Caption := kPrefLabels_de.hlPlacer;
      VariableGroupBox.Caption := kPrefLabels_de.hlVariableGroup;
      LOINCCheck.Caption := kPrefLabels_de.hlExport;
      MarkMandatoryCheck.Caption := kPrefLabels_de.otMandatory;
      PrintingGroupBox.Caption := kPrefLabels_de.otFont;
    end;
  PopulateEdits(Sender);
  MandatoryFieldsGrid.Rows[4].Text := 'Glucose';
  MandatoryFieldsGrid.Rows[5].Text := 'Insulin';
  MandatoryFieldsGrid.Rows[6].Text := 'C-Peptide';
  MandatoryFieldsGrid.Cells[1, 1] := 'HOMA-Beta';
  MandatoryFieldsGrid.Cells[1, 2] := 'AIGR';
  MandatoryFieldsGrid.Cells[2, 1] := 'HOMA-IR';
  MandatoryFieldsGrid.Cells[2, 2] := 'HOMA-IS';
  MandatoryFieldsGrid.Cells[2, 3] := 'QUICKI';
  MandatoryFieldsGrid.Cells[1, 4] := '+';
  MandatoryFieldsGrid.Cells[1, 5] := '+';
  MandatoryFieldsGrid.Cells[1, 6] := '——';
  MandatoryFieldsGrid.Cells[2, 4] := '+';
  MandatoryFieldsGrid.Cells[2, 5] := '+';
  MandatoryFieldsGrid.Cells[2, 6] := '——';
  MandatoryFieldsGrid.Cells[3, 4] := '+';
  MandatoryFieldsGrid.Cells[3, 5] := '+';
  MandatoryFieldsGrid.Cells[3, 6] := '——';
  MandatoryFieldsGrid.Cells[4, 4] := '+';
  MandatoryFieldsGrid.Cells[4, 5] := '——';
  MandatoryFieldsGrid.Cells[4, 6] := '+';
  {$IFDEF Darwin}
  MarkMandatoryCheck.Checked := false;
  MarkMandatoryCheck.Enabled := false;
  MarkMandatoryCheck.Hint := kMacUnavailable;
  {$ENDIF}
end;

procedure TPreferencesForm.FormPaint(Sender: TObject);
begin
  if DarkTheme then
    Color := clDefault
  else
    Color := clWhite;
end;

procedure TPreferencesForm.FormShow(Sender: TObject);
begin
  PopulateEdits(Sender);
end;

procedure TPreferencesForm.MandatoryFieldsGridDrawCell(Sender: TObject; aCol,
  aRow: Integer; aRect: TRect; aState: TGridDrawState);
var
  theContent: String;
  MyTxtStyle: TTextStyle;
begin
  if (aCol > 0) and (aRow > 3) then
  with MandatoryFieldsGrid do
  begin
    theContent := Cells[ACol, ARow];
    if (theContent = '-') or (theContent = '——') or (theContent = '—') then
      Canvas.Brush.Color := clRed
    else if theContent = '+' then
      Canvas.Brush.Color := clGreen
    else
      Canvas.Brush.Color := clGray;
    Canvas.FillRect(aRect);
    MyTxtStyle.Alignment := taCenter;
    MyTxtStyle.Layout := tlCenter;
    Canvas.TextRect(aRect, aRect.Left, aRect.Top, thecontent, MyTxtStyle);
  end;
end;

procedure TPreferencesForm.OKButtonClick(Sender: TObject);
begin
  gPreferences.MSH_ID := SendingFacEdit.Text;
  gPreferences.Placer_ID := PlacerEdit.Text;
  gPreferences.ReferenceValues.Glucose.ln :=
    StrToFloatDefL(GlucoseRRLEdit.Text, Math.Nan);
  gPreferences.ReferenceValues.Glucose.hn :=
    StrToFloatDefL(GlucoseRRHEdit.Text, Math.Nan);
  gPreferences.ReferenceValues.Glucose.UoM := GlucoseUnitsCombo.Text;
  gPreferences.ReferenceValues.Insulin.ln :=
    StrToFloatDefL(InsulinRRLEdit.Text, Math.Nan);
  gPreferences.ReferenceValues.Insulin.hn :=
    StrToFloatDefL(InsulinRRHEdit.Text, Math.Nan);
  gPreferences.ReferenceValues.Insulin.uom := InsulinUnitsCombo.Text;
  gPreferences.ReferenceValues.CPeptide.ln :=
    StrToFloatDefL(CPeptideRRLEdit.Text, Math.Nan);
  gPreferences.ReferenceValues.CPeptide.hn :=
    StrToFloatDefL(CPeptideRRHEdit.Text, Math.Nan);
  gPreferences.ReferenceValues.CPeptide.uom := CPeptideUnitsCombo.Text;
  gPreferences.ReferenceValues.SPINA_GBeta.ln :=
    StrToFloatDefL(GBetaRRLEdit.Text, Math.Nan);
  gPreferences.ReferenceValues.SPINA_GBeta.hn :=
    StrToFloatDefL(GBetaRRHEdit.Text, Math.Nan);
  gPreferences.ReferenceValues.SPINA_GBeta.UoM := GBetaUnitLabel.Caption;
  gPreferences.ReferenceValues.SPINA_GR.ln := StrToFloatDefL(GRRRLEdit.Text, Math.Nan);
  gPreferences.ReferenceValues.SPINA_GR.hn := StrToFloatDefL(GRRRHEdit.Text, Math.Nan);
  gPreferences.ReferenceValues.SPINA_GR.UoM := GRUnitLabel.Caption;
  gPreferences.ReferenceValues.SPINA_DI.ln := StrToFloatDefL(DIRRLEdit.Text, Math.Nan);
  gPreferences.ReferenceValues.SPINA_DI.hn := StrToFloatDefL(DIRRHEdit.Text, Math.Nan);
  gPreferences.ReferenceValues.HOMA_Beta.ln :=
    StrToFloatDefL(HOMABetaRRLEdit.Text, Math.Nan);
  gPreferences.ReferenceValues.HOMA_Beta.hn :=
    StrToFloatDefL(HOMABetaRRHEdit.Text, Math.Nan);
  gPreferences.ReferenceValues.HOMA_Beta.UoM := HOMABetaUnitLabel.Caption;
  gPreferences.ReferenceValues.HOMA_IR.ln := StrToFloatDefL(HOMAIRRRLEdit.Text, Math.Nan);
  gPreferences.ReferenceValues.HOMA_IR.hn := StrToFloatDefL(HOMAIRRRHEdit.Text, Math.Nan);
  gPreferences.ReferenceValues.HOMA_IS.ln := StrToFloatDefL(HOMAISRRLEdit.Text, Math.Nan);
  gPreferences.ReferenceValues.HOMA_IS.hn := StrToFloatDefL(HOMAISRRHEdit.Text, Math.Nan);
  gPreferences.ReferenceValues.QUICKI.ln := StrToFloatDefL(QUICKIRRLEdit.Text, Math.Nan);
  gPreferences.ReferenceValues.QUICKI.hn := StrToFloatDefL(QUICKIRRHEdit.Text, Math.Nan);
  gPreferences.ReferenceValues.AIGR.ln := StrToFloatDefL(AIGRRRLEdit.Text, Math.Nan);
  gPreferences.ReferenceValues.AIGR.hn := StrToFloatDefL(AIGRRRHEdit.Text, Math.Nan);
  gPreferences.ReferenceValues.AIGR.UoM := AIGRUnitLabel.Caption;
  gPreferences.ReferenceValues.CGR.ln := StrToFloatDefL(CGRRRLEdit.Text, Math.Nan);
  gPreferences.ReferenceValues.CGR.hn := StrToFloatDefL(CGRRRHEdit.Text, Math.Nan);
  gPreferences.ReferenceValues.CGR.UoM := CGRUnitLabel.Caption;
  gPreferences.PreferredUoMs.Glucose := GlucoseUnitsCombo.Text;
  gPreferences.PreferredUoMs.Insulin := InsulinUnitsCombo.Text;
  gPreferences.PreferredUoMs.CPeptide := CPeptideUnitsCombo.Text;
  if FontsCombobox.ItemIndex > 0 then
    gPreferences.PrintFont := FontsCombobox.Items[FontsCombobox.ItemIndex];
  CheckMandatoryColourising(Sender);
  if LOINCCheck.Checked then
    gPreferences.exportLOINC := true
  else
    gPreferences.exportLOINC := false;
  SavePreferences;
  SaveRefRanges;
  Close;
end;

end.
