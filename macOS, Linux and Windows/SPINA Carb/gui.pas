unit GUI;

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

{ GUI }

{ Source code released under the BSD License }
{ See http://spina.medical-cybernetics.de for details }

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  Menus, ActnList, StdActns, Math, LCLType, ComCtrls, StrUtils, Types,
  EditBtn, Clipbrd,
  EnvironmentInfo, SPINATypes, CaseBroker, SPINA_GUIServices,
  ResultWindow, SPINA_Aboutbox, Printers, PrintersDlgs, PrintCase,
  SetPreferences, UnitConverter, SPINA_Engine, HandleImpEx;
  { #todo : Move Formatting of reference ranges to CaseBroker and remove dependendy on SPINA Engine here }

type

  { THauptschirm }

  THauptschirm = class(TForm)
    ActionList1: TActionList;
    AppleMenu: TMenuItem;
    Bevel1: TBevel;
    Bevel2: TBevel;
    CaseDataMenuItem: TMenuItem;
    Divider01: TMenuItem;
    MacPreferencesMenuItem: TMenuItem;
    Divider23: TMenuItem;
    CopyResultMenuItem: TMenuItem;
    OpenCaseDialog: TOpenDialog;
    SaveResultsDialog: TSaveDialog;
    WinPreferencesMenuItem: TMenuItem;
    PageSetupMenuItem: TMenuItem;
    PrintDialog1: TPrintDialog;
    PrinterSetupDialog1: TPrinterSetupDialog;
    PrintMenuItem: TMenuItem;
    NextButton: TButton;
    CalculateButton: TButton;
    PageSetupDialog1: TPageSetupDialog;
    PlacerLabel: TLabel;
    ObDateEdit: TDateEdit;
    PlacerEdit: TEdit;
    GivenNameLabel: TLabel;
    DOBLabel: TLabel;
    PIDLabel: TLabel;
    DoBEdit: TDateEdit;
    GivenNameEdit: TEdit;
    CaseIDLabel: TLabel;
    PIDEdit: TEdit;
    CloseMenuItem: TMenuItem;
    CopyMenuItem: TMenuItem;
    CutMenuItem: TMenuItem;
    Divider11: TMenuItem;
    Seaparator2: TMenuItem;
    Divider21: TMenuItem;
    Divider22: TMenuItem;
    CaseIDEdit: TEdit;
    EditCopy1: TEditCopy;
    EditMenu: TMenuItem;
    EditSelectAll1: TEditSelectAll;
    FileMenu: TMenuItem;
    GlucoseUnitsCombo: TComboBox;
    GlucoseEdit: TEdit;
    EntryBox: TGroupBox;
    FeedbackImage: TImage;
    LogoBox: TGroupBox;
    HelpMenu: TMenuItem;
    ImageList1: TImageList;
    CPeptideLabel: TLabel;
    CPeptideEdit: TEdit;
    CPeptideUnitsCombo: TComboBox;
    MainPageControl: TPageControl;
    NameEdit: TEdit;
    NameLabel: TLabel;
    ObDateLabel: TLabel;
    Divider12: TMenuItem;
    Divider14: TMenuItem;
    Divider24: TMenuItem;
    SPINACarbLabel: TLabel;
    LogoImage: TImage;
    InsulinUnitsCombo: TComboBox;
    InsulinEdit: TEdit;
    GlucoseLabel: TLabel;
    InsulinLabel: TLabel;
    MacAboutItem: TMenuItem;
    MainMenu1: TMainMenu;
    ResultsMemo: TMemo;
    NewMenuItem: TMenuItem;
    OpenMenuItem: TMenuItem;
    PasteMenuItem: TMenuItem;
    QuitMenuItem: TMenuItem;
    RedoMenuItem: TMenuItem;
    ResultsBox: TGroupBox;
    HintBox: TGroupBox;
    HintsMemo: TMemo;
    SaveMenuItem: TMenuItem;
    SelectAllMenuItem: TMenuItem;
    CaseEditorSheet: TTabSheet;
    EntrySheet: TTabSheet;
    ObTimeEdit: TTimeEdit;
    UndoMenuItem: TMenuItem;
    WinAboutItem: TMenuItem;
    procedure CalculateButtonClick(Sender: TObject);
    procedure CaseDataMenuItemClick(Sender: TObject);
    procedure CaseEditorSheetShow(Sender: TObject);
    procedure CopyResultMenuItemClick(Sender: TObject);
    procedure EntrySheetShow(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure LogoImageClick(Sender: TObject);
    procedure MacAboutItemClick(Sender: TObject);
    procedure MacPreferencesMenuItemClick(Sender: TObject);
    procedure NextButtonClick(Sender: TObject);
    procedure OpenMenuItemClick(Sender: TObject);
    procedure PageSetupMenuItemClick(Sender: TObject);
    procedure PrintMenuItemClick(Sender: TObject);
    procedure QuitMenuItemClick(Sender: TObject);
    procedure ResultsMemoChange(Sender: TObject);
    procedure SaveMenuItemClick(Sender: TObject);
    procedure SPINACarbLabelClick(Sender: TObject);
    procedure WinAboutItemClick(Sender: TObject);
    procedure WinPreferencesMenuItemClick(Sender: TObject);
  private
    function DoPageSetup: boolean;
    function DoPrintSetup: boolean;
    procedure InsertValues(Sender: TObject);
    procedure FillFromCaseRecord;
  public
    CaseRecord: tCaseRecord;
    InsulinRaw, GlucoseRaw, CPeptideRaw: extended;
    InsulinUoM, GlucoseUoM, CPeptideUoM: string;
    procedure RegisterEntry(Sender: TObject);
    procedure CreateOutput(Sender: TObject);
    procedure AdaptForPlatform;
    procedure AdapttoTheme(Sender: TObject);
    procedure FocusEdit(Sender: TObject);
    procedure RegisterCaseData(Sender: TObject);
    procedure SaveResults(Sender: TObject);
    procedure ReadCaseRecord(Sender: TObject; const theCaseRecord: tCaseRecord);
  end;

var
  Hauptschirm: THauptschirm;

implementation

{$R *.lfm}

{ THauptschirm }

procedure THauptschirm.CalculateButtonClick(Sender: TObject);
begin
  RegisterEntry(Sender);
  Calculate(CaseRecord.LabRecord);
  CreateOutput(Sender);
  ResultsMemo.Text := LineEnding + CaseRecord.CombMessage;
  ResultsMemo.Hint := CaseRecord.RCombMessage2;
  ResultForm.ShowResults(caseRecord.BParMessage, caseRecord.SParMessage,
    caseRecord.BRefMessage2, caseRecord.SRefMessage2);
  ResultForm.Visible := True;
  ResultForm.ShowOnTop;
end;

procedure THauptschirm.CaseDataMenuItemClick(Sender: TObject);
begin
  MainPageControl.ActivePage := CaseEditorSheet;
  application.ProcessMessages;
  ActiveControl := CaseIDEdit;
end;

procedure THauptschirm.CaseEditorSheetShow(Sender: TObject);
begin
  NextButton.Default := True;
  ActiveControl := CaseIDEdit;
end;

procedure THauptschirm.CopyResultMenuItemClick(Sender: TObject);
var
  Clipboard: TClipboard;
begin
  Clipboard := TClipboard.Create();
  Clipboard.AsText := CaseRecord.CombMessage;
end;

procedure THauptschirm.EntrySheetShow(Sender: TObject);
begin
  CalculateButton.Default := True;
end;

procedure THauptschirm.FormActivate(Sender: TObject);
begin
  AdaptToTheme(Sender);
  PlacerEdit.Text := gPreferences.Placer_ID;
  FocusEdit(Sender);
end;

procedure THauptschirm.FormCreate(Sender: TObject);
begin
  AdaptForPlatform;
  SPINACarbLabel.Caption := 'SPINA Carb ' + FileVersion;
  FocusEdit(Sender);
end;

procedure THauptschirm.FormShow(Sender: TObject);
begin
  AdaptToTheme(Sender);
  FocusEdit(Sender);
end;

procedure THauptschirm.LogoImageClick(Sender: TObject);
begin
  MacAboutItemClick(Sender);
end;

procedure THauptschirm.MacAboutItemClick(Sender: TObject);
begin
  AboutBox.ShowOnTop;
end;

procedure THauptschirm.MacPreferencesMenuItemClick(Sender: TObject);
begin
  PreferencesForm.ShowModal;
end;

procedure THauptschirm.NextButtonClick(Sender: TObject);
begin
  RegisterCaseData(Sender);
  MainPageControl.ActivePage := EntrySheet;
end;

procedure THauptschirm.OpenMenuItemClick(Sender: TObject);
{ Open case from file }
var
  theFilterIndex: integer;
  theCaseRecord: tCaseRecord;
begin
  if OpenCaseDialog.Execute then
  begin
    theFilterIndex := OpenCaseDialog.FilterIndex;
    case theFilterIndex of
      1: OpenCaseRecord(theCaseRecord, OpenCaseDialog.FileName, HL7Message);
    end;
    ReadCaseRecord(Sender, theCaseRecord);
  end;
end;

procedure THauptschirm.PageSetupMenuItemClick(Sender: TObject);
begin
  if not DoPageSetup then
    Exit;
end;

procedure THauptschirm.PrintMenuItemClick(Sender: TObject);
begin
  assert(assigned(Printer));
  if DoPrintSetup then
    PrintCaseRecord(CaseRecord);
end;

procedure THauptschirm.QuitMenuItemClick(Sender: TObject);
begin
  application.Terminate;
end;

procedure THauptschirm.ResultsMemoChange(Sender: TObject);
begin

end;

procedure THauptschirm.SaveMenuItemClick(Sender: TObject);
begin
  SaveResults(Sender);
end;

procedure THauptschirm.SPINACarbLabelClick(Sender: TObject);
begin
  MacAboutItemClick(Sender);
end;

procedure THauptschirm.WinAboutItemClick(Sender: TObject);
begin
  MacAboutItemClick(Sender);
end;

procedure THauptschirm.WinPreferencesMenuItemClick(Sender: TObject);
begin
  MacPreferencesMenuItemClick(Sender);
end;

function THauptschirm.DoPageSetup: boolean;
begin
  Result := PageSetupDialog1.Execute;
end;

function THauptschirm.DoPrintSetup: boolean;
begin
  Result := PrinterSetupDialog1.Execute;
  {PrintDialog1.FromPage := 1;
  PrintDialog1.ToPage := 1;
  Result := PrintDialog1.Execute;}
end;

procedure THauptschirm.InsertValues(Sender: TObject);
{ Inserts values of case record into appropriate fields of form }
begin
  if not isNaN(caseRecord.LabRecord.Glucose) then
  begin
    GlucoseEdit.Text := FloatToStr(ConvertedValue(caseRecord.LabRecord.Glucose,
      kGlucoseMolarMass, 'mmol/l', GlucoseUnitsCombo.Caption));
  end;
end;

procedure THauptschirm.FillFromCaseRecord;
begin
  CaseIDEdit.Text := caseRecord.CaseID;
  PIDEdit.Text := caseRecord.PID;
  NameEdit.Text := caseRecord.Name;
  GivenNameEdit.Text := caseRecord.GivenNames;
  if not isNaN(caseRecord.DoBDate) then
    DoBEdit.Date := caseRecord.DoBDate;
  PlacerEdit.Text := caseRecord.Placer;
  if not isNaN(caseRecord.OBDate) then
    OBDateEdit.Date := caseRecord.OBDate;
  //CommentEdit.Text := caseRecord.Comment;
end;

procedure THauptschirm.AdapttoTheme(Sender: TObject);
begin
  if DarkTheme then
  begin
    Color := clDefault;
    EntryBox.Color := clDefault;
    ResultsBox.Color := clDefault;
    HintBox.Color := clDefault;
    LogoBox.Color := clDefault;
  end
  else
  begin
    Color := clWhite;
    EntryBox.Color := clWhite;
    ResultsBox.Color := clWhite;
    HintBox.Color := clWhite;
    LogoBox.Color := clWhite;
  end;
end;

procedure THauptschirm.FocusEdit(Sender: TObject);
begin
  if MainPageControl.ActivePage = EntrySheet then
    ActiveControl := GlucoseEdit;
end;

procedure THauptschirm.RegisterCaseData(Sender: TObject);
begin
  CaseRecord.CaseID := CaseIDEdit.Text;
  CaseRecord.PID := PIDEdit.Text;
  CaseRecord.Name := NameEdit.Text;
  CaseRecord.GivenNames := GivenNameEdit.Text;
  CaseRecord.DoBDate := DOBEdit.DATE;
  CaseRecord.Placer := PlacerEdit.Text;
  CaseRecord.OBDate := ObDateEdit.DATE;
  CaseRecord.OBTime := ObTimeEdit.TIME;
end;

procedure THauptschirm.SaveResults(Sender: TObject);
var
  filePath: string;
  theFilterIndex: integer;
begin
  SaveResultsDialog.FilterIndex := 1;
  if SaveResultsDialog.Execute then
  begin
    theFilterIndex := SaveResultsDialog.FilterIndex;
    filePath := SaveResultsDialog.FileName;
    case theFilterIndex of
      1: SaveCaseRecord(CaseRecord, filePath, HL7Message);
      2: SaveCaseRecord(CaseRecord, filePath, plainTextFile);
    end;
  end;
end;

procedure THauptschirm.ReadCaseRecord(Sender: TObject;
  const theCaseRecord: tCaseRecord);
{ reads a CaseRecord, validates the results and fills edits of main form }
var
  status: integer;
  DoBDateStr, OBDateStr: string;
begin
  { Is this a valid case record? }
  if isNaN(theCaseRecord.LabRecord.Glucose) and
    isNan(theCaseRecord.LabRecord.SPINA_GR) and (theCaseRecord.GivenNames = '') then
    ShowMessage(FILE_FORMAT_MESSAGE)
  else
  begin
    if isNaN(theCaseRecord.LabRecord.Glucose) and
      isNaN(theCaseRecord.LabRecord.Insulin) and
      isNaN(theCaseRecord.LabRecord.CPeptide) then
      ShowMessage(FILE_EMPTY_MESSAGE);
    caseRecord := theCaseRecord;
    InsertValues(Sender);
    FillFromCaseRecord;
  end;
end;

procedure THauptschirm.RegisterEntry(Sender: TObject);
var
  CheckedIns, CheckedGlc, CheckedCPt: extended;
begin
  InsulinRaw := StrToFloatDef(InsulinEdit.Text, Math.Nan);
  GlucoseRaw := StrToFloatDef(GlucoseEdit.Text, Math.Nan);
  CPeptideRaw := StrToFloatDef(CPeptideEdit.Text, Math.Nan);
  InsulinUoM := InsulinUnitsCombo.Text;
  GlucoseUoM := GlucoseUnitsCombo.Text;
  CPeptideUoM := CPeptideUnitsCombo.Text;
  CheckedIns := InsulinSI(InsulinRaw, InsulinUoM);
  CheckedGlc := GlucoseSI(GlucoseRaw, GlucoseUoM);
  CheckedCPt := CPeptideSI(CPeptideRaw, CPeptideUoM);
  CaseRecord.LabRecord.Insulin := CheckedIns;
  CaseRecord.LabRecord.Glucose := CheckedGlc;
  CaseRecord.LabRecord.CPeptide := CheckedCPt;
end;

procedure THauptschirm.CreateOutput(Sender: TObject);
const
  GapString = ' ' + LineEnding + ' ' + LineEnding;
begin
  CaseRecord.BParMessage := kBPars + LineEnding + '   ' + kGluc +
    ': ' + GlucoseEdit.Text + ' ' + GlucoseUoM + LineEnding + '   ' +
    kIns + ': ' + InsulinEdit.Text + ' ' + InsulinUoM + LineEnding +
    '   ' + kCpt + ': ' + CPeptideEdit.Text + ' ' + CPeptideUoM;
  CaseRecord.SParMessage := kSPars + LineEnding + '   ' + kSPINA_GBeta +
    ': ' + FloatToStrF(CaseRecord.LabRecord.SPINA_GBeta, ffFixed, 4, 2) +
    ' ' + GBetaUoM + LineEnding + '   ' + kSPINA_GR + ': ' +
    FloatToStrF(CaseRecord.LabRecord.SPINA_GR, ffFixed, 4, 2) + ' ' +
    GRUoM + LineEnding + '   ' + kSPINA_DI + ': ' +
    FloatToStrF(CaseRecord.LabRecord.SPINA_DI, ffFixed, 4, 2) +
    LineEnding + '   ' + kHOMA_Beta + ': ' +
    FloatToStrF(CaseRecord.LabRecord.HOMA_Beta, ffFixed, 4, 1) + ' ' +
    HOMABetaUoM + LineEnding + '   ' + kHOMA_IR + ': ' +
    FloatToStrF(CaseRecord.LabRecord.HOMA_IR, ffFixed, 4, 1) + LineEnding +
    '   ' + kHOMA_IS + ': ' + FloatToStrF(CaseRecord.LabRecord.HOMA_IS, ffFixed, 4, 1) +
    LineEnding + '   ' + kQUICKI + ': ' +
    FloatToStrF(CaseRecord.LabRecord.QUICKI, ffFixed, 4, 1) +
    LineEnding + '   ' + kAIGR + ': ' + FloatToStrF(CaseRecord.LabRecord.AIGR,
    ffFixed, 4, 1) + ' ' + AIGRUoM + LineEnding + '   ' + kCGR +
    ': ' + FloatToStrF(CaseRecord.LabRecord.CGR, ffFixed, 4, 1);
  CaseRecord.CombMessage := CaseRecord.BParMessage + LineEnding +
    '       ' + LineEnding + CaseRecord.SParMessage;
  CaseRecord.BRefMessage1 := kRR + LineEnding +
    FloatToStrF(ConvertedValue(gPreferences.ReferenceValues.Glucose.ln,
    kGlucoseMolarMass, gPreferences.ReferenceValues.Glucose.UoM, GlucoseUnitsCombo.Text),
    ffFixed, 4, 1) + '–' + FloatToStrF(
    ConvertedValue(gPreferences.ReferenceValues.Glucose.hn, kGlucoseMolarMass,
    gPreferences.ReferenceValues.Glucose.UoM, GlucoseUnitsCombo.Text), ffFixed, 4, 1) +
    ' ' + GlucoseUnitsCombo.Text + LineEnding + FloatToStrF(
    ConvertedValue(gPreferences.ReferenceValues.Insulin.ln,
    kInsulinConversionFactor, gPreferences.ReferenceValues.Insulin.UoM,
    InsulinUnitsCombo.Text), ffFixed, 4, 1) + '–' +
    FloatToStrF(ConvertedValue(gPreferences.ReferenceValues.Insulin.hn,
    kInsulinConversionFactor, gPreferences.ReferenceValues.Insulin.UoM,
    InsulinUnitsCombo.Text), ffFixed, 4, 1) + ' ' + InsulinUnitsCombo.Text +
    LineEnding + FloatToStrF(ConvertedValue(gPreferences.ReferenceValues.CPeptide.ln,
    kCPeptideMolarMass, gPreferences.ReferenceValues.CPeptide.UoM,
    CPeptideUnitsCombo.Text), ffFixed, 4, 1) + '–' +
    FloatToStrF(ConvertedValue(gPreferences.ReferenceValues.CPeptide.hn,
    kCPeptideMolarMass, gPreferences.ReferenceValues.CPeptide.UoM,
    CPeptideUnitsCombo.Text), ffFixed, 4, 1) + ' ' + CPeptideUnitsCombo.Text;
  CaseRecord.SRefMessage1 :=
    FloatToStrF(gPreferences.ReferenceValues.SPINA_GBeta.ln, ffFixed, 4, 2) +
    '–' + FloatToStrF(gPreferences.ReferenceValues.SPINA_GBeta.hn, ffFixed, 4, 2) +
    ' ' + gPreferences.ReferenceValues.SPINA_GBeta.UoM + LineEnding +
    FloatToStrF(gPreferences.ReferenceValues.SPINA_GR.ln, ffFixed, 4, 2) +
    '–' + FloatToStrF(gPreferences.ReferenceValues.SPINA_GR.hn, ffFixed, 4, 2) +
    ' ' + gPreferences.ReferenceValues.SPINA_GR.UoM + LineEnding +
    FloatToStrF(gPreferences.ReferenceValues.SPINA_DI.ln, ffFixed, 4, 2) +
    '–' + FloatToStrF(gPreferences.ReferenceValues.SPINA_DI.hn, ffFixed, 4, 2) +
    ' ' + gPreferences.ReferenceValues.SPINA_DI.UoM + LineEnding +
    FloatToStrF(gPreferences.ReferenceValues.HOMA_Beta.ln, ffFixed, 4, 1) +
    '–' + FloatToStrF(gPreferences.ReferenceValues.HOMA_Beta.hn, ffFixed, 4, 1) +
    ' ' + gPreferences.ReferenceValues.HOMA_Beta.UoM + LineEnding +
    FloatToStrF(gPreferences.ReferenceValues.HOMA_IR.ln, ffFixed, 4, 1) +
    '–' + FloatToStrF(gPreferences.ReferenceValues.HOMA_IR.hn, ffFixed, 4, 1) +
    ' ' + gPreferences.ReferenceValues.HOMA_IR.UoM + LineEnding +
    FloatToStrF(gPreferences.ReferenceValues.HOMA_IS.ln, ffFixed, 4, 1) +
    '–' + FloatToStrF(gPreferences.ReferenceValues.HOMA_IS.hn, ffFixed, 4, 1) +
    ' ' + gPreferences.ReferenceValues.HOMA_IS.UoM + LineEnding +
    FloatToStrF(gPreferences.ReferenceValues.QUICKI.ln, ffFixed, 4, 1) +
    '–' + FloatToStrF(gPreferences.ReferenceValues.QUICKI.hn, ffFixed, 4, 1) +
    ' ' + gPreferences.ReferenceValues.QUICKI.UoM + LineEnding +
    FloatToStrF(gPreferences.ReferenceValues.AIGR.ln, ffFixed, 4, 1) +
    '–' + FloatToStrF(gPreferences.ReferenceValues.AIGR.hn, ffFixed, 4, 1) +
    ' ' + gPreferences.ReferenceValues.AIGR.UoM + LineEnding + FloatToStrF(
    gPreferences.ReferenceValues.CGR.ln, ffFixed, 4, 1) + '–' +
    FloatToStrF(gPreferences.ReferenceValues.CGR.hn, ffFixed, 4, 1) +
    ' ' + gPreferences.ReferenceValues.CGR.UoM;
  CaseRecord.RCombMessage1 := CaseRecord.BRefMessage1 + LineEnding +
    GapString + CaseRecord.SRefMessage1;
  CaseRecord.RCombMessage2 := WithReferenceRanges(CaseRecord.RCombMessage1);
  CaseRecord.BRefMessage2 := SplitString(CaseRecord.RCombMessage2, GapString)[0];
  CaseRecord.SRefMessage2 := kRR + LineEnding +
    SplitString(CaseRecord.RCombMessage2, GapString)[1];
end;

procedure THauptschirm.AdaptForPlatform;
{ Adapts Menus, Shortcuts and other GUI elements to the interface style
  guidelines of the respective operating system }
var
  modifierKey, modifierKey2: TShiftState;
begin
  {$IFDEF Darwin}
  modifierKey := [ssMeta];
  modifierKey2 := [ssMeta, ssShift];
  WinAboutItem.Visible := False;
  AppleMenu.Visible := True;
  HintsMemo.Font.Height := HintsMemo.Font.Height - 1;
  ResultsMemo.Font.Height := ResultsMemo.Font.Height - 1;
  WinPreferencesMenuItem.Visible := false;
  Divider24.Visible := false;
  {$ELSE}
  modifierKey := [ssCtrl];
  modifierKey2 := [ssCtrl, ssShift];
  WinAboutItem.Visible := True;
  AppleMenu.Visible := False;
  {$ENDIF}
  NewMenuItem.ShortCut := ShortCut(VK_N, modifierKey);
  OpenMenuItem.ShortCut := ShortCut(VK_O, modifierKey);
  CloseMenuItem.ShortCut := ShortCut(VK_W, modifierKey);
  SaveMenuItem.ShortCut := ShortCut(VK_S, modifierKey);
  CaseDataMenuItem.ShortCut := ShortCut(VK_C, modifierKey2);
  ;
  PrintMenuItem.ShortCut := ShortCut(VK_P, modifierKey);
  QuitMenuItem.ShortCut := ShortCut(VK_Q, modifierKey);
  UndoMenuItem.ShortCut := ShortCut(VK_Z, modifierKey);
  RedoMenuItem.ShortCut := ShortCut(VK_Z, modifierKey2);
  CutMenuItem.ShortCut := ShortCut(VK_X, modifierKey);
  CopyMenuItem.ShortCut := ShortCut(VK_C, modifierKey);
  CopyResultMenuItem.ShortCut := ShortCut(VK_R, modifierKey);
  PasteMenuItem.ShortCut := ShortCut(VK_V, modifierKey);
  SelectAllMenuItem.ShortCut := ShortCut(VK_A, modifierKey);
end;

end.
