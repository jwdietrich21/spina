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

{$UNDEF BetaVersion} // Switch on for warnings on beta version

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  Menus, ActnList, StdActns, Math, LCLType, ComCtrls, StrUtils,
  EditBtn, Clipbrd,
  EnvironmentInfo, SPINATypes, SPINA_Resources, CaseBroker, SPINA_GUIServices,
  ResultWindow, SPINA_Aboutbox, Printers, PrintersDlgs, PrintCase,
  SetPreferences, UnitConverter, SPINA_Engine, HandleImpEx, LocaleServices, Types;

const
  MAIN_FORM_TITLE = 'SPINA Carb';

type

  { THauptschirm }

  THauptschirm = class(TForm)
    ActionList1: TActionList;
    AppleMenu: TMenuItem;
    Bevel1: TBevel;
    Bevel2: TBevel;
    CaseDataMenuItem: TMenuItem;
    Divider01: TMenuItem;
    CommentBox: TGroupBox;
    MacPreferencesMenuItem: TMenuItem;
    Divider23: TMenuItem;
    CopyResultMenuItem: TMenuItem;
    CommentMemo: TMemo;
    CopyFormattedMenuItem: TMenuItem;
    OKButton: TButton;
    OpenCaseDialog: TOpenDialog;
    SaveResultsDialog: TSaveDialog;
    CommentSheet: TTabSheet;
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
    procedure CommentSheetShow(Sender: TObject);
    procedure CopyFormattedMenuItemClick(Sender: TObject);
    procedure CopyResultMenuItemClick(Sender: TObject);
    procedure EntrySheetShow(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDropFiles(Sender: TObject; const FileNames: array of string);
    procedure FormPaint(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure LogoImageClick(Sender: TObject);
    procedure MacAboutItemClick(Sender: TObject);
    procedure MacPreferencesMenuItemClick(Sender: TObject);
    procedure MainPageControlChange(Sender: TObject);
    procedure NextButtonClick(Sender: TObject);
    procedure OKButtonClick(Sender: TObject);
    procedure OpenMenuItemClick(Sender: TObject);
    procedure PageSetupMenuItemClick(Sender: TObject);
    procedure PrintMenuItemClick(Sender: TObject);
    procedure QuitMenuItemClick(Sender: TObject);
    procedure ResultsMemoChange(Sender: TObject);
    procedure SaveMenuItemClick(Sender: TObject);
    procedure SPINACarbLabelClick(Sender: TObject);
    procedure WinAboutItemClick(Sender: TObject);
    procedure WinPreferencesMenuItemClick(Sender: TObject);
    procedure OpenFileList(Sender: TObject; const FileNames: array of string);
  private
    function DoPageSetup: boolean;
    function DoPrintSetup: boolean;
    procedure InsertValues(Sender: TObject);
    procedure FillFromCaseRecord;
    procedure MarkMandatoryFields(Sender: TObject);
  public
    CaseRecord: tCaseRecord;
    InsulinRaw, GlucoseRaw, CPeptideRaw: extended;
    InsulinUoM, GlucoseUoM, CPeptideUoM: string;
    gPreferencesHint: string;
    procedure RegisterUoMs(Sender: TObject);
    procedure RegisterEntry(Sender: TObject);
    procedure CreateOutput(Sender: TObject);
    procedure DisplayResults(Sender: TObject);
    procedure AdaptForPlatform;
    procedure AdapttoTheme(Sender: TObject);
    procedure FocusEdit(Sender: TObject);
    procedure RegisterCaseData(Sender: TObject);
    procedure SaveResults(Sender: TObject);
    procedure ReadCaseRecord(Sender: TObject; const theCaseRecord: tCaseRecord);
    procedure UpdateUnits(Sender: TObject);
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
  DisplayResults(Sender);
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

procedure THauptschirm.CommentSheetShow(Sender: TObject);
begin
  CommentMemo.Lines.Text := CaseRecord.Comment;
  OKButton.Default := True;
  ActiveControl := CommentMemo;
end;

procedure THauptschirm.CopyFormattedMenuItemClick(Sender: TObject);
var
  Clipboard: TClipboard;
begin
  Clipboard := TClipboard.Create();
  Clipboard.AsText := FormattedResults(CaseRecord.LabRecord);
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
  MarkMandatoryFields(Sender);
  PlacerEdit.Text := gPreferences.Placer_ID;
  FocusEdit(Sender);
end;

procedure THauptschirm.FormCreate(Sender: TObject);
begin
  AdaptForPlatform;
  {$IFDEF BetaVersion}
  Caption := MAIN_FORM_TITLE + ' (Beta Version)';
  SPINACarbLabel.Caption := 'SPINA Carb Beta Version for Testing';
  {$ELSE}
  Caption := MAIN_FORM_TITLE;
  SPINACarbLabel.Caption := 'SPINA Carb ' + FileVersion;
  {$ENDIF}
  if gPreferredLanguage = 'de' then
  begin
    gPreferencesHint := kPreferencesHint_de;
    CaseEditorSheet.Caption := kGUILabels_de.caseTab;
    EntrySheet.Caption := kGUILabels_de.EntryTab;
    CommentSheet.Caption := kGUILabels_de.CommentTab;
    CaseIDLabel.Caption := kGUILabels_de.ctCaseID;
    PIDLabel.Caption := kGUILabels_de.ctPID;
    NameLabel.Caption := kGUILabels_de.ctName;
    GivenNameLabel.Caption := kGUILabels_de.ctGivenName;
    DOBLabel.Caption := kGUILabels_de.ctDOB;
    PlacerLabel.Caption := kGUILabels_de.ctPlacer;
    OBDateLabel.Caption := kGUILabels_de.ctOBD;
    NextButton.Caption := kGUILabels_de.ctNext;

    HintBox.Caption := kGUILabels_de.enHintbox;
    EntryBox.Caption := kGUILabels_de.enBParBox;
    ResultsMemo.Caption := kGUILabels_de.enResults;
    GlucoseLabel.Caption := kGUILabels_de.enGlucose;
    InsulinLabel.Caption := kGUILabels_de.enInsulin;
    CPeptideLabel.Caption := kGUILabels_de.enCPep;
    CalculateButton.Caption := kGUILabels_de.enCalculate;

    HintsMemo.Lines.Clear;
    HintsMemo.Lines.Add(kGUILabels_de.enHintText);

    CommentBox.Caption := kGUILabels_de.coCommentBox;

    FileMenu.Caption := kMenuCaptions_de.FileMenu;
    EditMenu.Caption := kMenuCaptions_de.EditMenu;
    HelpMenu.Caption := kMenuCaptions_de.HelpMenu;

    MacAboutItem.Caption := kMenuCaptions_de.AboutItem;
    MacPreferencesMenuItem.Caption := kMenuCaptions_de.SettingsItem;

    NewMenuItem.Caption := kMenuCaptions_de.NewItem;
    OpenMenuItem.Caption := kMenuCaptions_de.OpenItem;
    SaveMenuItem.Caption := kMenuCaptions_de.SaveItem;
    CloseMenuItem.Caption := kMenuCaptions_de.CloseItem;
    CaseDataMenuItem.Caption := kMenuCaptions_de.CaseDataItem;
    PageSetupMenuItem.Caption := kMenuCaptions_de.PageSetupItem;
    PrintMenuItem.Caption := kMenuCaptions_de.PrintItem;
    QuitMenuItem.Caption := kMenuCaptions_de.QuitItem;

    UndoMenuItem.Caption := kMenuCaptions_de.UndoItem;
    RedoMenuItem.Caption := kMenuCaptions_de.RedoItem;
    CutMenuItem.Caption := kMenuCaptions_de.CutItem;
    CopyMenuItem.Caption := kMenuCaptions_de.CopyItem;
    PasteMenuItem.Caption := kMenuCaptions_de.PasteItem;
    SelectAllMenuItem.Caption := kMenuCaptions_de.SelectAllItem;
    CopyResultMenuItem.Caption := kMenuCaptions_de.CopyResultItem;
    CopyFormattedMenuItem.Caption := kMenuCaptions_de.CopyFormattedItem;
    WinPreferencesMenuItem.Caption := kMenuCaptions_de.SettingsItem;

    WinAboutItem.Caption := kMenuCaptions_de.AboutItem;
  end
  else
    gPreferencesHint := kPreferencesHint_en;
  MainPageControl.ActivePage := EntrySheet;
  FocusEdit(Sender);
end;

procedure THauptschirm.FormDropFiles(Sender: TObject; const FileNames: array of string);
begin
  OpenFileList(Sender, FileNames);
end;

procedure THauptschirm.FormPaint(Sender: TObject);
begin
  AdapttoTheme(Sender);
  MarkMandatoryFields(Sender);
end;

procedure THauptschirm.FormShow(Sender: TObject);
var
  i: integer;
  FileNames: array of string;
begin
  { opens files that have been dropped onto the icon before starting: }
  SetLength(FileNames, ParamCount);
  for i := 1 to ParamCount do
    FileNames[i - 1] := ParamStr(i);
  if ParamCount > 0 then
    OpenFileList(Sender, FileNames);

  AdaptToTheme(Sender);
  if gPreferences.new then
  {$IFDEF BetaVersion}
    ShowMessage(kBetaHint);
  {$ELSE}
    ShowMessage(gPreferencesHint);
  {$ENDIF}
  MarkMandatoryFields(Sender);
  FocusEdit(Sender);

  DoBEdit.DateFormat := DefaultFormatSettings.ShortDateFormat;
  ObDateEdit.DateFormat := DefaultFormatSettings.ShortDateFormat;
end;

procedure THauptschirm.LogoImageClick(Sender: TObject);
begin
  MacAboutItemClick(Sender);
end;

procedure THauptschirm.MacAboutItemClick(Sender: TObject);
begin
  AboutBox.ShowAbout;
end;

procedure THauptschirm.MacPreferencesMenuItemClick(Sender: TObject);
begin
  PreferencesForm.ShowModal;
  UpdateUnits(Sender);
end;

procedure THauptschirm.MainPageControlChange(Sender: TObject);
begin

end;

procedure THauptschirm.NextButtonClick(Sender: TObject);
begin
  RegisterCaseData(Sender);
  MainPageControl.ActivePage := EntrySheet;
end;

procedure THauptschirm.OKButtonClick(Sender: TObject);
begin
  CaseRecord.Comment := CommentMemo.Lines.Text;
  MainPageControl.ActivePage := EntrySheet;
end;

procedure THauptschirm.OpenMenuItemClick(Sender: TObject);
{ Open case from file }
var
  theFilterIndex: integer;
  theCaseRecord: tCaseRecord;
begin
  theCaseRecord.CaseID := '';
  NewCaseRecord(theCaseRecord);
  if OpenCaseDialog.Execute then
  begin
    theFilterIndex := OpenCaseDialog.FilterIndex;
    case theFilterIndex of
      1: OpenCaseRecord(theCaseRecord, OpenCaseDialog.FileName, HL7Message);
    end;
    ReadCaseRecord(Sender, theCaseRecord);
    DisplayResults(Sender);
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
    PrintCaseRecord(CaseRecord, gPreferredLanguage);
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

procedure THauptschirm.OpenFileList(Sender: TObject; const FileNames: array of string);
{ Open case from file via drag and drop }
var
  theCaseRecord: tCaseRecord;
  j: integer;
  thePath: string;
begin
  theCaseRecord.CaseID := '';
  NewCaseRecord(theCaseRecord);
  thePath := FileNames[0];
  j := pos('://', thePath);
  if DirectoryExists(thePath) then
    ShowMessage(FOLDER_NOT_SUPPORTED_MESSAGE)
  else if ((j > 0) and (j < 6)) then
    ShowMessage(URLS_NOT_SUPPORTED_MESSAGE)
  else if FileExists(thePath) then
  begin
    OpenCaseRecord(theCaseRecord, thePath, HL7Message);
    ReadCaseRecord(Sender, theCaseRecord);
    DisplayResults(Sender);
  end;
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
{ Inserts values of case record into appropriate fields of the main tab }
var
  correctedConvFac: real;
begin
  if not isNaN(caseRecord.LabRecord.Glucose) then
  begin
    correctedConvFac := 1e9 / kInsulinConversionFactor;
    GlucoseEdit.Text := FloatToStrF(ConvertedValue(caseRecord.LabRecord.Glucose,
      kGlucoseMolarMass, kEngineUoMs.Glucose, GlucoseUnitsCombo.Caption), ffFixed, 4, 1);
    InsulinEdit.Text := FloatToStrF(ConvertedValue(caseRecord.LabRecord.Insulin,
      correctedConvFac, kEngineUoMs.Insulin, InsulinUnitsCombo.Caption),
      ffFixed, 4, 1);
    CPeptideEdit.Text := FloatToStrF(ConvertedValue(caseRecord.LabRecord.CPeptide,
      kCPeptideMolarMass, kEngineUoMs.CPeptide, CPeptideUnitsCombo.Caption),
      ffFixed, 4, 1);
  end;
end;

procedure THauptschirm.FillFromCaseRecord;
{ Fill auxiliary fields with data from case record }
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
  CommentMemo.Lines.Text := CaseRecord.Comment;
end;

procedure THauptschirm.MarkMandatoryFields(Sender: TObject);
var
  MandatoryColor: TColor;
begin
  {$IFDEF LCLCocoa}
  MandatoryColor := clDefault;
  {$ELSE}
  MandatoryColor := gPreferences.MandatoryColor;
  {$ENDIF}
  if gPreferences.colouriseMandatoryFields then
  begin {should mandatory fields be colourised?}
    Hauptschirm.GlucoseEdit.Color := MandatoryColor;
    Hauptschirm.InsulinEdit.Color := MandatoryColor;
    Hauptschirm.CPeptideEdit.Color := MandatoryColor;
  end
  else
  begin
    Hauptschirm.GlucoseEdit.Color := clDefault;
    Hauptschirm.InsulinEdit.Color := clDefault;
    Hauptschirm.CPeptideEdit.Color := clDefault;
  end;
end;

procedure THauptschirm.AdapttoTheme(Sender: TObject);
begin
  if DarkTheme then
  begin
    {$IFDEF DARWIN}
    if AboutBox.finished then
      AboutBox.ImageContainer1.GetBitmap(1, LogoImage.Picture.Bitmap);
    {$ENDIF}
    Color := clDefault;
    EntryBox.Color := clDefault;
    ResultsBox.Color := clDefault;
    HintBox.Color := clDefault;
    LogoBox.Color := clDefault;
  end
  else
  begin
    {$IFDEF DARWIN}
    if AboutBox.finished then
      AboutBox.ImageContainer1.GetBitmap(0, LogoImage.Picture.Bitmap);
    {$ENDIF}
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
    if IsNaN(caseRecord.DoBDate) then
      DOBDateStr := ''
    else
      DOBDateStr := DateToStr(caseRecord.DoBDate);
    if IsNaN(caseRecord.OBDate) then
      OBDateStr := ''
    else
      OBDateStr := DateToStr(caseRecord.OBDate);
    Caption := MAIN_FORM_TITLE + ': ' + caseRecord.Name + ', ' +
      caseRecord.GivenNames + ', * ' + DOBDateStr + ' | ' + OBDateStr;
  end;
end;

procedure THauptschirm.UpdateUnits(Sender: TObject);
begin
  GlucoseUnitsCombo.Caption := gPreferences.PreferredUoMs.Glucose;
  InsulinUnitsCombo.Caption := gPreferences.PreferredUoMs.Insulin;
  CPeptideUnitsCombo.Caption := gPreferences.PreferredUoMs.CPeptide;
end;

procedure THauptschirm.RegisterUoMs(Sender: TObject);
begin
  InsulinUoM := InsulinUnitsCombo.Text;
  GlucoseUoM := GlucoseUnitsCombo.Text;
  CPeptideUoM := CPeptideUnitsCombo.Text;
end;

procedure THauptschirm.RegisterEntry(Sender: TObject);
var
  CheckedIns, CheckedGlc, CheckedCPt: extended;
begin
  InsulinRaw := StrToFloatDefL(InsulinEdit.Text, Math.Nan);
  GlucoseRaw := StrToFloatDefL(GlucoseEdit.Text, Math.Nan);
  CPeptideRaw := StrToFloatDefL(CPeptideEdit.Text, Math.Nan);
  RegisterUoMs(Sender);
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
var
  BPars, Gluc, Ins, CPt, RR: string;
  correctedConvFac: real;
begin
  correctedConvFac := 1e9 / kInsulinConversionFactor;
  if gPreferredLanguage = 'de' then
  begin
    BPars := KBPars_de;
    Gluc := kGluc_de;
    Ins := kIns_de;
    Cpt := kCpt_de;
    RR := kRR_de;
  end
  else
  begin
    BPars := KBPars_en;
    Gluc := kGluc_en;
    Ins := kIns_en;
    Cpt := kCpt_en;
    RR := kRR_en;
  end;
  RegisterUoMs(Sender);
  CreateMessages(CaseRecord);
  CaseRecord.BParMessage := BPars + LineEnding + '   ' + Gluc + ': ' +
    MarkedC(CaseRecord.LabRecord.Glucose, gPreferences.ReferenceValues.Glucose,
    kGlucoseMolarMass, kEngineUoMs.Glucose, GlucoseUoM, 4, 1) + ' ' +
    GlucoseUoM + LineEnding + '   ' + Ins + ': ' +
    MarkedC(CaseRecord.LabRecord.Insulin, gPreferences.ReferenceValues.Insulin,
    correctedConvFac, kEngineUoMs.Insulin, InsulinUnitsCombo.Text, 4, 1) +
    ' ' + InsulinUoM + LineEnding + '   ' + Cpt + ': ' +
    MarkedC(CaseRecord.LabRecord.CPeptide, gPreferences.ReferenceValues.CPeptide,
    kCPeptideMolarMass, kEngineUoMs.CPeptide, CPeptideUnitsCombo.Text, 4, 1) +
    ' ' + CPeptideUoM;
  CaseRecord.CombMessage := CaseRecord.BParMessage + LineEnding +
    '       ' + LineEnding + CaseRecord.SParMessage;
  CaseRecord.BRefMessage1 := RR + LineEnding +
    FloatToStrF(ConvertedValue(gPreferences.ReferenceValues.Glucose.ln,
    kGlucoseMolarMass, gPreferences.ReferenceValues.Glucose.UoM, GlucoseUoM),
    ffFixed, 4, 1) + '–' + FloatToStrF(
    ConvertedValue(gPreferences.ReferenceValues.Glucose.hn, kGlucoseMolarMass,
    gPreferences.ReferenceValues.Glucose.UoM, GlucoseUoM), ffFixed, 4, 1) +
    ' ' + GlucoseUoM + LineEnding + FloatToStrF(
    ConvertedValue(gPreferences.ReferenceValues.Insulin.ln, correctedConvFac,
    gPreferences.ReferenceValues.Insulin.UoM, InsulinUnitsCombo.Text), ffFixed, 4, 1) +
    '–' + FloatToStrF(ConvertedValue(gPreferences.ReferenceValues.Insulin.hn,
    correctedConvFac, gPreferences.ReferenceValues.Insulin.UoM,
    InsulinUnitsCombo.Text), ffFixed, 4, 1) + ' ' + InsulinUnitsCombo.Text +
    LineEnding + FloatToStrF(ConvertedValue(gPreferences.ReferenceValues.CPeptide.ln,
    kCPeptideMolarMass, gPreferences.ReferenceValues.CPeptide.UoM,
    CPeptideUnitsCombo.Text), ffFixed, 4, 1) + '–' +
    FloatToStrF(ConvertedValue(gPreferences.ReferenceValues.CPeptide.hn,
    kCPeptideMolarMass, gPreferences.ReferenceValues.CPeptide.UoM,
    CPeptideUnitsCombo.Text), ffFixed, 4, 1) + ' ' + CPeptideUnitsCombo.Text;
  CaseRecord.RCombMessage1 := CaseRecord.BRefMessage1 + LineEnding +
    GapString + CaseRecord.SRefMessage1;
  CaseRecord.RCombMessage2 := WithReferenceRanges(CaseRecord.RCombMessage1);
  CaseRecord.BRefMessage2 := SplitString(CaseRecord.RCombMessage2, GapString)[0];
  CaseRecord.SRefMessage2 := RR + LineEnding +
    SplitString(CaseRecord.RCombMessage2, GapString)[1];
end;

procedure THauptschirm.DisplayResults(Sender: TObject);
begin
  CreateOutput(Sender);
  ResultsMemo.Text := LineEnding + CaseRecord.CombMessage;
  ResultsMemo.Hint := CaseRecord.RCombMessage2;
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
  CopyFormattedMenuItem.ShortCut := ShortCut(VK_K, modifierKey);
  PasteMenuItem.ShortCut := ShortCut(VK_V, modifierKey);
  SelectAllMenuItem.ShortCut := ShortCut(VK_A, modifierKey);
end;

end.
