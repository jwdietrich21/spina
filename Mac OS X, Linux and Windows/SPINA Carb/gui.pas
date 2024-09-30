unit GUI;

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

{ GUI }

{ Source code released under the BSD License }
{ See http://spina.medical-cybernetics.de for details }

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  Spin, Menus, ActnList, StdActns, Math, LCLType, EnvironmentInfo,
  SPINATypes, CaseBroker, SPINA_GUIServices, ResultWindow;

type

  { THauptschirm }

  THauptschirm = class(TForm)
    ActionList1: TActionList;
    AppleMenu: TMenuItem;
    CalculateButton: TButton;
    CloseMenuItem: TMenuItem;
    CopyMenuItem: TMenuItem;
    CutMenuItem: TMenuItem;
    Divider11: TMenuItem;
    Divider12: TMenuItem;
    Divider21: TMenuItem;
    Divider22: TMenuItem;
    EditCopy1: TEditCopy;
    EditMenu: TMenuItem;
    EditSelectAll1: TEditSelectAll;
    FileMenu: TMenuItem;
    GlucoseUnitsCombo: TComboBox;
    GlucoseEdit: TEdit;
    EntryBox: TGroupBox;
    FeedbackImage: TImage;
    HelpMenu: TMenuItem;
    ImageList1: TImageList;
    CPeptideLabel: TLabel;
    CPeptideEdit: TEdit;
    CPeptideUnitsCombo: TComboBox;
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
    LogoPanel: TPanel;
    SaveMenuItem: TMenuItem;
    SelectAllMenuItem: TMenuItem;
    UndoMenuItem: TMenuItem;
    WinAboutItem: TMenuItem;
    procedure CalculateButtonClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure LogoImageClick(Sender: TObject);
    procedure MacAboutItemClick(Sender: TObject);
    procedure QuitMenuItemClick(Sender: TObject);
    procedure WinAboutItemClick(Sender: TObject);
  private

  public
    CaseRecord: tCaseRecord;
    InsulinRaw, GlucoseRaw, CPeptideRaw: extended;
    InsulinUoM, GlucoseUoM, CPeptideUoM: String;
    OutputB, OutputS, OutputC: string;
    procedure RegisterEntry(Sender: TObject);
    procedure CreateOutput(Sender: TObject);
    procedure AdaptForPlatform;
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
  ResultsMemo.Text := LineEnding + OutputC;
  ResultForm.ShowResults(OutputB, OutputS);
  ResultForm.Visible := true;
  ResultForm.ShowOnTop;
end;

procedure THauptschirm.FormActivate(Sender: TObject);
begin
  if DarkTheme then
    Color := clDefault
  else
    Color := clWhite;
  ActiveControl := GlucoseEdit;
end;

procedure THauptschirm.FormCreate(Sender: TObject);
begin
  AdaptForPlatform;
  SPINACarbLabel.Caption := 'SPINA Carb ' + FileVersion;
  ActiveControl := GlucoseEdit;
end;

procedure THauptschirm.FormShow(Sender: TObject);
begin
  if DarkTheme then
    Color := clDefault
  else
    Color := clWhite;
  ActiveControl := GlucoseEdit;
end;

procedure THauptschirm.LogoImageClick(Sender: TObject);
begin
  MacAboutItemClick(Sender);
end;

procedure THauptschirm.MacAboutItemClick(Sender: TObject);
begin
  ShowMessage('SPINA Carb – Version 5.1.0 Beta 1');
end;

procedure THauptschirm.QuitMenuItemClick(Sender: TObject);
begin
  application.Terminate;
end;

procedure THauptschirm.WinAboutItemClick(Sender: TObject);
begin
  MacAboutItemClick(Sender);
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
begin
  OutputB := kBPars +
             LineEnding +
             '   ' + kGluc + ': ' +
             GlucoseEdit.Text + ' ' + GlucoseUoM +
             LineEnding +
             '   ' + kIns + ': ' +
             InsulinEdit.Text + ' ' + InsulinUoM;
  OutputS := kSPars +
             LineEnding +
             '   ' + kSPINA_GBeta + ': ' +
             FloatToStrF(CaseRecord.LabRecord.SPINA_GBeta, ffFixed, 4, 2) +
             ' ' + GBetaUoM +
             LineEnding +
             '   ' + kSPINA_GR + ': ' +
             FloatToStrF(CaseRecord.LabRecord.SPINA_GR, ffFixed, 4, 2) +
             ' ' + GRUoM +
             LineEnding +
             '   ' + kSPINA_DI + ': ' +
             FloatToStrF(CaseRecord.LabRecord.SPINA_DI, ffFixed, 4, 2) +
             LineEnding +
             '   ' + kHOMA_Beta + ': ' +
             FloatToStrF(CaseRecord.LabRecord.HOMA_Beta, ffFixed, 4, 1) +
             ' ' + HOMABetaUoM +
             LineEnding +
             '   ' + kHOMA_IR + ': ' +
             FloatToStrF(CaseRecord.LabRecord.HOMA_IR, ffFixed, 4, 1) +
             LineEnding +
             '   ' + kHOMA_IS + ': ' +
             FloatToStrF(CaseRecord.LabRecord.HOMA_IS, ffFixed, 4, 1) +
             LineEnding +
             '   ' + kQUICKI + ': ' +
             FloatToStrF(CaseRecord.LabRecord.QUICKI, ffFixed, 4, 1);;
  OutputC := OutputB +
             LineEnding + LineEnding +
             OutputS;
end;

procedure THauptschirm.AdaptForPlatform;
{ Adapts Menus, Shortcuts and other GUI elements to the interface style
  guidelines of the respective operating system }
var
  modifierKey: TShiftState;
begin
  {$IFDEF LCLcarbon}
  modifierKey := [ssMeta];
  WinAboutItem.Visible := False;
  AppleMenu.Visible := True;
  HintsMemo.Font.Height := HintsMemo.Font.Height - 1;
  ResultsMemo.Font.Height := ResultsMemo.Font.Height - 1;
  {$ELSE}
  {$IFDEF LCLCocoa}
  modifierKey := [ssMeta];
  WinAboutItem.Visible := False;
  AppleMenu.Visible := True;
  HintsMemo.Font.Height := HintsMemo.Font.Height - 1;
  ResultsMemo.Font.Height := ResultsMemo.Font.Height - 1;
  {$ELSE}
  modifierKey := [ssCtrl];
  WinAboutItem.Visible := True;
  AppleMenu.Visible := False;
  {$ENDIF}
  {$ENDIF}
  NewMenuItem.ShortCut := ShortCut(VK_N, modifierKey);
  OpenMenuItem.ShortCut := ShortCut(VK_O, modifierKey);
  CloseMenuItem.ShortCut := ShortCut(VK_W, modifierKey);
  SaveMenuItem.ShortCut := ShortCut(VK_S, modifierKey);
  QuitMenuItem.ShortCut := ShortCut(VK_Q, modifierKey);
  UndoMenuItem.ShortCut := ShortCut(VK_Z, modifierKey);
  RedoMenuItem.ShortCut := ShortCut(VK_Z, modifierKey + [ssShift]);
  CutMenuItem.ShortCut := ShortCut(VK_X, modifierKey);
  CopyMenuItem.ShortCut := ShortCut(VK_C, modifierKey);
  PasteMenuItem.ShortCut := ShortCut(VK_V, modifierKey);
  SelectAllMenuItem.ShortCut := ShortCut(VK_A, modifierKey);
end;

end.

