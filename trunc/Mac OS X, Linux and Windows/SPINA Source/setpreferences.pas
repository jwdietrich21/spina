unit SetPreferences;

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

{ This unit draws a dialog box for setting global application preferences }

{ Source code released under the BSD License }
{ See http://spina.medical-cybernetics.de for details }

{$mode objfpc}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ComCtrls, ExtCtrls, Grids, Math, SPINA_Types, SPINA_Resources,
  CDISC, HandlePreferences;

type

  { TPreferencesForm }

  TPreferencesForm = class(TForm)
    SaveCDISCButton: TButton;
    CancelButton: TButton;
    CDISCGroupBox: TGroupBox;
    CDISCOpenDialog: TOpenDialog;
    Dashlabel5: TLabel;
    Dashlabel6: TLabel;
    Dashlabel4: TLabel;
    Dashlabel3: TLabel;
    Dashlabel2: TLabel;
    Dashlabel7: TLabel;
    GDRRHEdit: TEdit;
    GDUnitLabel: TLabel;
    GTRRHEdit: TEdit;
    Dashlabel1: TLabel;
    GTUnitLabel: TLabel;
    CDISCSaveDialog: TSaveDialog;
    MandatoryFieldsGrid: TStringGrid;
    T3UnitLabel: TLabel;
    T4UnitLabel: TLabel;
    TSHIRRHEdit: TEdit;
    TSHUnitLabel: TLabel;
    TTSIRRHEdit: TEdit;
    TSHIRRLEdit: TEdit;
    TTSIRRLEdit: TEdit;
    TTSIRRLabel: TLabel;
    TSHIRRLabel: TLabel;
    T3RRHEdit: TEdit;
    T4RRHEdit: TEdit;
    TSHRRHEdit: TEdit;
    IDGroupBox: TGroupBox;
    GUIGroupBox: TGroupBox;
    SendingFacLabel: TLabel;
    MarkMandatoryCheck: TCheckBox;
    GDRRLEdit: TEdit;
    GDRRLabel: TLabel;
    GTRRLEdit: TEdit;
    GTRRLabel: TLabel;
    MethodLabel: TLabel;
    SendingFacEdit: TEdit;
    OKButton: TButton;
    PageControl1: TPageControl;
    ReadCDISCButton: TButton;
    RememberCheckBox: TCheckBox;
    T3Label: TLabel;
    T3MethodComboBox: TComboBox;
    T3RRLEDit: TEdit;
    T3RRLabel: TLabel;
    T3UnitComboBox: TComboBox;
    T4Label: TLabel;
    T4MethodComboBox: TComboBox;
    T4RRLEdit: TEdit;
    T4RRLabel: TLabel;
    T4UnitComboBox: TComboBox;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    TabSheet3: TTabSheet;
    TSHLabel: TLabel;
    TSHRRLEdit: TEdit;
    TSHRRLabel: TLabel;
    TSHUnitComboBox: TComboBox;
    UnitLabel: TLabel;
    UnitsGroupBox: TGroupBox;
    procedure AdjustCombos(Sender: TObject);
    procedure DisplayReferenceRanges(Sender: TObject);
    procedure AdjustMethods(Sender: TObject; T4Method, T3Method: tLabMethod);
    procedure CancelButtonClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure GDRRHEditChange(Sender: TObject);
    procedure GDRRLEditChange(Sender: TObject);
    procedure GTRRHEditChange(Sender: TObject);
    procedure GTRRLEditChange(Sender: TObject);
    procedure MarkMandatoryCheckChange(Sender: TObject);
    procedure GetMethodsAndUnits(const Sender: TObject);
    procedure MethodComboBoxChange(Sender: TObject);
    procedure OKButtonClick(Sender: TObject);
    procedure ReadCDISCButtonClick(Sender: TObject);
    procedure RememberCheckBoxChange(Sender: TObject);
    procedure SaveCDISCButtonClick(Sender: TObject);
    procedure SaveEditedReferenceRanges(theFile: String);
    procedure T3MethodComboBoxAdjust(Sender: TObject);
    procedure T3MethodComboBoxChange(Sender: TObject);
    procedure T3RRHEditChange(Sender: TObject);
    procedure T3RRLEDitChange(Sender: TObject);
    procedure T3UnitComboBoxChange(Sender: TObject);
    procedure T4MethodComboBoxAdjust(Sender: TObject);
    procedure T4MethodComboBoxChange(Sender: TObject);
    procedure T4RRHEditChange(Sender: TObject);
    procedure T4RRLEditChange(Sender: TObject);
    procedure T4UnitComboBoxChange(Sender: TObject);
    procedure TSHIRRHEditChange(Sender: TObject);
    procedure TSHIRRLEditChange(Sender: TObject);
    procedure TSHRRHEditChange(Sender: TObject);
    procedure TSHRRLEditChange(Sender: TObject);
    procedure TSHUnitComboBoxChange(Sender: TObject);
    procedure TTSIRRHEditChange(Sender: TObject);
    procedure TTSIRRLEditChange(Sender: TObject);
    procedure UpdateDisplay(Sender: TObject);
  private
    { private declarations }
    procedure ReadCDISCFile(Sender: TObject; thePath: String);
  public
    { public declarations }
    Edited: boolean;
  end;

var
  PreferencesForm: TPreferencesForm;
  gSavedReferenceRanges, gSavedSIReferenceRanges, gSavedConvReferenceRanges: tReferenceValues;
  gCode: integer;

procedure DisplayPreferencesDlg;

implementation

uses
  SPINA_UserInterface;

procedure TPreferencesForm.UpdateDisplay(Sender: TObject);
{I18n: Adapts labels etc. to system language}
begin
  if gInterfaceLanguage = German then
    begin
      MethodLabel.Caption := kMethodLabel1;
      UnitLabel.Caption := kUnitLabel1;
      UnitsGroupBox.Caption := kUnitsGroupCaption1;
      PreferencesForm.Caption := kPreferences1;
      RememberCheckBox.Caption := kRemember1;
      CDISCGroupBox.Caption := kCDISCCaption1;
      CancelButton.Caption := kCancel1;
      ReadCDISCButton.Caption := kReadCDISC1;
      SaveCDISCButton.Caption := kSaveCDISC1;
      MarkMandatoryCheck.Caption := kPflichtfelder1;
      TabSheet1.Caption := kPrefsCaption11;
      TabSheet2.Caption := kPrefsCaption21;
      TabSheet3.Caption := kPrefsCaption31;
      IDGroupBox.Caption := kGroupCaption31;
    end
  else
    begin
      MethodLabel.Caption := kMethodLabel2;
      UnitLabel.Caption := kUnitLabel2;
      UnitsGroupBox.Caption := kUnitsGroupCaption2;
      PreferencesForm.Caption := kPreferences2;
      RememberCheckBox.Caption := kRemember2;
      CDISCGroupBox.Caption := kCDISCCaption2;
      CancelButton.Caption := kCancel2;
      ReadCDISCButton.Caption := kReadCDISC2;
      SaveCDISCButton.Caption := kSaveCDISC2;
      RememberCheckBox.Hint := 'Please click to reuse measurement units of previous calculations';
      ReadCDISCButton.Hint := 'Import reference ranges from file';
      MarkMandatoryCheck.Caption := kPflichtfelder2;
      TabSheet1.Caption := kPrefsCaption12;
      TabSheet2.Caption := kPrefsCaption22;
      TabSheet3.Caption := kPrefsCaption32;
      IDGroupBox.Caption := kGroupCaption32;
    end;
end;

procedure TPreferencesForm.DisplayReferenceRanges(Sender: TObject);
{Displays reference ranges and measurement units}
begin
  TSHRRLEdit.Text := FloatToStrF(gReferenceRanges.TSH.ln, ffFixed, 5, 2);
  TSHRRHEdit.Text := FloatToStrF(gReferenceRanges.TSH.hn, ffFixed, 5, 2);
  if gPreferences.T4.Method = freeHormone then
    begin
      T4RRLEdit.Text := FloatToStrF(gReferenceRanges.FT4.ln, ffFixed, 5, 2);
      T4RRHEdit.Text := FloatToStrF(gReferenceRanges.FT4.hn, ffFixed, 5, 2);
    end
  else
    begin
      T4RRLEdit.Text := FloatToStrF(gReferenceRanges.TT4.ln, ffFixed, 5, 2);
      T4RRHEdit.Text := FloatToStrF(gReferenceRanges.TT4.hn, ffFixed, 5, 2);
    end;
  if gPreferences.T3.Method = freeHormone then
    begin
      T3RRLEDit.Text := FloatToStrF(gReferenceRanges.FT3.ln, ffFixed, 5, 2);
      T3RRHEDit.Text := FloatToStrF(gReferenceRanges.FT3.hn, ffFixed, 5, 2);
    end
  else
    begin
      T3RRLEDit.Text := FloatToStrF(gReferenceRanges.TT3.ln, ffFixed, 5, 2);
      T3RRHEDit.Text := FloatToStrF(gReferenceRanges.TT3.hn, ffFixed, 5, 2);
    end;
  GTRRLEdit.Text := FloatToStrF(gReferenceRanges.GT.ln * 1e12, ffFixed, 5, 2);
  GTRRHEdit.Text := FloatToStrF(gReferenceRanges.GT.hn * 1e12, ffFixed, 5, 2);
  GDRRLEdit.Text := FloatToStrF(gReferenceRanges.GD.ln * 1e9, ffFixed, 5, 1);
  GDRRHEdit.Text := FloatToStrF(gReferenceRanges.GD.hn * 1e9, ffFixed, 5, 1);
  TSHIRRLEdit.Text := FloatToStrF(gReferenceRanges.TSHI.ln, ffFixed, 5, 1);
  TSHIRRHEdit.Text := FloatToStrF(gReferenceRanges.TSHI.hn, ffFixed, 5, 1);
  TTSIRRLEdit.Text := FloatToStrF(gReferenceRanges.TTSI.ln, ffFixed, 5, 0);
  TTSIRRHEdit.Text := FloatToStrF(gReferenceRanges.TTSI.hn, ffFixed, 5, 0);
  T4UnitLabel.Caption := gPreferences.T4.UOM;
  T3UnitLabel.Caption := gPreferences.T3.UOM;
  Edited := false;
end;

procedure TPreferencesForm.AdjustMethods(Sender: TObject; T4Method, T3Method: tLabMethod);
{Adjusts units depending from the selected method (levels of free or total hormone)}
begin
  if T4Method = freeHormone then
    begin
      T4MethodComboBox.ItemIndex := 0;
      T4UnitCombobox.Items.Assign(Hauptschirm.FT4Items.Items);
      T4UnitCombobox.Text := T4UnitCombobox.Items.Strings[0];
    end
  else
  begin
    T4MethodComboBox.ItemIndex := 1;
    T4UnitCombobox.Items.Assign(Hauptschirm.T4Items.Items);
    T4UnitCombobox.Text := T4UnitCombobox.Items.Strings[0];
  end;
  if T3Method = freeHormone then
    begin
      T3MethodComboBox.ItemIndex := 0;
      T3UnitCombobox.Items.Assign(Hauptschirm.FT3Items.Items);
      T3UnitCombobox.Text := T3UnitCombobox.Items.Strings[0];
    end
  else
  begin
    T3MethodComboBox.ItemIndex := 1;
    T3UnitCombobox.Items.Assign(Hauptschirm.T3Items.Items);
    T3UnitCombobox.Text := T3UnitCombobox.Items.Strings[0];
  end;
  DisplayReferenceRanges(Sender);
end;

procedure TPreferencesForm.AdjustCombos(Sender: TObject);
{adjust comboboxes according to measurement unit}
var
  found: boolean;
  i: integer;
begin
  AdjustMethods(Sender, gPreferences.T4.Method, gPreferences.T3.Method);
  found := False;
  with gPreferences do
  begin
    for i := 0 to TSHUnitComboBox.Items.Count - 1 do
    begin
      if TSH.UOM = TSHUnitComboBox.Items[i] then
      begin
        found := True;
        TSHUnitComboBox.ItemIndex := i;
        break;
      end;
    end;
    if found = False then
    begin
      TSHUnitComboBox.Items.Add(TSH.UOM);
      TSHUnitComboBox.ItemIndex := i + 1;
    end;
    for i := 0 to T4UnitComboBox.Items.Count - 1 do
    begin
      if T4.UOM = T4UnitComboBox.Items[i] then
      begin
        found := True;
        T4UnitComboBox.ItemIndex := i;
        break;
      end;
    end;
    if found = False then
    begin
      T4UnitComboBox.Items.Add(T4.UOM);
      T4UnitComboBox.ItemIndex := i + 1;
    end;
    for i := 0 to T3UnitComboBox.Items.Count - 1 do
    begin
      if T3.UOM = T3UnitComboBox.Items[i] then
      begin
        found := True;
        T3UnitComboBox.ItemIndex := i;
        break;
      end;
    end;
    if found = False then
    begin
      T3UnitComboBox.Items.Add(T3.UOM);
      T3UnitComboBox.ItemIndex := i + 1;
    end;
    T4MethodComboBoxAdjust(Hauptschirm);
    T3MethodComboBoxAdjust(Hauptschirm);
  end;
  DisplayReferenceRanges(Sender);
end;

procedure TPreferencesForm.GetMethodsAndUnits(const Sender: TObject);
begin
  if not RememberCheckBox.Checked then
  begin
    Hauptschirm.TSHUnitComboBox.ItemIndex := TSHUnitComboBox.ItemIndex;
    Hauptschirm.TSHUnitComboBoxChange(Sender);
    Hauptschirm.T4MethodComboBox.ItemIndex := T4MethodComboBox.ItemIndex;
    Hauptschirm.T4MethodComboBoxChange(Sender);
    Hauptschirm.T4UnitComboBox.ItemIndex := T4UnitComboBox.ItemIndex;
    Hauptschirm.T4UnitComboBoxChange(Sender);
    Hauptschirm.T3MethodComboBox.ItemIndex := T3MethodComboBox.ItemIndex;
    Hauptschirm.T3MethodComboBoxChange(Sender);
    Hauptschirm.T3UnitComboBox.ItemIndex := T3UnitComboBox.ItemIndex;
    Hauptschirm.T3UnitComboBoxChange(Sender);
  end;
end;

procedure TPreferencesForm.MethodComboBoxChange(Sender: TObject);
{reacts to a change in the combobox for the method {level of free or total hormone}}
var
  tempT4Method, tempT3Method: tLabMethod;
begin
  if T4MethodComboBox.ItemIndex = 0 then
    TempT4Method := freeHormone
  else
    TempT4Method := totalHormone;
  if T3MethodComboBox.ItemIndex = 0 then
    TempT3Method := freeHormone
  else
    TempT3Method := totalHormone;
  AdjustMethods(Sender, tempT4Method, tempT3Method);
end;

procedure TPreferencesForm.TSHUnitComboBoxChange(Sender: TObject);
begin
  GetMethodsAndUnits(Sender);
  DisplayReferenceRanges(Sender);
end;

procedure TPreferencesForm.TTSIRRHEditChange(Sender: TObject);
begin
  Edited := true;
end;

procedure TPreferencesForm.TTSIRRLEditChange(Sender: TObject);
begin
  Edited := true;
end;

procedure TPreferencesForm.T4UnitComboBoxChange(Sender: TObject);
begin
  GetMethodsAndUnits(Sender);
  DisplayReferenceRanges(Sender);
end;

procedure TPreferencesForm.TSHIRRHEditChange(Sender: TObject);
begin
  Edited := true;
end;

procedure TPreferencesForm.TSHIRRLEditChange(Sender: TObject);
begin
  Edited := true;
end;

procedure TPreferencesForm.TSHRRHEditChange(Sender: TObject);
begin
  Edited := true;
end;

procedure TPreferencesForm.TSHRRLEditChange(Sender: TObject);
begin
  Edited := true;
end;

procedure TPreferencesForm.T4MethodComboBoxChange(Sender: TObject);
begin
  MethodComboBoxChange(Sender);
  GetMethodsAndUnits(Sender);
  DisplayReferenceRanges(Sender);
end;

procedure TPreferencesForm.T4RRHEditChange(Sender: TObject);
begin
  Edited := true;
end;

procedure TPreferencesForm.T4RRLEditChange(Sender: TObject);
begin
  Edited := true;
end;

procedure TPreferencesForm.T3UnitComboBoxChange(Sender: TObject);
begin
  GetMethodsAndUnits(Sender);
  DisplayReferenceRanges(Sender);
end;

procedure TPreferencesForm.T3MethodComboBoxChange(Sender: TObject);
begin
  MethodComboBoxChange(Sender);
  GetMethodsAndUnits(Sender);
  DisplayReferenceRanges(Sender);
end;

procedure TPreferencesForm.T3RRHEditChange(Sender: TObject);
begin
  Edited := true;
end;

procedure TPreferencesForm.T3RRLEDitChange(Sender: TObject);
begin
  Edited := true;
end;

procedure CheckMandatoryColourising; {checks if mandatory fields should be coloured}
begin
  if PreferencesForm.MarkMandatoryCheck.Checked then
    gPreferences.colouriseMandatoryFields := true
  else
    gPreferences.colouriseMandatoryFields := false;
end;

procedure DisplayPreferencesDlg;
begin
  PreferencesForm.ShowModal;
end;

{ TPreferencesForm }

procedure TPreferencesForm.T4MethodComboBoxAdjust(Sender: TObject);
begin
  if T4MethodComboBox.Text = 'FT4' then
  begin
    T4UnitCombobox.Items.Assign(Hauptschirm.FT4Items.Items);
    T4UnitCombobox.Text :=
      Hauptschirm.T4UnitCombobox.Items.Strings[gPreferences.T4.PopUpItem];
  end
  else if T4MethodComboBox.Text = 'T4' then
  begin
    T4UnitCombobox.Items.Assign(Hauptschirm.T4Items.Items);
    T4UnitCombobox.Text :=
      Hauptschirm.T4UnitCombobox.Items.Strings[gPreferences.T4.PopUpItem];
  end;
end;

procedure TPreferencesForm.T3MethodComboBoxAdjust(Sender: TObject);
begin
  if T3MethodComboBox.Text = 'FT3' then
  begin
    T3UnitCombobox.Items.Assign(Hauptschirm.FT3Items.Items);
    T3UnitCombobox.Text :=
      Hauptschirm.T3UnitCombobox.Items.Strings[gPreferences.T3.PopUpItem];
  end
  else if T3MethodComboBox.Text = 'T3' then
  begin
    T3UnitCombobox.Items.Assign(Hauptschirm.T3Items.Items);
    T3UnitCombobox.Text :=
      Hauptschirm.T3UnitCombobox.Items.Strings[gPreferences.T3.PopUpItem];
  end;
end;

procedure TPreferencesForm.SaveEditedReferenceRanges(theFile: String);
var
  ReferenceRanges: tReferenceValues;
  returnCode: integer;
begin
  begin
    ReferenceRanges.TSH.ln := StrToFloatDef(TSHRRLEdit.Text, Math.NaN);
    ReferenceRanges.TSH.hn := StrToFloatDef(TSHRRHEdit.Text, Math.NaN);
    ReferenceRanges.TSH.lt := Math.Nan;
    ReferenceRanges.TSH.ht := Math.Nan;
    ReferenceRanges.TSH.lp := Math.Nan;
    ReferenceRanges.TSH.hp := Math.Nan;
    if gPreferences.T4.Method = freeHormone then
    begin
      ReferenceRanges.FT4.ln := StrToFloatDef(T4RRLEdit.Text, Math.NaN);
      ReferenceRanges.FT4.hn := StrToFloatDef(T4RRHEdit.Text, Math.NaN);
      ReferenceRanges.FT4.lt := Math.Nan;
      ReferenceRanges.FT4.ht := Math.Nan;
      ReferenceRanges.FT4.lp := Math.Nan;
      ReferenceRanges.FT4.hp := Math.Nan;
      ReferenceRanges.FT4.UOM := T4UnitComboBox.Caption;
      ReferenceRanges.TT4.UOM := 'N/A';
    end
    else
    begin
      ReferenceRanges.TT4.ln := StrToFloatDef(T4RRLEdit.Text, Math.NaN);
      ReferenceRanges.TT4.hn := StrToFloatDef(T4RRHEdit.Text, Math.NaN);
      ReferenceRanges.TT4.lt := Math.Nan;
      ReferenceRanges.TT4.ht := Math.Nan;
      ReferenceRanges.TT4.lp := Math.Nan;
      ReferenceRanges.TT4.hp := Math.Nan;
      ReferenceRanges.FT4.UOM := 'N/A';
      ReferenceRanges.TT4.UOM := T4UnitComboBox.Caption;
    end;
    if gPreferences.T3.Method = freeHormone then
    begin
      ReferenceRanges.FT3.ln := StrToFloatDef(T3RRLEdit.Text, Math.NaN);
      ReferenceRanges.FT3.hn := StrToFloatDef(T3RRHEdit.Text, Math.NaN);
      ReferenceRanges.FT3.lt := Math.Nan;
      ReferenceRanges.FT3.ht := Math.Nan;
      ReferenceRanges.FT3.lp := Math.Nan;
      ReferenceRanges.FT3.hp := Math.Nan;
      ReferenceRanges.FT3.UOM := T3UnitComboBox.Caption;
      ReferenceRanges.TT3.UOM := 'N/A';
    end
    else
    begin
      ReferenceRanges.TT3.ln := StrToFloatDef(T3RRLEdit.Text, Math.NaN);
      ReferenceRanges.TT3.hn := StrToFloatDef(T3RRHEdit.Text, Math.NaN);
      ReferenceRanges.TT3.lt := Math.Nan;
      ReferenceRanges.TT3.ht := Math.Nan;
      ReferenceRanges.TT3.lp := Math.Nan;
      ReferenceRanges.TT3.hp := Math.Nan;
      ReferenceRanges.FT3.UOM := 'N/A';
      ReferenceRanges.TT3.UOM := T3UnitComboBox.Caption;
    end;
    ReferenceRanges.GT.ln := StrToFloatDef(GTRRLEdit.Text, Math.NaN);
    ReferenceRanges.GT.hn := StrToFloatDef(GTRRhEdit.Text, Math.NaN);
    ReferenceRanges.GT.lt := Math.Nan;
    ReferenceRanges.GT.ht := Math.Nan;
    ReferenceRanges.GT.lp := Math.Nan;
    ReferenceRanges.GT.hp := Math.Nan;
    ReferenceRanges.GT.UOM := 'pmol/s';
    ReferenceRanges.GD.ln := StrToFloatDef(GDRRLEdit.Text, Math.NaN);
    ReferenceRanges.GD.hn := StrToFloatDef(GDRRhEdit.Text, Math.NaN);
    ReferenceRanges.GD.lt := Math.Nan;
    ReferenceRanges.GD.ht := Math.Nan;
    ReferenceRanges.GD.lp := Math.Nan;
    ReferenceRanges.GD.hp := Math.Nan;
    ReferenceRanges.GD.UOM := 'nmol/s';
    ReferenceRanges.TSHI.ln := StrToFloatDef(TSHIRRLEdit.Text, Math.NaN);
    ReferenceRanges.TSHI.hn := StrToFloatDef(TSHIRRhEdit.Text, Math.NaN);
    ReferenceRanges.TSHI.lt := Math.Nan;
    ReferenceRanges.TSHI.ht := Math.Nan;
    ReferenceRanges.TSHI.lp := Math.Nan;
    ReferenceRanges.TSHI.hp := Math.Nan;
    ReferenceRanges.TSHI.UOM := '';
    ReferenceRanges.TTSI.ln := StrToFloatDef(TTSIRRLEdit.Text, Math.NaN);
    ReferenceRanges.TTSI.hn := StrToFloatDef(TTSIRRhEdit.Text, Math.NaN);
    ReferenceRanges.TTSI.lt := Math.Nan;
    ReferenceRanges.TTSI.ht := Math.Nan;
    ReferenceRanges.TTSI.lp := Math.Nan;
    ReferenceRanges.TTSI.hp := Math.Nan;
    ReferenceRanges.TTSI.UOM := '';
    SaveCDISC_RRFile(theFile, ReferenceRanges, returnCode);
    if returnCode <> 0 then
      ShowMessage(SAVE_ERROR_MESSAGE);
  end;
end;

procedure TPreferencesForm.OKButtonClick(Sender: TObject);
{save preferences}
var
  CDISCStream: TMemoryStream;
  originalFileName: string;
begin
  GetMethodsAndUnits(Sender);
  originalFileName := CDISCOpenDialog.FileName;
  if (originalFileName <> '') and ((gCode = 0) or (gCode = 10)) and (Edited = false) then
  begin
    CDISCStream := TMemoryStream.Create;
    try
      CDISCStream.LoadFromFile(originalFileName);
      CDISCStream.SaveToFile(RRFile);
    finally
      CDISCStream.Free;
    end;
    ComposeRRHints;
  end
  else if Edited = true then
  begin
    SaveEditedReferenceRanges(RRFile);
    ComposeRRHints;
  end;
  CheckMandatoryColourising;
  gPreferences.MSH_ID := SendingFacEdit.Text;
  SavePreferences;
  PreferencesForm.Close;
  SPINA_UserInterface.GetPreferences;
end;

procedure TPreferencesForm.ReadCDISCFile(Sender: TObject; thePath: String);
{reads an XML file that is compliant with CDISC lab model standards}
var
  theCode: integer;
begin
  GetReferenceValues(thePath, gCode);
  if (gCode = 0) or (gCode = 10) then  {no error or new file created}
    DisplayReferenceRanges(Sender)
  else
  begin
    case gCode of
      1: ShowMessage(RR_FORMAT_ERROR_MESSAGE);
      2: ShowMessage(RR_SPINA_ERROR_MESSAGE);
      6: ShowMessage(PREFERENCES_SAVE_ERROR_MESSAGE);
    end;
    GetReferenceValues(RRFile, theCode);
  end;
end;

procedure TPreferencesForm.ReadCDISCButtonClick(Sender: TObject);
begin
  if CDISCOpenDialog.Execute then
  begin
    ReadCDISCFile(Sender, CDISCOpenDialog.FileName);
  end;
end;

procedure TPreferencesForm.RememberCheckBoxChange(Sender: TObject);
begin
  if RememberCheckBox.Checked then
  begin
    gPreferences.rememberUsedUnits := True;
    T4MethodComboBox.Enabled := False;
    T3MethodComboBox.Enabled := False;
    TSHUnitComboBox.Enabled := False;
    T4UnitComboBox.Enabled := False;
    T3UnitComboBox.Enabled := False;
  end
  else
  begin
    gPreferences.rememberUsedUnits := False;
    T4MethodComboBox.Enabled := True;
    T3MethodComboBox.Enabled := True;
    TSHUnitComboBox.Enabled := True;
    T4UnitComboBox.Enabled := True;
    T3UnitComboBox.Enabled := True;
  end;
end;

procedure TPreferencesForm.SaveCDISCButtonClick(Sender: TObject);
begin
  if CDISCSaveDialog.Execute then
    SaveEditedReferenceRanges(CDISCSaveDialog.FileName);
end;

procedure TPreferencesForm.FormActivate(Sender: TObject);
begin
  UpdateDisplay(Sender);
  if gPreferences.colouriseMandatoryFields then
    MarkMandatoryCheck.Checked := true
  else
    MarkMandatoryCheck.Checked := false;
  gSavedReferenceRanges := gReferenceRanges;
  gSavedSIReferenceRanges := gSIReferenceRanges;
  gSavedConvReferenceRanges := gConvReferenceRanges;
  AdjustCombos(Sender);
  if gPreferences.rememberUsedUnits then
    RememberCheckBox.Checked := True
  else
    RememberCheckBox.Checked := False;
  DisplayReferenceRanges(Sender);
  SendingFacEdit.Text := gPreferences.MSH_ID;
end;

procedure TPreferencesForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
var
  theCode: integer;
begin
  GetReferenceValues(RRFile, theCode);
  Hauptschirm.FormActivate(Sender);
end;

procedure TPreferencesForm.FormCreate(Sender: TObject);
begin
  PageControl1.TabIndex := 0;
  Edited := false;
  MandatoryFieldsGrid.Rows[1].Text := 'TSH';
  MandatoryFieldsGrid.Rows[2].Text := '(F)F4';
  MandatoryFieldsGrid.Rows[3].Text := '(F)T3';
  MandatoryFieldsGrid.Rows[4].Text := 'L-T4';
  MandatoryFieldsGrid.Rows[5].Text := 'L-T3';
  MandatoryFieldsGrid.Rows[6].Text := 'rhTSH';
  MandatoryFieldsGrid.Cells[1, 1] := '+';
  MandatoryFieldsGrid.Cells[1, 2] := '+';
  MandatoryFieldsGrid.Cells[1, 4] := '--';
  MandatoryFieldsGrid.Cells[2, 2] := '+';
  MandatoryFieldsGrid.Cells[2, 3] := '+';
  MandatoryFieldsGrid.Cells[2, 5] := '--';
  MandatoryFieldsGrid.Cells[3, 1] := '+';
  MandatoryFieldsGrid.Cells[3, 2] := '+';
  MandatoryFieldsGrid.Cells[3, 6] := '--';
  MandatoryFieldsGrid.Cells[4, 1] := '+';
  MandatoryFieldsGrid.Cells[4, 2] := '+';
  MandatoryFieldsGrid.Cells[4, 6] := '--';
end;

procedure TPreferencesForm.GDRRHEditChange(Sender: TObject);
begin
  Edited := true;
end;

procedure TPreferencesForm.GDRRLEditChange(Sender: TObject);
begin
  Edited := true;
end;

procedure TPreferencesForm.GTRRHEditChange(Sender: TObject);
begin
  Edited := true;
end;

procedure TPreferencesForm.GTRRLEditChange(Sender: TObject);
begin
  Edited := true;
end;

procedure TPreferencesForm.MarkMandatoryCheckChange(Sender: TObject);
begin
  CheckMandatoryColourising;
end;

procedure TPreferencesForm.CancelButtonClick(Sender: TObject);
var
  theCode: integer;
begin
  GetReferenceValues(RRFile, theCode);
  if (theCode <> 0) and (theCode <> 10) then
  begin
    gReferenceRanges := gSavedReferenceRanges;
    gSIReferenceRanges := gSavedSIReferenceRanges;
    gConvReferenceRanges := gSavedConvReferenceRanges;
  end;
  PreferencesForm.Close;
end;

initialization
  {$I setpreferences.lrs}

end.


