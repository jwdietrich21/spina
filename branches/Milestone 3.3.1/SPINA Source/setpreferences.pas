unit SetPreferences;

{ SPINA-Thyr }

{ Application for calculating structure parameters }
{ of thyrotropic feedback control }

{ Programm zur Berechnung von Strukturparametern }
{ des thyreotropen Regelkreises }

{ Version 3.3.1 }

{ (c) J. W. Dietrich, 1994 - 2013 }
{ (c) Ludwig Maximilian University of Munich 1995 - 2002 }
{ (c) University of Ulm Hospitals 2002-2004 }
{ (c) Ruhr University of Bochum 2005 - 2013 }

{ This unit draws a dialog box for setting global application preferences }

{ Source code released under the BSD License }
{ See http://spina.medical-cybernetics.de for details }

{$mode objfpc}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, SPINA_Types, HandlePreferences;

type

  { TPreferencesForm }

  TPreferencesForm = class(TForm)
    CancelButton: TButton;
    CDISCGroupBox: TGroupBox;
    CDISCOpenDialog: TOpenDialog;
    MarkMandatoryCheck: TCheckBox;
    GDRREdit: TEdit;
    GDRRLabel: TLabel;
    GTRREdit: TEdit;
    GTRRLabel: TLabel;
    MethodLabel: TLabel;
    OKButton: TButton;
    ReadCDISCButton: TButton;
    RememberCheckBox: TCheckBox;
    T3Label: TLabel;
    T3MethodComboBox: TComboBox;
    T3RREDit: TEdit;
    T3RRLabel: TLabel;
    T3UnitComboBox: TComboBox;
    T4Label: TLabel;
    T4MethodComboBox: TComboBox;
    T4RREdit: TEdit;
    T4RRLabel: TLabel;
    T4UnitComboBox: TComboBox;
    TSHLabel: TLabel;
    TSHRREdit: TEdit;
    TSHRRLabel: TLabel;
    TSHUnitComboBox: TComboBox;
    UnitLabel: TLabel;
    UnitsGroupBox: TGroupBox;
    procedure AdjustCombos(Sender: TObject);
    procedure AdjustMethods(Sender: TObject; T4Method, T3Method: tLabMethod);
    procedure CancelButtonClick(Sender: TObject);
    procedure DisplayReferenceRanges(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure MarkMandatoryCheckChange(Sender: TObject);
    procedure MethodComboBoxChange(Sender: TObject);
    procedure OKButtonClick(Sender: TObject);
    procedure ReadCDISCButtonClick(Sender: TObject);
    procedure RememberCheckBoxChange(Sender: TObject);
    procedure T3MethodComboBoxAdjust(Sender: TObject);
    procedure T3MethodComboBoxChange(Sender: TObject);
    procedure T3UnitComboBoxChange(Sender: TObject);
    procedure T4MethodComboBoxAdjust(Sender: TObject);
    procedure T4MethodComboBoxChange(Sender: TObject);
    procedure T4UnitComboBoxChange(Sender: TObject);
    procedure TSHUnitComboBoxChange(Sender: TObject);
    procedure UpdateDisplay(Sender: TObject);
  private
    { private declarations }
    procedure ReadCDISCFile(Sender: TObject; thePath: String);
  public
    { public declarations }
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
      PreferencesForm.MarkMandatoryCheck.Caption := kPflichtfelder1;
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
      RememberCheckBox.Hint := 'Please click to reuse measurement units of previous calculations';
      ReadCDISCButton.Hint := 'Import reference ranges from file';
      MarkMandatoryCheck.Caption := kPflichtfelder2;
    end;
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
      if TSH.measurementUnit = TSHUnitComboBox.Items[i] then
      begin
        found := True;
        TSHUnitComboBox.ItemIndex := i;
        break;
      end;
    end;
    if found = False then
    begin
      TSHUnitComboBox.Items.Add(TSH.measurementUnit);
      TSHUnitComboBox.ItemIndex := i + 1;
    end;
    for i := 0 to T4UnitComboBox.Items.Count - 1 do
    begin
      if T4.measurementUnit = T4UnitComboBox.Items[i] then
      begin
        found := True;
        T4UnitComboBox.ItemIndex := i;
        break;
      end;
    end;
    if found = False then
    begin
      T4UnitComboBox.Items.Add(T4.measurementUnit);
      T4UnitComboBox.ItemIndex := i + 1;
    end;
    for i := 0 to T3UnitComboBox.Items.Count - 1 do
    begin
      if T3.measurementUnit = T3UnitComboBox.Items[i] then
      begin
        found := True;
        T3UnitComboBox.ItemIndex := i;
        break;
      end;
    end;
    if found = False then
    begin
      T3UnitComboBox.Items.Add(T3.measurementUnit);
      T3UnitComboBox.ItemIndex := i + 1;
    end;
    T4MethodComboBoxAdjust(Hauptschirm);
    T3MethodComboBoxAdjust(Hauptschirm);
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

end;

procedure TPreferencesForm.T4UnitComboBoxChange(Sender: TObject);
begin

end;

procedure TPreferencesForm.T4MethodComboBoxChange(Sender: TObject);
begin
  MethodComboBoxChange(Sender);
end;

procedure TPreferencesForm.T3UnitComboBoxChange(Sender: TObject);
begin

end;

procedure TPreferencesForm.T3MethodComboBoxChange(Sender: TObject);
begin
  MethodComboBoxChange(Sender);
end;

procedure TPreferencesForm.DisplayReferenceRanges(Sender: TObject);
begin
  TSHRREdit.Text := gTSHRR;
  if gPreferences.T4.Method = freeHormone then
    T4RREdit.Text := gFT4RR
  else
    T4RREdit.Text := gTT4RR;
  if gPreferences.T3.Method = freeHormone then
    T3RREdit.Text := gFT3RR
  else
    T3RREdit.Text := gTT3RR;
  GTRREdit.Text := gGTRR;
  GDRREdit.Text := gGDRR;
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

procedure TPreferencesForm.OKButtonClick(Sender: TObject);
{save preferences}
var
  CDISCStream: TMemoryStream;
  originalFileName: string;
begin
  if not RememberCheckBox.Checked then
  begin
    Hauptschirm.TSHUnitComboBox.ItemIndex := TSHUnitComboBox.ItemIndex;
    Hauptschirm.TSHUnitComboBoxChange(Sender);
    Hauptschirm.T4MethodComboBox.ItemIndex := T4MethodComboBox.ItemIndex;
    Hauptschirm.T4MethodComboBoxChange(Sender);
    Hauptschirm.T4UnitComboBox.ItemIndex := T4UnitComboBox.ItemIndex;
    hauptschirm.T4UnitComboBoxChange(Sender);
    Hauptschirm.T3MethodComboBox.ItemIndex := T3MethodComboBox.ItemIndex;
    Hauptschirm.T3MethodComboBoxChange(Sender);
    Hauptschirm.T3UnitComboBox.ItemIndex := T3UnitComboBox.ItemIndex;
    Hauptschirm.T3UnitComboBoxChange(Sender);
  end;
  originalFileName := CDISCOpenDialog.FileName;
  if (originalFileName <> '') and ((gCode = 0) or (gCode = 10)) then
  begin
    CDISCStream := TMemoryStream.Create;
    try
      CDISCStream.LoadFromFile(originalFileName);
      CDISCStream.SaveToFile(RRFile);
    finally
      CDISCStream.Free;
    end;
    ComposeRRHints;
  end;
  CheckMandatoryColourising;
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
end;

procedure TPreferencesForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
var
  theCode: integer;
begin
  GetReferenceValues(RRFile, theCode);
  Hauptschirm.FormActivate(Sender);
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

