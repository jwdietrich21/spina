unit SetPreferences;

{ SPINA-Thyr }

{ Application for calculating structure parameters }
{ of thyrotropic feedback control }

{ Programm zur Berechnung von Strukturparametern }
{ des thyreotropen Regelkreises }

{ Version 3.3 }

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
    CDISCOpenDialog: TOpenDialog;
    ReadCDISCButton: TButton;
    CancelButton: TButton;
    CDISCGroupBox: TGroupBox;
    T3Label: TLabel;
    T3MethodComboBox: TComboBox;
    TSHLabel: TLabel;
    RememberCheckBox: TCheckBox;
    GTRREdit: TEdit;
    GDRREdit: TEdit;
    GTRRLabel: TLabel;
    GDRRLabel: TLabel;
    T3RREDit: TEdit;
    T4RREdit: TEdit;
    TSHRREdit: TEdit;
    T3RRLabel: TLabel;
    T3UnitComboBox: TComboBox;
    T4Label: TLabel;
    T4RRLabel: TLabel;
    T4MethodComboBox: TComboBox;
    T4UnitComboBox: TComboBox;
    TSHRRLabel: TLabel;
    TSHUnitComboBox: TComboBox;
    UnitLabel: TLabel;
    MethodLabel: TLabel;
    UnitsGroupBox: TGroupBox;
    OKButton: TButton;
    procedure DisplayReferenceRanges(Sender: TObject);
    procedure CancelButtonClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure OKButtonClick(Sender: TObject);
    procedure ReadCDISCButtonClick(Sender: TObject);
    procedure RememberCheckBoxChange(Sender: TObject);
    procedure T4MethodComboBoxAdjust(Sender: TObject);
    procedure T3MethodComboBoxAdjust(Sender: TObject);
    procedure GetPreferences(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  PreferencesForm: TPreferencesForm;

procedure DisplayPreferencesDlg;

implementation

uses
  SPINA_UserInterface;

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

procedure TPreferencesForm.GetPreferences(Sender: TObject);
var
  found: boolean;
  i: integer;
begin
  {ReadPreferences; }  {read preferences from XML file}
  if gPreferences.T4.Method = freeHormone then
    T4MethodComboBox.ItemIndex := 0
  else
    begin
      T4MethodComboBox.ItemIndex := 1;
      T4UnitCombobox.Items.Assign(Hauptschirm.T4Items.Items);
    end;
  if gPreferences.T3.Method = freeHormone then
    T3MethodComboBox.ItemIndex := 0
  else
    begin
      T3MethodComboBox.ItemIndex := 1;
      T3UnitCombobox.Items.Assign(Hauptschirm.T3Items.Items);
    end;
  found := false;
  with gPreferences do
  begin
    for i := 0 to TSHUnitComboBox.Items.Count - 1 do
      begin
        if TSH.measurementUnit = TSHUnitComboBox.Items[i] then
          begin
            found := true;
            TSHUnitComboBox.ItemIndex := i;
            break;
          end;
      end;
    if found = false then
      begin
        TSHUnitComboBox.Items.Add(TSH.measurementUnit);
        TSHUnitComboBox.ItemIndex := i + 1;
      end;
    for i := 0 to T4UnitComboBox.Items.Count - 1 do
      begin
        if T4.measurementUnit = T4UnitComboBox.Items[i] then
          begin
            found := true;
            T4UnitComboBox.ItemIndex := i;
            break;
          end;
      end;
    if found = false then
      begin
        T4UnitComboBox.Items.Add(T4.measurementUnit);
        T4UnitComboBox.ItemIndex := i + 1;
      end;
    for i := 0 to T3UnitComboBox.Items.Count - 1 do
      begin
        if T3.measurementUnit = T3UnitComboBox.Items[i] then
          begin
            found := true;
            T3UnitComboBox.ItemIndex := i;
            break;
          end;
      end;
    if found = false then
      begin
        T3UnitComboBox.Items.Add(T3.measurementUnit);
        T3UnitComboBox.ItemIndex := i + 1;
      end;
    T4MethodComboBoxAdjust(Hauptschirm);
    T3MethodComboBoxAdjust(Hauptschirm);
  end;
end;

procedure TPreferencesForm.OKButtonClick(Sender: TObject);
begin
  if not RememberCheckBox.Checked then
    begin
      AdjustUnitFactors;
    end;
  PreferencesForm.Close;
  SPINA_UserInterface.GetPreferences;
end;

procedure TPreferencesForm.ReadCDISCButtonClick(Sender: TObject);
begin
  if CDISCOpenDialog.Execute then
    begin
      GetReferenceValues(CDISCOpenDialog.FileName);
      DisplayReferenceRanges(Sender);
    end;
end;

procedure TPreferencesForm.RememberCheckBoxChange(Sender: TObject);
begin
  if RememberCheckBox.Checked then
    begin
      gPreferences.rememberUsedUnits := true;
      T4MethodComboBox.Enabled := false;
      T3MethodComboBox.Enabled := false;
      TSHUnitComboBox.Enabled := false;
      T4UnitComboBox.Enabled := false;
      T3UnitComboBox.Enabled := false;
    end
  else
    begin
      gPreferences.rememberUsedUnits := false;
      T4MethodComboBox.Enabled := true;
      T3MethodComboBox.Enabled := true;
      TSHUnitComboBox.Enabled := true;
      T4UnitComboBox.Enabled := true;
      T3UnitComboBox.Enabled := true;
    end;
end;

procedure TPreferencesForm.FormActivate(Sender: TObject);
begin
  GetPreferences(Sender);
  DisplayReferenceRanges(Sender);
end;

procedure TPreferencesForm.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  GetReferenceValues(RRFile);
end;

procedure TPreferencesForm.CancelButtonClick(Sender: TObject);
begin
  GetReferenceValues(RRFile);
  PreferencesForm.Close;
end;

initialization
  {$I setpreferences.lrs}

end.

