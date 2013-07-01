unit SPINA_UserInterface;

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

{ This unit implements the GUI }

{ Source code released under the BSD License }
{ See http://spina.medical-cybernetics.de for details }

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, StdActns, StdCtrls, LCLType, Menus, ActnList, VersionSupport,
  gettext, SPINA_Types, SPINA_Engine, SPINA_AboutBox, SPINA_ResultDialog,
  spina_help, HandlePreferences, SetPreferences, Math, LCLIntf
  {$IFDEF win32}
  , Windows
  {$ELSE}
  , Unix
    {$IFDEF LCLCarbon}
  , MacOSAll
    {$ENDIF}
  {$ENDIF}
  , Printers, ComCtrls, PrintersDlgs;

const
  AnInch = 2.54;

type

  { THauptschirm }

  THauptschirm = class(TForm)
    AboutMenuItem: TMenuItem;
    ActionList1: TActionList;
    AppleAboutMenuItem: TMenuItem;
    AppleMenu: TMenuItem;
    Ausschneiden1: TMenuItem;
    Beenden1: TMenuItem;
    berSPINAThyr1: TMenuItem;
    Calculate_Button: TButton;
    CDISC_defaults: TMemo;
    CloseMenuItem: TMenuItem;
    CopyMenuItem: TMenuItem;
    CopyResultMenuItem: TMenuItem;
    CutMenuItem: TMenuItem;
    DeleteMenuItem: TMenuItem;
    Divider_0_1: TMenuItem;
    Divider_0_2: TMenuItem;
    Divider_1_1: TMenuItem;
    Divider_1_2: TMenuItem;
    Divider_2_1: TMenuItem;
    Divider_2_2: TMenuItem;
    Divider_2_3: TMenuItem;
    Divider_3_1: TMenuItem;
    Drucken1: TMenuItem;
    EditCopy1: TEditCopy;
    EditCut1: TEditCut;
    EditDelete1: TEditDelete;
    EditMenu: TMenuItem;
    EditPaste1: TEditPaste;
    EditUndo1: TEditUndo;
    Einfgen1: TMenuItem;
    Ergebniskopieren1: TMenuItem;
    ffnen1: TMenuItem;
    FileMenu: TMenuItem;
    FT3Items: TComboBox;
    FT3_Text: TEdit;
    FT4Items: TComboBox;
    FT4_Text: TEdit;
    HelpItem: TMenuItem;
    HelpMenu: TMenuItem;
    Hintergrundbild: TImage;
    HintField: TMemo;
    HintGroupBox: TGroupBox;
    Image1: TImage;
    SPINALabel: TImage;
    ImageList1: TImageList;
    Kopieren1: TMenuItem;
    Label1: TLabel;
    Logo: TImage;
    Lschen1: TMenuItem;
    MacPreferencesItem: TMenuItem;
    MainMenu: TMainMenu;
    N1: TMenuItem;
    N2: TMenuItem;
    N3: TMenuItem;
    N4: TMenuItem;
    N5: TMenuItem;
    N6: TMenuItem;
    NewMenuItem: TMenuItem;
    OnlineInfoItem: TMenuItem;
    PageSetupDialog1: TPageSetupDialog;
    PageSetupMenuItem: TMenuItem;
    Panel1: TPanel;
    PasteMenuItem: TMenuItem;
    PopupCopy: TMenuItem;
    PopupCopyResult: TMenuItem;
    PopupCut: TMenuItem;
    PopupDiv1: TMenuItem;
    PopupMenu1: TPopupMenu;
    PopupPaste: TMenuItem;
    PopupUndo: TMenuItem;
    PoputDiv2: TMenuItem;
    PrintDialog1: TPrintDialog;
    PrinterSetupDialog1: TPrinterSetupDialog;
    PrintMenuItem: TMenuItem;
    QuitMenuItem: TMenuItem;
    Rckgngig1: TMenuItem;
    ResultField: TMemo;
    ResultGroupBox: TGroupBox;
    Schlieen1: TMenuItem;
    Seiteneinrichtung1: TMenuItem;
    SPINAThyrHilfe1: TMenuItem;
    SPINAThyrLabel: TLabel;
    T3Items: TComboBox;
    T3MethodComboBox: TComboBox;
    T3UnitComboBox: TComboBox;
    T4Items: TComboBox;
    T4MethodComboBox: TComboBox;
    T4UnitComboBox: TComboBox;
    TherapyCheckGroup: TCheckGroup;
    TSHUnitComboBox: TComboBox;
    TSH_Text: TEdit;
    UndoMenuItem: TMenuItem;
    ValuesGroupBox: TGroupBox;
    WinPreferencesItem: TMenuItem;
    procedure AppleMenuClick(Sender: TObject);
    procedure Beenden1Click(Sender: TObject);
    procedure Calculate_ButtonClick(Sender: TObject);
    procedure CloseMenuItemClick(Sender: TObject);
    procedure CopyMenuItemClick(Sender: TObject);
    procedure CutMenuItemClick(Sender: TObject);
    procedure DeleteMenuItemClick(Sender: TObject);
    procedure Ergebniskopieren1Click(Sender: TObject);
    procedure MarkMandatoryFields(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FT4ItemsChange(Sender: TObject);
    procedure HandleAbout(Sender: TObject);
    procedure HelpItemClick(Sender: TObject);
    procedure SPINALabelClick(Sender: TObject);
    procedure MacPreferencesItemClick(Sender: TObject);
    procedure MenuItem4Click(Sender: TObject);
    procedure NewMenuItemClick(Sender: TObject);
    procedure OnlineInfoItemClick(Sender: TObject);
    procedure PageSetupMenuItemClick(Sender: TObject);
    procedure PasteMenuItemClick(Sender: TObject);
    procedure PopupCopyClick(Sender: TObject);
    procedure PopupCopyResultClick(Sender: TObject);
    procedure PopupCutClick(Sender: TObject);
    procedure PopupPasteClick(Sender: TObject);
    procedure PopupUndoClick(Sender: TObject);
    procedure PrintMenuItemClick(Sender: TObject);
    procedure SPINAThyrLabelClick(Sender: TObject);
    procedure T3ItemsChange(Sender: TObject);
    procedure T3MethodComboBoxAdjust(Sender: TObject);
    procedure T3MethodComboBoxChange(Sender: TObject);
    procedure T3UnitComboBoxChange(Sender: TObject);
    procedure T4MethodComboBoxAdjust(Sender: TObject);
    procedure T4MethodComboBoxChange(Sender: TObject);
    procedure T4UnitComboBoxChange(Sender: TObject);
    procedure TherapyCheckGroupClick(Sender: TObject);
    procedure TherapyCheckGroupItemClick(Sender: TObject; Index: integer);
    procedure TSHUnitComboBoxChange(Sender: TObject);
    procedure UndoMenuItemClick(Sender: TObject);
    procedure WinPreferencesItemClick(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  TSH, T4, T3: real;
  Hauptschirm: THauptschirm;
  Language, UserName: array[0..128] of char;
  gSysLanguage, gUserName: string;
  arraySize: DWord;
  gTSHUnitFactor, gT4UnitFactor, gT3UnitFactor: real;
  gcalcCounter: longint;
  gAppPath, gAppDir, gAppName: string;
  gGermanCodes: tCodeList;
  tabX: integer;
{$IFDEF LCLCarbon}
  gItl0Handle: Intl0Hndl;
  gRegion: integer;
{$ENDIF}
  gTopMargin, gBottomMargin, gLeftMargin, gRightMargin: double;
  gLineSpacing: integer;

procedure AdaptMenus;
procedure AdjustUnitFactors;
procedure ComposeRRHints;
procedure GetPreferences;

implementation

function GetOSLanguage: string;
{platform-independent method to read the language of the user interface}
var
  l, fbl: string;
  {$IFDEF LCLCarbon}
  theLocaleRef: CFLocaleRef;
  locale: CFStringRef;
  buffer: StringPtr;
  bufferSize: CFIndex;
  encoding: CFStringEncoding;
  success: boolean;
  {$ENDIF}
begin
  {$IFDEF LCLCarbon}
  theLocaleRef := CFLocaleCopyCurrent;
  locale := CFLocaleGetIdentifier(theLocaleRef);
  encoding := 0;
  bufferSize := 256;
  buffer := new(StringPtr);
  success := CFStringGetPascalString(locale, buffer, bufferSize, encoding);
  if success then
    l := string(buffer^)
  else
    l := '';
  fbl := Copy(l, 1, 2);
  dispose(buffer);
  {$ELSE}
  {$IFDEF LINUX}
  fbl := Copy(GetEnvironmentVariable('LC_CTYPE'), 1, 2);
    {$ELSE}
  GetLanguageIDs(l, fbl);
    {$ENDIF}
  {$ENDIF}
  Result := fbl;
end;

procedure AdjustUnitFactors;
{calculates conversion factors from parsed unit strings}
var
  unitElements: TUnitElements;
  i, mpIndex, muIndex, vpIndex, j4: integer;
  tempT4Factor, tempT3Factor: real;
begin
  mpIndex := 0;    {Index for mass prefix}
  muIndex := 0;    {index for mass unit}
  vpIndex := 0;    {index for volume prefix}
  UnitElements := ParsedUnitString(EncodeGreek(Hauptschirm.T4UnitComboBox.Text));
  for i := MAXFACTORS - 1 downto 0 do
    begin
      if unitElements.MassPrefix = PrefixLabel[i] then mpIndex := i;
      if unitElements.MassUnit = T4UnitLabel[i] then muIndex := i;
      if unitElements.VolumePrefix = PrefixLabel[i] then vpIndex := i;
    end;
  tempT4Factor := PrefixFactor[mpIndex] * T4UnitFactor[muIndex] / PrefixFactor[vpIndex];
  if unitElements.MassUnit = 'mol' then
    gPreferences.T4.isSI := true
  else
    gPreferences.T4.isSI := false;
  mpIndex := 0;
  muIndex := 0;
  vpIndex := 0;
  UnitElements := ParsedUnitString(EncodeGreek(Hauptschirm.T3UnitComboBox.Text));
  for i := MAXFACTORS - 1 downto 0 do
    begin
      if unitElements.MassPrefix = PrefixLabel[i] then mpIndex := i;
      if unitElements.MassUnit = T3UnitLabel[i] then muIndex := i;
      if unitElements.VolumePrefix = PrefixLabel[i] then vpIndex := i;
    end;
  tempT3Factor := PrefixFactor[mpIndex] * T3UnitFactor[muIndex] / PrefixFactor[vpIndex];
  if unitElements.MassUnit = 'mol' then
    gPreferences.T3.isSI := true
  else
    gPreferences.T3.isSI := false;
  UnitElements := ParsedUnitString(EncodeGreek(Hauptschirm.TSHUnitComboBox.Text));
  if Hauptschirm.TSHUnitCombobox.Text = 'mU/l' then
    gTSHUnitFactor := 1
  else
    gTSHUnitFactor := 1;
  gTSHUnit := Hauptschirm.TSHUnitCombobox.Text;
  gT4Unit := Hauptschirm.T4UnitCombobox.Text;
  gT3Unit := Hauptschirm.T3UnitCombobox.Text;
  with gPreferences do
  begin
    TSH.measurementUnit := Hauptschirm.TSHUnitCombobox.Caption;
    T4.measurementUnit := Hauptschirm.T4UnitCombobox.Caption;
    T3.measurementUnit := Hauptschirm.T3UnitCombobox.Caption;
    TSH.UnitFactor := gTSHUnitFactor;
    T4.UnitFactor := gT4UnitFactor;
    T3.UnitFactor := gT3UnitFactor;
  end;
  gT4UnitFactor := tempT4Factor;
  gT3UnitFactor := tempT3Factor;
  ComposeRRStrings;
end;

procedure THauptschirm.T4MethodComboBoxAdjust(Sender: TObject);
begin
  if Hauptschirm.T4MethodComboBox.Text = 'FT4' then
  begin
    gPreferences.T4.Method := freeHormone;
    Hauptschirm.T4UnitCombobox.Items.Assign(FT4Items.Items);
    Hauptschirm.T4UnitCombobox.Text :=
      Hauptschirm.T4UnitCombobox.Items.Strings[gPreferences.T4.PopUpItem];
  end
  else if Hauptschirm.T4MethodComboBox.Text = 'T4' then
  begin
    gPreferences.T4.Method := totalHormone;
    Hauptschirm.T4UnitCombobox.Items.Assign(T4Items.Items);
    Hauptschirm.T4UnitCombobox.Text :=
      Hauptschirm.T4UnitCombobox.Items.Strings[gPreferences.T4.PopUpItem];
  end;
  AdjustUnitFactors;
end;

procedure THauptschirm.T3MethodComboBoxAdjust(Sender: TObject);
begin
  if Hauptschirm.T3MethodComboBox.Text = 'FT3' then
  begin
    gPreferences.T3.Method := freeHormone;
    Hauptschirm.T3UnitCombobox.Items.Assign(FT3Items.Items);
    Hauptschirm.T3UnitCombobox.Text :=
      Hauptschirm.T3UnitCombobox.Items.Strings[gPreferences.T3.PopUpItem];
  end
  else if Hauptschirm.T3MethodComboBox.Text = 'T3' then
  begin
    gPreferences.T3.Method := totalHormone;
    Hauptschirm.T3UnitCombobox.Items.Assign(T3Items.Items);
    Hauptschirm.T3UnitCombobox.Text :=
      Hauptschirm.T3UnitCombobox.Items.Strings[gPreferences.T3.PopUpItem];
  end;
  AdjustUnitFactors;
end;

{ THauptschirm }

procedure GetPreferences;
{gets preferences and adjust controls in the main form accordingly}
var
  found: boolean;
  i: integer;
begin
  ReadPreferences;   {read preferences from XML file}
  if gPreferences.T4.Method = freeHormone then
    Hauptschirm.T4MethodComboBox.ItemIndex := 0
  else
    begin
      Hauptschirm.T4MethodComboBox.ItemIndex := 1;
      Hauptschirm.T4UnitCombobox.Items.Assign(Hauptschirm.T4Items.Items);
    end;
  if gPreferences.T3.Method = freeHormone then
    Hauptschirm.T3MethodComboBox.ItemIndex := 0
  else
    begin
      Hauptschirm.T3MethodComboBox.ItemIndex := 1;
      Hauptschirm.T3UnitCombobox.Items.Assign(Hauptschirm.T3Items.Items);
    end;
  found := false;
  with gPreferences do
  begin
    for i := 0 to Hauptschirm.TSHUnitComboBox.Items.Count - 1 do
      begin
        if TSH.measurementUnit = Hauptschirm.TSHUnitComboBox.Items[i] then
          begin
            found := true;
            Hauptschirm.TSHUnitComboBox.ItemIndex := i;
            break;
          end;
      end;
    if found = false then
      begin
        Hauptschirm.TSHUnitComboBox.Items.Add(TSH.measurementUnit);
        Hauptschirm.TSHUnitComboBox.ItemIndex := i + 1;
      end;
    TSH.PopupItem := Hauptschirm.TSHUnitComboBox.ItemIndex;
    for i := 0 to Hauptschirm.T4UnitComboBox.Items.Count - 1 do
      begin
        if T4.measurementUnit = Hauptschirm.T4UnitComboBox.Items[i] then
          begin
            found := true;
            Hauptschirm.T4UnitComboBox.ItemIndex := i;
            break;
          end;
      end;
    if found = false then
      begin
        Hauptschirm.T4UnitComboBox.Items.Add(T4.measurementUnit);
        Hauptschirm.T4UnitComboBox.ItemIndex := i + 1;
      end;
    T4.PopUpItem := Hauptschirm.T4UnitComboBox.ItemIndex;
    for i := 0 to Hauptschirm.T3UnitComboBox.Items.Count - 1 do
      begin
        if T3.measurementUnit = Hauptschirm.T3UnitComboBox.Items[i] then
          begin
            found := true;
            Hauptschirm.T3UnitComboBox.ItemIndex := i;
            break;
          end;
      end;
    if found = false then
      begin
        Hauptschirm.T3UnitComboBox.Items.Add(T3.measurementUnit);
        Hauptschirm.T3UnitComboBox.ItemIndex := i + 1;
      end;
    T3.PopUpItem := Hauptschirm.T3UnitComboBox.ItemIndex;
    Hauptschirm.T4MethodComboBoxAdjust(Hauptschirm);
    Hauptschirm.T3MethodComboBoxAdjust(Hauptschirm);
    gTSHUnitFactor := TSH.UnitFactor;
    gT4UnitFactor := T4.UnitFactor;
    gT3UnitFactor := T3.UnitFactor;
    AdjustUnitFactors;
  end;
end;

procedure THauptschirm.Beenden1Click(Sender: TObject);
begin
  application.Terminate;
end;

procedure ComposeRRHints;
{create hints with reference ranges}
begin
  if gPreferences.T4.Method = freeHormone then
  begin
    if gPreferences.T3.Method = freeHormone then
    begin
      gReferenceValueString1 :=
        concat(gReferenzbereiche, kCR, kLF, gTSHRR, kCR, kLF, gFT4RR,
        kCR, kLF, gFT3RR, kCR, kLF, kCR, kLF, kCR, kLF, gGTRR, kCR, kLF, gGDRR);
      gReferenceValueString2 :=
        concat(gReferenzbereiche, kCR, kLF, 'TSH: ', gTSHRR, kCR, kLF,
        'FT4: ', gFT4RR, kCR, kLF, 'FT3: ', gFT3RR, kCR, kLF, kCR,
        kLF, kCR, kLF, 'GT: ', gGTRR, kCR, kLF, 'GD: ', gGDRR);
    end
    else if gPreferences.T3.Method = totalHormone then
    begin
      gReferenceValueString1 :=
        concat(gReferenzbereiche, kCR, kLF, gTSHRR, kCR, kLF, gFT4RR,
        kCR, kLF, gTT3RR, kCR, kLF, kCR, kLF, kCR, kLF, gGTRR, kCR, kLF, gGDRR);
      gReferenceValueString2 :=
        concat(gReferenzbereiche, kCR, kLF, 'TSH: ', gTSHRR, kCR, kLF,
        'FT4: ', gFT4RR, kCR, kLF, 'TT3: ', gTT3RR, kCR, kLF, kCR,
        kLF, kCR, kLF, 'GT: ', gGTRR, kCR, kLF, 'GD: ', gGDRR);
    end;
  end
  else
  begin
    if gPreferences.T3.Method = freeHormone then
    begin
      gReferenceValueString1 :=
        concat(gReferenzbereiche, kCR, kLF, gTSHRR, kCR, kLF, gTT4RR,
        kCR, kLF, gFT3RR, kCR, kLF, kCR, kLF, kCR, kLF, gGTRR, kCR, kLF, gGDRR);
      gReferenceValueString2 :=
        concat(gReferenzbereiche, kCR, kLF, 'TSH: ', gTSHRR, kCR, kLF,
        'TT4: ', gTT4RR, kCR, kLF, 'FT3: ', gFT3RR, kCR, kLF, kCR,
        kLF, kCR, kLF, 'GT: ', gGTRR, kCR, kLF, 'GD: ', gGDRR);
    end
    else if gPreferences.T3.Method = totalHormone then
    begin
      gReferenceValueString1 :=
        concat(gReferenzbereiche, kCR, kLF, gTSHRR, kCR, kLF, gTT4RR,
        kCR, kLF, gTT3RR, kCR, kLF, kCR, kLF, kCR, kLF, gGTRR, kCR, kLF, gGDRR);
      gReferenceValueString2 :=
        concat(gReferenzbereiche, kCR, kLF, 'TSH: ', gTSHRR, kCR, kLF,
        'TT4: ', gTT4RR, kCR, kLF, 'TT3: ', gTT3RR, kCR, kLF, kCR,
        kLF, kCR, kLF, 'GT: ', gGTRR, kCR, kLF, 'GD: ', gGDRR);
    end;
  end;
  Hauptschirm.ResultField.Hint := gReferenceValueString2;
end;

procedure ShowMessage(TSH_String, T4_String, T3_String: Str255; theResult: String);
{displays the result of the calculation}
const
  kTSH_Label = '   TSH: ';
  kFT4_Label = '   FT4: ';
  ksFT4_Label = 'FT4: ';
  kT4_Label = '   T4: ';
  ksT4_Label = 'T4: ';
  kFT3_Label = '   FT3: ';
  ksFT3_Label = 'FT3: ';
  kT3_Label = '   T3: ';
  ksT3_Label = 'T3: ';
var
  theString, vhString: Str255;
  T4Label, T3Label: Str255;
begin
  if gPreferences.T4.Method = freeHormone then  {free or total T4?}
    T4Label := kFT4_Label
  else
    T4Label := kT4_Label;
  if gPreferences.T3.Method = freeHormone then  {free or total T3?}
    T3Label := kFT3_Label
  else
    T3Label := kT3_Label;
  vhString := concat(gVerhaltensparameter, kCR, kLF, '   TSH: ',
    TSH_String, ' ', gTSHUnit, kCR, kLF, T4Label, T4_String, ' ',
    gT4Unit, kCR, kLF, T3Label, T3_String, ' ', gT3Unit);
  theString := concat(vhString, kCR, kLF, kCR, kLF, gStrukturparameter,
    kCR, kLF, theResult);
  Hauptschirm.ResultField.Text := theString;
  gResultString := theString;
  gResultDialogString1 := vhString;
  gResultDialogString2 := concat(gStrukturparameter, kCR, kLF, theResult);
  ComposeRRHints;
end;

procedure bell; {platform-independent implementation of acustical warning}
var
  s: longint;
begin
  {$IFDEF win32}
  MessageBeep(0);
  {$ELSE}
    {$IFDEF LCLCarbon}
  SysBeep(30);
    {$ELSE}
  s := fpSystem('echo -ne ''\007''');
  {s := fpSystem('echo -ne "\a"');}
  {s := fpSystem('tput bel');}
  {beep;}
    {$ENDIF}
  {$ENDIF}
end;

procedure HandleInput;
var
  Buffer: PChar;
  Size: byte;
  resultRecord: tCaseRecord;
  oldSeparator: Char;
  strucPars: String;
  TSH_String, T4_String, T3_String: Str255;
  TSH_Flag, T4_Flag, T3_Flag: Char;
begin
  oldSeparator := decimalSeparator;
  try
    Size := Hauptschirm.TSH_Text.GetTextLen;
    {Laenge des Strings in Edit1 ermitteln}
    if Size = 0 then                                      {Feld leer?}
      TSH := Math.Nan
    else
    begin
      TSH_String := Hauptschirm.TSH_Text.Text;
      if pos(DEC_COMMA, TSH_String) > 0 then
        decimalSeparator := DEC_COMMA
      else
        decimalSeparator := DEC_POINT;
      TSH := StrToFloat(TSH_String);
      TSH := TSH * gTSHUnitFactor;
      decimalSeparator := oldSeparator;
    end;
    TSH_String := FloatToStrF(TSH, ffFixed, 3, 2);
    Size := Hauptschirm.FT4_Text.GetTextLen;
    {Laenge des Strings in Edit1 ermitteln}
    if Size = 0 then                                      {Feld leer?}
      T4 := Math.NaN
    else
    begin
      T4_String := Hauptschirm.FT4_Text.Text;
      if pos(DEC_COMMA, T4_String) > 0 then
        decimalSeparator := DEC_COMMA
      else
        decimalSeparator := DEC_POINT;
      T4 := StrToFloat(T4_String);
      decimalSeparator := oldSeparator;
    end;
    T4_String := FloatToStrF(T4, ffFixed, 3, 2);
    T4 := T4 * gT4UnitFactor;
    Size := Hauptschirm.FT3_Text.GetTextLen;
    {Laenge des Strings in Edit1 ermitteln}
    if Size = 0 then                                      {Feld leer?}
      T3 := Math.NaN
    else
    begin
      T3_String := Hauptschirm.FT3_Text.Text;
      if pos(DEC_COMMA, T3_String) > 0 then
        decimalSeparator := DEC_COMMA
      else
        decimalSeparator := DEC_POINT;
      T3 := StrToFloat(T3_String);
      decimalSeparator := oldSeparator;
    end;
    T3_String := FloatToStrF(T3, ffFixed, 3, 2);
    T3 := T3 * gT3UnitFactor;
    if Hauptschirm.TherapyCheckGroup.Checked[0] then
      gTSHTherapy := True
    else
      gTSHTherapy := False;
    if Hauptschirm.TherapyCheckGroup.Checked[1] then
      gT4Therapy := True
    else
      gT4Therapy := False;
    if Hauptschirm.TherapyCheckGroup.Checked[2] then
      gT3Therapy := True
    else
      gT3Therapy := False;
  except
    on ex: Exception do
    begin
      bell;
      TSH := Math.NaN;
      T4 := Math.NaN;
      T3 := Math.NaN;
    end;
  end;
  decimalSeparator := oldSeparator;
  resultRecord := Calculate(TSH, T4, T3);
  if gUseReferenceRanges then
    strucPars := concat('   GT: ', resultRecord.flaggedGTs, kCR,
      kLF, '   GD: ', resultRecord.flaggedGDs)
  else
    strucPars := concat('   GT: ', resultRecord.GTs, kCR, kLF,
      '   GD: ', resultRecord.GDs);
  ShowMessage(TSH_String, T4_String, T3_String, strucPars);
end;

procedure THauptschirm.Calculate_ButtonClick(Sender: TObject);
{invokes calculation engine}
begin
  HandleInput;
  ResultForm.Visible := True;
  ResultForm.AlphaBlendValue := 220;
  Hauptschirm.HintField.Text := gAnleitung2;
end;

procedure THauptschirm.CopyMenuItemClick(Sender: TObject);
begin
  ActionList1.Actions[2].Execute;
end;

procedure THauptschirm.CutMenuItemClick(Sender: TObject);
begin
  ActionList1.Actions[1].Execute;
end;

procedure THauptschirm.DeleteMenuItemClick(Sender: TObject);
begin
  ActionList1.Actions[4].Execute;
end;

procedure AdaptMenus;
{platform-sensitive adjustmens of menus}
var
  modifierKey: TShiftState;
  theForm: TForm;
begin
  {$IFDEF LCLcarbon}
  modifierKey := [ssMeta];
  Hauptschirm.AboutMenuItem.Visible := False;
  Hauptschirm.Divider_3_1.Visible := False;
  Hauptschirm.AppleMenu.Visible := True;
  Hauptschirm.Divider_2_3.Visible := False;
  Hauptschirm.WinPreferencesItem.Visible := False;
  {$ELSE}
  modifierKey := [ssCtrl];
  Hauptschirm.AboutMenuItem.Visible := True;
  Hauptschirm.Divider_3_1.Visible := True;
  Hauptschirm.AppleMenu.Visible := False;
  Hauptschirm.Divider_2_3.Visible := True;
  Hauptschirm.WinPreferencesItem.Visible := True;
  Hauptschirm.HelpItem.ShortCut := ShortCut(VK_F1, []);
  {$ENDIF}
  Hauptschirm.NewMenuItem.ShortCut := ShortCut(VK_N, modifierKey);
  Hauptschirm.CloseMenuItem.ShortCut := ShortCut(VK_W, modifierKey);
  Hauptschirm.PrintMenuItem.ShortCut := ShortCut(VK_P, modifierKey);
  Hauptschirm.QuitMenuItem.ShortCut := ShortCut(VK_Q, modifierKey);
  Hauptschirm.UndoMenuItem.ShortCut := ShortCut(VK_Z, modifierKey);
  Hauptschirm.CutMenuItem.ShortCut := ShortCut(VK_X, modifierKey);
  Hauptschirm.CopyMenuItem.ShortCut := ShortCut(VK_C, modifierKey);
  Hauptschirm.PasteMenuItem.ShortCut := ShortCut(VK_V, modifierKey);
  Hauptschirm.CopyResultMenuItem.ShortCut := ShortCut(VK_R, modifierKey);
end;

procedure THauptschirm.FormCreate(Sender: TObject);
var
  theCode: integer;
begin
  Hauptschirm.HorzScrollBar.Visible := False;
  Hauptschirm.VertScrollBar.Visible := False;
  Hauptschirm.AutoScroll := False;
  Hauptschirm.SPINAThyrLabel.Caption := 'SPINA Thyr ' + GetFileVersion;
  gCDISC_RR := Hauptschirm.CDISC_defaults.Lines;
  GetPreferences;
  GetReferenceValues(RRFile, theCode);
end;

procedure THauptschirm.Ergebniskopieren1Click(Sender: TObject);
{copy result}
begin
  Hauptschirm.ResultField.SelectAll;
  Hauptschirm.ResultField.CopyToClipboard;
end;

procedure THauptschirm.MarkMandatoryFields(Sender: TObject);
begin
  if gPreferences.colouriseMandatoryFields then begin {should mandatory fields be colourised?}
    if Hauptschirm.TherapyCheckGroup.Checked[0] then {rhTSH therapy}
      Hauptschirm.TSH_Text.Color:= clDefault
    else
      Hauptschirm.TSH_Text.Color:= gMandatoryColor;
    if Hauptschirm.TherapyCheckGroup.Checked[1] then {T4 substitution}
      Hauptschirm.FT4_Text.Color:= clDefault
    else
      Hauptschirm.FT4_Text.Color:= gMandatoryColor;
    if Hauptschirm.TherapyCheckGroup.Checked[2] then {T3 substitution}
      Hauptschirm.FT3_Text.Color:= clDefault
    else
      Hauptschirm.FT3_Text.Color:= gMandatoryColor;
  end
  else
  begin
    Hauptschirm.TSH_Text.Color:= clDefault;
    Hauptschirm.FT4_Text.Color:= clDefault;
    Hauptschirm.FT3_Text.Color:= clDefault;
  end;
end;

procedure THauptschirm.FT4ItemsChange(Sender: TObject);
begin

end;

procedure THauptschirm.TSHUnitComboBoxChange(Sender: TObject);
begin
  gPreferences.TSH.measurementUnit := Hauptschirm.TSHUnitComboBox.Caption;
  gPreferences.TSH.PopUpItem := Hauptschirm.TSHUnitCombobox.ItemIndex;
  AdjustUnitFactors;
end;

procedure THauptschirm.T3UnitComboBoxChange(Sender: TObject);
begin
  gPreferences.T3.measurementUnit := Hauptschirm.T3UnitComboBox.Caption;
  gPreferences.T3.PopUpItem := Hauptschirm.T3UnitCombobox.ItemIndex;
  AdjustUnitFactors;
end;

procedure THauptschirm.T4UnitComboBoxChange(Sender: TObject);
begin
  gPreferences.T4.measurementUnit := Hauptschirm.T4UnitComboBox.Caption;
  gPreferences.T4.PopUpItem := Hauptschirm.T4UnitCombobox.ItemIndex;
  AdjustUnitFactors;
end;

procedure THauptschirm.TherapyCheckGroupClick(Sender: TObject);
begin
  Hauptschirm.MarkMandatoryFields(Sender);
end;

procedure THauptschirm.TherapyCheckGroupItemClick(Sender: TObject;
  Index: integer);
begin
  Hauptschirm.MarkMandatoryFields(Sender);
end;

procedure THauptschirm.FormActivate(Sender: TObject);
begin
  {Adjustments for small screens:}
  if Screen.Width < Hauptschirm.Width then
  begin
    Hauptschirm.SPINALabel.Visible := false;
    Hauptschirm.HintGroupBox.Visible := false;
    Hauptschirm.HintField.Visible := false;
    Hauptschirm.Width := Hauptschirm.Constraints.MinWidth;
    Hauptschirm.Left := (Screen.Width - Hauptschirm.Width) div 2;
  end;
  {Some preparations in startup situation:}
  if gStartup then
  begin
    Hauptschirm.TSH_Text.SetFocus;
    Hauptschirm.VertScrollBar.Visible := False;
    Hauptschirm.AutoScroll := False;
  end;
  Hauptschirm.MarkMandatoryFields(Sender);
  gLastActiveCustomForm := Hauptschirm;
end;


procedure THauptschirm.HandleAbout(Sender: TObject);
begin
  ShowAboutBox;
end;

procedure THauptschirm.AppleMenuClick(Sender: TObject);
begin

end;

procedure THauptschirm.HelpItemClick(Sender: TObject);
begin
  HelpWindow.ShowOnTop;
end;

procedure THauptschirm.SPINALabelClick(Sender: TObject);
begin
  ShowAboutBox;
end;

procedure THauptschirm.MacPreferencesItemClick(Sender: TObject);
begin
  DisplayPreferencesDlg;
end;

procedure THauptschirm.OnlineInfoItemClick(Sender: TObject);
begin
  OpenURL(BASE_URL);
end;

procedure THauptschirm.SPINAThyrLabelClick(Sender: TObject);
begin
  ShowAboutBox;
end;

procedure THauptschirm.WinPreferencesItemClick(Sender: TObject);
begin
  DisplayPreferencesDlg;
end;

procedure THauptschirm.NewMenuItemClick(Sender: TObject);
begin

end;

procedure THauptschirm.MenuItem4Click(Sender: TObject);
begin

end;

procedure THauptschirm.CloseMenuItemClick(Sender: TObject);
begin
  Close;
end;

function DoPageSetup: boolean;
var
  D: TPageSetupDialog;
begin
  D := TPageSetupDialog.Create(nil);
  try
    Result := D.Execute;
  finally
    D.Free;
  end;
end;

function DoPrintSetup: boolean;
var
  D: TPrintDialog;
begin
  D := TPrintDialog.Create(nil);
  try
    Result := D.Execute;
  finally
    D.Free;
  end;
end;

procedure THauptschirm.PageSetupMenuItemClick(Sender: TObject);
begin
  if not DoPageSetup then
    Exit;
end;

procedure THauptschirm.PasteMenuItemClick(Sender: TObject);
begin
  ActionList1.Actions[3].Execute;
end;

function GetLinesPerPage(ACanvas: TCanvas; ACanvasHeight, ADPI: integer): integer;
var
  H, DPC: integer;
begin
  DPC := Round(ADPI / AnInch);
  H := ACanvas.TextHeight('X') + gLineSpacing;
  Result := Round((ACanvasHeight - DPC * (gTopMargin - gBottomMargin)) / H - 3);
end;

function GetPoints(AUnits: double; ADPI: integer): integer;
begin
  Result := Round(AUnits * (ADPI / AnInch));
end;

procedure PrinterWriteln(H: integer; var currentX, currentY: integer; theString: string);
begin
  Printer.Canvas.TextOut(currentX, currentY, theString);
  Inc(currentY, H);
end;

procedure PrinterWrite(H: integer; var currentX, currentY: integer; theString: string);
begin
  Printer.Canvas.TextOut(currentX, currentY, theString);
end;

procedure PrintCaption(H: integer; var currentX, currentY, rightMargin: integer);
var
  theSize: integer;
begin
  theSize := Printer.Canvas.Font.Size;
  Printer.Canvas.Font.Size := trunc(theSize * 1.7);
  Printer.Canvas.Font.Style := [fsBold];
  PrinterWriteln(H, currentX, currentY, 'SPINA Thyr Report');
  PrinterWriteln(H, currentX, currentY, '');
  PrinterWriteln(H, currentX, currentY, '');
  Printer.Canvas.MoveTo(currentX, currentY - H div 2);
  Printer.Canvas.LineTo(Printer.PageWidth - rightMargin, currentY - H div 2);
  PrinterWriteln(H, currentX, currentY, '');
  PrinterWriteln(H, currentX, currentY, '');
  Printer.Canvas.Font.Style := [];
  Printer.Canvas.Font.Size := theSize;
  if gInterfaceLanguage = German then
  begin
    PrinterWrite(H, currentX, currentY, kPatientenname1);
    PrinterWriteln(H, tabX, currentY, kEinsender1);
    PrinterWrite(H, currentX, currentY, kGeburtsdatum1);
    PrinterWriteln(H, tabX, currentY, kUntersuchungsdatum1);
    PrinterWriteln(H, currentX, currentY, '');
    PrinterWriteln(H, currentX, currentY, '');
    PrinterWriteln(H, currentX, currentY, '');
  end
  else
  begin
    PrinterWrite(H, currentX, currentY, kPatientenname2);
    PrinterWriteln(H, tabX, currentY, kEinsender2);
    PrinterWrite(H, currentX, currentY, kGeburtsdatum2);
    PrinterWriteln(H, tabX, currentY, kUntersuchungsdatum2);
    PrinterWriteln(H, currentX, currentY, '');
    PrinterWriteln(H, currentX, currentY, '');
    PrinterWriteln(H, currentX, currentY, '');
  end;
end;

procedure PrintFooter(H: integer; var currentX, currentY, rightMargin: integer);
var
  theDate, theTime: string;
begin
  DateTimeToString(theDate, 'dddd"," dd mmmm yyyy', date);
  DateTimeToString(theTime, '"," t', time);
  PrinterWriteln(H, currentX, currentY, '');
  Printer.Canvas.MoveTo(currentX, currentY - H div 2);
  Printer.Canvas.LineTo(Printer.PageWidth - rightMargin, currentY - H div 2);
  Printer.Canvas.Font.Color := clGray;
  PrinterWriteln(H, currentX, currentY, concat(gBenutzername, gUserName,
    '  |  ', gDruckdatum, theDate, theTime));
  PrinterWriteln(H, currentX, currentY, 'SPINA Thyr ' + GetFileVersion);
  PrinterWriteln(H, currentX, currentY, '');
  Printer.Canvas.Font.Color := clBlack;
end;

procedure THauptschirm.PrintMenuItemClick(Sender: TObject);
var
  H, ADPI, marginX, marginXr, currentX, currentY, lastY, returnPos, lastPos: integer;
  resultLine, remainder: Str255;
begin
  if DoPrintSetup then
  begin
    gTopMargin := 2;
    gLeftMargin := 2;
    gRightMargin := 2;
    gBottomMargin := 2;
    gLineSpacing := 2;
    ADPI := Printer.YDPI;
    currentY := GetPoints(gTopMargin, ADPI);
    marginX := GetPoints(gLeftMargin, ADPI);
    marginXr := GetPoints(gRightMargin, ADPI) div 2;
    Printer.Title := 'SPINA Thyr Report';
    currentX := marginX;
    Printer.BeginDoc;
    try
      Printer.Canvas.Font := HintField.Font;
      Printer.Canvas.Font.Size := 9;
      Printer.Canvas.Font.Style := [];
      Printer.Canvas.Pen.Color := clBlack;
      Printer.Canvas.Pen.Width := 2;
      H := (Printer.Canvas.TextHeight('X') + gLineSpacing);
      tabX := Printer.PageWidth - marginXr - trunc(2.5 * Printer.Canvas.TextWidth(gUntersuchungsdatum));
      PrintCaption(H, currentX, currentY, marginXr);
      lastPos := 1;
      lastY := currentY;
      remainder := gResultString;
      repeat
        returnPos := pos(kCR, remainder);
        if returnPos > 0 then
          resultLine := copy(remainder, 1, returnPos - 1)
        else
          resultLine := remainder;
        remainder := copy(remainder, returnPos + 2, length(remainder));
        PrinterWriteln(H, currentX, currentY, resultLine);
      until returnPos = 0;
      currentY := lastY;
      remainder := gReferenceValueString1;
      repeat
        returnPos := pos(kCR, remainder);
        if returnPos > 0 then
          resultLine := copy(remainder, 1, returnPos - 1)
        else
          resultLine := remainder;
        remainder := copy(remainder, returnPos + 2, length(remainder));
        PrinterWriteln(H, tabX, currentY, resultLine);
      until returnPos = 0;
      currentX := marginX;
      currentY := Printer.PageHeight - 5 * H;
      PrintFooter(H, currentX, currentY, marginXr);
      Printer.EndDoc;
    except
      on E: Exception do
      begin
        Printer.Abort;
        raise;
      end;
    end;
  end;
end;

procedure THauptschirm.T3ItemsChange(Sender: TObject);
begin

end;

procedure THauptschirm.T4MethodComboBoxChange(Sender: TObject);
begin
  gPreferences.T4.MethodPopUpItem := Hauptschirm.T4MethodComboBox.ItemIndex;
  gPreferences.T4.PopUpItem := 0;
  Hauptschirm.T4MethodComboBoxAdjust(Sender);
  ComposeRRHints;
end;

procedure THauptschirm.T3MethodComboBoxChange(Sender: TObject);
begin
  gPreferences.T3.MethodPopUpItem := Hauptschirm.T3MethodComboBox.ItemIndex;
  gPreferences.T3.PopUpItem := 0;
  Hauptschirm.T3MethodComboBoxAdjust(Sender);
  ComposeRRHints;
end;

procedure THauptschirm.UndoMenuItemClick(Sender: TObject);
begin
  ActionList1.Actions[0].Execute;
end;

procedure THauptschirm.PopupCopyClick(Sender: TObject);
begin
  CopyMenuItemClick(Sender);
end;

procedure THauptschirm.PopupCopyResultClick(Sender: TObject);
begin
  Ergebniskopieren1Click(Sender);
end;

procedure THauptschirm.PopupCutClick(Sender: TObject);
begin
  CutMenuItemClick(Sender);
end;

procedure THauptschirm.PopupPasteClick(Sender: TObject);
begin
  PasteMenuItemClick(Sender);
end;

procedure THauptschirm.PopupUndoClick(Sender: TObject);
begin
  UndoMenuItemClick(Sender);
end;

initialization
{$I spina_userinterface.lrs}

  arraySize := SizeOf(UserName);
  gInterfaceLanguage := German;

  gPrefsDir := GetPreferencesFolder;
  gPrefsFileName := GetPreferencesFile;

  gAppName := ApplicationName;
  gAppPath := ParamStr(0);
  gAppPath := Application.Location;

  gSysLanguage := GetOSLanguage;
  {$IFDEF UNIX}
  gUserName := GetEnvironmentVariable('USER');
  {$ELSE}
  GetUserName(UserName, arraySize);
  gUserName := string(UserName);
  {$ENDIF}
  if gSysLanguage = 'de' then
  begin
    gInterfaceLanguage := German;
    DecimalSeparator := DEC_COMMA;
  end
  else
  begin
    gInterfaceLanguage := English;
    DecimalSeparator := DEC_POINT;
  end;

finalization

end.
