unit SPINA_UserInterface;

{ SPINA-Thyr }

{ Application for calculating structure parameters }
{ of thyrotropic feedback control }

{ Programm zur Berechnung von Strukturparametern }
{ des thyreotropen Regelkreises }

{ Version 4.0.1 (Mercator) }

{ (c) J. W. Dietrich, 1994 - 2015 }
{ (c) Ludwig Maximilian University of Munich 1995 - 2002 }
{ (c) University of Ulm Hospitals 2002-2004 }
{ (c) Ruhr University of Bochum 2005 - 2015 }

{ This unit implements the GUI }

{ Source code released under the BSD License }
{ See http://spina.medical-cybernetics.de for details }

{$mode objfpc}
{$H+}
{$ASSERTIONS ON}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, StdActns, StdCtrls, LCLType, Menus, ActnList, VersionSupport,
  gettext, SPINA_Types, SPINA_Resources, UnitConverter, SPINA_Engine, SPINA_AboutBox,
  SPINA_ResultDialog, spina_help, HandlePreferences, SetPreferences, CaseEditor,
  HandleImpEx, Math, InterfaceBase, LCLIntf, Barcode
  {$IFDEF MSWINDOWS}
  , Windows
  {$ENDIF}
  {$IFDEF UNIX}
  , Unix
  {$IF (DEFINED(LINUX)) OR (DEFINED(FREEBSD))}
  , users // not available on OSX
  {$ENDIF}
  , baseunix // for fpgetuid
  {$ENDIF}
  {$IFDEF LCLCarbon}
  , MacOSAll, CarbonProc
  {$ENDIF}
  , lazutf8, Printers, ComCtrls, PrintersDlgs;

const
  AnInch = 2.54;
  MaxLen = 256;

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
    CaseItem: TMenuItem;
    Divider_1_3: TMenuItem;
    SaveMenuItem: TMenuItem;
    OpenMenuItem: TMenuItem;
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
    procedure FormDropFiles(Sender: TObject; const FileNames: array of string);
    procedure FormPaint(Sender: TObject);
    procedure FormWindowStateChange(Sender: TObject);
    procedure InsertValues(Sender: TObject);
    procedure AppleMenuClick(Sender: TObject);
    procedure Beenden1Click(Sender: TObject);
    procedure Calculate_ButtonClick(Sender: TObject);
    procedure CaseItemClick(Sender: TObject);
    procedure CloseMenuItemClick(Sender: TObject);
    procedure CopyMenuItemClick(Sender: TObject);
    procedure CutMenuItemClick(Sender: TObject);
    procedure DeleteMenuItemClick(Sender: TObject);
    procedure Ergebniskopieren1Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure MarkMandatoryFields(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FT4ItemsChange(Sender: TObject);
    procedure HandleAbout(Sender: TObject);
    procedure HelpItemClick(Sender: TObject);
    procedure OpenMenuItemClick(Sender: TObject);
    procedure CheckEntries(Sender: TObject);
    procedure SaveMenuItemClick(Sender: TObject);
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
    caseIDBarCode: TBarcode;
  public
    { public declarations }
    caseRecord: tCaseRecord;
  end;

var
  TSH, T4, T3: real;
  Hauptschirm: THauptschirm;
  gSysLanguage, gUserName: string;
  UserNameSize: DWord;
  wUserName: WideString;
  gTSHUnitFactor, gT4UnitFactor, gT3UnitFactor: real;
  gcalcCounter: longint;
  gAppPath, gAppDir, gAppName: string;
  gGermanCodes: tCodeList;
  tabX1, tabX2, tabX3: integer;
{$IFDEF LCLCarbon}
  gItl0Handle: Intl0Hndl;
  gRegion: integer;
  theFormatString: string;
  theFormatter: CFDateFormatterRef;
{$ENDIF}
  gTopMargin, gBottomMargin, gLeftMargin, gRightMargin: double;
  gLineSpacing: integer;

procedure bell;
procedure AdaptMenus;
procedure AdjustUnitLabels;
procedure ComposeRRHints;
procedure GetMacDateFormats;
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
  {$IFDEF UNIX}
  fbl := Copy(GetEnvironmentVariable('LC_CTYPE'), 1, 2);
    {$ELSE}
  GetLanguageIDs(l, fbl);
    {$ENDIF}
  {$ENDIF}
  Result := fbl;
end;

procedure AdjustUnitLabels;
{determines labels and UOMs from parsed unit strings}
var
  unitElements: TUnitElements;
  i, mpIndex, muIndex, vpIndex, j4: integer;
  tempT4Factor, tempT3Factor: real;
begin
  UnitElements := ParsedUnitString(EncodeGreek(Hauptschirm.T4UnitComboBox.Text));
  if unitElements.MassUnit = 'mol' then
    gPreferences.T4.isSI := True
  else
    gPreferences.T4.isSI := False;
  UnitElements := ParsedUnitString(EncodeGreek(Hauptschirm.T3UnitComboBox.Text));
  if unitElements.MassUnit = 'mol' then
    gPreferences.T3.isSI := True
  else
    gPreferences.T3.isSI := False;
  UnitElements := ParsedUnitString(EncodeGreek(Hauptschirm.TSHUnitComboBox.Text));
  gTSHUnit := Hauptschirm.TSHUnitCombobox.Text;
  gT4Unit := Hauptschirm.T4UnitCombobox.Text;
  gT3Unit := Hauptschirm.T3UnitCombobox.Text;
  with gPreferences do
  begin
    TSH.UOM := Hauptschirm.TSHUnitCombobox.Caption;
    T4.UOM := Hauptschirm.T4UnitCombobox.Caption;
    T3.UOM := Hauptschirm.T3UnitCombobox.Caption;
  end;
  ComposeRRStrings;
end;

procedure THauptschirm.T4MethodComboBoxAdjust(Sender: TObject);
{changes between free T4 and total T4}
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
  AdjustUnitLabels;
end;

procedure THauptschirm.T3MethodComboBoxAdjust(Sender: TObject);
{changes between free T3 and total T3}
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
  AdjustUnitLabels;
end;

{ THauptschirm }

procedure GetMacDateFormats;
begin
  {$IFDEF LCLCarbon}
  theFormatter := CFDateFormatterCreate(kCFAllocatorDefault,
    CFLocaleCopyCurrent, kCFDateFormatterMediumStyle, kCFDateFormatterNoStyle);
  theFormatString := CFStringToStr(CFDateFormatterGetFormat(theFormatter));
  if pos('.', theFormatString) > 0 then
    DefaultFormatSettings.DateSeparator := '.'
  else if pos('/', theFormatString) > 0 then
    DefaultFormatSettings.DateSeparator := '/'
  else if pos('-', theFormatString) > 0 then
    DefaultFormatSettings.DateSeparator := '-';
  DefaultFormatSettings.ShortDateFormat := theFormatString;
  CFRelease(theFormatter);
  theFormatter := CFDateFormatterCreate(kCFAllocatorDefault,
    CFLocaleCopyCurrent, kCFDateFormatterLongStyle, kCFDateFormatterNoStyle);
  theFormatString := CFStringToStr(CFDateFormatterGetFormat(theFormatter));
  DefaultFormatSettings.LongDateFormat := theFormatString;
  CFRelease(theFormatter);
  {$ENDIF}
end;

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
  found := False;
  with gPreferences do
  begin
    for i := 0 to Hauptschirm.TSHUnitComboBox.Items.Count - 1 do
    begin
      if TSH.UOM = Hauptschirm.TSHUnitComboBox.Items[i] then
      begin
        found := True;
        Hauptschirm.TSHUnitComboBox.ItemIndex := i;
        break;
      end;
    end;
    if found = False then
    begin
      Hauptschirm.TSHUnitComboBox.Items.Add(TSH.UOM);
      Hauptschirm.TSHUnitComboBox.ItemIndex := i + 1;
    end;
    TSH.PopupItem := Hauptschirm.TSHUnitComboBox.ItemIndex;
    for i := 0 to Hauptschirm.T4UnitComboBox.Items.Count - 1 do
    begin
      if T4.UOM = Hauptschirm.T4UnitComboBox.Items[i] then
      begin
        found := True;
        Hauptschirm.T4UnitComboBox.ItemIndex := i;
        break;
      end;
    end;
    if found = False then
    begin
      Hauptschirm.T4UnitComboBox.Items.Add(T4.UOM);
      Hauptschirm.T4UnitComboBox.ItemIndex := i + 1;
    end;
    T4.PopUpItem := Hauptschirm.T4UnitComboBox.ItemIndex;
    for i := 0 to Hauptschirm.T3UnitComboBox.Items.Count - 1 do
    begin
      if T3.UOM = Hauptschirm.T3UnitComboBox.Items[i] then
      begin
        found := True;
        Hauptschirm.T3UnitComboBox.ItemIndex := i;
        break;
      end;
    end;
    if found = False then
    begin
      Hauptschirm.T3UnitComboBox.Items.Add(T3.UOM);
      Hauptschirm.T3UnitComboBox.ItemIndex := i + 1;
    end;
    T3.PopUpItem := Hauptschirm.T3UnitComboBox.ItemIndex;
    Hauptschirm.T4MethodComboBoxAdjust(Hauptschirm);
    Hauptschirm.T3MethodComboBoxAdjust(Hauptschirm);
    gTSHUnitFactor := TSH.UnitFactor;
    gT4UnitFactor := T4.UnitFactor;
    gT3UnitFactor := T3.UnitFactor;
    AdjustUnitLabels;
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
        kCR, kLF, gFT3RR, kCR, kLF, kCR, kLF, kCR, kLF, gGTRR, kCR,
        kLF, gGDRR, kCR, kLF, gsGDRR, kCR, kLF, gTSHIRR, kCR, kLF,
        gsTSHIRR, kCR, kLF, GTTSIRR);
      gReferenceValueString2 :=
        concat(gReferenzbereiche, kCR, kLF, 'TSH:  ', gTSHRR, kCR,
        kLF, 'FT4:  ', gFT4RR, kCR, kLF, 'FT3:  ', gFT3RR, kCR, kLF,
        kCR, kLF, kCR, kLF, 'GT:  ', gGTRR, kCR, kLF, 'GD:  ', gGDRR,
        kCR, kLF, 'sGD:  ', gsGDRR, kCR, kLF, 'TSHI: ', gTSHIRR, kCR,
        kLF, 'sTSHI: ', gsTSHIRR, kCR, kLF, 'TTSI: ', GTTSIRR);
    end
    else if gPreferences.T3.Method = totalHormone then
    begin
      gReferenceValueString1 :=
        concat(gReferenzbereiche, kCR, kLF, gTSHRR, kCR, kLF, gFT4RR,
        kCR, kLF, gTT3RR, kCR, kLF, kCR, kLF, kCR, kLF, gGTRR, kCR,
        kLF, gGDRR, kCR, kLF, gsGDRR, kCR, kLF, gTSHIRR, kCR, kLF,
        gsTSHIRR, kCR, kLF, GTTSIRR);
      gReferenceValueString2 :=
        concat(gReferenzbereiche, kCR, kLF, 'TSH:  ', gTSHRR, kCR,
        kLF, 'FT4:  ', gFT4RR, kCR, kLF, 'TT3:  ', gTT3RR, kCR, kLF,
        kCR, kLF, kCR, kLF, 'GT:  ', gGTRR, kCR, kLF, 'GD:  ', gGDRR,
        kCR, kLF, 'sGD:  ', gsGDRR, kCR, kLF, 'TSHI: ', gTSHIRR, kCR,
        kLF, 'sTSHI: ', gsTSHIRR, kCR, kLF, 'TTSI: ', GTTSIRR);
    end;
  end
  else
  begin
    if gPreferences.T3.Method = freeHormone then
    begin
      gReferenceValueString1 :=
        concat(gReferenzbereiche, kCR, kLF, gTSHRR, kCR, kLF, gTT4RR,
        kCR, kLF, gFT3RR, kCR, kLF, kCR, kLF, kCR, kLF, gGTRR, kCR,
        kLF, gGDRR, kCR, kLF, gsGDRR, kCR, kLF, gTSHIRR, kCR, kLF,
        gsTSHIRR, kCR, kLF, GTTSIRR);
      gReferenceValueString2 :=
        concat(gReferenzbereiche, kCR, kLF, 'TSH:  ', gTSHRR, kCR,
        kLF, 'TT4:  ', gTT4RR, kCR, kLF, 'FT3:  ', gFT3RR, kCR, kLF,
        kCR, kLF, kCR, kLF, 'GT:  ', gGTRR, kCR, kLF, 'GD:  ', gGDRR,
        kCR, kLF, 'sGD:  ', gsGDRR, kCR, kLF, 'TSHI: ', gTSHIRR, kCR,
        kLF, 'sTSHI: ', gsTSHIRR, kCR, kLF, 'TTSI: ', GTTSIRR);
    end
    else if gPreferences.T3.Method = totalHormone then
    begin
      gReferenceValueString1 :=
        concat(gReferenzbereiche, kCR, kLF, gTSHRR, kCR, kLF, gTT4RR,
        kCR, kLF, gTT3RR, kCR, kLF, kCR, kLF, kCR, kLF, gGTRR, kCR,
        kLF, gGDRR, kCR, kLF, gsGDRR, kCR, kLF, gTSHIRR, kCR, kLF,
        gsTSHIRR, kCR, kLF, GTTSIRR);
      gReferenceValueString2 :=
        concat(gReferenzbereiche, kCR, kLF, 'TSH:  ', gTSHRR, kCR,
        kLF, 'TT4:  ', gTT4RR, kCR, kLF, 'TT3:  ', gTT3RR, kCR, kLF,
        kCR, kLF, kCR, kLF, 'GT:  ', gGTRR, kCR, kLF, 'GD:  ', gGDRR,
        kCR, kLF, 'sGD:  ', gsGDRR, kCR, kLF, 'TSHI: ', gTSHIRR, kCR,
        kLF, 'sTSHI: ', gsTSHIRR, kCR, kLF, 'TTSI: ', GTTSIRR);
    end;
  end;
  Hauptschirm.ResultField.Hint := gReferenceValueString2;
end;

procedure ShowMessage(TSH_String, T4_String, T3_String: Str255; theResult: string);
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
  TSH_withUnit, T4_withUnit, T3_withUnit: Str255;
begin
  if gPreferences.T4.Method = freeHormone then  {free or total T4?}
    T4Label := kFT4_Label
  else
    T4Label := kT4_Label;
  if gPreferences.T3.Method = freeHormone then  {free or total T3?}
    T3Label := kFT3_Label
  else
    T3Label := kT3_Label;
  if LeftStr(TSH_String, 3) = NA_MARK then
    TSH_withUnit := NA_MARK
  else
    TSH_withUnit := concat(TSH_String, ' ', gTSHUnit);
  if LeftStr(T4_String, 3) = NA_MARK then
    T4_withUnit := NA_MARK
  else
    T4_withUnit := concat(T4_String, ' ', gT4Unit);
  if LeftStr(T3_String, 3) = NA_MARK then
    T3_withUnit := NA_MARK
  else
    T3_withUnit := concat(T3_String, ' ', gT3Unit);
  vhString := concat(gVerhaltensparameter, kCR, kLF, '   TSH: ',
    TSH_withUnit, kCR, kLF, T4Label, T4_withUnit, kCR, kLF, T3Label, T3_withUnit);
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

procedure CopyPatientData(fromCaseRecord: tCaseRecord; var toCaseRecord: tCaseRecord);
{ restores meta-information like PID, which is necessary since }
{ the Calculate function of SPINA Engine creates a new case record }
begin
  toCaseRecord.CaseID := fromCaseRecord.CaseID;
  toCaseRecord.PID := fromCaseRecord.PID;
  toCaseRecord.Name := fromCaseRecord.Name;
  toCaseRecord.GivenNames := fromCaseRecord.GivenNames;
  toCaseRecord.DoBDate := fromCaseRecord.DoBDate;
  toCaseRecord.OBDate := fromCaseRecord.OBDate;
  toCaseRecord.Placer := fromCaseRecord.Placer;
end;

procedure HandleInput(var status: integer);
{reads inputs and invokes calculation engine}
{liest Eingabefelder und startet die Berechnungseinheit}
var
  Size: byte;
  oldSeparator: char;
  strucPars: string;
  TSH_String, T4_String, T3_String: Str255;
  TSH_Flag, T4_Flag, T3_Flag: string;
  FT4UpperLimitforTTSI: real;
begin
  status := sOK;
  oldSeparator := DefaultFormatSettings.decimalSeparator;
  TSH_Flag := '';
  T4_Flag := '';
  T3_Flag := '';
  Hauptschirm.caseRecord.TSH := Math.NaN; // clears entry to enable disctinction
  Hauptschirm.caseRecord.FT4 := Math.NaN; // between total and free hormone
  Hauptschirm.caseRecord.TT4 := Math.NaN; // method
  Hauptschirm.caseRecord.FT3 := Math.NaN;
  Hauptschirm.caseRecord.TT3 := Math.NaN;
  try
    Size := Hauptschirm.TSH_Text.GetTextLen;
    {Laenge des Strings in TSH_Text ermitteln}
    if Size = 0 then                                      {Feld leer?}
    begin
      TSH := Math.Nan;
      TSH_String := NA_MARK;
    end
    else
    begin
      TSH_String := Hauptschirm.TSH_Text.Text;
      if pos(DEC_COMMA, TSH_String) > 0 then
        DefaultFormatSettings.decimalSeparator := DEC_COMMA
      else
        DefaultFormatSettings.decimalSeparator := DEC_POINT;
      TSH := StrToFloatDef(TSH_String, Math.NaN);
      TSH := ConvertedValue(TSH, 1, 'mU/l', 'mU/l');
      Hauptschirm.caseRecord.TSH := TSH;
      DefaultFormatSettings.decimalSeparator := oldSeparator;
      if (isNan(gReferenceRanges.TSH.ln) or isNan(gReferenceRanges.TSH.hn)) then
        TSH_Flag := ''
      else if (TSH < gReferenceRanges.TSH.ln) or (TSH > gReferenceRanges.TSH.hn) then
        TSH_Flag := REF_RANGE_FLAG;
      if IsNaN(TSH) then
        TSH_String := NA_MARK
      else
        TSH_String := FloatToStrF(TSH, ffFixed, 3, 2) + TSH_Flag;
    end;
    Size := Hauptschirm.FT4_Text.GetTextLen;
    {Laenge des Strings in FT4_Text ermitteln}
    if Size = 0 then                                      {Feld leer?}
    begin
      T4 := Math.NaN;
      T4_String := NA_MARK;
    end
    else
    begin
      T4_String := Hauptschirm.FT4_Text.Text;
      if pos(DEC_COMMA, T4_String) > 0 then
        DefaultFormatSettings.decimalSeparator := DEC_COMMA
      else
        DefaultFormatSettings.decimalSeparator := DEC_POINT;
      T4 := StrToFloatDef(T4_String, Math.NaN);
      DefaultFormatSettings.decimalSeparator := oldSeparator;
      if IsNaN(T4) then
      begin
        T4_String := NA_MARK;
        T4_Flag := '';
      end
      else
      begin
        if gPreferences.T4.Method = freeHormone then  {free or total T4?}
        begin
          if (isNan(gReferenceRanges.FT4.ln) or isNan(gReferenceRanges.FT4.hn)) then
            T4_Flag := ''
          else if (T4 < gReferenceRanges.FT4.ln) or (T4 > gReferenceRanges.FT4.hn) then
            T4_Flag := REF_RANGE_FLAG;
        end
        else
        begin
          if (isNan(gReferenceRanges.TT4.ln) or isNan(gReferenceRanges.TT4.hn)) then
            T4_Flag := ''
          else if (T4 < gReferenceRanges.TT4.ln) or (T4 > gReferenceRanges.TT4.hn) then
            T4_Flag := REF_RANGE_FLAG;
        end;
        T4_String := FloatToStrF(T4, ffFixed, 3, 2) + T4_Flag;
        T4 := ConvertedValue(T4, T4_MOLAR_MASS,
          Hauptschirm.T4UnitComboBox.Caption, 'mol/l');
        if gPreferences.T4.Method = freeHormone then
          Hauptschirm.caseRecord.FT4 := T4
        else
          Hauptschirm.caseRecord.TT4 := T4;
      end;
    end;
    Size := Hauptschirm.FT3_Text.GetTextLen;
    {Laenge des Strings in FT3_Text ermitteln}
    if Size = 0 then                                      {Feld leer?}
    begin
      T3 := Math.NaN;
      T3_String := NA_MARK;
    end
    else
    begin
      T3_String := Hauptschirm.FT3_Text.Text;
      if pos(DEC_COMMA, T3_String) > 0 then
        DefaultFormatSettings.decimalSeparator := DEC_COMMA
      else
        DefaultFormatSettings.decimalSeparator := DEC_POINT;
      T3 := StrToFloatDef(T3_String, Math.NaN);
      DefaultFormatSettings.decimalSeparator := oldSeparator;
      if IsNaN(T3) then
      begin
        T3_String := NA_MARK;
        T3_Flag := '';
      end
      else
      begin
        if gPreferences.T3.Method = freeHormone then  {free or total T3?}
        begin
          if (isNan(gReferenceRanges.FT3.ln) or isNan(gReferenceRanges.FT3.hn)) then
            T3_Flag := ''
          else if (T3 < gReferenceRanges.FT3.ln) or (T3 > gReferenceRanges.FT3.hn) then
            T3_Flag := REF_RANGE_FLAG;
        end
        else
        begin
          if (isNan(gReferenceRanges.TT3.ln) or isNan(gReferenceRanges.TT3.hn)) then
            T3_Flag := ''
          else if (T3 < gReferenceRanges.TT3.ln) or (T3 > gReferenceRanges.TT3.hn) then
            T3_Flag := REF_RANGE_FLAG;
        end;
        T3_String := FloatToStrF(T3, ffFixed, 3, 2) + T3_Flag;
        T3 := ConvertedValue(T3, T3_MOLAR_MASS,
          Hauptschirm.T3UnitComboBox.Caption, 'mol/l');
        if gPreferences.T3.Method = freeHormone then
          Hauptschirm.caseRecord.FT3 := T3
        else
          Hauptschirm.caseRecord.TT3 := T3;
      end;
    end;
    if Hauptschirm.TherapyCheckGroup.Checked[0] then
      Hauptschirm.caseRecord.TSHTherapy := True
    else
      Hauptschirm.caseRecord.TSHTherapy := False;
    if Hauptschirm.TherapyCheckGroup.Checked[1] then
      Hauptschirm.caseRecord.T4Therapy := True
    else
      Hauptschirm.caseRecord.T4Therapy := False;
    if Hauptschirm.TherapyCheckGroup.Checked[2] then
      Hauptschirm.caseRecord.T3Therapy := True
    else
      Hauptschirm.caseRecord.T3Therapy := False;
  except
    on ex: Exception do
    begin
      bell;
      TSH := Math.NaN;
      T4 := Math.NaN;
      T3 := Math.NaN;
      Hauptschirm.caseRecord.TSH := Math.NaN;
      Hauptschirm.caseRecord.FT4 := Math.NaN;
      Hauptschirm.caseRecord.TT4 := Math.NaN;
      Hauptschirm.caseRecord.FT3 := Math.NaN;
      Hauptschirm.caseRecord.TT3 := Math.NaN;
    end;
  end;
  DefaultFormatSettings.decimalSeparator := oldSeparator;
  with Hauptschirm.caseRecord do
    if (isNan(TSH) or (TSH >= 0)) and (isNan(FT4) or (FT4 >= 0)) and
      (isNan(TT4) or (TT4 >= 0)) and (isNan(FT3) or (FT3 >= 0)) and
      (isNan(TT3) or (TT3 >= 0)) then
    begin
      Calculate(Hauptschirm.caseRecord);
      if gPreferences.T4.isSI then
        FT4UpperLimitforTTSI :=
          ConvertedValue(gSIReferenceRanges.FT4.hn, T4_MOLAR_MASS,
          Hauptschirm.T4UnitComboBox.Caption, 'mol/l')
      else
        FT4UpperLimitforTTSI :=
          ConvertedValue(gReferenceRanges.FT4.hn, T4_MOLAR_MASS,
          Hauptschirm.T4UnitComboBox.Caption, 'mol/l');
      InsertTTSI(Hauptschirm.caseRecord, FT4UpperLimitforTTSI);
      Insert_sTSHI(Hauptschirm.caseRecord, gReferenceRanges);
      Insert_sGD(Hauptschirm.caseRecord, gReferenceRanges);
      FormatCase(Hauptschirm.caseRecord, gReferenceRanges);
      if gUseReferenceRanges then
        strucPars := concat('   GT: ', Hauptschirm.caseRecord.flaggedGTs,
          kCR, kLF, '   GD: ', Hauptschirm.caseRecord.flaggedGDs,
          kCR, kLF, '   sGD: ', Hauptschirm.caseRecord.flaggedsGDs,
          kCR, kLF, '   TSHI: ', Hauptschirm.caseRecord.flaggedTSHIs,
          kCR, kLF, '   sTSHI: ', Hauptschirm.caseRecord.flaggedsTSHIs,
          kCR, kLF, '   TTSI: ', Hauptschirm.caseRecord.flaggedTTSIs)
      else
        strucPars := concat('   GT: ', Hauptschirm.caseRecord.GTs,
          kCR, kLF, '   GD: ', Hauptschirm.caseRecord.GDs, kCR, kLF,
          '   TSHI: ', Hauptschirm.caseRecord.TSHIs, kCR, kLF,
          '   TTSI: ', Hauptschirm.caseRecord.TTSIs);
      ShowMessage(TSH_String, T4_String, T3_String, strucPars);
    end
    else
    begin
      bell;
      MessageDlg(gNegativeError, mtWarning, [mbOK], 0);
      status := sNegative;
    end;
end;

procedure THauptschirm.Calculate_ButtonClick(Sender: TObject);
{invokes calculation engine}
var
  status: integer;
begin
  HandleInput(status);
  if status = sOK then
  begin
    ResultForm.Visible := True;
    ResultForm.AlphaBlendValue := 220;
    Hauptschirm.HintField.Text := gAnleitung2;
    Hauptschirm.HintField.Hint := Hauptschirm.HintField.Text;
  end;
end;

procedure THauptschirm.CaseItemClick(Sender: TObject);
begin
  CaseEditorForm.ShowModal;
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
  Hauptschirm.OpenMenuItem.ShortCut := ShortCut(VK_O, modifierKey);
  Hauptschirm.SaveMenuItem.ShortCut := ShortCut(VK_S, modifierKey);
  Hauptschirm.CloseMenuItem.ShortCut := ShortCut(VK_W, modifierKey);
  Hauptschirm.CaseItem.ShortCut := ShortCut(VK_C, modifierKey + [ssShift]);
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
  if YosemiteORNewer then
    Calculate_Button.Height := 22;
  HorzScrollBar.Visible := False;
  VertScrollBar.Visible := False;
  AutoScroll := False;
  SPINAThyrLabel.Caption := 'SPINA Thyr ' + GetFileVersion;
  GetPreferences;
  GetReferenceValues(RRFile, theCode);
  NewCaseRecord(caseRecord);
  caseRecord.Placer := gPreferences.Placer_ID;
end;

procedure THauptschirm.Ergebniskopieren1Click(Sender: TObject);
{copy result}
begin
  ResultField.SelectAll;
  ResultField.CopyToClipboard;
end;

procedure THauptschirm.FormShow(Sender: TObject);
begin

end;

procedure THauptschirm.MarkMandatoryFields(Sender: TObject);
begin
  if gPreferences.colouriseMandatoryFields then
  begin {should mandatory fields be colourised?}
    if Hauptschirm.TherapyCheckGroup.Checked[0] then {rhTSH therapy}
    begin
      if Hauptschirm.TherapyCheckGroup.Checked[1] then {T4 substitution}
        Hauptschirm.TSH_Text.Color := clDefault
      else
        Hauptschirm.TSH_Text.Color := gPreferences.MandatoryColor;
    end
    else
    begin
      Hauptschirm.TSH_Text.Color := gPreferences.MandatoryColor;
    end;
    if Hauptschirm.TherapyCheckGroup.Checked[1] then {T4 substitution}
    begin
      if Hauptschirm.TherapyCheckGroup.Checked[2] then {T3 substitution}
        Hauptschirm.FT4_Text.Color := clDefault
      else
        Hauptschirm.FT4_Text.Color := gPreferences.MandatoryColor;
    end
    else
    begin
      Hauptschirm.FT4_Text.Color := gPreferences.MandatoryColor;
    end;
    if Hauptschirm.TherapyCheckGroup.Checked[2] then {T3 substitution}
    begin
      Hauptschirm.FT3_Text.Color := clDefault;
    end
    else
    begin
      Hauptschirm.FT3_Text.Color := gPreferences.MandatoryColor;
    end;
  end
  else
  begin
    Hauptschirm.TSH_Text.Color := clDefault;
    Hauptschirm.FT4_Text.Color := clDefault;
    Hauptschirm.FT3_Text.Color := clDefault;
  end;
end;

procedure THauptschirm.FT4ItemsChange(Sender: TObject);
begin

end;

procedure THauptschirm.TSHUnitComboBoxChange(Sender: TObject);
begin
  gPreferences.TSH.UOM := Hauptschirm.TSHUnitComboBox.Caption;
  gPreferences.TSH.PopUpItem := Hauptschirm.TSHUnitCombobox.ItemIndex;
  AdjustUnitLabels;
end;

procedure THauptschirm.T3UnitComboBoxChange(Sender: TObject);
begin
  gPreferences.T3.UOM := Hauptschirm.T3UnitComboBox.Caption;
  gPreferences.T3.PopUpItem := Hauptschirm.T3UnitCombobox.ItemIndex;
  AdjustUnitLabels;
end;

procedure THauptschirm.T4UnitComboBoxChange(Sender: TObject);
begin
  gPreferences.T4.UOM := Hauptschirm.T4UnitComboBox.Caption;
  gPreferences.T4.PopUpItem := Hauptschirm.T4UnitCombobox.ItemIndex;
  AdjustUnitLabels;
end;

procedure THauptschirm.TherapyCheckGroupClick(Sender: TObject);
var
  status: integer;
begin
  MarkMandatoryFields(Sender);
  HandleInput(status);
end;

procedure THauptschirm.TherapyCheckGroupItemClick(Sender: TObject; Index: integer);
var
  status: integer;
begin
  Hauptschirm.MarkMandatoryFields(Sender);
  HandleInput(status);
end;

procedure THauptschirm.FormActivate(Sender: TObject);
begin
  {Adjustments for small screens:}
  if Screen.Width < Hauptschirm.Width then
  begin
    SPINALabel.Visible := False;
    HintGroupBox.Visible := False;
    HintField.Visible := False;
    Width := Constraints.MinWidth;
    Left := (Screen.Width - Width) div 2;
  end;
  {Some preparations in startup situation:}
  if gStartup then
  begin
    TSH_Text.SetFocus;
    VertScrollBar.Visible := False;
    AutoScroll := False;
  end;
  MarkMandatoryFields(Sender);
  gLastActiveCustomForm := Hauptschirm;
end;


procedure THauptschirm.HandleAbout(Sender: TObject);
begin
  ShowAboutBox;
end;

procedure THauptschirm.InsertValues(Sender: TObject);
{ Inserts values of case record into appropriate fields of form }
begin
  if not isNaN(caseRecord.TSH) then
    TSH_Text.Text := FloatToStr(caseRecord.TSH)
  else
    TSH_Text.Text := '';
  if not isNaN(caseRecord.FT4) then
  begin
    T4MethodComboBox.ItemIndex := 0;
    T4MethodComboBoxAdjust(Sender);
    T4UnitComboBox.Caption := caseRecord.FT4_UOM;
    FT4_Text.Text := FloatToStr(ConvertedValue(caseRecord.FT4,
      T4_MOLAR_MASS, 'mol/l', T4UnitComboBox.Caption));
  end
  else
  begin
    T4MethodComboBox.ItemIndex := 1;
    T4MethodComboBoxAdjust(Sender);
    T4UnitComboBox.Caption := caseRecord.TT4_UOM;
    FT4_Text.Text := FloatToStr(ConvertedValue(caseRecord.TT4,
      T4_MOLAR_MASS, 'mol/l', T4UnitComboBox.Caption));
  end;
  if not isNaN(caseRecord.FT3) then
  begin
    T3MethodComboBox.ItemIndex := 0;
    T3MethodComboBoxAdjust(Sender);
    T3UnitComboBox.Caption := caseRecord.FT3_UOM;
    FT3_Text.Text := FloatToStr(ConvertedValue(caseRecord.FT3,
      T3_MOLAR_MASS, 'mol/l', T3UnitComboBox.Caption));
  end
  else
  begin
    T3MethodComboBox.ItemIndex := 1;
    T3MethodComboBoxAdjust(Sender);
    T3UnitComboBox.Caption := caseRecord.TT3_UOM;
    FT3_Text.Text := FloatToStr(ConvertedValue(caseRecord.TT3,
      T3_MOLAR_MASS, 'mol/l', T3UnitComboBox.Caption));
  end;
  if caseRecord.TSHTherapy then
    TherapyCheckGroup.Checked[0] := True
  else
    TherapyCheckGroup.Checked[0] := False;
  if caseRecord.T4Therapy then
    TherapyCheckGroup.Checked[1] := True
  else
    TherapyCheckGroup.Checked[1] := False;
  if caseRecord.T3Therapy then
    TherapyCheckGroup.Checked[2] := True
  else
    TherapyCheckGroup.Checked[2] := False;
end;

procedure THauptschirm.FormPaint(Sender: TObject);
begin

end;

procedure THauptschirm.FormDropFiles(Sender: TObject; const FileNames: array of string);
var
  status: integer;
begin
  ReadHL7Message(FileNames[0], Hauptschirm.caseRecord);
  CaseEditorForm.FillFromCaseRecord(caseRecord);
  InsertValues(Sender);
  HandleInput(status);
end;

procedure THauptschirm.FormWindowStateChange(Sender: TObject);
begin

end;

procedure THauptschirm.AppleMenuClick(Sender: TObject);
begin

end;

procedure THauptschirm.HelpItemClick(Sender: TObject);
begin
  HelpWindow.ShowOnTop;
end;

procedure THauptschirm.OpenMenuItemClick(Sender: TObject);
var
  status: integer;
begin
  ReadCaseResults(caseRecord);
  CaseEditorForm.FillFromCaseRecord(caseRecord);
  InsertValues(Sender);
  HandleInput(status);
end;

procedure THauptschirm.CheckEntries(Sender: TObject);
begin
  if ResultField.Lines.Count > 0 then
    PopupMenu1.Items[6].Enabled := True
  else
    PopupMenu1.Items[6].Enabled := False;
end;

procedure THauptschirm.SaveMenuItemClick(Sender: TObject);
begin
  CaseEditorForm.SetCaseRecord(caseRecord);
  SaveResults(caseRecord);
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
{ Inspired by Adnan Shameem's blog at }
{ http://lazplanet.blogspot.de/2013/05/clear-all-edit-boxes-in-form.html }
var
  i: integer;
begin
  NewCaseRecord(caseRecord);
  caseRecord.Placer := gPreferences.Placer_ID;
  for i := 0 to ComponentCount - 1 do
    if (Components[i] is TEdit) then
      TEdit(Components[i]).Text := '0';
  ActiveControl := TSH_Text;
  ResultField.Text := '';
  TherapyCheckGroup.Checked[0] := False;
  TherapyCheckGroup.Checked[1] := False;
  TherapyCheckGroup.Checked[2] := False;
  HintField.Text := gAnleitung1;
  HintField.Hint := gAnleitung1;
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

procedure PrinterWriteln(H: integer; var currentX, currentY: integer;
  theString: string; bold: boolean);
begin
  if bold then
    Printer.Canvas.Font.Style := [fsBold]
  else
    Printer.Canvas.Font.Style := [];
  Printer.Canvas.TextOut(currentX, currentY, theString);
  Inc(currentY, H);
end;

procedure PrinterWrite(H: integer; var currentX, currentY: integer;
  theString: string; bold: boolean);
begin
  if bold then
    Printer.Canvas.Font.Style := [fsBold]
  else
    Printer.Canvas.Font.Style := [];
  Printer.Canvas.TextOut(currentX, currentY, theString);
end;

procedure PrintCaption(H: integer; var currentX, currentY, rightMargin: integer);
var
  theSize, IDWidth: integer;
  IDPos: TPoint;
  slash: string[3];
begin
  { Print main header: }
  theSize := Printer.Canvas.Font.Size;
  Printer.Canvas.Font.Size := trunc(theSize * 1.7);
  PrinterWrite(H, currentX, currentY, 'SPINA Thyr Report', True);
  { Print hospital / physician / placer ID: }
  if gPreferences.MSH_ID <> '' then
  begin
    Printer.Canvas.Font.Size := theSize;
    IDWidth := Printer.Canvas.TextWidth(gPreferences.MSH_ID);
    IDPos.x := Printer.PageWidth - rightMargin - IDWidth;
    IDPos.y := currentY + H div 2;
    PrinterWrite(H, IDPos.x, IDPos.y, gPreferences.MSH_ID, True);
    Printer.Canvas.Font.Size := trunc(theSize * 1.7);
  end;
  PrinterWriteln(H, currentX, currentY, '', True);
  PrinterWriteln(H, currentX, currentY, '', True);
  PrinterWriteln(H, currentX, currentY, '', True);
  Printer.Canvas.MoveTo(currentX, currentY - H div 2);
  Printer.Canvas.LineTo(Printer.PageWidth - rightMargin, currentY - H div 2);
  PrinterWriteln(H, currentX, currentY, '', True);
  Printer.Canvas.Font.Style := [];
  Printer.Canvas.Font.Size := theSize;
  { Print bar code: }
  if Hauptschirm.caseRecord.CaseID <> '' then
  begin
    ;
    Hauptschirm.caseIDBarCode.Top := currentY;
    Hauptschirm.caseIDBarCode.Left := tabX2;
    Hauptschirm.caseIDBarCode.Typ := bcCode128B;
    Hauptschirm.caseIDBarCode.Modul := GetPoints(0.02, Printer.YDPI);
    Hauptschirm.caseIDBarCode.Ratio := 2.0;
    Hauptschirm.caseIDBarCode.Height := GetPoints(0.3, Printer.YDPI);
    Hauptschirm.caseIDBarCode.Text := Hauptschirm.caseRecord.CaseID;
    Hauptschirm.caseIDBarCode.DrawBarcode(Printer.Canvas);
  end;
  { Print case-specific entries: }
  if (Hauptschirm.caseRecord.PID <> '') and (Hauptschirm.caseRecord.CaseID <> '') then
    slash := ' / '
  else
    slash := '';
  if gInterfaceLanguage = German then
  begin
    PrinterWrite(H, currentX, currentY, kPID1, False);
    if (Hauptschirm.caseRecord.PID <> '') or (Hauptschirm.caseRecord.CaseID <> '') then
      PrinterWrite(H, tabX1, currentY, Hauptschirm.caseRecord.PID +
        slash + Hauptschirm.caseRecord.CaseID, False);
    PrinterWriteln(H, currentX, currentY, '', False);
    PrinterWrite(H, currentX, currentY, kPatientenname1, False);
    if (Hauptschirm.caseRecord.Name <> '') and
      (Hauptschirm.caseRecord.GivenNames <> '') then
      PrinterWrite(H, tabX1, currentY, Hauptschirm.caseRecord.Name +
        ', ' + Hauptschirm.caseRecord.GivenNames, False);
    PrinterWrite(H, tabX2, currentY, kEinsender1, False);
    PrinterWriteln(H, tabX3, currentY, Hauptschirm.caseRecord.Placer, False);
    PrinterWrite(H, currentX, currentY, kGeburtsdatum1, False);
    if not isNaN(Hauptschirm.caseRecord.DoBDate) then
      PrinterWrite(H, tabX1, currentY, DateToStr(Hauptschirm.caseRecord.DoBDate)
        , False);
    PrinterWrite(H, tabX2, currentY, kUntersuchungsdatum1, False);
    if not isNaN(Hauptschirm.caseRecord.OBDate) then
      PrinterWrite(H, tabX3, currentY, DateToStr(Hauptschirm.caseRecord.OBDate)
        , False);
    PrinterWriteln(H, currentX, currentY, '', False);
    PrinterWriteln(H, currentX, currentY, '', False);
    PrinterWriteln(H, currentX, currentY, '', False);
  end
  else
  begin
    PrinterWrite(H, currentX, currentY, kPID2, False);
    if (Hauptschirm.caseRecord.PID <> '') or (Hauptschirm.caseRecord.CaseID <> '') then
      PrinterWrite(H, tabX1, currentY, Hauptschirm.caseRecord.PID +
        slash + Hauptschirm.caseRecord.CaseID, False);
    PrinterWriteln(H, currentX, currentY, '', False);
    PrinterWrite(H, currentX, currentY, kPatientenname2, False);
    if (Hauptschirm.caseRecord.Name <> '') and
      (Hauptschirm.caseRecord.GivenNames <> '') then
      PrinterWrite(H, tabX1, currentY, Hauptschirm.caseRecord.Name +
        ', ' + Hauptschirm.caseRecord.GivenNames, False);
    PrinterWrite(H, tabX2, currentY, kEinsender2, False);
    PrinterWriteln(H, tabX3, currentY, Hauptschirm.caseRecord.Placer, False);
    PrinterWrite(H, currentX, currentY, kGeburtsdatum2, False);
    if not isNaN(Hauptschirm.caseRecord.DoBDate) then
      PrinterWrite(H, tabX1, currentY, DateToStr(Hauptschirm.caseRecord.DoBDate)
        , False);
    PrinterWrite(H, tabX2, currentY, kUntersuchungsdatum2, False);
    if not isNaN(Hauptschirm.caseRecord.OBDate) then
      PrinterWrite(H, tabX3, currentY, DateToStr(Hauptschirm.caseRecord.OBDate)
        , False);
    PrinterWriteln(H, currentX, currentY, '', False);
    PrinterWriteln(H, currentX, currentY, '', False);
    PrinterWriteln(H, currentX, currentY, '', False);
  end;
  Printer.Canvas.MoveTo(currentX, currentY - H div 2);
  Printer.Canvas.LineTo(Printer.PageWidth - rightMargin, currentY - H div 2);
  PrinterWriteln(H, currentX, currentY, '', False);
end;

procedure PrintFooter(H: integer; var currentX, currentY, rightMargin: integer);
var
  theDate, theTime: string;
begin
  DateTimeToString(theDate, 'dddd"," dd mmmm yyyy', date);
  DateTimeToString(theTime, '"," t', time);
  theDate := SysToUTF8(theDate);
  PrinterWriteln(H, currentX, currentY, '', False);
  Printer.Canvas.MoveTo(currentX, currentY - H div 2);
  Printer.Canvas.LineTo(Printer.PageWidth - rightMargin, currentY - H div 2);
  Printer.Canvas.Font.Color := clGray;
  PrinterWriteln(H, currentX, currentY, concat(gBenutzername, gUserName,
    '  |  ', gDruckdatum, theDate, theTime), False);
  PrinterWriteln(H, currentX, currentY, 'SPINA Thyr ' + GetFileVersion, False);
  PrinterWriteln(H, currentX, currentY, '', False);
  Printer.Canvas.Font.Color := clBlack;
end;

procedure THauptschirm.PrintMenuItemClick(Sender: TObject);
var
  H, ADPI, marginX, marginXr, currentX, currentY, lastY, returnPos, lastPos: integer;
  resultLine, remainder: string;
begin
  assert(assigned(Printer));
  if DoPrintSetup then
  begin
    CaseEditorForm.SetCaseRecord(Hauptschirm.caseRecord);
    caseIDBarCode := TBarcode.Create(self);
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
    //Printer.FileName := Printer.Title;
    currentX := marginX;
    Printer.BeginDoc;
    try
      Printer.Canvas.Font := HintField.Font;
      Printer.Canvas.Font.Size := 9;
      Printer.Canvas.Font.Style := [];
      Printer.Canvas.Pen.Color := clBlack;
      Printer.Canvas.Pen.Width := 2;
      H := (Printer.Canvas.TextHeight('X') + gLineSpacing);
      tabX1 := marginX + Printer.Canvas.TextWidth(gGeburtsdatum) + GetPoints(1, ADPI);
      tabX2 := Printer.PageWidth - marginXr -
        trunc(2.5 * Printer.Canvas.TextWidth(gUntersuchungsdatum));
      if gInterfaceLanguage = German then
        tabX3 := tabX2 + Printer.Canvas.TextWidth(kFallnummer1) + GetPoints(0.5, ADPI)
      else
        tabX3 := tabX2 + Printer.Canvas.TextWidth(kFallnummer2) + GetPoints(0.5, ADPI);
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
        PrinterWriteln(H, currentX, currentY, resultLine, False);
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
        PrinterWriteln(H, tabX2, currentY, resultLine, False);
      until returnPos = 0;
      currentX := marginX;
      currentY := Printer.PageHeight - 5 * H;
      PrintFooter(H, currentX, currentY, marginXr);
      Printer.EndDoc;
      caseIDBarCode.Destroy;
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

  gInterfaceLanguage := German;

  gPrefsDir := GetPreferencesFolder;
  gPrefsFileName := GetPreferencesFile;

  gAppName := ApplicationName;
  gAppPath := Application.Location;

  gSysLanguage := GetOSLanguage;
  {$IFDEF WINDOWS}
  UserNameSize := MaxLen;
  {$IFNDEF WINCE}
  if Win32MajorVersion <= 4 then
  begin
    SetLength(gUserName, MaxLen);
    if Windows.GetUserName(@gUserName[1], UserNameSize) then
    begin
      SetLength(gUserName, UserNameSize - 1);
      gUserName := SysToUtf8(gUserName);
    end
    else
      gUserName := '';
  end
  else
  {$ENDIF NOT WINCE}
  begin
    SetLength(wUserName, MaxLen - 1);
    if Windows.GetUserNameW(@wUserName[1], UserNameSize) then
    begin
      SetLength(wUserName, UserNameSize - 1);
      gUserName := Utf16ToUtf8(wUserName);
    end
    else
      gUserName := '';
  end;
  {$ENDIF WINDOWS}
  {$IFDEF UNIX}
  {$IF (DEFINED(LINUX)) OR (DEFINED(FREEBSD))}
  gUserName := SysToUtf8(GetUserName(fpgetuid));
  {$ENDIF}
  if gUserName = '' then
    gUserName := GetEnvironmentVariableUTF8('USER');
  {$ENDIF}

  if gSysLanguage = 'de' then
  begin
    gInterfaceLanguage := German;
    DefaultFormatSettings.DecimalSeparator := DEC_COMMA;
  end
  else
  begin
    gInterfaceLanguage := English;
    DefaultFormatSettings.DecimalSeparator := DEC_POINT;
  end;

finalization

end.
