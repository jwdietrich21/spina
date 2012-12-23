{ SPINA-Thyr }

{ Application for calculating structure parameters }
{ of thyrotropic feedback control }

{ Programm zur Berechnung von Strukturparametern }
{ des thyreotropen Regelkreises }

{ Version 3.3 }

{ (c) J. W. Dietrich, 1994 - 2012 }
{ (c) Ludwig Maximilian University of Munich 1995 - 2002 }
{ (c) University of Ulm Hospitals 2002-2004 }
{ (c) Ruhr University of Bochum 2005 - 2012 }

{ This unit implements the GUI }

{ Source code released under the BSD License }
{ See http://spina.medical-cybernetics.de for details }

unit SPINA_UserInterface;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, StdActns, StdCtrls, LCLType, Menus, ActnList, VersionSupport,
  gettext, SPINA_Types, SPINA_Engine, SPINA_AboutBox, SPINA_ResultDialog,
  HandlePreferences
  {$IFDEF win32}
  , Windows
  {$ELSE}
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
    MainMenu: TMainMenu;
    ActionList1: TActionList;
    Panel1: TPanel;
    SPINAThyrLabel: TLabel;
    TherapyCheckGroup: TCheckGroup;
    EditMenu: TMenuItem;
    Calculate_Button: TButton;
    FileMenu: TMenuItem;
    EditCopy1: TEditCopy;
    EditCut1: TEditCut;
    EditDelete1: TEditDelete;
    EditPaste1: TEditPaste;
    EditUndo1: TEditUndo;
    FT3ComboBox: TComboBox;
    FT4ComboBox: TComboBox;
    ValuesGroupBox: TGroupBox;
    HintGroupBox: TGroupBox;
    ResultGroupBox: TGroupBox;
    Image1: TImage;
    Image2: TImage;
    ImageList1: TImageList;
    ffnen1: TMenuItem;
    HelpMenu: TMenuItem;
    AboutMenuItem: TMenuItem;
    AppleMenu: TMenuItem;
    MenuItem10: TMenuItem;
    AppleAboutMenuItem: TMenuItem;
    MenuItem3: TMenuItem;
    ResultField: TMemo;
    PageSetupDialog1: TPageSetupDialog;
    PrintDialog1: TPrintDialog;
    PrinterSetupDialog1: TPrinterSetupDialog;
    PrintMenuItem: TMenuItem;
    PageSetupMenuItem: TMenuItem;
    MenuItem13: TMenuItem;
    QuitMenuItem: TMenuItem;
    DeleteMenuItem: TMenuItem;
    MenuItem16: TMenuItem;
    CopyResultMenuItem: TMenuItem;
    NewMenuItem: TMenuItem;
    UndoMenuItem: TMenuItem;
    MenuItem5: TMenuItem;
    CutMenuItem: TMenuItem;
    CopyMenuItem: TMenuItem;
    PasteMenuItem: TMenuItem;
    CloseMenuItem: TMenuItem;
    Schlieen1: TMenuItem;
    N1: TMenuItem;
    Beenden1: TMenuItem;
    Rckgngig1: TMenuItem;
    N2: TMenuItem;
    Ausschneiden1: TMenuItem;
    Kopieren1: TMenuItem;
    Einfgen1: TMenuItem;
    Lschen1: TMenuItem;
    N3: TMenuItem;
    Ergebniskopieren1: TMenuItem;
    N4: TMenuItem;
    berSPINAThyr1: TMenuItem;
    Hintergrundbild: TImage;
    T3MethodComboBox: TComboBox;
    TSH_Text: TEdit;
    FT3_Text: TEdit;
    FT4_Text: TEdit;
    HintField: TMemo;
    TSHComboBox: TComboBox;
    Logo: TImage;
    Drucken1: TMenuItem;
    Seiteneinrichtung1: TMenuItem;
    N5: TMenuItem;
    N6: TMenuItem;
    SPINAThyrHilfe1: TMenuItem;
    T4MethodComboBox: TComboBox;
    FT4Items: TComboBox;
    T4Items: TComboBox;
    FT3Items: TComboBox;
    T3Items: TComboBox;
    procedure Beenden1Click(Sender: TObject);
    procedure Calculate_ButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure Ergebniskopieren1Click(Sender: TObject);
    procedure FT4ItemsChange(Sender: TObject);
    procedure HandleAbout(Sender: TObject);
    procedure AppleMenuClick(Sender: TObject);
    procedure NewMenuItemClick(Sender: TObject);
    procedure MenuItem4Click(Sender: TObject);
    procedure CloseMenuItemClick(Sender: TObject);
    procedure PageSetupMenuItemClick(Sender: TObject);
    procedure PrintMenuItemClick(Sender: TObject);
    procedure T3ItemsChange(Sender: TObject);
    procedure TSHComboBoxChange(Sender: TObject);
    procedure FT3ComboBoxChange(Sender: TObject);
    procedure FT4ComboBoxChange(Sender: TObject);
    procedure T4MethodComboBoxAdjust(Sender: TObject);
    procedure T3MethodComboBoxAdjust(Sender: TObject);
    procedure T4MethodComboBoxChange(Sender: TObject);
    procedure T3MethodComboBoxChange(Sender: TObject);
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
{$IFDEF LCLCarbon}
  gItl0Handle: Intl0Hndl;
  gRegion: integer;
{$ENDIF}
  gTopMargin, gBottomMargin, gLeftMargin, gRightMargin: double;
  gLineSpacing: integer;

procedure AdaptMenus;

implementation

uses SPINA_SplashScreen;

function GetOSLanguage: string;
var
  l, fbl: String;
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
      GetLanguageIDs(l,fbl);
    {$ENDIF}
  {$ENDIF}
  Result := fbl;
end;

procedure AdjustUnitFactors;
begin
  if Hauptschirm.T4MethodComboBox.Text = 'FT4' then
  begin
    if Hauptschirm.FT4ComboBox.Text = 'ng/dl' then
      gT4UnitFactor := 1
    else if Hauptschirm.FT4ComboBox.Text = 'ng/l' then
      gT4UnitFactor := 0.1
    else if Hauptschirm.FT4ComboBox.Text = 'pmol/l' then
      gT4UnitFactor := 1 / UFT4 / 1e12;
  end
  else if Hauptschirm.T4MethodComboBox.Text = 'T4' then
  begin
    if Hauptschirm.FT4ComboBox.Text = 'µg/l' then
      gT4UnitFactor := 100
    else if Hauptschirm.FT4ComboBox.Text = 'µg/dl' then
      gT4UnitFactor := 1000
    else if Hauptschirm.FT4ComboBox.Text = 'nmol/l' then
      gT4UnitFactor := 1 / UFT4 / 1e9;
  end;
  if Hauptschirm.T3MethodComboBox.Text = 'FT3' then
  begin
    if Hauptschirm.FT3ComboBox.Text = 'pg/ml' then
      gT3UnitFactor := 1
    else if Hauptschirm.FT3ComboBox.Text = 'ng/l' then
      gT3UnitFactor := 1
    else if Hauptschirm.FT3ComboBox.Text = 'pmol/l' then
      gT3UnitFactor := 1 / UFT3 / 1e12;
  end
  else if Hauptschirm.T3MethodComboBox.Text = 'T3' then
  begin
    if Hauptschirm.FT3ComboBox.Text = 'µg/l' then
      gT3UnitFactor := 1000
    else if Hauptschirm.FT3ComboBox.Text = 'ng/dl' then
      gT3UnitFactor := 10
    else if Hauptschirm.FT3ComboBox.Text = 'nmol/l' then
      gT3UnitFactor := 1 / UFT3 / 1e9;
  end;
  if Hauptschirm.TSHComboBox.Text = 'mU/l' then
    gTSHUnitFactor := 1
  else
    gTSHUnitFactor := 1;
  gTSHUnit := Hauptschirm.TSHComboBox.Text;
  gT4Unit := Hauptschirm.FT4ComboBox.Text;
  gT3Unit := Hauptschirm.FT3ComboBox.Text;
  with gPreferences do
  begin
    TSHUnitFactor := gTSHUnitFactor;
    T4UnitFactor := gT4UnitFactor;
    T3UnitFactor := gT3UnitFactor;
  end;
end;

procedure THauptschirm.T4MethodComboBoxAdjust(Sender: TObject);
begin
  if Hauptschirm.T4MethodComboBox.Text = 'FT4' then
  begin
    gPreferences.T4Method := freeHormone;
    Hauptschirm.FT4ComboBox.Items.Assign(FT4Items.Items);
    Hauptschirm.FT4ComboBox.Text :=
      Hauptschirm.FT4ComboBox.Items.Strings[gPreferences.T4PopUpItem];
  end
  else if Hauptschirm.T4MethodComboBox.Text = 'T4' then
  begin
    gPreferences.T4Method := totalHormone;
    Hauptschirm.FT4ComboBox.Items.Assign(T4Items.Items);
    Hauptschirm.FT4ComboBox.Text :=
      Hauptschirm.FT4ComboBox.Items.Strings[gPreferences.T4PopUpItem];
  end;
  AdjustUnitFactors;
end;

procedure THauptschirm.T3MethodComboBoxAdjust(Sender: TObject);
begin
  if Hauptschirm.T3MethodComboBox.Text = 'FT3' then
  begin
    gPreferences.T3Method := freeHormone;
    Hauptschirm.FT3ComboBox.Items.Assign(FT3Items.Items);
    Hauptschirm.FT3ComboBox.Text :=
      Hauptschirm.FT3ComboBox.Items.Strings[gPreferences.T3PopUpItem];
  end
  else if Hauptschirm.T3MethodComboBox.Text = 'T3' then
  begin
    gPreferences.T3Method := totalHormone;
    Hauptschirm.FT3ComboBox.Items.Assign(T3Items.Items);
    Hauptschirm.FT3ComboBox.Text :=
      Hauptschirm.FT3ComboBox.Items.Strings[gPreferences.T3PopUpItem];
  end;
  AdjustUnitFactors;
end;

{ THauptschirm }

procedure GetPreferences;
begin
  ReadPreferences;
  with gPreferences do
  begin
    Hauptschirm.T4MethodComboBox.ItemIndex := T4MethodPopUpItem;
    Hauptschirm.T3MethodComboBox.ItemIndex := T3MethodPopUpItem;
    Hauptschirm.T4MethodComboBoxAdjust(Hauptschirm);
    Hauptschirm.T3MethodComboBoxAdjust(Hauptschirm);
    Hauptschirm.TSHComboBox.ItemIndex := TSHPopUpItem;
    Hauptschirm.FT4ComboBox.ItemIndex := T4PopUpItem;
    Hauptschirm.FT3ComboBox.ItemIndex := T3PopUpItem;
    gTSHUnitFactor := TSHUnitFactor;
    gT4UnitFactor := T4UnitFactor;
    gT3UnitFactor := T3UnitFactor;
  end;
end;

procedure THauptschirm.Beenden1Click(Sender: TObject);
begin
  application.Terminate;
end;

procedure ShowMessage;
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
  T4Label, T3Label, sT4Label, sT3Label: Str255;
begin
  if gPreferences.T4Method = freeHormone then
  begin
    T4Label := kFT4_Label;
    sT4Label := ksFT4_Label;
  end
  else
  begin
    T4Label := kT4_Label;
    sT4Label := ksT4_Label;
  end;
  if gPreferences.T3Method = freeHormone then
  begin
    T3Label := kFT3_Label;
    sT3Label := ksFT3_Label;
  end
  else
  begin
    T3Label := kT3_Label;
    sT3Label := ksT3_Label;
  end;
  vhString := concat(gVerhaltensparameter, kCR, kLF, '   TSH: ', TSH_String,
    ' ', gTSHUnit, kCR, kLF, T4Label, T4_String, ' ', gT4Unit, kCR, kLF, T3Label,
    T3_String, ' ', gT3Unit);
  theString := concat(vhString, kCR, kLF, kCR, kLF, gStrukturparameter, kCR, kLF, gMessageString);
  Hauptschirm.ResultField.Text := theString;
  gResultString := theString;
  gResultDialogString1 := concat('TSH: ', TSH_String, ' ', gTSHUnit, kCR,
    kLF, sT4Label, T4_String, ' ', gT4Unit, kCR, kLF, sT3Label, T3_String, ' ', gT3Unit);
  gResultDialogString2 := concat(gMessageString);
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
  s := Shell('echo -ne ''\007''');
  {s := fpSystem('echo -ne "\a"');}
  {s := fpSystem('tput bel');}
  {beep;}
    {$ENDIF}
  {$ENDIF}
end;

function ValidKey(theKey: char): boolean;
  {Beschr??nkt Texteingabe auf zul??ssige Zeichen}
type
  format1 = set of char;
var
  theformatn, theformatd, theformate, theformato, theformatc, thevalidformat: format1;
begin
  ValidKey := False;
  theformatn := ['1'..'9', '0', kTAB];
  theformatd := ['.', ','];
  theformate := ['e', 'E'];
  theformato := ['+', '-'];
  theformatc := [char($1c), char($1d), char($1e), char($1f), char($08)];
  thevalidformat := theformatn + theformatd + theformate + theformatc;
  if thekey in thevalidformat then
    ValidKey := True
  else
    bell;
end;

procedure HandleInput;
var
  Buffer: PChar;
  Size: byte;
  resultRecord: tCaseRecord;
begin
  try
    Size := Hauptschirm.TSH_Text.GetTextLen;
    {L??nge des Strings in Edit1 ermitteln}
    if Size = 0 then                                      {Feld leer?}
      TSH := 0
    else
    begin
      Inc(Size);
      {Platz f??r NULL-Zeichen hinzuf??gen}
      GetMem(Buffer, Size);
      {Buffer als dynamische Variable definieren}
      Hauptschirm.TSH_Text.GetTextBuf(Buffer, Size);     {Edit1.Text in Buffer ablegen}
      TSH_String := StrPas(Buffer);
      FreeMem(Buffer, Size);                            {Speicher von Buffer freigeben}
      TSH := StrToFloat(TSH_String);
      TSH := TSH * gTSHUnitFactor;
    end;
    TSH_String := FloatToStrF(TSH, ffFixed, 3, 2);
    Size := Hauptschirm.FT4_Text.GetTextLen;
    {L??nge des Strings in Edit1 ermitteln}
    if Size = 0 then                                      {Feld leer?}
      T4 := 0
    else
    begin
      Inc(Size);
      {Platz f??r NULL-Zeichen hinzuf??gen}
      GetMem(Buffer, Size);
      {Buffer als dynamische Variable definieren}
      Hauptschirm.FT4_Text.GetTextBuf(Buffer, Size);     {Edit1.Text in Buffer ablegen}
      T4_String := StrPas(Buffer);
      FreeMem(Buffer, Size);                            {Speicher von Buffer freigeben}
      T4 := StrToFloat(T4_String);
    end;
    T4_String := FloatToStrF(T4, ffFixed, 3, 2);
    T4 := T4 * UFT4;
    T4 := T4 * gT4UnitFactor;
    Size := Hauptschirm.FT3_Text.GetTextLen;
    {L??nge des Strings in Edit1 ermitteln}
    if Size = 0 then                                      {Feld leer?}
      T3 := 0
    else
    begin
      Inc(Size);
      {Platz f??r NULL-Zeichen hinzuf??gen}
      GetMem(Buffer, Size);
      {Buffer als dynamische Variable definieren}
      Hauptschirm.FT3_Text.GetTextBuf(Buffer, Size);     {Edit1.Text in Buffer ablegen}
      T3_String := StrPas(Buffer);
      FreeMem(Buffer, Size);                            {Speicher von Buffer freigeben}
      T3 := StrToFloat(T3_String);
    end;
    T3_String := FloatToStrF(T3, ffFixed, 3, 2);
    T3 := T3 * UFT3;
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
      TSH := 0;
      T4 := 0;
      T3 := 0;
    end;
  end;
  resultRecord := Calculate(TSH, T4, T3);
  gMessageString := concat('   GT: ', resultRecord.GTs, kCR, kLF, '   GD: ', resultRecord.GDs);
  ShowMessage;
end;

procedure THauptschirm.Calculate_ButtonClick(Sender: TObject);
begin
  HandleInput;
  ResultForm.Visible := True;
  ResultForm.AlphaBlendValue := 220;
end;

procedure AdaptMenus;
var
  modifierKey: TShiftState;
  theForm: TForm;
begin
  {$IFDEF LCLcarbon}
  modifierKey := [ssMeta];
  Hauptschirm.HelpMenu.Visible := False;
  Hauptschirm.AppleMenu.Visible := True;
  {$ELSE}
  modifierKey := [ssCtrl];
  Hauptschirm.HelpMenu.Visible := True;
  Hauptschirm.AppleMenu.Visible := False;
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
begin
  AdjustUnitFactors;
  Hauptschirm.HorzScrollBar.Visible := False;
  Hauptschirm.VertScrollBar.Visible := False;
  Hauptschirm.AutoScroll := False;
  Hauptschirm.SPINAThyrLabel.Caption := 'SPINA Thyr ' + GetFileVersion;
  GetPreferences;
end;

procedure THauptschirm.Ergebniskopieren1Click(Sender: TObject);
begin
  Hauptschirm.ResultField.SelectAll;
  Hauptschirm.ResultField.CopyToClipboard;
end;

procedure THauptschirm.FT4ItemsChange(Sender: TObject);
begin

end;

procedure THauptschirm.TSHComboBoxChange(Sender: TObject);
begin
  gPreferences.TSHPopUpItem := Hauptschirm.TSHComboBox.ItemIndex;
  AdjustUnitFactors;
end;

procedure THauptschirm.FT3ComboBoxChange(Sender: TObject);
begin
  gPreferences.T3PopUpItem := Hauptschirm.FT3ComboBox.ItemIndex;
  AdjustUnitFactors;
end;

procedure THauptschirm.FT4ComboBoxChange(Sender: TObject);
begin
  gPreferences.T4PopUpItem := Hauptschirm.FT4ComboBox.ItemIndex;
  AdjustUnitFactors;
end;

procedure THauptschirm.FormActivate(Sender: TObject);
begin
  if gStartup then
  begin
    Hauptschirm.TSH_Text.SetFocus;
    Hauptschirm.VertScrollBar.Visible := False;
    Hauptschirm.AutoScroll := False;
  end;
end;


procedure THauptschirm.HandleAbout(Sender: TObject);
begin
  ShowAboutBox;
end;

procedure THauptschirm.AppleMenuClick(Sender: TObject);
begin

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
  tabX: integer;
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
  Printer.Canvas.Font.Style := [];
  Printer.Canvas.Font.Size := theSize;
  tabX := Printer.PageWidth - rightMargin - 200;
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
  H, ADPI, marginX, marginXr, currentX, currentY, returnPos, lastPos: integer;
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
      PrintCaption(H, currentX, currentY, marginXr);
      lastPos := 1;
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
  gPreferences.T4MethodPopUpItem := Hauptschirm.T4MethodComboBox.ItemIndex;
  gPreferences.T4PopUpItem := 0;
  Hauptschirm.T4MethodComboBoxAdjust(Sender);
end;

procedure THauptschirm.T3MethodComboBoxChange(Sender: TObject);
begin
  gPreferences.T3MethodPopUpItem := Hauptschirm.T3MethodComboBox.ItemIndex;
  gPreferences.T3PopUpItem := 0;
  Hauptschirm.T3MethodComboBoxAdjust(Sender);
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

