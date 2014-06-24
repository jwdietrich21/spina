{ SPINA-Thyr }
{ Programm zur Berechnung von Strukturparametern }
{ des thyreotropen Regelkreises }
{ Version 3.2 }

{ J. W. Dietrich, Klinikum der LMU München 1997-2001 }
{ J. W. Dietrich, Universitätsklinikum Ulm 2002-2004 }
{ J. W. Dietrich, Universitätsklinikum Bergmannsheil 2005-2010 }

{ This software is provided via a BSD licence }
{ See http://spina.medical-cybernetics.de for details }

unit SPINA_UserInterface;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, StdActns, StdCtrls, LCLType, Menus, ActnList, SPINA_Engine, SPINA_AboutBox, SPINA_ResultDialog
  {$IFDEF win32}
  , Windows
  {$ELSE}
    {$IFDEF LCLCarbon}
      , MacOSAll
    {$ENDIF}
  {$ENDIF}
  , Printers, ComCtrls, PrintersDlgs;

const
kTAB=chr(9);
kLF=chr(10);
kCR=chr(13);
DEC_POINT='.';
DEC_COMMA=',';
TEXT_WIDTH=10;
TSH_UNIT=' mU/l';
FT4_UNIT=' ng/dl';
FT3_UNIT=' pg/ml';
kAnleitung1='Bitte geben Sie die gemessenen Werte für TSH, T4 (oder FT4) und T3 (oder FT3) ein und klicken Sie dann auf "Berechnen".';
kAnleitung2='Please enter simultaneously obtained values for TSH, T4 (or FT4) and T3 (or FT3), and click on "Calculate".';
kVerhaltensparameter1='Verhaltensparameter:';
kVerhaltensparameter2='Behavioural parameters:';
kStrukturparameter1='Strukturparameter:';
kStrukturparameter2='Structure parameters:';
kReferenzbereiche1='Referenzbereiche:'#13#10'   GT~: 1,41 - 8,67 pmol/s'#13#10'   GD~: 20,4-39,4 nmol/s';
kReferenzbereiche2='Reference ranges:'#13#10'   GT~: 1,41 - 8,67 pmol/s'#13#10'   GD~: 20,4-39,4 nmol/s';
kNotCalculatable1='<Nicht berechenbar>';
kNotCalculatable2='<Not calculatable>';
kPatientenname1='Patientenname: ';
kPatientenname2='Patient name: ';
kUntersuchungsdatum1='Untersuchungsdatum: ';
kUntersuchungsdatum2='Examination Date: ';
kGeburtsdatum1='Geburtsdatum: ';
kGeburtsdatum2='Birth date: ';
kEinsender1='Einsender: ';
kEinsender2='Sender: ';
kDruckdatum1='Druckdatum: ';
kDruckdatum2='Printing Date: ';
kBenutzername1='Benutzerkennung: ';
kBenutzername2='User name: ';
kResultHint1='Ergebnis:';
kResultHint2='Result:';
kMarginSpaces='                                    ';
AnInch = 2.54;


type
  tInterfaceLanguage = (English, German);
  tPrefsFile = file of tPreferences;

  { THauptschirm }

  THauptschirm = class(TForm)
    ActionList1: TActionList;
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
    HintResultGroupBox: TGroupBox;
    Image1: TImage;
    Image2: TImage;
    ImageList1: TImageList;
    MainMenu: TMainMenu;
    ffnen1: TMenuItem;
    HelpMenu: TMenuItem;
    AboutMenuItem: TMenuItem;
    AppleMenu: TMenuItem;
    MenuItem10: TMenuItem;
    AppleAboutMenuItem: TMenuItem;
    MenuItem3: TMenuItem;
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
    MessageField: TMemo;
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
  Startup:Boolean;
  gInterfaceLanguage: tInterfaceLanguage;
  Language, UserName: array[0..128] of char;
  gSysLanguage, gUserName: String;
  arraySize: DWord;
  gAnleitung, gVerhaltensparameter, gStrukturparameter: String;
  gResultHint, gBenutzername: String;
  gPatientenname, gGeburtsdatum, gUntersuchungsdatum, gEinsender, gDruckdatum: String;
  gTSHUnitFactor, gT4UnitFactor, gT3UnitFactor: real;
  gcalcTitle, gcalcString, gnotcalculatableString: Str255;
  gExplanationString, gMessageString, TSH_String, T4_String, T3_String: Str255;
  gRefExp, gGTRef, gGDRef, gSignalString, gParameterString: Str255;
  gTSHUnit, gT4Unit, gT3Unit, gResultString: Str255;
  gcalcCounter: longint;
  gPrefsFile: tPrefsFile;
  gResultDialogString1, gResultDialogString2: Str255;
  gPrefsDir, gPrefsFileName: String;
  gAppPath, gAppDir, gAppName: String;
  gSysLocale: TSysLocale;
  gGermanCodes: tCodeList;
{$IFDEF LCLCarbon}
  gItl0Handle: Intl0Hndl;
  gRegion: integer;
{$ENDIF}
  gTopMargin, gBottomMargin, gLeftMargin, gRightMargin: Double;
  gLineSpacing: integer;


procedure SavePreferences;

implementation

function GetOSLanguage: string;
  var
    {$IFDEF win32}
    LanguageID:LangID;
    {$ENDIF}
    Len: Integer;
  begin
    {$IFDEF win32}
    SetLength(Result, 255);
    LanguageID:=GetSystemDefaultLangID;
    Len:=VerLanguageName(LanguageID,PChar(Result), Length(Result));
    SetLength(Result, Len);
    {$ELSE}
           {$IFDEF LCLCarbon}
           gItl0Handle := Intl0Hndl(GetIntlResource(0));
	   gRegion := BitAnd(gItl0Handle^^.intl0Vers, $FF00);
	   gRegion := BitShift(gRegion, -8);
           if gRegion in gGermanCodes then
              Result:='Deutsch'
           else
              Result:='English';
           {$ELSE}
           Result:='English';
           {$ENDIF}
    {$ENDIF}
    gSysLocale:=SysLocale;
  end;

procedure AdjustUnitFactors;
begin
if Hauptschirm.T4MethodComboBox.text='FT4'
then
  begin
  if Hauptschirm.FT4ComboBox.text='ng/dl'
  then
    gT4UnitFactor:=1
  else if Hauptschirm.FT4ComboBox.text='ng/l'
  then
    gT4UnitFactor:=0.1
  else if Hauptschirm.FT4ComboBox.text='pmol/l'
  then
    gT4UnitFactor:=1/UFT4/1e12;
  end
else if Hauptschirm.T4MethodComboBox.text='T4'
then
  begin
  if Hauptschirm.FT4ComboBox.text='µg/l'
  then
    gT4UnitFactor:=100
  else if Hauptschirm.FT4ComboBox.text='µg/dl'
  then
    gT4UnitFactor:=1000
  else if Hauptschirm.FT4ComboBox.text='nmol/l'
  then
    gT4UnitFactor:=1/UFT4/1e9;
  end;
if Hauptschirm.T3MethodComboBox.text='FT3'
then
  begin
  if Hauptschirm.FT3ComboBox.text='pg/ml'
  then
    gT3UnitFactor:=1
  else if Hauptschirm.FT3ComboBox.text='ng/l'
  then
    gT3UnitFactor:=1
  else if Hauptschirm.FT3ComboBox.text='pmol/l'
  then
    gT3UnitFactor:=1/UFT3/1e12;
  end
else if Hauptschirm.T3MethodComboBox.text='T3'
then
  begin
  if Hauptschirm.FT3ComboBox.text='µg/l'
  then
    gT3UnitFactor:=1000
  else if Hauptschirm.FT3ComboBox.text='ng/dl'
  then
    gT3UnitFactor:=10
  else if Hauptschirm.FT3ComboBox.text='nmol/l'
  then
    gT3UnitFactor:=1/UFT3/1e9;
  end;
if Hauptschirm.TSHComboBox.text='mU/l'
then
   gTSHUnitFactor:=1
else
   gTSHUnitFactor:=1;
gTSHUnit := Hauptschirm.TSHComboBox.text;
gT4Unit := Hauptschirm.FT4ComboBox.text;
gT3Unit := Hauptschirm.FT3ComboBox.text;
with gPreferences do
  begin
    TSHUnitFactor := gTSHUnitFactor;
    T4UnitFactor := gT4UnitFactor;
    T3UnitFactor := gT3UnitFactor;
  end;
end;

procedure THauptschirm.T4MethodComboBoxAdjust(Sender: TObject);
begin
if Hauptschirm.T4MethodComboBox.text='FT4'
then
begin
gPreferences.T4Method := freeHormone;
Hauptschirm.FT4ComboBox.Items.Assign(FT4Items.Items);
Hauptschirm.FT4ComboBox.text:=Hauptschirm.FT4ComboBox.Items.Strings[gPreferences.T4PopUpItem];
end
else if Hauptschirm.T4MethodComboBox.text='T4'
then
begin
gPreferences.T4Method := totalHormone;
Hauptschirm.FT4ComboBox.Items.Assign(T4Items.Items);
Hauptschirm.FT4ComboBox.text:=Hauptschirm.FT4ComboBox.Items.Strings[gPreferences.T4PopUpItem];
end;
AdjustUnitFactors;
end;

procedure THauptschirm.T3MethodComboBoxAdjust(Sender: TObject);
begin
if Hauptschirm.T3MethodComboBox.text='FT3'
then
begin
gPreferences.T3Method := freeHormone;
Hauptschirm.FT3ComboBox.Items.Assign(FT3Items.Items);
Hauptschirm.FT3ComboBox.text:=Hauptschirm.FT3ComboBox.Items.Strings[gPreferences.T3PopUpItem];
end
else if Hauptschirm.T3MethodComboBox.text='T3'
then
begin
gPreferences.T3Method := totalHormone;
Hauptschirm.FT3ComboBox.Items.Assign(T3Items.Items);
Hauptschirm.FT3ComboBox.text:=Hauptschirm.FT3ComboBox.Items.Strings[gPreferences.T3PopUpItem];
end;
AdjustUnitFactors;
end;

{ THauptschirm }

procedure SavePreferences;
begin
  CreateDir(gPrefsDir);
  assignFile(gPrefsFile, gPrefsFileName);
  rewrite(gPrefsFile);
  write(gPrefsFile, gPreferences);
  CloseFile(gPrefsFile);
end;

procedure GetPreferences;
begin
  assignFile(gPrefsFile, gPrefsFileName);
  try
    reset(gPrefsFile);
    read(gPrefsFile,gPreferences);
    CloseFile(gPrefsFile);
    with gPreferences do
      begin
        Hauptschirm.T4MethodComboBox.ItemIndex := T4MethodPopUpItem;
        Hauptschirm.T3MethodComboBox.ItemIndex := T3MethodPopUpItem;
        Hauptschirm.T4MethodComboBoxAdjust(Hauptschirm);
        Hauptschirm.T3MethodComboBoxAdjust(Hauptschirm);
        Hauptschirm.TSHComboBox.ItemIndex := TSHPopUpItem;
        Hauptschirm.FT4ComboBox.ItemIndex := T4PopUpItem;
        Hauptschirm.FT3ComboBox.ItemIndex := T3PopUpItem;
        gTSHUnitFactor:=TSHUnitFactor;
        gT4UnitFactor:=T4UnitFactor;
        gT3UnitFactor:=T3UnitFactor;
      end;
    except
  on Ex: EInOutError do
    begin
      with gPreferences do
      begin
        T4Method:=freeHormone;
        T3Method:=freeHormone;
        TSHUnitFactor:=1;
        T4UnitFactor:=1;
        T3UnitFactor:=1;
        Hauptschirm.TSHComboBox.ItemIndex := 1;
        Hauptschirm.FT4ComboBox.ItemIndex := 1;
        Hauptschirm.FT3ComboBox.ItemIndex := 1;
        Hauptschirm.T4MethodComboBox.ItemIndex := 1;
        Hauptschirm.T3MethodComboBox.ItemIndex := 1;
      end;
    end;
  end;
end;

procedure THauptschirm.Beenden1Click(Sender: TObject);
begin
  {SavePreferences;}
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
theString,vhString: Str255;
T4Label,T3Label,sT4Label,sT3Label: Str255;
begin
if gPreferences.T4Method = freeHormone
then
  begin
  T4Label := kFT4_Label;
  sT4Label := ksFT4_Label;
  end
else
  begin
  T4Label := kT4_Label;
  sT4Label := ksT4_Label;
  end;
if gPreferences.T3Method = freeHormone
then
  begin
  T3Label := kFT3_Label;
  sT3Label := ksFT3_Label;
  end
else
  begin
  T3Label := kT3_Label;
  sT3Label := ksT3_Label;
  end;
  vhString:=concat(gVerhaltensparameter,kCR,kLF,'   TSH: ',TSH_String,' ',gTSHUnit,kCR,kLF,T4Label,T4_String,' ',gT4Unit,kCR,kLF,T3Label,T3_String,' ',gT3Unit);
theString:=concat(vhString,kCR,kLF,kCR,kLF,gStrukturparameter,kCR,kLF,gMessageString);
Hauptschirm.MessageField.text:=theString;
gResultString:=theString;
gResultDialogString1 := concat('TSH: ',TSH_String,' ',gTSHUnit,kCR,kLF,sT4Label,T4_String,' ',gT4Unit,kCR,kLF,sT3Label,T3_String,' ',gT3Unit);
gResultDialogString2 := concat(gMessageString);
end;

procedure bell;
begin
  {$IFDEF win32}
    MessageBeep(0);
  {$ELSE}
    {$IFDEF LCLCarbon}
      SysBeep(30);
    {$ELSE}
      beep;
    {$ENDIF}
  {$ENDIF}
end;

function ValidKey (theKey: char): boolean;
{Beschränkt Texteingabe auf zulässige Zeichen}
type
format1 = set of char;
var
theformatn, theformatd, theformate, theformato, theformatc, thevalidformat: format1;
begin
     ValidKey := false;
     theformatn := ['1'..'9', '0', kTAB];
     theformatd := ['.', ','];
     theformate := ['e', 'E'];
     theformato := ['+', '-'];
     theformatc := [char($1c), char($1d), char($1e), char($1f), char($08)];
     thevalidformat := theformatn + theformatd + theformate + theformatc;
     if thekey in thevalidformat then
        ValidKey := true
     else
       bell;
     end;

procedure HandleInput;
var
  Buffer: PChar;
  Size: Byte;
  resultRecord: tCaseRecord;
begin
  try
    Size := Hauptschirm.TSH_Text.GetTextLen;              {Länge des Strings in Edit1 ermitteln}
    if Size = 0 then                                      {Feld leer?}
      TSH := 0
    else
      begin
        Inc(Size);                                        {Platz für NULL-Zeichen hinzufügen}
        GetMem(Buffer, Size);                             {Buffer als dynamische Variable definieren}
        Hauptschirm.TSH_Text.GetTextBuf(Buffer,Size);     {Edit1.Text in Buffer ablegen}
        TSH_String := StrPas(Buffer);
        FreeMem(Buffer, Size);                            {Speicher von Buffer freigeben}
        TSH := StrToFloat(TSH_String);
        TSH := TSH * gTSHUnitFactor;
      end;
    TSH_String := FloatToStrF(TSH, ffFixed, 3,2);
    Size := Hauptschirm.FT4_Text.GetTextLen;              {Länge des Strings in Edit1 ermitteln}
    if Size = 0 then                                      {Feld leer?}
      T4 := 0
    else
      begin
        Inc(Size);                                        {Platz für NULL-Zeichen hinzufügen}
        GetMem(Buffer, Size);                             {Buffer als dynamische Variable definieren}
        Hauptschirm.FT4_Text.GetTextBuf(Buffer,Size);     {Edit1.Text in Buffer ablegen}
        T4_String := StrPas(Buffer);
        FreeMem(Buffer, Size);                            {Speicher von Buffer freigeben}
        T4 := StrToFloat(T4_String);
      end;
    T4_String := FloatToStrF(T4, ffFixed, 3,2);
    T4 := T4 * UFT4;
    T4 := T4 * gT4UnitFactor;
    Size := Hauptschirm.FT3_Text.GetTextLen;              {Länge des Strings in Edit1 ermitteln}
    if Size = 0 then                                      {Feld leer?}
      T3 := 0
    else
      begin
        Inc(Size);                                        {Platz für NULL-Zeichen hinzufügen}
        GetMem(Buffer, Size);                             {Buffer als dynamische Variable definieren}
        Hauptschirm.FT3_Text.GetTextBuf(Buffer,Size);     {Edit1.Text in Buffer ablegen}
        T3_String := StrPas(Buffer);
        FreeMem(Buffer, Size);                            {Speicher von Buffer freigeben}
        T3 := StrToFloat(T3_String);
      end;
    T3_String := FloatToStrF(T3, ffFixed, 3,2);
    T3 := T3 * UFT3;
    T3 := T3 * gT3UnitFactor;
  except
    on ex: exception do
      begin
        bell;
        TSH := 0;
        T4 := 0;
        T3 := 0;
      end;
    end;
  resultRecord := Calculate(TSH, T4, T3);
  gMessageString:=concat('   GT: ',resultRecord.GTs,kCR,kLF,'   GD: ',resultRecord.GDs);
  ShowMessage;
end;

procedure THauptschirm.Calculate_ButtonClick(Sender: TObject);
begin
  Hauptschirm.HintResultGroupBox.Caption := gResultHint;
  HandleInput;
  ResultForm.Visible:=true;
end;

procedure AdaptMenus;
var
  modifierKey: TShiftState;
begin
  {$IFDEF LCLcarbon}
    modifierKey := [ssMeta];
    Hauptschirm.HelpMenu.Visible:=false;
    Hauptschirm.AppleMenu.Visible:=true;
  {$ELSE}
    modifierKey := [ssCtrl];
    Hauptschirm.HelpMenu.Visible:=true;
    Hauptschirm.AppleMenu.Visible:=false;
  {$ENDIF}
  Hauptschirm.NewMenuItem.ShortCut:=ShortCut(VK_N, modifierKey);
  Hauptschirm.CloseMenuItem.ShortCut:=ShortCut(VK_W, modifierKey);
  Hauptschirm.PrintMenuItem.ShortCut:=ShortCut(VK_P, modifierKey);
  Hauptschirm.QuitMenuItem.ShortCut:=ShortCut(VK_Q, modifierKey);
  Hauptschirm.UndoMenuItem.ShortCut:=ShortCut(VK_Z, modifierKey);
  Hauptschirm.CutMenuItem.ShortCut:=ShortCut(VK_X, modifierKey);
  Hauptschirm.CopyMenuItem.ShortCut:=ShortCut(VK_C, modifierKey);
  Hauptschirm.PasteMenuItem.ShortCut:=ShortCut(VK_V, modifierKey);
  Hauptschirm.CopyResultMenuItem.ShortCut:=ShortCut(VK_R, modifierKey);
end;

procedure AdaptLanguages;
begin
   if gInterfaceLanguage = English then
   begin
      gAnleitung := kAnleitung2;
      gVerhaltensparameter := kVerhaltensparameter2;
      gStrukturparameter := kStrukturparameter2;
      gNotCalculatable := kNotCalculatable2;
      gResultHint := kResultHint2;
      gPatientenname := kPatientenname2;
      gGeburtsdatum := kGeburtsdatum2;
      gUntersuchungsdatum := kUntersuchungsdatum2;
      gEinsender := kEinsender2;
      gBenutzername := kBenutzername2;
      gDruckdatum := kDruckdatum2;
      Hauptschirm.Calculate_Button.Caption := 'Calculate';
      Hauptschirm.HintResultGroupBox.Caption := 'Hint:';
      Hauptschirm.FileMenu.Caption := 'File';
      Hauptschirm.NewMenuItem.Caption := 'New Calculation...';
      Hauptschirm.CloseMenuItem.Caption := 'Close';
      Hauptschirm.PrintMenuItem.Caption := 'Print';
      Hauptschirm.PageSetupMenuItem.Caption := 'Page Setup...';
      Hauptschirm.QuitMenuItem.Caption := 'Quit';
      Hauptschirm.EditMenu.Caption := 'Edit';
      Hauptschirm.UndoMenuItem.Caption := 'Undo';
      Hauptschirm.CutMenuItem.Caption := 'Cut';
      Hauptschirm.CopyMenuItem.Caption := 'Copy';
      Hauptschirm.PasteMenuItem.Caption := 'Paste';
      Hauptschirm.DeleteMenuItem.Caption := 'Clear';
      Hauptschirm.CopyResultMenuItem.Caption := 'Copy Result';
      Hauptschirm.AboutMenuItem.Caption := 'SPINA-Thyr Info...';
   end
   else
   begin
      {$IFDEF LCLcarbon}
      Hauptschirm.FileMenu.Caption:='Ablage';
      Hauptschirm.UndoMenuItem.Caption:='Widerrufen';
      {$ELSE}
      Hauptschirm.FileMenu.Caption:='Datei';
      Hauptschirm.UndoMenuItem.Caption:='Rückgängig';
      {$ENDIF}
      gAnleitung := kAnleitung1;
      gVerhaltensparameter := kVerhaltensparameter1;
      gStrukturparameter := kStrukturparameter1;
      gNotCalculatable := kNotCalculatable1;
      gResultHint := kResultHint1;
      gPatientenname := kPatientenname1;
      gGeburtsdatum := kGeburtsdatum1;
      gUntersuchungsdatum := kUntersuchungsdatum1;
      gEinsender := kEinsender1;
      gBenutzername := kBenutzername1;
      gDruckdatum := kDruckdatum1;
   end;
   AdaptMenus;
end;

procedure THauptschirm.FormCreate(Sender: TObject);
begin
AdaptLanguages;
AdjustUnitFactors;
Hauptschirm.MessageField.text:=gAnleitung;
Hauptschirm.HorzScrollBar.Visible := false;
Hauptschirm.VertScrollBar.Visible := false;
Hauptschirm.AutoScroll := false;
GetPreferences;
Startup:=true;
end;

procedure THauptschirm.Ergebniskopieren1Click(Sender: TObject);
begin
Hauptschirm.MessageField.SelectAll;
Hauptschirm.MessageField.CopyToClipboard
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
 if Startup then
 begin
 Startup := False;
 Hauptschirm.TSH_Text.SetFocus;
 Hauptschirm.VertScrollBar.Visible := false;
 Hauptschirm.AutoScroll := false;
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

function DoPageSetup: Boolean;
var
  D: TPageSetupDialog;
begin
  D:=TPageSetupDialog.Create(nil);
  Try
    Result:=D.Execute;
  Finally
    D.Free;
  end;
end;

function DoPrintSetup: Boolean;
var
  D: TPrintDialog;
begin
  D:=TPrintDialog.Create(nil);
  Try
    Result:=D.Execute;
  Finally
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
  DPC := Round(ADPI/AnInch);
  H := ACanvas.TextHeight('X') + gLineSpacing;
  Result := Round((ACanvasHeight-DPC*(gTopMargin-gBottomMargin))/H-3);
end;

function GetPoints(AUnits: Double; ADPI: integer): integer    ;
begin
  Result := Round(AUnits*(ADPI/AnInch));
end;

procedure PrinterWriteln(H: integer; var currentX, currentY: integer; theString:String);
  begin
    Printer.Canvas.TextOut(currentX,currentY,theString);
    Inc(currentY,H);
  end;

procedure PrintCaption(H: integer; var currentX, currentY, marginX: integer);
  var
    theSize: integer;
  begin
  theSize := Printer.Canvas.Font.Size;
  Printer.Canvas.Font.Size := trunc(Printer.Canvas.Font.Size * 1.7);
  Printer.Canvas.Font.Style := [fsBold];
  PrinterWriteln(H, currentX, currentY, 'SPINA Thyr Report');
  PrinterWriteln(H, currentX, currentY, '');
  PrinterWriteln(H, currentX, currentY, '');
  Printer.Canvas.MoveTo(currentX, currentY - H div 2);
  Printer.Canvas.LineTo(Printer.PageWidth - marginX, currentY - H div 2);
  PrinterWriteln(H, currentX, currentY, '');
  Printer.Canvas.Font.Style := [];
  Printer.Canvas.Font.Size := theSize;
  if gInterfaceLanguage = German then
  begin
    PrinterWriteln(H, currentX, currentY, kPatientenname1);
    PrinterWriteln(H, currentX, currentY, kGeburtsdatum1);
    PrinterWriteln(H, currentX, currentY, kEinsender1);
    PrinterWriteln(H, currentX, currentY, kUntersuchungsdatum1);
    PrinterWriteln(H, currentX, currentY, '');
    PrinterWriteln(H, currentX, currentY, '');
    PrinterWriteln(H, currentX, currentY, '');
  end
  else
  begin
    PrinterWriteln(H, currentX, currentY, kPatientenname2);
    PrinterWriteln(H, currentX, currentY, kGeburtsdatum2);
    PrinterWriteln(H, currentX, currentY, kEinsender2);
    PrinterWriteln(H, currentX, currentY, kUntersuchungsdatum2);
    PrinterWriteln(H, currentX, currentY, '');
    PrinterWriteln(H, currentX, currentY, '');
    PrinterWriteln(H, currentX, currentY, '');
  end;
end;

procedure PrintFooter(H: integer; var currentX, currentY, marginX: integer);
var
  theDate, theTime: String;
begin
  DateTimeToString(theDate, 'dddd"," dd mmmm yyyy', date);
  DateTimeToString(theTime, '"," t', time);
  PrinterWriteln(H, currentX, currentY, '');
  Printer.Canvas.MoveTo(currentX, currentY - H div 2);
  Printer.Canvas.LineTo(Printer.PageWidth - marginX, currentY - H div 2);
  Printer.Canvas.Font.Color := clGray;
  PrinterWriteln(H, currentX, currentY, concat(gBenutzername, gUserName, '  |  ', gDruckdatum, theDate, theTime));
  PrinterWriteln(H, currentX, currentY, '');
  Printer.Canvas.Font.Color := clBlack;
end;

procedure THauptschirm.PrintMenuItemClick(Sender: TObject);
  var
H, ADPI, marginX, currentX, currentY, returnPos, lastPos: integer;
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
    Printer.Title := 'SPINA Thyr Report';
    currentX := marginX;
    Printer.BeginDoc;
    try
      Printer.Canvas.Font := MessageField.Font;
      Printer.Canvas.Font.Size := 9;
      Printer.Canvas.Font.Style := [];
      Printer.Canvas.Pen.Color := clBlack;
      Printer.Canvas.Pen.Width := 2;
      H := (Printer.Canvas.TextHeight('X') + gLineSpacing) ;
      PrintCaption(H, currentX, currentY, marginX);
      lastPos:=1;
      remainder := gResultString;
      repeat
        returnPos:=pos(kCR, remainder);
        if returnPos > 0 then
          resultLine := copy(remainder, 1, returnPos-1)
        else
          resultLine := remainder;
        remainder := copy(remainder, returnPos + 2, length(remainder));
        PrinterWriteln(H, currentX,currentY,resultLine);
      until returnPos = 0;
      currentX := marginX;
      currentY := Printer.PageHeight - 4 * H;
      PrintFooter(H, currentX, currentY, marginX);
      Printer.EndDoc;
    except
      on E:Exception do
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
  gGermanCodes := [3, 19, 70, 92];

  gPrefsDir:=GetAppConfigDir(false);
  gPrefsFileName:=GetAppConfigFile(false);

  gAppName := ApplicationName;
  gAppPath := ParamStr(0);
  gAppPath := Application.Location;

  gSysLanguage:=GetOSLanguage;
  {$IFDEF UNIX}
  gUserName := GetEnvironmentVariable('USER');
  {$ELSE}
  GetUserName(UserName, arraySize);
  gUserName := String(UserName);
  {$ENDIF}
  if pos('Deutsch', gSysLanguage)>0 then
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

