{ SPINA-Thyr }
{ Programm zur Berechnung von Strukturparametern }
{ des thyreotropen Regelkreises }
{ Version 3.1 }

{ J. W. Dietrich, Klinikum der LMU München 1997-2001 }
{ J. W. Dietrich, Universitätsklinikum Ulm 2002-2004 }

unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Printers, ActnList, ImgList, StdActns, Menus, ExtCtrls, StdCtrls, Clipbrd,
  Unit2, Unit3;

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
kVerhaltensparameter1='Hormonspiegel (Signale):';
kVerhaltensparameter2='Hormone levels (signals):';
kStrukturparameter1='Übertragungsparameter:';
kStrukturparameter2='Transfer parameters:';
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
UFT4 = 1.28e-11;
UFT3 = 1.54e-12;
ALPHAT = 0.1;
BETAT = 1.1e-6;
THY = 1;
I = 1;
DT = 2.75;
KT = 1;
KI = 1;
TBG = 3e-7;
TBPA = 4.5e-6;
K30 = 2e9;
K41 = 2e10;
K42 = 2e8;
ALPHA31 = 0.026;
BETA31 = 8e-6;
KM1 = 5e-7;

type
  Str255 = String[255];
  CaseRecord = record
	TSH, FT4, FT3, GT, GD: real;
	MessageString: String[255];
			end;
  tMethod = (freeHormone, totalHormone);
  tInterfaceLanguage = (English, German);
  tPreferences = record
         T4Method, T3Method: tMethod;
         TSHUnitFactor, FT4UnitFactor, FT3UnitFactor: real;
         TSHPopUpItem, FT4PopUpItem, FT3PopUpItem: integer;
         T4MethodPopUpItem, T3MethodPopUpItem: integer;
         end;
  tPrefsFile = file of tPreferences;
  THauptschirm = class(TForm)
    ImageList1: TImageList;
    ActionList1: TActionList;
    EditCopy1: TEditCopy;
    EditCut1: TEditCut;
    EditDelete1: TEditDelete;
    EditPaste1: TEditPaste;
    EditUndo1: TEditUndo;
    WindowClose1: TWindowClose;
    MainMenu1: TMainMenu;
    Datei1: TMenuItem;
    ffnen1: TMenuItem;
    Schlieen1: TMenuItem;
    N1: TMenuItem;
    Beenden1: TMenuItem;
    Bearbeiten1: TMenuItem;
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
    TSH_Text: TEdit;
    FT3_Text: TEdit;
    FT4_Text: TEdit;
    Calculate_Button: TButton;
    MessageField: TMemo;
    TSHComboBox: TComboBox;
    Logo: TImage;
    FT3ComboBox: TComboBox;
    FT4ComboBox: TComboBox;
    Drucken1: TMenuItem;
    Seiteneinrichtung1: TMenuItem;
    N5: TMenuItem;
    PrintDialog1: TPrintDialog;
    PrinterSetupDialog1: TPrinterSetupDialog;
    N6: TMenuItem;
    SPINAThyrHilfe1: TMenuItem;
    T4MethodComboBox: TComboBox;
    T3MethodComboBox: TComboBox;
    FT4Items: TComboBox;
    T4Items: TComboBox;
    FT3Items: TComboBox;
    T3Items: TComboBox;
    HintResultGroupBox: TGroupBox;
    procedure Beenden1Click(Sender: TObject);
    procedure Calculate_ButtonClick(Sender: TObject);
    procedure berSPINAThyr1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Ergebniskopieren1Click(Sender: TObject);
    procedure TSHComboBoxChange(Sender: TObject);
    procedure FT3ComboBoxChange(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure Print1Click(Sender: TObject);
    procedure Seiteneinrichtung1Click(Sender: TObject);
    procedure AppHelp(Sender: TObject);
    procedure FT4ComboBoxChange(Sender: TObject);
    procedure T4MethodComboBoxAdjust(Sender: TObject);
    procedure T3MethodComboBoxAdjust(Sender: TObject);
    procedure T4MethodComboBoxChange(Sender: TObject);
    procedure T3MethodComboBoxChange(Sender: TObject);
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  end;

var
  Startup:Boolean;
  gInterfaceLanguage: tInterfaceLanguage;
  LanguageID: LangID;
  Language, UserName: array[0..128] of char;
  gSysLanguage, gUserName: String;
  arraySize: DWord;
  gAnleitung, gVerhaltensparameter, gStrukturparameter: String;
  gNotCalculatable, gResultHint, gBenutzername: String;
  gPatientenname, gGeburtsdatum, gUntersuchungsdatum, gEinsender, gDruckdatum: String;
  Hauptschirm: THauptschirm;
  TSH, FT4, FT3, GT, GD: real;
  gTSHUnitFactor, gFT4UnitFactor, gFT3UnitFactor: real;
  gT4Method, gT3Method: tMethod;
  gcalcTitle, gcalcString, gnotcalculatableString, GTs, GDs: Str255;
  gExplanationString, gMessageString, TSH_String, FT4_String, FT3_String: Str255;
  gRefExp, gGTRef, gGDRef, gSignalString, gParameterString: Str255;
  gTSHUnit, gFT4Unit, gFT3Unit, gResultString: Str255;
  gcalcCounter: longint;
  gPreferences: tPreferences;
  gPrefsFile: tPrefsFile;
  gResultDialogString1, gResultDialogString2: Str255;

implementation

uses Unit4;

{$R *.DFM}

procedure AdjustUnitFactors;
begin
if Hauptschirm.T4MethodComboBox.text='FT4'
then
  begin
  if Hauptschirm.FT4ComboBox.text='ng/dl'
  then
    gFT4UnitFactor:=1
  else if Hauptschirm.FT4ComboBox.text='ng/l'
  then
    gFT4UnitFactor:=0.1
  else if Hauptschirm.FT4ComboBox.text='pmol/l'
  then
    gFT4UnitFactor:=1/UFT4/1e12;
  end
else if Hauptschirm.T4MethodComboBox.text='T4'
then
  begin
  if Hauptschirm.FT4ComboBox.text='µg/l'
  then
    gFT4UnitFactor:=100
  else if Hauptschirm.FT4ComboBox.text='µg/dl'
  then
    gFT4UnitFactor:=1000
  else if Hauptschirm.FT4ComboBox.text='nmol/l'
  then
    gFT4UnitFactor:=1/UFT4/1e9;
  end;
if Hauptschirm.T3MethodComboBox.text='FT3'
then
  begin
  if Hauptschirm.FT3ComboBox.text='pg/ml'
  then
    gFT3UnitFactor:=1
  else if Hauptschirm.FT3ComboBox.text='ng/l'
  then
    gFT3UnitFactor:=1
  else if Hauptschirm.FT3ComboBox.text='pmol/l'
  then
    gFT3UnitFactor:=1/UFT3/1e12;
  end
else if Hauptschirm.T3MethodComboBox.text='T3'
then
  begin
  if Hauptschirm.FT3ComboBox.text='µg/l'
  then
    gFT3UnitFactor:=1000
  else if Hauptschirm.FT3ComboBox.text='ng/dl'
  then
    gFT3UnitFactor:=10
  else if Hauptschirm.FT3ComboBox.text='nmol/l'
  then
    gFT3UnitFactor:=1/UFT3/1e9;
  end;
if Hauptschirm.TSHComboBox.text='mU/l'
then
   gTSHUnitFactor:=1
else
   gTSHUnitFactor:=1;
gTSHUnit := Hauptschirm.TSHComboBox.text;
gFT4Unit := Hauptschirm.FT4ComboBox.text;
gFT3Unit := Hauptschirm.FT3ComboBox.text;
with gPreferences do
  begin
    T4Method := gT4Method;
    T3Method := gT3Method;
    TSHUnitFactor := gTSHUnitFactor;
    FT4UnitFactor := gFT4UnitFactor;
    FT3UnitFactor := gFT3UnitFactor;
  end;
end;

procedure THauptschirm.T4MethodComboBoxAdjust(Sender: TObject);
begin
if Hauptschirm.T4MethodComboBox.text='FT4'
then
begin
gT4Method := freeHormone;
Hauptschirm.FT4ComboBox.Items.Assign(FT4Items.Items);
Hauptschirm.FT4ComboBox.text:=Hauptschirm.FT4ComboBox.Items.Strings[gPreferences.FT4PopUpItem];
end
else if Hauptschirm.T4MethodComboBox.text='T4'
then
begin
gT4Method := totalHormone;
Hauptschirm.FT4ComboBox.Items.Assign(T4Items.Items);
Hauptschirm.FT4ComboBox.text:=Hauptschirm.FT4ComboBox.Items.Strings[gPreferences.FT4PopUpItem];
end;
AdjustUnitFactors;
end;

procedure THauptschirm.T3MethodComboBoxAdjust(Sender: TObject);
begin
if Hauptschirm.T3MethodComboBox.text='FT3'
then
begin
gT3Method := freeHormone;
Hauptschirm.FT3ComboBox.Items.Assign(FT3Items.Items);
Hauptschirm.FT3ComboBox.text:=Hauptschirm.FT3ComboBox.Items.Strings[gPreferences.FT3PopUpItem];
end
else if Hauptschirm.T3MethodComboBox.text='T3'
then
begin
gT3Method := totalHormone;
Hauptschirm.FT3ComboBox.Items.Assign(T3Items.Items);
Hauptschirm.FT3ComboBox.text:=Hauptschirm.FT3ComboBox.Items.Strings[gPreferences.Ft3PopUpItem];
end;
AdjustUnitFactors;
end;

procedure SavePreferences;
begin
  assignFile(gPrefsFile, 'spina-thyr.dat');
  rewrite(gPrefsFile);
  write(gPrefsFile, gPreferences);
  CloseFile(gPrefsFile);
end;

procedure GetPreferences;
begin
  assignFile(gPrefsFile, 'spina-thyr.dat');
  try
    reset(gPrefsFile);
    read(gPrefsFile,gPreferences);
    CloseFile(gPrefsFile);
    with gPreferences do
      begin
        gT4Method := T4Method;
        gT3Method := T3Method;
        Hauptschirm.T4MethodComboBox.ItemIndex := T4MethodPopUpItem;
        Hauptschirm.T3MethodComboBox.ItemIndex := T3MethodPopUpItem;
        Hauptschirm.T4MethodComboBoxAdjust(Hauptschirm);
        Hauptschirm.T3MethodComboBoxAdjust(Hauptschirm);
        Hauptschirm.TSHComboBox.ItemIndex := TSHPopUpItem;
        Hauptschirm.FT4ComboBox.ItemIndex := FT4PopUpItem;
        Hauptschirm.FT3ComboBox.ItemIndex := FT3PopUpItem;
        gTSHUnitFactor:=TSHUnitFactor;
        gFT4UnitFactor:=FT4UnitFactor;
        gFT3UnitFactor:=FT3UnitFactor;
      end;
    except
  on Ex: EInOutError do
    begin
      with gPreferences do
      begin
        T4Method:=freeHormone;
        T3Method:=freeHormone;
        TSHUnitFactor:=1;
        FT4UnitFactor:=1;
        FT3UnitFactor:=1;
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
SavePreferences;
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
if gT4Method = freeHormone
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
if gT3Method = freeHormone
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
  vhString:=concat(gVerhaltensparameter,kCR,kLF,'   TSH: ',TSH_String,' ',gTSHUnit,kCR,kLF,T4Label,FT4_String,' ',gFT4Unit,kCR,kLF,T3Label,FT3_String,' ',gFT3Unit);
theString:=concat(vhString,kCR,kLF,kCR,kLF,gStrukturparameter,kCR,kLF,gMessageString);
Hauptschirm.MessageField.text:=theString;
gResultString:=theString;
gResultDialogString1 := concat('TSH: ',TSH_String,' ',gTSHUnit,kCR,kLF,sT4Label,FT4_String,' ',gFT4Unit,kCR,kLF,sT3Label,FT3_String,' ',gFT3Unit);
gResultDialogString2 := concat(gMessageString);
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
     	Beep;
     end;

procedure Calculate;
begin
if TSH > 0 then
   begin
   case gT4Method of
   freeHormone:
      GT := betaT * (DT + TSH) * (1 + k41 * TBG + k42 * TBPA) * FT4 / (alphaT * TSH);
   totalHormone:
      GT := betaT * (DT + TSH) * FT4 / (alphaT * TSH);
   end;
   GTs:=FloatToStrF(1e12*GT, ffFixed, 5,2);
   GTs:=concat(GTs,' pmol/s');
    end
else
   GTs := gNotCalculatable;
if FT4 > 0 then
   begin
   case gT4Method of
   freeHormone:
   begin
     case gT3Method of
     freeHormone:
        GD := beta31 * (kM1 + FT4) * (1 + k30 * TBG) * FT3 / (alpha31 * FT4);
     totalHormone:
        GD := beta31 * (kM1 + FT4) * FT3 / (alpha31 * FT4);
     end;
   end;
   totalHormone:
   begin
     FT4 := FT4 / (1 + k41 * TBG + k42 * TBPA);
     case gT3Method of
     freeHormone:
        GD := beta31 * (kM1 + FT4) * (1 + k30 * TBG) * FT3 / (alpha31 * FT4);
     totalHormone:
        GD := beta31 * (kM1 + FT4) * FT3 / (alpha31 * FT4);
     end;
   end;
   end;
   GDs:=FloatToStrF(1e9*GD, ffFixed, 5,2);
   GDs:=concat(GDs,' nmol/s');
   end
else
   GDs := gNotCalculatable;
end;

procedure HandleInput;
type TCaption=string;
var
Buffer: PChar;
Size: Byte;
begin
Size := Hauptschirm.TSH_Text.GetTextLen;       {Länge des Strings in Edit1 ermitteln}
Inc(Size);                      {Platz für NULL-Zeichen hinzufügen}
GetMem(Buffer, Size);           {Buffer als dynamische Variable definieren}
Hauptschirm.TSH_Text.GetTextBuf(Buffer,Size);  {Edit1.Text in Buffer ablegen}
TSH_String:= StrPas(Buffer);
FreeMem(Buffer, Size);{Speicher von Buffer freigeben}
TSH:=StrToFloat(TSH_String);
TSH:=TSH*gTSHUnitFactor;
TSH_String:=FloatToStrF(TSH, ffFixed, 3,2);
Size := Hauptschirm.FT4_Text.GetTextLen;       {Länge des Strings in Edit1 ermitteln}
Inc(Size);                      {Platz für NULL-Zeichen hinzufügen}
GetMem(Buffer, Size);           {Buffer als dynamische Variable definieren}
Hauptschirm.FT4_Text.GetTextBuf(Buffer,Size);  {Edit1.Text in Buffer ablegen}
FT4_String:= StrPas(Buffer);
FreeMem(Buffer, Size);{Speicher von Buffer freigeben}
FT4:=StrToFloat(FT4_String);
FT4:=FT4*UFT4;
FT4:=FT4*gFT4UnitFactor;
FT4_String:=FloatToStrF(FT4/(gFT4UnitFactor*UFT4), ffFixed, 3,2);
Size := Hauptschirm.FT3_Text.GetTextLen;       {Länge des Strings in Edit1 ermitteln}
Inc(Size);                      {Platz für NULL-Zeichen hinzufügen}
GetMem(Buffer, Size);           {Buffer als dynamische Variable definieren}
Hauptschirm.FT3_Text.GetTextBuf(Buffer,Size);  {Edit1.Text in Buffer ablegen}
FT3_String:= StrPas(Buffer);
FreeMem(Buffer, Size);{Speicher von Buffer freigeben}
FT3:=StrToFloat(FT3_String);
FT3:=FT3*UFT3;
FT3:=FT3*gFT3UnitFactor;
FT3_String:=FloatToStrF(FT3/(gFT3UnitFactor*UFT3), ffFixed, 3,2);
Calculate;
{MessageDlg(concat('GT: ',GTs,kCR,'GD: ',GDs), mtInformation,[mbOk], 0);}
gMessageString:=concat('   GT: ',GTs,kCR,kLF,'   GD: ',GDs);
ShowMessage;
end;

procedure THauptschirm.Calculate_ButtonClick(Sender: TObject);
begin
Hauptschirm.HintResultGroupBox.Caption := gResultHint;
HandleInput;
ResultForm.Visible:=true;
end;

procedure THauptschirm.berSPINAThyr1Click(Sender: TObject);
begin
ShowAboutBox;
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
      Hauptschirm.MainMenu1.Items[0].Caption := 'File';
      Hauptschirm.MainMenu1.Items[1].Caption := 'Edit';
      Hauptschirm.Datei1.Items[0].Caption := 'New Calculation...';
      Hauptschirm.Datei1.Items[1].Caption := 'Close';
      Hauptschirm.Datei1.Items[3].Caption := 'Print';
      Hauptschirm.Datei1.Items[4].Caption := 'Page Setup...';
      Hauptschirm.Datei1.Items[6].Caption := 'Quit';
      Hauptschirm.Bearbeiten1.Items[0].Caption := 'Undo';
      Hauptschirm.Bearbeiten1.Items[2].Caption := 'Cut';
      Hauptschirm.Bearbeiten1.Items[3].Caption := 'Copy';
      Hauptschirm.Bearbeiten1.Items[4].Caption := 'Paste';
      Hauptschirm.Bearbeiten1.Items[5].Caption := 'Clear';
      Hauptschirm.Bearbeiten1.Items[7].Caption := 'Copy Result';
      Hauptschirm.N4.Items[0].Caption := 'SPINA-Thyr Help';
      Hauptschirm.N4.Items[2].Caption := 'SPINA-Thyr Info...';
   end
   else
   begin
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
end;

procedure THauptschirm.FormCreate(Sender: TObject);
begin
AdaptLanguages;
Hauptschirm.MessageField.text:=gAnleitung;
GetPreferences;
Startup:=true;
end;

procedure THauptschirm.Ergebniskopieren1Click(Sender: TObject);
begin
Hauptschirm.MessageField.SelectAll;
Hauptschirm.MessageField.CopyToClipboard
end;

procedure THauptschirm.TSHComboBoxChange(Sender: TObject);
begin
   gPreferences.TSHPopUpItem := Hauptschirm.TSHComboBox.ItemIndex;
   AdjustUnitFactors;
end;

procedure THauptschirm.FT3ComboBoxChange(Sender: TObject);
begin
   gPreferences.FT3PopUpItem := Hauptschirm.FT3ComboBox.ItemIndex;
   AdjustUnitFactors;
end;

procedure THauptschirm.FT4ComboBoxChange(Sender: TObject);
begin
   gPreferences.FT4PopUpItem := Hauptschirm.FT4ComboBox.ItemIndex;
   AdjustUnitFactors;
end;

procedure THauptschirm.FormActivate(Sender: TObject);
begin
 if Startup then
 begin
 Startup := False;
 SplashScreen.Show;
 Printer;
 end;
end;

procedure THauptschirm.Print1Click(Sender: TObject);
var
  Line: Integer;
  PrintText: TextFile;   {Deklaration einer Dateivariablen}
begin
  if PrintDialog1.Execute then
  begin
    AssignPrn(PrintText);   {Zuweisung von PrintText an den Drucker}
    Rewrite(PrintText);     {Erzeugen und Öffnen der Ausgabedatei}
    Printer.Canvas.Font := MessageField.Font;{Zuweisung der eingestellten Schriftart an die Leinwand}
    Printer.Canvas.Font.Size := 10;
    Printer.Title := 'SPINA Thyr Report';
    Writeln(PrintText, '');
    Writeln(PrintText, '');
    Writeln(PrintText, '');
    Writeln(PrintText, kMarginSpaces,'SPINA Thyr');
    Writeln(PrintText, '');
    Writeln(PrintText, '');
    Writeln(PrintText, '');
    Writeln(PrintText, kMarginSpaces,gPatientenname);
    Writeln(PrintText, kMarginSpaces,gGeburtsdatum);
    Writeln(PrintText, kMarginSpaces,gEinsender);
    Writeln(PrintText, kMarginSpaces,gUntersuchungsdatum);
    Writeln(PrintText, '');
    Writeln(PrintText, '');
    Writeln(PrintText, '');
    Writeln(PrintText, '');
    for Line := 0 to MessageField.Lines.Count - 1 do
      Writeln(PrintText, kMarginSpaces,MessageField.Lines[Line]);	{Schreiben des Inhalts von Memo1 in das Druckerobjekt}
    Writeln(PrintText, '');
    Writeln(PrintText, '');
    Writeln(PrintText, '');
    Writeln(PrintText, '');
    Writeln(PrintText, kMarginSpaces, gBenutzername, gUserName, '  |  ', gDruckdatum, DateToStr(Date));
    CloseFile(PrintText); {Schließen der Druckervariablen}
  end;
end;

procedure THauptschirm.Seiteneinrichtung1Click(Sender: TObject);
begin
PrinterSetupDialog1.execute;
end;

procedure THauptschirm.AppHelp(Sender: TObject);
begin
Application.HelpJump('Contents');
end;

procedure THauptschirm.T4MethodComboBoxChange(Sender: TObject);
begin
   gPreferences.T4MethodPopUpItem := Hauptschirm.T4MethodComboBox.ItemIndex;
   gPreferences.FT4PopUpItem := 0;
   Hauptschirm.T4MethodComboBoxAdjust(Sender);
end;

procedure THauptschirm.T3MethodComboBoxChange(Sender: TObject);
begin
   gPreferences.T3MethodPopUpItem := Hauptschirm.T3MethodComboBox.ItemIndex;
   gPreferences.FT3PopUpItem := 0;
   Hauptschirm.T3MethodComboBoxAdjust(Sender);
end;

initialization
arraySize := SizeOf(UserName);
GetUserName(UserName, arraySize);
gUserName := String(UserName);
LanguageID := GetSystemDefaultLangID;
VerLanguageName(LanguageID, Language, 100);
gSysLanguage := String(Language);
if pos('Deutsch', gSysLanguage)>0 then
   gInterfaceLanguage := German
else
   gInterfaceLanguage := English;
{gInterfaceLanguage := English;   {for Testing only}

finalization
SavePreferences;

end.

