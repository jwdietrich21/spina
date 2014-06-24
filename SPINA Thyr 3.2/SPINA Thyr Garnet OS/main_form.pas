{ SPINA-Thyr }
{ Programm zur Berechnung von Strukturparametern }
{ des thyreotropen Regelkreises }
{ Version 3.2 }

{ J. W. Dietrich, Klinikum der LMU München 1997-2001 }
{ J. W. Dietrich, Universitätsklinikum Ulm 2002-2004 }
{ J. W. Dietrich, Universitätsklinikum Bergmannsheil 2005-2010 }

{ This software is provided via a BSD licence }
{ See http://spina.medical-cybernetics.de for details }

unit main_form;

interface

uses PSL, SonyChars, SysUtils, spina_engine, about_form;

const
  {MainForm declared in unit about_form}
  kTAB=chr(9);
  kLF=chr(10);
  kCR=chr(13);
  kNotCalculatable='<Not calculatable>';

  THE_CREATOR = 'SPIt';
  PREFS_ID = 1;

var
  TSH_String, T4_String, T3_String: String;

function HandleEvent(var Event: EventType): Boolean;
procedure GetPreferences;
procedure SavePreferences;

implementation

const
  AboutHelpString = AutoID;
  Calculate_Button = AutoID;
  TSH_Text = AutoID;
  TSH_Label = AutoID;
  T4_Text = AutoID;
  T3_Text = AutoID;
  TSH_Unit_Popup = AutoID;
  T4_Unit_Popup = AutoID;
  T3_Unit_Popup = AutoID;
  TSH_Unit_List = AutoID;
  T4_Method_Popup = AutoID;
  T3_Method_Popup = AutoID;
  T4_Method_List = AutoID;
  T3_Method_List = AutoID;
  T4_Unit_List = AutoID;
  T3_Unit_List = AutoID;
  MainMenu = AutoID;
  AboutMenuItem = AutoID;
  NewMenuItem = AutoID;
  MenuItem1 = AutoID;
  MenuItem2 = AutoID;
  QuitMenuItem = AutoID;
  UndoMenuItem = AutoID;
  MenuItem3 = AutoID;
  CutMenuItem = AutoID;
  CopyMenuItem = AutoID;
  PasteMenuItem = AutoID;
  MenuItem4 = AutoID;
  CopyResultMenuItem = AutoID;
  MainFormBackground = AutoID;
  MiniAppIcon = AutoID;
  AlertInformation = AutoID;

var
  TSH, T4, T3: real;
  gExplanationString, gMessageString: String;
  gRefExp, gGTRef, gGDRef, gSignalString, gParameterString: String;
  gTSHUnit, gT4Unit, gT3Unit, gResultString: String;

resource
  MENU MainMenu
  BEGIN
    PULLDOWN 'Options'
    BEGIN
      MENUITEM AboutMenuItem 'About SPINA Thyr'
      MENUITEM MenuItem1 '-'
      MENUITEM NewMenuItem 'New Calculation' 'N'
      MENUITEM MenuItem2 '-'
      MENUITEM QuitMenuItem 'Quit' 'Q'
    END
    PULLDOWN 'Edit'
    BEGIN
      MENUITEM UndoMenuItem 'Undo' 'Z'
      MENUITEM MenuItem3 '-'
      MENUITEM CutMenuItem 'Cut' 'X'
      MENUITEM CopyMenuItem 'Copy' 'C'
      MENUITEM PasteMenuItem 'Paste' 'P'
      MENUITEM MenuItem4 '-'
      MENUITEM CopyResultMenuItem 'Copy Result' 'R'
    END
  END;

  BITMAP MainFormBackground 'main_form_bw.bmp' 'main_form_c.bmp' DENSITY 72 {DENSITY 72/108/144} {DIRECTCOLOR} TRANSPARENCY 0 0;
  BITMAP MiniAppIcon 'IconMBW.bmp' 'IconMC.bmp' {DENSITY 72/108/144} {DIRECTCOLOR} TRANSPARENCY 0 0;

  FORM MainForm AT (0 0 160 160) NOFRAME DEFAULTBUTTON Calculate_Button MENU MainMenu
  BEGIN
    TITLE 'SPINA Thyr'
    FORMBITMAP AT (4 17) BITMAP MainFormBackground
    LIST T4_Unit_List AT (65 137 34 22) NONUSABLE
    LIST T3_Unit_List AT (101 73 34 34) NONUSABLE
    LIST T3_Method_List 'FT3' 'T3' AT (101 49 34 30) NONUSABLE
    LIST T4_Method_List 'FT4' 'T4' AT (65 113 34 26) NONUSABLE
    LIST TSH_Unit_List 'mU/l' #181'U/ml' AT (21 72 46 26) NONUSABLE
    BUTTON Calculate_Button 'Calc' AT (118 145 37 12)
    LABEL TSH_Label 'TSH' AT (28 48)
    FIELD TSH_Text AT (24 60 36 14) UNDERLINED MAXCHARS 10 NUMERIC
    FIELD T4_Text AT (64 124 36 14) UNDERLINED MAXCHARS 10 NUMERIC
    FIELD T3_Text AT (100 60 36 14) UNDERLINED MAXCHARS 10 NUMERIC
    POPUPTRIGGER TSH_Unit_Popup 'mU/l' AT (20 72 44 12)
    POPUPTRIGGER T4_Unit_Popup 'ng/dl' AT (64 136 44 12)
    FORMBITMAP AT (144 0) BITMAP MiniAppIcon
    POPUPTRIGGER T4_Method_Popup 'FT4' AT (64 112 32 12)
    POPUPTRIGGER T3_Method_Popup 'FT3' AT (100 48 32 12)
    POPUPTRIGGER T3_Unit_Popup 'pg/ml' AT (100 72 44 12)
    GRAFFITISTATEINDICATOR AT (8 147)
    POPUPLIST TSH_Unit_Popup TSH_Unit_List
    POPUPLIST T4_Unit_Popup T4_Unit_List
    POPUPLIST T4_Method_Popup T4_Method_List
    POPUPLIST T3_Method_Popup T3_Method_List
    POPUPLIST T3_Unit_Popup T3_Unit_List
  END;

  ALERT AlertInformation {HELP String1} {DEFAULTBUTTON 0} INFORMATION
  BEGIN
    TITLE 'Information'
    MESSAGE '^1'
    BUTTONS 'OK'
  END;

function NumericCreator(theString: String): UInt32;
  var tempValue: UInt32;
begin
  tempValue := UInt32(theString[1])*16777216 + UInt32(theString[2])*65536;
  tempValue := tempValue + UInt16(theString[3])*256 + UInt8(theString[4]);
  result := tempValue;
end;

procedure SavePreferences;
var
  thePrefs: ^tPreferences;
  theVersion: Int16;
  theSize: UInt16;
  creator_numeric: UInt32;
  save: boolean;
begin
  save := false;
  theVersion := 0;
  theSize := sizeof(gPreferences);
  thePrefs := @gPreferences;
  creator_numeric := NumericCreator(THE_CREATOR);
  PrefSetAppPreferences(creator_numeric, PREFS_ID, theVersion, thePrefs, theSize, save);
end;

procedure GetPreferences;
var
  thePrefs: ^tPreferences;
  theVersion: Int16;
  theSize: UInt16;
  creator_numeric: UInt32;
  save: boolean;
begin
  save := false;
  theVersion := 0;
  theSize := 0;
  thePrefs := nil;
  creator_numeric := NumericCreator(THE_CREATOR);
  theVersion := PrefGetAppPreferences(creator_numeric, PREFS_ID, thePrefs, theSize, save) ;
  theSize := sizeof(gPreferences);
  thePrefs := @gPreferences;
  if theVersion <> noPreferenceFound then
  begin
    theVersion := PrefGetAppPreferences(creator_numeric, PREFS_ID, thePrefs, theSize, save) ;
  end
  else
  begin
      SavePreferences;
  end;
end;

procedure HandleNewCalculation;
begin
  {FrmCustomAlert(AlertInformation, 'This function has not yet been implemented.','',''); }
  PSField.SetText(TSH_Text, '');
  PSField.SetText(T4_Text, '');
  PSField.SetText(T3_Text, '');
  PSForm.SetFocus(TSH_Text);
  TSH_String := '0000000000';
  T4_String := '0000000000';
  T3_String := '0000000000';
end;

procedure MainFormOpen;
begin
  gNotCalculatable:=kNotCalculatable;
  gVerhaltensparameter := 'You entered:';
  gStrukturparameter := 'Structure parameters:';
  PSForm.Draw;
  PSForm.SetFocus(TSH_Text);
  TSH_String := '0000000000';
  T4_String := '0000000000';
  T3_String := '0000000000';
end;

function GetFocusedField: FieldPtr;
var
  ObjType: FormObjectKind;
begin
  if FrmGetFocus(FrmGetActiveForm) = noFocus then
    begin
      beep;
      Result := nil;
    end
  else
  begin
    ObjType := FrmGetObjectType(FrmGetActiveForm, FrmGetFocus(FrmGetActiveForm));
    if objType = frmFieldObj then
      Result := (FrmGetObjectPtr(FrmGetActiveForm, FrmGetFocus(FrmGetActiveForm)))
    else if objType = frmTableObj then
      Result := TblGetCurrentField(FrmGetObjectPtr(FrmGetActiveForm, FrmGetFocus(FrmGetActiveForm)));
  end;
end;

function MainFormCustomEvent: Boolean;
begin
  Result := False;
end;

procedure MainFormMenu(Item: UInt16);
var
  theEvent: EventType;
  theField: FieldPtr;
begin
  case Item of
  AboutMenuItem:
    PSForm.InitModal(AboutForm, about_form.HandleEvent);
  NewMenuItem:
    HandleNewCalculation;
  QuitMenuItem:
      begin
        theEvent.eType := appStopEvent;
        EvtAddEventToQueue(@theEvent);
      end;
  UndoMenuItem:
    begin
      theField := GetFocusedField;
      FldUndo(theField);
    end;
  CutMenuItem:
    begin
      theField := GetFocusedField;
      {if editable(TField(theField))
      then}
        FldCut(theField)
      {else
        handled := true;}
    end;
  CopyMenuItem:
    begin
      theField := GetFocusedField;
      FldCopy(theField);
    end;
  PasteMenuItem:
    begin
      theField := GetFocusedField;
      FldPaste(theField);
    end;
  CopyResultMenuItem:
    begin
      ClipboardAddItem(clipboardText, gResultString, Length(gResultString));
    end;
  end;
end;

procedure AdjustUnitFactors;
begin
  gTSHUnit := PSButton.Caption(TSH_Unit_Popup);
  gT4Unit := PSButton.Caption(T4_Unit_Popup);
  gT3Unit := PSButton.Caption(T3_Unit_Popup);
if PSButton.Caption(T4_Method_Popup)='FT4'
then
  begin
  if gT4Unit='ng/dl'
  then
    gPreferences.T4UnitFactor:=1
  else if gT4Unit='ng/l'
  then
    gPreferences.T4UnitFactor:=0.1
  else if gT4Unit='pmol/l'
  then
    gPreferences.T4UnitFactor:=1/UT4/1e12;
  end
else if PSButton.Caption(T4_Method_Popup)='T4'
then
  begin
  if gT4Unit='µg/l'
  then
    gPreferences.T4UnitFactor:=100
  else if gT4Unit='µg/dl'
  then
    gPreferences.T4UnitFactor:=1000
  else if gT4Unit='nmol/l'
  then
    gPreferences.T4UnitFactor:=1/UT4/1e9;
  end;
if PSButton.Caption(T3_Method_Popup)='FT3'
then
  begin
  if gT3Unit='pg/ml'
  then
    gPreferences.T3UnitFactor:=1
  else if gT3Unit='ng/l'
  then
    gPreferences.T3UnitFactor:=1
  else if gT3Unit='pmol/l'
  then
    gPreferences.T3UnitFactor:=1/UT3/1e12;
  end
else if PSButton.Caption(T3_Method_Popup)='T3'
then
  begin
  if gT3Unit='µg/l'
  then
    gPreferences.T3UnitFactor:=1000
  else if gT3Unit='ng/dl'
  then
    gPreferences.T3UnitFactor:=10
  else if gT3Unit='nmol/l'
  then
    gPreferences.T3UnitFactor:=1/UT3/1e9;
  end;
if gTSHUnit='mU/l'
then
   gPreferences.TSHUnitFactor:=1
else
   gPreferences.TSHUnitFactor:=1;
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
  theString,vhString: String;
  T4Label,T3Label,sT4Label,sT3Label: String;
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
    vhString:=gVerhaltensparameter+kLF+'   TSH: '+TSH_String+' '+gTSHUnit+kLF+T4Label+T4_String+' '+gT4Unit+kLF+T3Label+T3_String+' '+gT3Unit;
    theString:=vhString+kLF+kLF+gStrukturparameter+kLF+gMessageString;
    gResultString:=theString;
    FrmCustomAlert(AlertInformation, gResultString,'','');
end;

procedure HandleInput;
begin
  PSField.Text(TSH_Text, TSH_String, PSField.TextLength(TSH_Text)+1);
  TSH := StrToFloat(TSH_String);
  TSH := TSH * gPreferences.TSHUnitFactor;
  TSH_String := FloatToStr(TSH, 2);
  PSField.Text(T4_Text, T4_String, PSField.TextLength(T4_Text)+1);
  T4:=StrToFloat(T4_String);
  T4_String:=FloatToStr(T4, 2);
  T4:=T4*UT4;
  T4:=T4*gPreferences.T4UnitFactor;
  PSField.Text(T3_Text, T3_String, PSField.TextLength(T3_Text)+1);
  T3:=StrToFloat(T3_String);
  T3_String:=FloatToStr(T3, 2);
  T3:=T3*UT3;
  T3:=T3*gPreferences.T3UnitFactor;
  Calculate(TSH, T4, T3);
  gMessageString:='   GT: ' + gCaseRecord.GTs+kLF+'   GD: ' + gCaseRecord.GDs;
  ShowMessage;
end;

procedure Calculate_ButtonSelect;
begin
    AdjustUnitFactors;
    HandleInput;
end;

procedure TSH_Unit_PopupPopupSelect(Selection: Int16);
begin
  PSApplication.Handled := False; // leave event unhandled
end;

procedure TSH_Unit_ListSelect(Selection: Int16);
begin
end;

procedure T4_Unit_PopupPopupSelect(Selection: Int16);
begin
  gPreferences.T4PopupItem := Selection;
  PSApplication.Handled := False; // leave event unhandled
end;

procedure T3_Unit_PopupPopupSelect(Selection: Int16);
begin
  gPreferences.T3PopupItem := Selection;
  PSApplication.Handled := False; // leave event unhandled
end;

procedure T4_Method_PopupPopupSelect(Selection: Int16);
var
  theItem: Int16;
begin
  theItem := 0;
  PSLIst.ClearCopy(T4_Unit_List);
  if Selection=0 then  {FT4}
  begin
    gPreferences.T4Method := freeHormone;
    PSList.AddCopy(T4_Unit_List,'ng/dl');
    PSList.AddCopy(T4_Unit_List,'ng/l');
    PSList.AddCopy(T4_Unit_List,'pmol/l');
  end
  else if Selection=1 then   {TT4}
  begin
    gPreferences.T4Method := totalHormone;
    PSList.AddCopy(T4_Unit_List,'µg/l');
    PSList.AddCopy(T4_Unit_List,'µg/dl');
    PSList.AddCopy(T4_Unit_List,'nmol/l');
  end;
  PSButton.SetCaption(T4_Unit_Popup, PSList.Item(T4_Unit_List, theItem));
  PSList.SetSelectionIndex(T4_Unit_List, theItem);
  gPreferences.T4MethodPopupItem := Selection;
  PSApplication.Handled := False; // leave event unhandled so that OS can redraw the Popup correctly
end;

procedure T3_Method_PopupPopupSelect(Selection: Int16);
var
  theItem: Int16;
begin
  theItem := 0;
  PSLIst.ClearCopy(T3_Unit_List);
  if Selection=0 then  {FT3}
  begin
    gPreferences.T3Method := freeHormone;
    PSList.AddCopy(T3_Unit_List,'pg/ml');
    PSList.AddCopy(T3_Unit_List,'ng/l');
    PSList.AddCopy(T3_Unit_List,'pmol/l');
  end
  else if Selection=1 then   {TT3}
  begin
    gPreferences.T3Method := totalHormone;
    PSList.AddCopy(T3_Unit_List,'µg/l');
    PSList.AddCopy(T3_Unit_List,'ng/dl');
    PSList.AddCopy(T3_Unit_List,'nmol/l');
  end;
  PSButton.SetCaption(T3_Unit_Popup, PSList.Item(T3_Unit_List, theItem));
  PSList.SetSelectionIndex(T3_Unit_List, theItem);
  gPreferences.T3MethodPopupItem := Selection;
  PSApplication.Handled := False; // leave event unhandled so that OS can redraw the Popup correctly
end;

procedure InitPopups;
begin
    PSLIst.ClearCopy(T4_Unit_List);
    PSLIst.ClearCopy(T3_Unit_List);
    if gPreferences.T4Method = freeHormone then
    begin
      PSButton.SetCaption(T4_Method_Popup, PSList.Item(T4_Method_List, gPreferences.T4MethodPopUpItem));
      PSList.SetSelectionIndex(T4_Method_List, gPreferences.T4MethodPopUpItem);
      PSList.AddCopy(T4_Unit_List,'ng/dl');
      PSList.AddCopy(T4_Unit_List,'ng/l');
      PSList.AddCopy(T4_Unit_List,'pmol/l');
    end
    else
    begin
      PSButton.SetCaption(T4_Method_Popup, PSList.Item(T4_Method_List, gPreferences.T4MethodPopUpItem));
      PSList.SetSelectionIndex(T4_Method_List, gPreferences.T4MethodPopUpItem);
      PSList.AddCopy(T4_Unit_List,'µg/l');
      PSList.AddCopy(T4_Unit_List,'µg/dl');
      PSList.AddCopy(T4_Unit_List,'nmol/l');
    end;
    if gPreferences.T3Method = freeHormone then
    begin
      PSButton.SetCaption(T3_Method_Popup, PSList.Item(T3_Method_List, gPreferences.T3MethodPopUpItem));
      PSList.SetSelectionIndex(T3_Method_List, gPreferences.T3MethodPopUpItem);
      PSList.AddCopy(T3_Unit_List,'pg/ml');
      PSList.AddCopy(T3_Unit_List,'ng/l');
      PSList.AddCopy(T3_Unit_List,'pmol/l');
    end
    else
    begin
      PSButton.SetCaption(T3_Method_Popup, PSList.Item(T3_Method_List, gPreferences.T3MethodPopUpItem));
      PSList.SetSelectionIndex(T3_Method_List, gPreferences.T3MethodPopUpItem);
      PSList.AddCopy(T3_Unit_List,'µg/l');
      PSList.AddCopy(T3_Unit_List,'ng/dl');
      PSList.AddCopy(T3_Unit_List,'nmol/l');
    end;
    PSButton.SetCaption(T3_Unit_Popup, PSList.Item(T3_Unit_List, gPreferences.T3PopUpItem));
    PSList.SetSelectionIndex(T3_Unit_List, gPreferences.T3PopUpItem);
    PSButton.SetCaption(T4_Unit_Popup, PSList.Item(T4_Unit_List, gPreferences.T4PopUpItem));
    PSList.SetSelectionIndex(T4_Unit_List, gPreferences.T4PopUpItem);
end;

procedure OpenMainForm;
begin
   MainFormOpen;
   InitPopUps;
end;


procedure MainFormClose;
begin
  PSLIst.ClearCopy(T4_Unit_List);
  PSLIst.ClearCopy(T3_Unit_List);
  PSApplication.Handled := False; // leave event unhandled
end;

procedure RotateFocus(clockwise: boolean);
begin
  if PSForm.Focus = TSH_Text then
    begin
     if clockwise then
       PSForm.SetFocus(T3_Text)
     else
       PSForm.SetFocus(T4_Text)
    end
  else if PSForm.Focus = T4_Text then
    begin
     if clockwise then
       PSForm.SetFocus(TSH_Text)
     else
       PSForm.SetFocus(T3_Text)
    end
  else if PSForm.Focus = T3_Text then
    begin
     if clockwise then
       PSForm.SetFocus(T4_Text)
     else
       PSForm.SetFocus(TSH_Text)
    end
end;

procedure MainFormKeyDown(Key, Modifiers: UInt16);
begin
  case Key of
    vchrJogUp: RotateFocus(true);
    vchrJogDown: RotateFocus(false);
    chrHorizontalTabulation, chrFormFeed: RotateFocus(false);
    chrVerticalTabulation: RotateFocus(true);
    chrLineFeed, chrCarriageReturn: Calculate_ButtonSelect;
  end;
  PSApplication.Handled := False; // leave event unhandled
end;

function HandleEvent(var Event: EventType): Boolean;
begin
  // generated code, don't modify this function
  PSApplication.Event := @Event;
  PSApplication.Handled := True;

  if not MainFormCustomEvent then
    case Event.eType of
      ctlSelectEvent:
        with Event.ctlSelect do
          case controlID of
            Calculate_Button:
              Calculate_ButtonSelect;

            else
              PSApplication.Handled := False;
          end;

      popSelectEvent:
        with Event.popSelect do
          case controlID of
            TSH_Unit_Popup:
              TSH_Unit_PopupPopupSelect(selection);

            T4_Unit_Popup:
              T4_Unit_PopupPopupSelect(selection);

            T4_Method_Popup:
              T4_Method_PopupPopupSelect(selection);

            T3_Method_Popup:
              T3_Method_PopupPopupSelect(selection);

            T3_Unit_Popup:
              T3_Unit_PopupPopupSelect(selection);

            else
              PSApplication.Handled := False;
          end;

      lstSelectEvent:
        with Event.lstSelect do
          case listID of
            TSH_Unit_List:
              TSH_Unit_ListSelect(selection);

            else
              PSApplication.Handled := False;
          end;

      keyDownEvent:
        MainFormKeyDown(Event.keyDown.chr, Event.keyDown.modifiers);

      menuEvent:
        MainFormMenu(Event.menu.itemID);

      frmOpenEvent:
        OpenMainForm;

      frmCloseEvent:
        MainFormClose;

      else
        PSApplication.Handled := False;
    end;

  Result := PSApplication.Handled;
end;
end.