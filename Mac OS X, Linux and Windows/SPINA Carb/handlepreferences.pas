unit HandlePreferences;

{ SPINA Carb }

{ Application for calculating structure parameters }
{ of insulin-glucose feedback control }

{ Programm zur Berechnung von Strukturparametern }
{ des Insulin-Glukose-Regelkreises }

{ Version 5.1.0 (Cyclone) }

{ (c) J. W. Dietrich, 1994 - 2025 }
{ (c) Ludwig Maximilian University of Munich 1995 - 2002 }
{ (c) University of Ulm Hospitals 2002 - 2004 }
{ (c) Ruhr University of Bochum 2005 - 2025 }

{ This unit handles global application preferences }

{ Source code released under the BSD License }
{ See http://spina.medical-cybernetics.de for details }

{$mode objfpc}
{$ASSERTIONS ON}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, DOM, XMLRead, XMLWrite, StrUtils, Math
  {$IFDEF Windows}
  , Windows
  {$ELSE}
  {$IFDEF LCLCarbon}
      , MacOSAll
  {$ELSE}
  {$IFDEF LCLCocoa}
        , CocoaAll, MacOSAll
  {$ENDIF}
  {$ENDIF}
  , Unix
  {$ENDIF}
  , SPINATypes, HandleCDISC;

function ComputerName: string;
function PreferencesFile: string;
function PreferencesFolder: string;
function RefRangeFile: string;
procedure ReadPreferences;
procedure SavePreferences;


implementation

function PreferencesFolder: string;
  {platform-independend method to search for the location of preferences folder}
const
  kMaxPath = 1024;
  FallBackPath = '~';
  {$IFDEF DARWIN}
  var
    theError: OSErr;
    theRef: FSRef;
    pathBuffer: PChar;
  {$ENDIF}
begin
  {$IFDEF Darwin}
 {standard method for macOS Carbon and Cocoa}
    try
      pathBuffer := Allocmem(kMaxPath);
    except on exception do
      begin
        ShowMessage(PREFERENCES_READ_ERROR_MESSAGE);
        exit;
      end
    end;
    try
      Fillchar(pathBuffer^, kMaxPath, #0);
      Fillchar(theRef, Sizeof(theRef), #0);
      theError := FSFindFolder(kOnAppropriateDisk, kPreferencesFolderType, kDontCreateFolder, theRef);
      if (pathBuffer <> nil) and (theError = noErr) then
      begin
        theError := FSRefMakePath(theRef, pathBuffer, kMaxPath);
        if theError = noErr then
          PreferencesFolder := UTF8ToAnsi(StrPas(pathBuffer)) + '/'
        else
          ShowMessage(PREFERENCES_SAVE_ERROR_MESSAGE);
      end;
    finally
      Freemem(pathBuffer);
    end
  {$ELSE}
  {$IFDEF WINDOWS}
    PreferencesFolder := IncludeTrailingPathDelimiter(sysutils.GetEnvironmentVariable('appdata')) + ApplicationName;
  {$ELSE}
  PreferencesFolder := GetAppConfigDir(False); {standard method for Linux and Unix}
  {$ENDIF}
  {$ENDIF}
end;

function PreferencesFile: string;
  {delivers path to preferences file}
var
  prefsFolder: string;
begin
  {$IFDEF DARWIN}
    prefsFolder := PreferencesFolder;
    if prefsFolder = '' then
      PreferencesFile := ''
    else
      PreferencesFile := PreferencesFolder + SPINA_CARB_GLOBAL_ID + '.xml';
  {$ELSE}
  {$IFDEF WINDOWS}
    PreferencesFile := IncludeTrailingPathDelimiter(PreferencesFolder) + 'SPINA Carb.cfg';
  {$ELSE}
  PreferencesFile := GetAppConfigFile(False);
  {$ENDIF}
  {$ENDIF}
end;

function RRFile: string;
  {delivers path to CDISC-compliant XML file with reference values}
var
  prefsFolder: string;
begin
  prefsFolder := PreferencesFolder;
  if prefsFolder = '' then
    RRFile := ''
  else
    RRFile := IncludeTrailingPathDelimiter(PreferencesFolder) +
      SPINA_CARB_GLOBAL_ID + '.ref-ranges.xml';
end;

function ComputerName: string;
  {inspired by Zoran's post at http://forum.lazarus.freepascal.org/index.php?topic=23622.0 }
  {$IFDEF mswindows}
const
  INFO_BUFFER_SIZE = 32767;
var
  Buffer: array[0..INFO_BUFFER_SIZE] of widechar;
  Ret: DWORD;
begin
  Ret := INFO_BUFFER_SIZE;
  if (GetComputerNameW(@Buffer[0], Ret)) then
  begin
    Result := UTF8Encode(WideString(Buffer));
  end
  else
  begin
    Result := 'ERROR_NO_COMPUTERNAME_RETURNED';
  end;
end;
{$ENDIF}
{$IFDEF UNIX}
begin
  result := GetHostName;
end;
{$ENDIF}

procedure SubstitutePreferences;
{get standard values for preferences if preferences file nonexistent or corrupt}
begin
  gNumberFormat := STANDARD_NUM_FORMAT;
  gDateTimeFormat := STANDARD_TIME_FORMAT;
end;

function NodeContent(theRoot: TDOMNode; Name: string): string;
  {supports XML routines, gets the contents of a node in a file}
var
  theNode: TDOMNode;
begin
  if assigned(theRoot) then
    theNode := theRoot.FindNode(Name)
  else
    theNode := nil;
  if assigned(theNode) then
  begin
    try
      Result := UTF8Encode(theNode.TextContent);
    except
      Result := 'NA';
    end;
  end
  else
    Result := 'NA';
end;

procedure VarFromNode(theRoot: TDOMNode; Name: string; var theVar: real);
{supports XML routines}
var
  theString: string;
begin
  theString := NodeContent(theRoot, Name);
  if theString <> 'NA' then
    theVar := StrToFloatDef(theString, Math.Nan);
end;

function SimpleNode(Doc: TXMLDocument; Name, Value: string): TDOMNode;
  {supports XML routines, creates an XML node from the contents of a string}
var
  ItemNode, TextNode: TDOMNode;
begin
  assert(assigned(Doc));
  ItemNode := Doc.CreateElement(Name);
  TextNode := Doc.CreateTextNode(UTF8Decode(Value));
  ItemNode.AppendChild(TextNode);
  Result := ItemNode;
end;

function AttributeValue(theNode: TDOMNode; theName: string): string;
  {this functions finds an attribute of an XML tag and delivers its value}
var
  i: integer;
  foundValue: string;
begin
  foundValue := '';
  if assigned(theNode) then
    for i := 0 to theNode.Attributes.Length - 1 do
    begin
      if theNode.Attributes[i].NodeName = theName then
        foundValue := theNode.Attributes[i].NodeValue;
    end;
  Result := foundValue;
end;

procedure SavePreferences;
{save preferences file}
var
  theFileName, PreferencesFolder: string;
  Doc: TXMLDocument;
  {StartComment: TDOMComment;}
  RootNode, ElementNode: TDOMNode;
begin
  theFileName := PreferencesFile;
  PreferencesFolder := PreferencesFolder;
  Doc := TXMLDocument.Create;
  try
    {StartComment := Doc.CreateComment('SPINA Preferences');
    RootNode.AppendChild(StartComment);}

    RootNode := Doc.CreateElement('preferences');
    Doc.Appendchild(RootNode);
    RootNode := Doc.DocumentElement;

    if gPreferences.rememberUsedUnits then
      RootNode.AppendChild(SimpleNode(Doc, 'remember', 'true'))
    else
      RootNode.AppendChild(SimpleNode(Doc, 'remember', 'false'));

    ElementNode := Doc.CreateElement('mandatoryfields');
    if gPreferences.colouriseMandatoryFields then
      ElementNode.AppendChild(SimpleNode(Doc, 'colourise', 'true'))
    else
      ElementNode.AppendChild(SimpleNode(Doc, 'colourise', 'false'));
    ElementNode.AppendChild(SimpleNode(Doc, 'colour',
      Dec2Numb(gPreferences.MandatoryColor, 6, 16)));
    RootNode.AppendChild(ElementNode);

    ElementNode := Doc.CreateElement('mshinfo');
    {$IFDEF Darwin}
    TDOMElement(ElementNode).SetAttribute('id', UTF8Decode(gPreferences.MSH_ID));
    {$ELSE}
    TDOMElement(ElementNode).SetAttribute('id', gPreferences.MSH_ID);
    {$ENDIF}
    RootNode.AppendChild(ElementNode);

    ElementNode := Doc.CreateElement('placer');
    {$IFDEF Darwin}
    TDOMElement(ElementNode).SetAttribute('id', UTF8Decode(gPreferences.Placer_ID));
    {$ELSE}
    TDOMElement(ElementNode).SetAttribute('id', gPreferences.Placer_ID);
    {$ENDIF}
    RootNode.AppendChild(ElementNode);

    ElementNode := Doc.CreateElement('LOINC');
    if gPreferences.exportLOINC then
      ElementNode.AppendChild(SimpleNode(Doc, 'export', 'true'))
    else
      ElementNode.AppendChild(SimpleNode(Doc, 'export', 'false'));
    RootNode.AppendChild(ElementNode);

    ElementNode := Doc.CreateElement('fonts');
    ElementNode.AppendChild(SimpleNode(Doc, 'printFont', gPreferences.PrintFont));
    RootNode.AppendChild(ElementNode);

    if not DirectoryExists(PreferencesFolder) then
      if not CreateDir(PreferencesFolder) then
        ShowMessage(PREFERENCES_SAVE_ERROR_MESSAGE);
    if DirectoryExists(PreferencesFolder) then
    begin
      if FileExists(theFileName) then
        SysUtils.DeleteFile(theFileName);
      WriteXMLFile(Doc, theFileName);
    end;
  finally
    Doc.Free;
  end;
end;

procedure CreateNewPreferences;
{creates a new datastructure for preferences with standard entries}
begin
  gStandardMandatoryColor := clLtYellow;
  with gPreferences do
  begin
    rememberUsedUnits := True;
    colouriseMandatoryFields := True;
    exportLOINC := True;
    MandatoryColor := gStandardMandatoryColor;
    MSH_ID := '';
    gPreferences.new := True;
  end;
  SavePreferences;
end;

function RefRangeFile: string;
  {delivers path to CDISC-compliant XML file with reference values}
var
  prefsFolder: string;
begin
  prefsFolder := PreferencesFolder;
  if prefsFolder = '' then
    Result := ''
  else
    Result := IncludeTrailingPathDelimiter(PreferencesFolder) +
      SPINA_CARB_GLOBAL_ID + '.ref-ranges.xml';
end;

procedure ReadPreferences;
{reads preferences file}
var
  Doc: TXMLDocument;
  RootNode: TDOMNode;
  theFileName, theString: string;
  theFileHandle: longint;
  XMLfound: boolean;
begin
  gStandardMandatoryColor := clLtYellow;
  XMLfound := False;
  theFileName := PreferencesFile;
  if FileExists(theFileName) then {simple check for XML file}
  begin
    theFileHandle := FileOpen(theFileName, fmOpenRead);
    try
      FileRead(theFileHandle, theString, SizeOf(theString));
      if pos('xml', LowerCase(theString)) > 0 then
        XMLfound := True;
    finally
      FileClose(theFileHandle);
    end;
  end;
  if XMLfound then {file present and marked as XML file}
  try
    ReadXMLFile(Doc, theFileName);

    theString := NodeContent(Doc.DocumentElement, 'remember');
    if theString = 'true' then
      gPreferences.rememberUsedUnits := True
    else
      gPreferences.rememberUsedUnits := False;

    RootNode := Doc.DocumentElement.FindNode('mandatoryfields');
    theString := NodeContent(RootNode, 'colourise');
    if theString = 'true' then
      gPreferences.colouriseMandatoryFields := True
    else
      gPreferences.colouriseMandatoryFields := False;
    theString := NodeContent(RootNode, 'colour');
    if (theString = '') or (theString = 'NA') then
      gPreferences.MandatoryColor := gStandardMandatoryColor  {Standard colour}
    else
    try
      gPreferences.MandatoryColor := TColor(Hex2Dec(theString));
    except
      gPreferences.MandatoryColor := clDefault;
    end;

    RootNode := Doc.DocumentElement.FindNode('LOINC');
    theString := NodeContent(RootNode, 'export');
    if theString = 'true' then
      gPreferences.exportLOINC := True
    else
      gPreferences.exportLOINC := False;

    RootNode := Doc.DocumentElement.FindNode('mshinfo');

    if RootNode <> nil then
      if RootNode.HasAttributes and (RootNode.Attributes.Length > 0) then
        {$IFDEF LCLCarbon}
         gPreferences.MSH_ID := UTF8Encode(RootNode.Attributes[0].NodeValue);
        {$ELSE}
        gPreferences.MSH_ID := RootNode.Attributes[0].NodeValue;
    {$ENDIF}

    RootNode := Doc.DocumentElement.FindNode('placer');

    if RootNode <> nil then
      if RootNode.HasAttributes and (RootNode.Attributes.Length > 0) then
        {$IFDEF LCLCarbon}
         gPreferences.Placer_ID := UTF8Encode(RootNode.Attributes[0].NodeValue);
        {$ELSE}
        gPreferences.Placer_ID := RootNode.Attributes[0].NodeValue;
    {$ENDIF}

    RootNode := Doc.DocumentElement.FindNode('fonts');
    theString := NodeContent(RootNode, 'printFont');
    gPreferences.PrintFont := theString;

    gPreferences.new := False;
  finally
    Doc.Free;
  end
  else  {Standards from dialog, if preference file does not exist}
    CreateNewPreferences;  {fall-back solution, if file does not exist}
end;

initialization

begin
  gPreferences.ReferenceValues := sReferenceValues;
end;

end.
