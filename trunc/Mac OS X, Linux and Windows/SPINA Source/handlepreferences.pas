unit HandlePreferences;

{ SPINA-Thyr }

{ Application for calculating structure parameters }
{ of thyrotropic feedback control }

{ Programm zur Berechnung von Strukturparametern }
{ des thyreotropen Regelkreises }

{ Version 3.4.0 }

{ (c) J. W. Dietrich, 1994 - 2014 }
{ (c) Ludwig Maximilian University of Munich 1995 - 2002 }
{ (c) University of Ulm Hospitals 2002-2004 }
{ (c) Ruhr University of Bochum 2005 - 2014 }

{ This unit handles global application preferences }

{ Source code released under the BSD License }
{ See http://spina.medical-cybernetics.de for details }

{$mode objfpc}

{Return codes of procedure GetReferenceValues:
0: No Error.
1: Malformatted XML file
2: Root node with ID "SPIt" not found
6: Error saving file.
10: New file created.
}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, SPINA_Types, DOM, XMLRead, XMLWrite, StrUtils, Math
  {$IFDEF win32}
  , Windows
  {$ELSE}
    {$IFDEF LCLCarbon}
      , MacOSAll
    {$ENDIF}
  {$ENDIF},
  UnitConverter, CDISC;

function DecodeGreek(theString: string): string;
function EncodeGreek(theString: string): string;
function GetPreferencesFile: String;
function GetPreferencesFolder: String;
function RRFile: String;
procedure ComposeRRStrings;
procedure GetReferenceValues(theFileName: String; var returnCode: integer);
procedure InitThyroidHormoneConversionFactors;
procedure ReadPreferences;
procedure SavePreferences;


implementation

procedure InitThyroidHormoneConversionFactors;
{sets labels and appropriate conversion factors for the elements of measurement units}
var
  UFT4, UFT3: real;
begin
  T4UnitLabel[0] := 'g';
  T4UnitLabel[1] := 'mol';
  UFT4 := ConvertedValue(1, T4_MOLAR_MASS, 'ng/dl', 'mol/l');
  T4UnitFactor[0] := PrefixFactor[1] * UFT4 / PrefixFactor[5]; {since UFT4 converts between ng/dl and mol/l}
  T4UnitFactor[1] := 1;
  T3UnitLabel[0] := 'g';
  T3UnitLabel[1] := 'mol';
  UFT3 := ConvertedValue(1, T3_MOLAR_MASS, 'pg/ml', 'mol/l');
  T3UnitFactor[0] := PrefixFactor[3] * UFT3 / PrefixFactor[6]; {as UFT3 converts between pg/ml and mol/l}
  T3UnitFactor[1] := 1;
end;

function GetPreferencesFolder: String;
{platform-independend method to search for the location of preferences folder}
const
  kMaxPath = 1024;
var
  {$IFDEF DARWIN}
  theError: OSErr;
  theRef: FSRef;
  {$ENDIF}
  pathBuffer: PChar;
begin
  {$IFDEF DARWIN} {standard method for Mac OS X}
    try
      pathBuffer := Allocmem(kMaxPath);
    except on exception do
      begin
        GetPreferencesFolder := '';
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
        if theError = noErr then GetPreferencesFolder := UTF8ToAnsi(StrPas(pathBuffer)) + '/';
      end;
    finally
      Freemem(pathBuffer);
    end
  {$ELSE}
    GetPreferencesFolder := GetAppConfigDir(false); {standard method for Linux and Windows}
  {$ENDIF}
end;

function GetPreferencesFile: String;
{delivers path to preferences file}
var
  prefsFolder: String;
begin
  {$IFDEF LCLCarbon}
    prefsFolder := GetPreferencesFolder;
    if prefsFolder = '' then
      GetPreferencesFile := ''
    else
      GetPreferencesFile := GetPreferencesFolder + SPINA_THYR_GLOBAL_ID + '.xml';
  {$ELSE}
    GetPreferencesFile := GetAppConfigFile(false);
  {$ENDIF}
end;

function RRFile: String;
{delivers path to CDISC-compliant XML file with reference values}
var
  prefsFolder: String;
begin
   prefsFolder := GetPreferencesFolder;
   if prefsFolder = '' then
     RRFile := ''
   else
     RRFile := GetPreferencesFolder + SPINA_THYR_GLOBAL_ID + '.ref-ranges.xml';
end;

function EncodeGreek(theString: string): string;
{encodes greek mu letter as ASCII substitution sequence}
var
  theFlags: TReplaceFlags;
begin
  theFlags := [rfReplaceAll, rfIgnoreCase];
  Result := StringReplace(theString, #194#181, 'mc', theFlags);
end;

function DecodeGreek(theString: string): string;
{decodes ASCII substitution sequence for greek mu letter}
var
  theFlags: TReplaceFlags;
begin
  theFlags := [rfReplaceAll, rfIgnoreCase];
  result := UTF8Decode(StringReplace(theString, 'mc', #194#181, theFlags));
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
    theVar := StrToFloat(theString);
end;

function SimpleNode(Doc: TXMLDocument; Name, Value: string): TDOMNode;
  {supports XML routines, creates an XML node from the contents of a string}
var
  ItemNode, TextNode: TDOMNode;
begin
  ItemNode := Doc.CreateElement(Name);
  TextNode := Doc.CreateTextNode(UTF8Decode(Value));
  ItemNode.AppendChild(TextNode);
  Result := ItemNode;
end;

function AttributeValue(theNode: TDOMNode; theName: String): String;
{this functions finds an attribute of an XML tag and delivers its value}
var
  i: integer;
  foundValue: String;
begin
  foundValue := '';
  if assigned(theNode) then
    for i := 0 to theNode.Attributes.Length - 1 do
      begin
        if theNode.Attributes[i].NodeName = theName then
          foundValue := theNode.Attributes[i].NodeValue;
      end;
  result := foundValue;
end;

procedure SavePreferences;
{save preferences file}
var
  theFileName, PreferencesFolder: String;
  Doc: TXMLDocument;
  StartComment: TDOMComment;
  RootNode, ElementNode, ItemNode, TextNode: TDOMNode;
begin
  theFileName := GetPreferencesFile;
  PreferencesFolder := GetPreferencesFolder;
  try
    Doc := TXMLDocument.Create;

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
    ElementNode.AppendChild(SimpleNode(Doc, 'colour', Dec2Numb(gPreferences.MandatoryColor, 6, 16)));
    RootNode.AppendChild(ElementNode);

    ElementNode := Doc.CreateElement('methods');
    if gPreferences.T4.Method = freeHormone then
      ElementNode.AppendChild(SimpleNode(Doc, 'T4', 'free'))
    else
      ElementNode.AppendChild(SimpleNode(Doc, 'T4', 'total'));
    if gPreferences.T3.Method = freeHormone then
      ElementNode.AppendChild(SimpleNode(Doc, 'T3', 'free'))
    else
      ElementNode.AppendChild(SimpleNode(Doc, 'T3', 'total'));
    RootNode.AppendChild(ElementNode);

    ElementNode:=Doc.CreateElement('units');
    ElementNode.AppendChild(SimpleNode(Doc, 'TSH', gPreferences.TSH.UOM));
    ElementNode.AppendChild(SimpleNode(Doc, 'T4', gPreferences.T4.UOM));
    ElementNode.AppendChild(SimpleNode(Doc, 'T3', gPreferences.T3.UOM));
    RootNode.AppendChild(ElementNode);

    ElementNode := Doc.CreateElement('mshinfo');
    {$IFDEF LCLCarbon}
    TDOMElement(ElementNode).SetAttribute('id', UTF8Decode(gPreferences.MSH_ID));
    {$ELSE}
    TDOMElement(ElementNode).SetAttribute('id', UTF8ToSys(gPreferences.MSH_ID));
    {$ENDIF}
    RootNode.AppendChild(ElementNode);

    if not DirectoryExists(PreferencesFolder) then
      if not CreateDir(PreferencesFolder) then
        ShowMessage(PREFERENCES_SAVE_ERROR_MESSAGE);
    if DirectoryExists(PreferencesFolder) then
        begin
          if FileExists(theFileName) then
            SysUtils.DeleteFile(theFileName);
          WriteXMLFile(Doc,theFileName);
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
      rememberUsedUnits := true;
      colouriseMandatoryFields := true;
      T4.Method := freeHormone;
      T3.Method := freeHormone;
      TSH.UOM := TSH_UNIT;
      T4.UOM := FT4_UNIT;
      T3.UOM := FT3_UNIT;
      TSH.PopUpItem := 0;
      T4.PopUpItem := 0;
      T3.PopUpItem := 0;
      T4.MethodPopUpItem := 0;
      T3.MethodPopUpItem := 0;
      MandatoryColor := gStandardMandatoryColor;
      MSH_ID := '';
      gPreferences.new := true;
    end;
  SavePreferences;
end;

procedure ReadPreferences;
{reads preferences file}
var
  Doc: TXMLDocument;
  RootNode, theNode: TDOMNode;
  theFileName, theString: String;
  theFileHandle: longint;
  XMLfound: boolean;
begin
  gStandardMandatoryColor := clLtYellow;
  XMLfound := false;
  theFileName := GetPreferencesFile;
  if FileExists(theFileName) then {simple check for XML file}
  try
    theFileHandle := FileOpen(theFileName, fmOpenRead);
    FileRead(theFileHandle, theString, SizeOf(theString));
    if pos('xml', LowerCase(theString)) > 0 then
      XMLfound := true;
  finally
    FileClose(theFileHandle);
  end;
  if XMLfound then {file present and marked as XML file}
    try
      ReadXMLFile(Doc, theFileName);

      theString := NodeContent(Doc.DocumentElement, 'remember');
      if theString = 'true' then
        gPreferences.rememberUsedUnits := true
      else
        gPreferences.rememberUsedUnits := false;

      RootNode := Doc.DocumentElement.FindNode('mandatoryfields');
      theString := NodeContent(RootNode, 'colourise');
      if theString = 'true' then
        gPreferences.colouriseMandatoryFields := true
      else
        gPreferences.colouriseMandatoryFields := false;
      theString := NodeContent(RootNode, 'colour');
      if (theString = '') or (theString = 'NA') then
        gPreferences.MandatoryColor := gStandardMandatoryColor  {Standard colour}
      else
        try
          gPreferences.MandatoryColor := TColor(Hex2Dec(theString));
        except
          gPreferences.MandatoryColor := clDefault;
        end;

      RootNode := Doc.DocumentElement.FindNode('methods');
      theString := NodeContent(RootNode, 'T4');
      if theString = 'free' then
        gPreferences.T4.Method := freeHormone
      else
        gPreferences.T4.Method := totalHormone;
      theString := NodeContent(RootNode, 'T3');
      if theString = 'free' then
        gPreferences.T3.Method := freeHormone
      else
        gPreferences.T3.Method := totalHormone;

      RootNode := Doc.DocumentElement.FindNode('units');
      theString := NodeContent(RootNode, 'TSH');
      gPreferences.TSH.UOM := theString;
      theString := NodeContent(RootNode, 'T4');
      gPreferences.T4.UOM := theString;
      theString := NodeContent(RootNode, 'T3');
      gPreferences.T3.UOM := theString;

      RootNode := Doc.DocumentElement.FindNode('mshinfo');

      if RootNode <> nil then
        if RootNode.HasAttributes and (RootNode.Attributes.Length>0) then
         {$IFDEF LCLCarbon}
         gPreferences.MSH_ID := UTF8Encode(RootNode.Attributes[0].NodeValue);
         {$ELSE}
         gPreferences.MSH_ID := SysToUTF8(RootNode.Attributes[0].NodeValue);
         {$ENDIF}

      if (gPreferences.TSH.UOM = 'NA') or (gPreferences.T4.UOM = 'NA') or (gPreferences.T3.UOM = 'NA') then
        CreateNewPreferences;  {fall-back solution, if file is corrupt or in obsolete format}
      gPreferences.new := false;
    finally
      Doc.Free;
    end
  else  {Standards from dialog, if preference file does not exist}
    CreateNewPreferences;  {fall-back solution, if file does not exist}
end;

function UnitFactor(unitElements: TUnitElements; baseFactor: real): real;
{calculates conversion factors from parsed unit strings}
var
  i, mpIndex, vpIndex, j4: integer;
  tempFactor: real;
begin
  mpIndex := 0;    {Index for mass prefix}
  vpIndex := 0;    {index for volume prefix}
  for i := MAXFACTORS - 1 downto 0 do
    begin
      if unitElements.MassPrefix = PrefixLabel[i] then mpIndex := i;
      if unitElements.VolumePrefix = PrefixLabel[i] then vpIndex := i;
    end;
  tempFactor := PrefixFactor[mpIndex] * baseFactor / PrefixFactor[vpIndex];
  UnitFactor := tempFactor;
end;

function UnitFactor(theUnit: String; baseFactor: real): real;
{calculates conversion factors from parsed unit strings}
var
  unitElements: TUnitElements;
  tempFactor: real;
begin
  UnitElements := ParsedUnitString(EncodeGreek(theUnit));
  tempFactor := UnitFactor(unitElements, baseFactor);
  UnitFactor := tempFactor;
end;

procedure ComposeRRStrings;
{creates strings with upper and lower bounds of reference ranges for hints and printout}
begin
  if gPreferences.TSH.isSI then
    begin
      gReferenceRanges.TSH.ln := gSIReferenceRanges.TSH.ln;
      gReferenceRanges.TSH.hn := gSIReferenceRanges.TSH.hn;
      gReferenceRanges.TSH.UOM := gSIReferenceRanges.TSH.UOM;
    end
  else
    begin
      gReferenceRanges.TSH.ln := gConvReferenceRanges.TSH.ln;
      gReferenceRanges.TSH.hn := gConvReferenceRanges.TSH.hn;
      gReferenceRanges.TSH.UOM := gConvReferenceRanges.TSH.UOM;
    end;
  if gPreferences.T4.isSI then
    begin
      gReferenceRanges.FT4.ln := gSIReferenceRanges.FT4.ln;
      gReferenceRanges.FT4.hn := gSIReferenceRanges.FT4.hn;
      gReferenceRanges.TT4.ln := gSIReferenceRanges.TT4.ln;
      gReferenceRanges.TT4.hn := gSIReferenceRanges.TT4.hn;
      gReferenceRanges.FT4.UOM := gSIReferenceRanges.FT4.UOM;
      gReferenceRanges.TT4.UOM := gSIReferenceRanges.TT4.UOM;
      gReferenceRanges.FT4.ln := gReferenceRanges.FT4.ln * UnitFactor(gReferenceRanges.FT4.UOM, T4UnitFactor[1]) / UnitFactor(gPreferences.T4.UOM, T4UnitFactor[1]);
      gReferenceRanges.FT4.hn := gReferenceRanges.FT4.hn * UnitFactor(gReferenceRanges.FT4.UOM, T4UnitFactor[1]) / UnitFactor(gPreferences.T4.UOM, T4UnitFactor[1]);
      gReferenceRanges.TT4.ln := gReferenceRanges.TT4.ln * UnitFactor(gReferenceRanges.TT4.UOM, T4UnitFactor[1]) / UnitFactor(gPreferences.T4.UOM, T4UnitFactor[1]);
      gReferenceRanges.TT4.hn := gReferenceRanges.TT4.hn * UnitFactor(gReferenceRanges.TT4.UOM, T4UnitFactor[1]) / UnitFactor(gPreferences.T4.UOM, T4UnitFactor[1]);
    end
  else
    begin
      gReferenceRanges.FT4.ln := gConvReferenceRanges.FT4.ln;
      gReferenceRanges.FT4.hn := gConvReferenceRanges.FT4.hn;
      gReferenceRanges.TT4.ln := gConvReferenceRanges.TT4.ln;
      gReferenceRanges.TT4.hn := gConvReferenceRanges.TT4.hn;
      gReferenceRanges.FT4.UOM := gConvReferenceRanges.FT4.UOM;
      gReferenceRanges.TT4.UOM := gConvReferenceRanges.TT4.UOM;
      gReferenceRanges.FT4.ln := gReferenceRanges.FT4.ln * UnitFactor(gReferenceRanges.FT4.UOM, T4UnitFactor[0]) / UnitFactor(gPreferences.T4.UOM, T4UnitFactor[0]);
      gReferenceRanges.FT4.hn := gReferenceRanges.FT4.hn * UnitFactor(gReferenceRanges.FT4.UOM, T4UnitFactor[0]) / UnitFactor(gPreferences.T4.UOM, T4UnitFactor[0]);
      gReferenceRanges.TT4.ln := gReferenceRanges.TT4.ln * UnitFactor(gReferenceRanges.TT4.UOM, T4UnitFactor[0]) / UnitFactor(gPreferences.T4.UOM, T4UnitFactor[0]);
      gReferenceRanges.TT4.hn := gReferenceRanges.TT4.hn * UnitFactor(gReferenceRanges.TT4.UOM, T4UnitFactor[0]) / UnitFactor(gPreferences.T4.UOM, T4UnitFactor[0]);
    end;
  if gPreferences.T3.isSI then
    begin
      gReferenceRanges.FT3.ln := gSIReferenceRanges.FT3.ln;
      gReferenceRanges.FT3.hn := gSIReferenceRanges.FT3.hn;
      gReferenceRanges.TT3.ln := gSIReferenceRanges.TT3.ln;
      gReferenceRanges.TT3.hn := gSIReferenceRanges.TT3.hn;
      gReferenceRanges.FT3.UOM := gSIReferenceRanges.FT3.UOM;
      gReferenceRanges.TT3.UOM := gSIReferenceRanges.TT3.UOM;
      gReferenceRanges.FT3.ln := gReferenceRanges.FT3.ln * UnitFactor(gReferenceRanges.FT3.UOM, T3UnitFactor[1]) / UnitFactor(gPreferences.T3.UOM, T3UnitFactor[1]);
      gReferenceRanges.FT3.hn := gReferenceRanges.FT3.hn * UnitFactor(gReferenceRanges.FT3.UOM, T3UnitFactor[1]) / UnitFactor(gPreferences.T3.UOM, T3UnitFactor[1]);
      gReferenceRanges.TT3.ln := gReferenceRanges.TT3.ln * UnitFactor(gReferenceRanges.TT3.UOM, T3UnitFactor[1]) / UnitFactor(gPreferences.T3.UOM, T3UnitFactor[1]);
      gReferenceRanges.TT3.hn := gReferenceRanges.TT3.hn * UnitFactor(gReferenceRanges.TT3.UOM, T3UnitFactor[1]) / UnitFactor(gPreferences.T3.UOM, T3UnitFactor[1]);
    end
  else
    begin
      gReferenceRanges.FT3.ln := gConvReferenceRanges.FT3.ln;
      gReferenceRanges.FT3.hn := gConvReferenceRanges.FT3.hn;
      gReferenceRanges.TT3.ln := gConvReferenceRanges.TT3.ln;
      gReferenceRanges.TT3.hn := gConvReferenceRanges.TT3.hn;
      gReferenceRanges.FT3.UOM := gConvReferenceRanges.FT3.UOM;
      gReferenceRanges.TT3.UOM := gConvReferenceRanges.TT3.UOM;
      gReferenceRanges.FT3.ln := gReferenceRanges.FT3.ln * UnitFactor(gReferenceRanges.FT3.UOM, T3UnitFactor[0]) / UnitFactor(gPreferences.T3.UOM, T3UnitFactor[0]);
      gReferenceRanges.FT3.hn := gReferenceRanges.FT3.hn * UnitFactor(gReferenceRanges.FT3.UOM, T3UnitFactor[0]) / UnitFactor(gPreferences.T3.UOM, T3UnitFactor[0]);
      gReferenceRanges.TT3.ln := gReferenceRanges.TT3.ln * UnitFactor(gReferenceRanges.TT3.UOM, T3UnitFactor[0]) / UnitFactor(gPreferences.T3.UOM, T3UnitFactor[0]);
      gReferenceRanges.TT3.hn := gReferenceRanges.TT3.hn * UnitFactor(gReferenceRanges.TT3.UOM, T3UnitFactor[0]) / UnitFactor(gPreferences.T3.UOM, T3UnitFactor[0]);
    end;
  if IsNan(gReferenceRanges.TSH.ln) then
    gTSHRR := NA_MARK
  else
    gTSHRR := FloatToStrF(gReferenceRanges.TSH.ln, ffFixed, 5, 2) + ' - ' + FloatToStrF(gReferenceRanges.TSH.hn, ffFixed, 5, 2) + ' mU/L';
  if IsNan(gReferenceRanges.FT4.ln) then
    gFT4RR := NA_MARK
  else
    gFT4RR := FloatToStrF(gReferenceRanges.FT4.ln, ffFixed, 5, 2) + ' - ' + FloatToStrF(gReferenceRanges.FT4.hn, ffFixed, 5, 2) + ' ' + gPreferences.T4.UOM;
  if IsNan(gReferenceRanges.FT3.ln) then
    gFT3RR := NA_MARK
  else
    gFT3RR := FloatToStrF(gReferenceRanges.FT3.ln, ffFixed, 5, 2) + ' - ' + FloatToStrF(gReferenceRanges.FT3.hn, ffFixed, 5, 2) + ' ' + gPreferences.T3.UOM;
  if IsNan(gReferenceRanges.TT4.ln) then
    gTT4RR := NA_MARK
  else
    gTT4RR := FloatToStrF(gReferenceRanges.TT4.ln, ffFixed, 5, 2) + ' - ' + FloatToStrF(gReferenceRanges.TT4.hn, ffFixed, 5, 2) + ' ' + gPreferences.T4.UOM;
  if IsNan(gReferenceRanges.TT3.ln) then
    gTT3RR := NA_MARK
  else
    gTT3RR := FloatToStrF(gReferenceRanges.TT3.ln, ffFixed, 5, 2) + ' - ' + FloatToStrF(gReferenceRanges.TT3.hn, ffFixed, 5, 2) + ' ' + gPreferences.T3.UOM;
  if IsNan(gReferenceRanges.GT.ln) then
    gGTRR := NA_MARK
  else
    gGTRR := FloatToStrF(gReferenceRanges.GT.ln * 1e12, ffFixed, 5, 2) + ' - ' + FloatToStrF(gReferenceRanges.GT.hn * 1e12, ffFixed, 5, 2) + ' pmol/s';
  if IsNan(gReferenceRanges.GD.ln) then
    gGDRR := NA_MARK
  else
    gGDRR := FloatToStrF(gReferenceRanges.GD.ln * 1e9, ffFixed, 5, 0) + ' - ' + FloatToStrF(gReferenceRanges.GD.hn * 1e9, ffFixed, 5, 0) + ' nmol/s';
  if IsNan(gReferenceRanges.TSHI.ln) then
    gTSHIRR := NA_MARK
  else
    gTSHIRR := FloatToStrF(gReferenceRanges.TSHI.ln, ffFixed, 5, 1) + ' - ' + FloatToStrF(gReferenceRanges.TSHI.hn, ffFixed, 5, 1) + ' ';
  if IsNan(gReferenceRanges.TTSI.ln) then
    gTTSIRR := NA_MARK
  else
    gTTSIRR := FloatToStrF(gReferenceRanges.TTSI.ln, ffFixed, 5, 0) + ' - ' + FloatToStrF(gReferenceRanges.TTSI.hn, ffFixed, 5, 0) + ' ';
end;

procedure GetReferenceValues(theFileName: String; var returnCode: integer);
{reads reference values from a CDISC LAB model-compliant XML file.}
{This version of the routine ignores sex- and age-specific reference values}
var
  Doc: TXMLDocument;
  RootNode, theNode, BatteryNode, BaseTestNode, FlagUOMNode, NormalNode, UnitsNode, NormalDefinitionNode: TDOMNode;
  theString: String;
  theStream: TStringStream;
  oldSeparator: Char;
  SI: boolean;
begin
  returnCode := 0;           {no error}
  with gReferenceRanges do
    begin                   {define emtpy default values}
      TSH.ln := Math.NaN;
      TSH.hn := Math.NaN;
      FT4.ln := Math.NaN;
      FT4.hn := Math.NaN;
      TT4.ln := Math.NaN;
      TT4.hn := Math.NaN;
      FT3.ln := Math.NaN;
      FT3.hn := Math.NaN;
      TT3.ln := Math.NaN;
      TT3.hn := Math.NaN;
      GT.ln := Math.NaN;
      GT.hn := Math.NaN;
      GD.ln := Math.NaN;
      GD.hn := Math.NaN;
      TSHI.ln := Math.NaN;
      TSHI.hn := Math.NaN;
      TTSI.ln := Math.NaN;
      TTSI.hn := Math.NaN;
    end;
  with gSIReferenceRanges do
    begin                   {define emtpy default values}
      TSH.ln := Math.NaN;
      TSH.hn := Math.NaN;
      FT4.ln := Math.NaN;
      FT4.hn := Math.NaN;
      TT4.ln := Math.NaN;
      TT4.hn := Math.NaN;
      FT3.ln := Math.NaN;
      FT3.hn := Math.NaN;
      TT3.ln := Math.NaN;
      TT3.hn := Math.NaN;
      GT.ln := Math.NaN;
      GT.hn := Math.NaN;
      GD.ln := Math.NaN;
      GD.hn := Math.NaN;
      TSHI.ln := Math.NaN;
      TSHI.hn := Math.NaN;
      TTSI.ln := Math.NaN;
      TTSI.hn := Math.NaN;
   end;
  with gConvReferenceRanges do
    begin                   {define emtpy default values}
      TSH.ln := Math.NaN;
      TSH.hn := Math.NaN;
      FT4.ln := Math.NaN;
      FT4.hn := Math.NaN;
      TT4.ln := Math.NaN;
      TT4.hn := Math.NaN;
      FT3.ln := Math.NaN;
      FT3.hn := Math.NaN;
      TT3.ln := Math.NaN;
      TT3.hn := Math.NaN;
      GT.ln := Math.NaN;
      GT.hn := Math.NaN;
      GD.ln := Math.NaN;
      GD.hn := Math.NaN;
      TSHI.ln := Math.NaN;
      TSHI.hn := Math.NaN;
      TTSI.ln := Math.NaN;
      TTSI.hn := Math.NaN;
    end;
  oldSeparator := DecimalSeparator;
  DecimalSeparator := DEC_POINT;
  if not FileExists(theFileName) then
    begin
      SaveStandardCDISC_RRFile(theFileName, returnCode);  {saves a minimal standard file}
      if returnCode = 0 then    {no error,}
        returnCode := 10;       {therefore new file created}
    end;
  if FileExists(theFileName) then       {could this file be created (or did it already exist)?}
  try
    ReadXMLFile(Doc, theFileName);
    RootNode := Doc.DocumentElement.FindNode('Study');
    if assigned(RootNode) then
      if AttributeValue(RootNode, 'ID') = 'SPIt' then
        begin
          BatteryNode := RootNode.FindNode('BaseBattery');
          while assigned(BatteryNode) do
            begin
            if AttributeValue(BatteryNode, 'ID') = 'SPIt' then
              begin
                BaseTestNode := BatteryNode.FindNode('BaseTest');
                while assigned(BaseTestNode) do
                begin
                  theNode := BaseTestNode.FindNode('LabTest');
                  if assigned(theNode) then
                    if AttributeValue(theNode, 'ID') = 'GT' then  {SPINA-GT}
                      begin
                        theNode := theNode.NextSibling;
                        while assigned(theNode) do
                          begin
                            if theNode.NodeName = 'SubjectCharacteristics' then
                              begin
                                FlagUOMNode := theNode.FindNode('FlagUOM');
                                if assigned(FlagUOMNode) then
                                  begin
                                    NormalNode := FlagUOMNode.FindNode('Normal');
                                    if assigned(NormalNode) then  {skips exclusion definition}
                                    begin
                                      NormalDefinitionNode := NormalNode.FindNode('NormalDefinition');
                                      while assigned(NormalDefinitionNode) do
                                        begin
                                          if (AttributeValue(NormalDefinitionNode, 'NormalLevel') = 'L') or (AttributeValue(NormalDefinitionNode, 'AlertLevel') = 'LN') then
                                             gReferenceRanges.GT.ln := StrToFloat(AttributeValue(NormalDefinitionNode, 'Value')) / 1e12;
                                          if (AttributeValue(NormalDefinitionNode, 'NormalLevel') = 'H') or (AttributeValue(NormalDefinitionNode, 'AlertLevel') = 'HN') then
                                             gReferenceRanges.GT.hn := StrToFloat(AttributeValue(NormalDefinitionNode, 'Value')) / 1e12;
                                          NormalDefinitionNode := NormalDefinitionNode.NextSibling;
                                        end;
                                      break;
                                    end;
                                  end;
                              end;
                            theNode := theNode.NextSibling;
                          end;
                      end
                     else if (AttributeValue(theNode, 'ID') = 'GD') or (AttributeValue(theNode, 'ID') = 'GD1') then  {SPINA-GD}
                      begin
                        theNode := theNode.NextSibling;
                        while assigned(theNode) do
                          begin
                            if theNode.NodeName = 'SubjectCharacteristics' then
                              begin
                                FlagUOMNode := theNode.FindNode('FlagUOM');
                                if assigned(FlagUOMNode) then
                                  begin
                                    NormalNode := FlagUOMNode.FindNode('Normal');
                                    if assigned(NormalNode) then  {skips exclusion definition}
                                    begin
                                      NormalDefinitionNode := NormalNode.FindNode('NormalDefinition');
                                      while assigned(NormalDefinitionNode) do
                                        begin
                                          if (AttributeValue(NormalDefinitionNode, 'NormalLevel') = 'L') or (AttributeValue(NormalDefinitionNode, 'AlertLevel') = 'LN') then
                                             gReferenceRanges.GD.ln := StrToFloat(AttributeValue(NormalDefinitionNode, 'Value')) / 1e9;
                                          if (AttributeValue(NormalDefinitionNode, 'NormalLevel') = 'H') or (AttributeValue(NormalDefinitionNode, 'AlertLevel') = 'HN') then
                                             gReferenceRanges.GD.hn := StrToFloat(AttributeValue(NormalDefinitionNode, 'Value')) / 1e9;
                                          NormalDefinitionNode := NormalDefinitionNode.NextSibling;
                                        end;
                                      break;
                                    end;
                                  end;
                              end;
                            theNode := theNode.NextSibling;
                          end;
                      end;
                  BaseTestNode := BaseTestNode.NextSibling;
                end;
              end
            else if AttributeValue(BatteryNode, 'ID') = 'Thyroid_Hormones' then
            begin
              BaseTestNode := BatteryNode.FindNode('BaseTest');
              while assigned(BaseTestNode) do
              begin
                theNode := BaseTestNode.FindNode('LabTest');
                if assigned(theNode) then
                  if AttributeValue(theNode, 'ID') = 'TSH' then  {TSH}
                    begin
                      theNode := theNode.NextSibling;
                      while assigned(theNode) do
                        begin
                          if theNode.NodeName = 'SubjectCharacteristics' then
                            begin
                              FlagUOMNode := theNode.FindNode('FlagUOM');
                              while assigned(FlagUOMNode) do
                                begin
                                  if AttributeValue(FlagUOMNode, 'ResultClass') = 'S' then
                                    SI := true
                                  else
                                    SI := false;
                                  UnitsNode := FlagUOMNode.FindNode('ResultUnits');
                                  If assigned(UnitsNode) then
                                    if SI then
                                      gSIReferenceRanges.TSH.UOM := AttributeValue(UnitsNode, 'Value')
                                    else
                                      gConvReferenceRanges.TSH.UOM := AttributeValue(UnitsNode, 'Value');
                                  NormalNode := FlagUOMNode.FindNode('Normal');
                                  if assigned(NormalNode) then  {skips exclusion definition}
                                  begin
                                    NormalDefinitionNode := NormalNode.FindNode('NormalDefinition');
                                    while assigned(NormalDefinitionNode) do
                                      begin
                                        if (AttributeValue(NormalDefinitionNode, 'NormalLevel') = 'L') or (AttributeValue(NormalDefinitionNode, 'AlertLevel') = 'LN') then
                                          if SI then
                                            gSIReferenceRanges.TSH.ln := StrToFloat(AttributeValue(NormalDefinitionNode, 'Value'))
                                          else
                                            gConvReferenceRanges.TSH.ln := StrToFloat(AttributeValue(NormalDefinitionNode, 'Value'));
                                        if (AttributeValue(NormalDefinitionNode, 'NormalLevel') = 'H') or (AttributeValue(NormalDefinitionNode, 'AlertLevel') = 'HN') then
                                          if SI then
                                            gSIReferenceRanges.TSH.hn := StrToFloat(AttributeValue(NormalDefinitionNode, 'Value'))
                                          else
                                            gConvReferenceRanges.TSH.hn := StrToFloat(AttributeValue(NormalDefinitionNode, 'Value'));
                                        NormalDefinitionNode := NormalDefinitionNode.NextSibling;
                                      end;
                                  end;
                                  FlagUOMNode := FlagUOMNode.NextSibling;
                                end;
                            end;
                          theNode := theNode.NextSibling;
                        end;
                    end
                   else if AttributeValue(theNode, 'ID') = 'FT4' then  {FT4}
                    begin
                      theNode := theNode.NextSibling;
                      while assigned(theNode) do
                        begin
                          if theNode.NodeName = 'SubjectCharacteristics' then
                            begin
                              FlagUOMNode := theNode.FindNode('FlagUOM');
                              while assigned(FlagUOMNode) do
                                begin
                                  if AttributeValue(FlagUOMNode, 'ResultClass') = 'S' then
                                    SI := true
                                  else
                                    SI := false;
                                  UnitsNode := FlagUOMNode.FindNode('ResultUnits');
                                  If assigned(UnitsNode) then
                                    if SI then
                                      gSIReferenceRanges.FT4.UOM := AttributeValue(UnitsNode, 'Value')
                                    else
                                      gConvReferenceRanges.FT4.UOM := AttributeValue(UnitsNode, 'Value');
                                  NormalNode := FlagUOMNode.FindNode('Normal');
                                  if assigned(NormalNode) then  {skips exclusion definition}
                                  begin
                                    NormalDefinitionNode := NormalNode.FindNode('NormalDefinition');
                                    while assigned(NormalDefinitionNode) do
                                      begin
                                        if (AttributeValue(NormalDefinitionNode, 'NormalLevel') = 'L') or (AttributeValue(NormalDefinitionNode, 'AlertLevel') = 'LN') then
                                          if SI then
                                            gSIReferenceRanges.FT4.ln := StrToFloat(AttributeValue(NormalDefinitionNode, 'Value'))
                                          else
                                            gConvReferenceRanges.FT4.ln := StrToFloat(AttributeValue(NormalDefinitionNode, 'Value'));
                                        if (AttributeValue(NormalDefinitionNode, 'NormalLevel') = 'H') or (AttributeValue(NormalDefinitionNode, 'AlertLevel') = 'HN') then
                                          if SI then
                                            gSIReferenceRanges.FT4.hn := StrToFloat(AttributeValue(NormalDefinitionNode, 'Value'))
                                          else
                                            gConvReferenceRanges.FT4.hn := StrToFloat(AttributeValue(NormalDefinitionNode, 'Value'));
                                        NormalDefinitionNode := NormalDefinitionNode.NextSibling;
                                      end;
                                  end;
                                  FlagUOMNode := FlagUOMNode.NextSibling;
                                end;
                            end;
                          theNode := theNode.NextSibling;
                        end;
                    end
                   else if AttributeValue(theNode, 'ID') = 'FT3' then  {FT3}
                    begin
                      theNode := theNode.NextSibling;
                      while assigned(theNode) do
                        begin
                          if theNode.NodeName = 'SubjectCharacteristics' then
                            begin
                              FlagUOMNode := theNode.FindNode('FlagUOM');
                              while assigned(FlagUOMNode) do
                                begin
                                  if AttributeValue(FlagUOMNode, 'ResultClass') = 'S' then
                                    SI := true
                                  else
                                    SI := false;
                                  UnitsNode := FlagUOMNode.FindNode('ResultUnits');
                                  If assigned(UnitsNode) then
                                    if SI then
                                      gSIReferenceRanges.FT3.UOM := AttributeValue(UnitsNode, 'Value')
                                    else
                                      gConvReferenceRanges.FT3.UOM := AttributeValue(UnitsNode, 'Value');
                                  NormalNode := FlagUOMNode.FindNode('Normal');
                                  if assigned(NormalNode) then  {skips exclusion definition}
                                  begin
                                    NormalDefinitionNode := NormalNode.FindNode('NormalDefinition');
                                    while assigned(NormalDefinitionNode) do
                                      begin
                                        if (AttributeValue(NormalDefinitionNode, 'NormalLevel') = 'L') or (AttributeValue(NormalDefinitionNode, 'AlertLevel') = 'LN') then
                                          if SI then
                                            gSIReferenceRanges.FT3.ln := StrToFloat(AttributeValue(NormalDefinitionNode, 'Value'))
                                          else
                                            gConvReferenceRanges.FT3.ln := StrToFloat(AttributeValue(NormalDefinitionNode, 'Value'));
                                        if (AttributeValue(NormalDefinitionNode, 'NormalLevel') = 'H') or (AttributeValue(NormalDefinitionNode, 'AlertLevel') = 'HN') then
                                          if SI then
                                            gSIReferenceRanges.FT3.hn := StrToFloat(AttributeValue(NormalDefinitionNode, 'Value'))
                                          else
                                            gConvReferenceRanges.FT3.hn := StrToFloat(AttributeValue(NormalDefinitionNode, 'Value'));
                                        NormalDefinitionNode := NormalDefinitionNode.NextSibling;
                                      end;
                                  end;
                                  FlagUOMNode := FlagUOMNode.NextSibling;
                                end;
                            end;
                          theNode := theNode.NextSibling;
                        end;
                    end
                    else if AttributeValue(theNode, 'ID') = 'TT4' then  {TT4}
                    begin
                      theNode := theNode.NextSibling;
                      while assigned(theNode) do
                        begin
                          if theNode.NodeName = 'SubjectCharacteristics' then
                            begin
                              FlagUOMNode := theNode.FindNode('FlagUOM');
                              while assigned(FlagUOMNode) do
                                begin
                                  if AttributeValue(FlagUOMNode, 'ResultClass') = 'S' then
                                    SI := true
                                  else
                                    SI := false;
                                  UnitsNode := FlagUOMNode.FindNode('ResultUnits');
                                  If assigned(UnitsNode) then
                                    if SI then
                                      gSIReferenceRanges.TT4.UOM := AttributeValue(UnitsNode, 'Value')
                                    else
                                      gConvReferenceRanges.TT4.UOM := AttributeValue(UnitsNode, 'Value');
                                  NormalNode := FlagUOMNode.FindNode('Normal');
                                  if assigned(NormalNode) then  {skips exclusion definition}
                                  begin
                                    NormalDefinitionNode := NormalNode.FindNode('NormalDefinition');
                                    while assigned(NormalDefinitionNode) do
                                      begin
                                        if (AttributeValue(NormalDefinitionNode, 'NormalLevel') = 'L') or (AttributeValue(NormalDefinitionNode, 'AlertLevel') = 'LN') then
                                          if SI then
                                            gSIReferenceRanges.TT4.ln := StrToFloat(AttributeValue(NormalDefinitionNode, 'Value'))
                                          else
                                            gConvReferenceRanges.TT4.ln := StrToFloat(AttributeValue(NormalDefinitionNode, 'Value'));
                                        if (AttributeValue(NormalDefinitionNode, 'NormalLevel') = 'H') or (AttributeValue(NormalDefinitionNode, 'AlertLevel') = 'HN') then
                                          if SI then
                                            gSIReferenceRanges.TT4.hn := StrToFloat(AttributeValue(NormalDefinitionNode, 'Value'))
                                          else
                                            gConvReferenceRanges.TT4.hn := StrToFloat(AttributeValue(NormalDefinitionNode, 'Value'));
                                        NormalDefinitionNode := NormalDefinitionNode.NextSibling;
                                      end;
                                  end;
                                  FlagUOMNode := FlagUOMNode.NextSibling;
                                end;
                            end;
                          theNode := theNode.NextSibling;
                        end;
                    end
                   else if AttributeValue(theNode, 'ID') = 'TT3' then   {TT3}
                    begin
                      theNode := theNode.NextSibling;
                      while assigned(theNode) do
                        begin
                          if theNode.NodeName = 'SubjectCharacteristics' then
                            begin
                              FlagUOMNode := theNode.FindNode('FlagUOM');
                              while assigned(FlagUOMNode) do
                                begin
                                  if AttributeValue(FlagUOMNode, 'ResultClass') = 'S' then
                                    SI := true
                                  else
                                    SI := false;
                                  UnitsNode := FlagUOMNode.FindNode('ResultUnits');
                                  If assigned(UnitsNode) then
                                    if SI then
                                      gSIReferenceRanges.TT3.UOM := AttributeValue(UnitsNode, 'Value')
                                    else
                                      gConvReferenceRanges.TT3.UOM := AttributeValue(UnitsNode, 'Value');
                                  NormalNode := FlagUOMNode.FindNode('Normal');
                                  if assigned(NormalNode) then  {skips exclusion definition}
                                  begin
                                    NormalDefinitionNode := NormalNode.FindNode('NormalDefinition');
                                    while assigned(NormalDefinitionNode) do
                                      begin
                                        if (AttributeValue(NormalDefinitionNode, 'NormalLevel') = 'L') or (AttributeValue(NormalDefinitionNode, 'AlertLevel') = 'LN') then
                                          if SI then
                                            gSIReferenceRanges.TT3.ln := StrToFloat(AttributeValue(NormalDefinitionNode, 'Value'))
                                          else
                                            gConvReferenceRanges.TT3.ln := StrToFloat(AttributeValue(NormalDefinitionNode, 'Value'));
                                        if (AttributeValue(NormalDefinitionNode, 'NormalLevel') = 'H') or (AttributeValue(NormalDefinitionNode, 'AlertLevel') = 'HN') then
                                          if SI then
                                            gSIReferenceRanges.TT3.hn := StrToFloat(AttributeValue(NormalDefinitionNode, 'Value'))
                                          else
                                            gConvReferenceRanges.TT3.hn := StrToFloat(AttributeValue(NormalDefinitionNode, 'Value'));
                                        NormalDefinitionNode := NormalDefinitionNode.NextSibling;
                                      end;
                                  end;
                                  FlagUOMNode := FlagUOMNode.NextSibling;
                                end;
                            end;
                          theNode := theNode.NextSibling;
                        end;
                    end;
               BaseTestNode := BaseTestNode.NextSibling;
              end;
            end
            else if AttributeValue(BatteryNode, 'ID') = 'Other' then
            begin
              BaseTestNode := BatteryNode.FindNode('BaseTest');
              while assigned(BaseTestNode) do
              begin
                theNode := BaseTestNode.FindNode('LabTest');
                if assigned(theNode) then
                  if AttributeValue(theNode, 'ID') = 'TSHI' then  {TSHI}
                    begin
                      theNode := theNode.NextSibling;
                      while assigned(theNode) do
                        begin
                          if theNode.NodeName = 'SubjectCharacteristics' then
                            begin
                              FlagUOMNode := theNode.FindNode('FlagUOM');
                              if assigned(FlagUOMNode) then
                                begin
                                  NormalNode := FlagUOMNode.FindNode('Normal');
                                  if assigned(NormalNode) then  {skips exclusion definition}
                                  begin
                                    NormalDefinitionNode := NormalNode.FindNode('NormalDefinition');
                                    while assigned(NormalDefinitionNode) do
                                      begin
                                        if (AttributeValue(NormalDefinitionNode, 'NormalLevel') = 'L') or (AttributeValue(NormalDefinitionNode, 'AlertLevel') = 'LN') then
                                           gReferenceRanges.TSHI.ln := StrToFloat(AttributeValue(NormalDefinitionNode, 'Value'));
                                        if (AttributeValue(NormalDefinitionNode, 'NormalLevel') = 'H') or (AttributeValue(NormalDefinitionNode, 'AlertLevel') = 'HN') then
                                           gReferenceRanges.TSHI.hn := StrToFloat(AttributeValue(NormalDefinitionNode, 'Value'));
                                        NormalDefinitionNode := NormalDefinitionNode.NextSibling;
                                      end;
                                    break;
                                  end;
                                end;
                            end;
                          theNode := theNode.NextSibling;
                        end;
                    end
                   else if AttributeValue(theNode, 'ID') = 'TTSI' then  {TTSI}
                    begin
                      theNode := theNode.NextSibling;
                      while assigned(theNode) do
                        begin
                          if theNode.NodeName = 'SubjectCharacteristics' then
                            begin
                              FlagUOMNode := theNode.FindNode('FlagUOM');
                              if assigned(FlagUOMNode) then
                                begin
                                  NormalNode := FlagUOMNode.FindNode('Normal');
                                  if assigned(NormalNode) then  {skips exclusion definition}
                                  begin
                                    NormalDefinitionNode := NormalNode.FindNode('NormalDefinition');
                                    while assigned(NormalDefinitionNode) do
                                      begin
                                        if (AttributeValue(NormalDefinitionNode, 'NormalLevel') = 'L') or (AttributeValue(NormalDefinitionNode, 'AlertLevel') = 'LN') then
                                           gReferenceRanges.TTSI.ln := StrToFloat(AttributeValue(NormalDefinitionNode, 'Value'));
                                        if (AttributeValue(NormalDefinitionNode, 'NormalLevel') = 'H') or (AttributeValue(NormalDefinitionNode, 'AlertLevel') = 'HN') then
                                           gReferenceRanges.TTSI.hn := StrToFloat(AttributeValue(NormalDefinitionNode, 'Value'));
                                        NormalDefinitionNode := NormalDefinitionNode.NextSibling;
                                      end;
                                    break;
                                  end;
                                end;
                            end;
                          theNode := theNode.NextSibling;
                        end;
                    end;
                BaseTestNode := BaseTestNode.NextSibling;
              end;
            end;
            if BatteryNode.NextSibling <> nil then
              BatteryNode := BatteryNode.NextSibling
            else BatteryNode := nil;
          end;
        end
    else
      returnCode := 2;
    ;
  finally
    Doc.Free;
  end
  else
  begin  {fall-back solution, if file does not exist}
    with gReferenceRanges do
      begin
        TSH.ln := 0.4;
        TSH.hn := 4;
        FT4.ln := 13;
        FT4.hn := 20;
        TT4.ln := 70;
        TT4.hn := 130;
        FT3.ln := 3.9;
        FT3.hn := 6.7;
        TT3.ln := 1.3;
        TT3.hn := 2.8;
        GT.ln := 1.41 / 1e12;
        GT.hn := 8.67 / 1e12;
        GD.ln := 20 / 1e9;
        GD.hn := 40 / 1e9;
        TSHI.ln := 1.3;
        TSHI.hn := 4.1;
        TTSI.ln := 122;
        TTSI.hn := 150;
      end;
    returnCode := 6;
  end;
  DecimalSeparator := oldSeparator;
  ComposeRRStrings;
end;

end.
