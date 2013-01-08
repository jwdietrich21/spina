unit HandlePreferences;

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

{ This unit handles global application preferences }

{ Source code released under the BSD License }
{ See http://spina.medical-cybernetics.de for details }

{$mode objfpc}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, SPINA_Types, DOM, XMLRead, XMLWrite, Math
  {$IFDEF win32}
  , Windows
  {$ELSE}
    {$IFDEF LCLCarbon}
      , MacOSAll
    {$ENDIF}
  {$ENDIF};

procedure InitConversionFactors;
function ParsedUnitString(theString: String): TUnitElements;
function GetPreferencesFolder: String;
function GetPreferencesFile: String;
function EncodeGreek(theString: string): string;
function DecodeGreek(theString: string): string;
procedure ReadPreferences;
procedure GetReferenceValues;
procedure SavePreferences;


implementation

procedure InitConversionFactors;
{sets labels and factors for the elements of measurement units}
begin
  PrefixLabel[0] := '';
  PrefixLabel[1] := 'd';
  PrefixLabel[2] := 'c';
  PrefixLabel[3] := 'm';
  PrefixLabel[4] := 'µ';
  PrefixLabel[5] := 'n';
  PrefixLabel[6] := 'p';
  PrefixLabel[7] := 'f';
  PrefixFactor[0] := 1;
  PrefixFactor[1] := 1e-1;
  PrefixFactor[2] := 1e-2;
  PrefixFactor[3] := 1e-3;
  PrefixFactor[4] := 1e-6;
  PrefixFactor[5] := 1e-9;
  PrefixFactor[6] := 1e-12;
  PrefixFactor[7] := 1e-15;
  T4UnitLabel[0] := 'g';
  T4UnitLabel[1] := 'mol';
  T4UnitFactor[0] := PrefixFactor[1] * UFT4 / PrefixFactor[5]; {since UFT4 converts between ng/dl and mol/l}
  T4UnitFactor[1] := 1;
  T3UnitLabel[0] := 'g';
  T3UnitLabel[1] := 'mol';
  T3UnitFactor[0] := PrefixFactor[3] * UFT3 / PrefixFactor[6]; {as UFT3 converts between pg/ml and mol/l}
  T3UnitFactor[1] := 1;
end;

function ParsedUnitString(theString: String): TUnitElements;
  { parses a string for measurement unit and breaks it up in single parts }
var
  theElements: TUnitElements;
begin
  if theString <> 'NA' then
    begin
      with theElements do
      begin
        if copy(theString, 1, 1) = 'm' then
          begin
            if copy(theString, 2, 1) = 'c' then
              MassPrefix := PrefixLabel[4] {mc -> µ}
            else
              MassPrefix := 'm';
          end
          else
            MassPrefix := copy(theString, 1, 1);
        MassUnit := copy(theString, 2, pos('/', theString) - 2);
        VolumePrefix := copy(theString, pos('/', theString) + 1, 1);
        VolumeUnit := 'l';
        if VolumePrefix = VolumeUnit then VolumePrefix := '';  {no prefix set}
      end;
    end
  else
  begin
    with theElements do
    begin
      MassPrefix := 'NA';
      MassUnit := 'NA';
      VolumePrefix := 'NA';
      VolumeUnit := 'NA';
    end;
  end;
  ParsedUnitString := theElements;
end;

function GetPreferencesFolder: String;
  { platform-independend method to search for the location of preferences folder}
const
  kMaxPath = 1024;
var
  {$IFDEF DARWIN}
  theError: OSErr;
  theRef: FSRef;
  {$ENDIF}
  pathBuffer: PChar;
begin
  {$IFDEF DARWIN}
    try
      pathBuffer := Allocmem(kMaxPath);
    except on exception do exit;
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
    GetPreferencesFolder := GetAppConfigDir(false);
  {$ENDIF}
end;

function GetPreferencesFile: String;
{delivers path to preferences file}
begin
  {$IFDEF LCLCarbon}
    GetPreferencesFile := GetPreferencesFolder + SPINA_GLOBAL_ID + '.xml';
  {$ELSE}
    GetPreferencesFile := GetAppConfigFile(false);
  {$ENDIF}
end;

function GetRRFile: String;
{delivers path to XML file with reference values}
begin
   GetRRFile := GetPreferencesFolder + SPINA_GLOBAL_ID + '.ref-ranges.xml';
end;

function EncodeGreek(theString: string): string;
{encodes greek mu letter}
var
  theFlags: TReplaceFlags;
begin
  theFlags := [rfReplaceAll, rfIgnoreCase];
  Result := StringReplace(theString, #194#181, 'mc', theFlags);
end;

function DecodeGreek(theString: string): string;
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
    theNode := theRoot.FindNode(Name);
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
  for i := 0 to theNode.Attributes.Length - 1 do
  begin
    if theNode.Attributes[i].NodeName = theName then
      foundValue := theNode.Attributes[i].NodeValue;
  end;
  result := foundValue;
end;

procedure ReadPreferences;
{reads preferences file}
var
  Doc: TXMLDocument;
  RootNode, theNode: TDOMNode;
  theFileName, theString: String;
begin
  theFileName := GetPreferencesFile;
  if FileExists(theFileName) then
  try
    ReadXMLFile(Doc, theFileName);

    RootNode := Doc.DocumentElement.FindNode('methods');
    theString := NodeContent(RootNode, 'T4');
    if theString = 'free' then
      gPreferences.T4Method := freeHormone
    else
      gPreferences.T4Method := totalHormone;
    theString := NodeContent(RootNode, 'T3');
    if theString = 'free' then
      gPreferences.T3Method := freeHormone
    else
      gPreferences.T3Method := totalHormone;

    RootNode := Doc.DocumentElement.FindNode('units');
    theString := NodeContent(RootNode, 'TSH');
    gPreferences.TSHUnit := theString;
    theString := NodeContent(RootNode, 'T4');
    gPreferences.T4Unit := theString;
    theString := NodeContent(RootNode, 'T3');
    gPreferences.T3Unit := theString;

    {RootNode := Doc.DocumentElement.FindNode('factors');
    theString := NodeContent(RootNode, 'TSH');
    gPreferences.TSHUnitFactor := StrToFloat(theString);
    theString := NodeContent(RootNode, 'T4');
    gPreferences.T4UnitFactor := StrToFloat(theString);
    theString := NodeContent(RootNode, 'T3');
    gPreferences.T3UnitFactor := StrToFloat(theString);}

    {RootNode := Doc.DocumentElement.FindNode('unititems');
    theString := NodeContent(RootNode, 'TSH');
    gPreferences.TSHPopUpItem := StrToInt(theString);
    theString := NodeContent(RootNode, 'T4');
    gPreferences.T4PopUpItem := StrToInt(theString);
    theString := NodeContent(RootNode, 'T3');
    gPreferences.T3PopUpItem := StrToInt(theString); }

    RootNode := Doc.DocumentElement.FindNode('methoditems');
    theString := NodeContent(RootNode, 'T4');
    gPreferences.T4MethodPopUpItem := StrToInt(theString);
    theString := NodeContent(RootNode, 'T3');
    gPreferences.T3MethodPopUpItem := StrToInt(theString);

    gPreferences.new := false;
  finally
    Doc.Free;
  end
  else  {Standards from dialog, if preference file does not exist}
  begin  {fall-back solution, if file does not exist}
    with gPreferences do
    begin
      T4Method := freeHormone;
      T3Method := freeHormone;
      {TSHUnitFactor := 1;
      T4UnitFactor := 1;
      T3UnitFactor := 1;}
      TSHUnit := TSH_UNIT;
      T4Unit := FT4_UNIT;
      T3Unit := FT3_UNIT;
      TSHPopUpItem := 0;
      T4PopUpItem := 0;
      T3PopUpItem := 0;
      T4MethodPopUpItem := 0;
      T3MethodPopUpItem := 0;
      gPreferences.new := true;
    end;
  end;
end;

procedure GetReferenceValues;
{reads reference values from a CDISC LAB model-compliant XML file}
var
  Doc: TXMLDocument;
  RootNode, theNode, BaseTestNode, FlagUOMNode, NormalNode, NormalDefinitionNode: TDOMNode;
  theFileName, theString: String;
  theStream: TStringStream;
  oldSeparator: Char;
begin
  oldSeparator := DecimalSeparator;
  DecimalSeparator := DEC_POINT;
  theFileName := GetRRFile;
  if not FileExists(theFileName) then
    gCDISC_RR.SaveToFile(theFileName);
  if FileExists(theFileName) then {could this file be created?}
  try
    ReadXMLFile(Doc, theFileName);
    RootNode := Doc.DocumentElement.FindNode('Study');
    if assigned(RootNode) then
      if AttributeValue(RootNode, 'ID') = 'SPIt' then
        begin
          theNode := RootNode.FindNode('BaseBattery');
          if assigned(theNode) then
            if AttributeValue(theNode, 'ID') = 'SPIt' then
              begin
                BaseTestNode := theNode.FindNode('BaseTest');
                while assigned(BaseTestNode) do
                begin
                  theNode := BaseTestNode.FindNode('LabTest');
                  if assigned(theNode) then
                    if AttributeValue(theNode, 'ID') = 'GT' then
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
                                    if assigned(NormalNode) then
                                    begin
                                      NormalDefinitionNode := NormalNode.FindNode('NormalDefinition');
                                      while assigned(NormalDefinitionNode) do
                                        begin
                                          if AttributeValue(NormalDefinitionNode, 'NormalLevel') = 'L' then
                                             gReferenceRanges.GT.ln := StrToFloat(AttributeValue(NormalDefinitionNode, 'Value')) / 1e12;
                                          if AttributeValue(NormalDefinitionNode, 'NormalLevel') = 'H' then
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
                     else if (AttributeValue(theNode, 'ID') = 'GD') or (AttributeValue(theNode, 'ID') = 'GD1') then
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
                                    if assigned(NormalNode) then
                                    begin
                                      NormalDefinitionNode := NormalNode.FindNode('NormalDefinition');
                                      while assigned(NormalDefinitionNode) do
                                        begin
                                          if AttributeValue(NormalDefinitionNode, 'NormalLevel') = 'L' then
                                             gReferenceRanges.GD.ln := StrToFloat(AttributeValue(NormalDefinitionNode, 'Value')) / 1e9;
                                          if AttributeValue(NormalDefinitionNode, 'NormalLevel') = 'H' then
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
              end;
        end;
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
      end;
  end;
  DecimalSeparator := oldSeparator;
  gTSHRR := 'N/A';
  gFT4RR := 'N/A';
  gFT3RR := 'N/A';
  gTT4RR := 'N/A';
  gTT3RR := 'N/A';
  gGTRR := FloatToStrF(gReferenceRanges.GT.ln * 1e12, ffFixed, 5, 2) + ' - ' + FloatToStrF(gReferenceRanges.GT.hn * 1e12, ffFixed, 5, 2) + ' pmol/s';
  gGDRR := FloatToStrF(gReferenceRanges.GD.ln * 1e9, ffFixed, 5, 0) + ' - ' + FloatToStrF(gReferenceRanges.GD.hn * 1e9, ffFixed, 5, 0) + ' nmol/s';
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

    ElementNode := Doc.CreateElement('methods');
    if gPreferences.T4Method = freeHormone then
      ElementNode.AppendChild(SimpleNode(Doc, 'T4', 'free'))
    else
      ElementNode.AppendChild(SimpleNode(Doc, 'T4', 'total'));
    if gPreferences.T3Method = freeHormone then
      ElementNode.AppendChild(SimpleNode(Doc, 'T3', 'free'))
    else
      ElementNode.AppendChild(SimpleNode(Doc, 'T3', 'total'));
    RootNode.AppendChild(ElementNode);

    ElementNode:=Doc.CreateElement('units');
    ElementNode.AppendChild(SimpleNode(Doc, 'TSH', gPreferences.TSHUnit));
    ElementNode.AppendChild(SimpleNode(Doc, 'T4', gPreferences.T4Unit));
    ElementNode.AppendChild(SimpleNode(Doc, 'T3', gPreferences.T3Unit));
    RootNode.AppendChild(ElementNode);

    {ElementNode:=Doc.CreateElement('factors');
    ElementNode.AppendChild(SimpleNode(Doc, 'TSH', FloatToStr(gPreferences.TSHUnitFactor)));
    ElementNode.AppendChild(SimpleNode(Doc, 'T4', FloatToStr(gPreferences.T4UnitFactor)));
    ElementNode.AppendChild(SimpleNode(Doc, 'T3', FloatToStr(gPreferences.T3UnitFactor)));
    RootNode.AppendChild(ElementNode);  }

    {ElementNode:=Doc.CreateElement('unititems');
    ElementNode.AppendChild(SimpleNode(Doc, 'TSH', FloatToStr(gPreferences.TSHPopUpItem)));
    ElementNode.AppendChild(SimpleNode(Doc, 'T4', FloatToStr(gPreferences.T4PopUpItem)));
    ElementNode.AppendChild(SimpleNode(Doc, 'T3', FloatToStr(gPreferences.T3PopUpItem)));
    RootNode.AppendChild(ElementNode);}

    ElementNode:=Doc.CreateElement('methoditems');
    ElementNode.AppendChild(SimpleNode(Doc, 'T4', FloatToStr(gPreferences.T4MethodPopUpItem)));
    ElementNode.AppendChild(SimpleNode(Doc, 'T3', FloatToStr(gPreferences.T3MethodPopUpItem)));
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

end.
