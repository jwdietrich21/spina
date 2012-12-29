unit HandlePreferences;

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

{ This unit handles global application preferences }

{ Source code released under the BSD License }
{ See http://spina.medical-cybernetics.de for details }

{$mode objfpc}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, SPINA_Types, DOM, XMLRead, XMLWrite
  {$IFDEF win32}
  , Windows
  {$ELSE}
    {$IFDEF LCLCarbon}
      , MacOSAll
    {$ENDIF}
  {$ENDIF};

function GetPreferencesFolder: String;
function GetPreferencesFile: String;
procedure ReadPreferences;
procedure GetReferenceValues;
procedure SavePreferences;


implementation

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
begin
  {$IFDEF LCLCarbon}
    GetPreferencesFile := GetPreferencesFolder + SPINA_GLOBAL_ID + '.xml';
  {$ELSE}
    GetPreferencesFile := GetAppConfigFile(false);
  {$ENDIF}
end;

function GetRRFile: String;
begin
   GetRRFile := GetPreferencesFolder + SPINA_GLOBAL_ID + '.ref-ranges.xml';
end;

function EncodeGreek(theString: string): string;
var
  theFlags: TReplaceFlags;
begin
  theFlags := [rfReplaceAll, rfIgnoreCase];
  Result := StringReplace(theString, #194#181, 'mc', theFlags);
end;

function DecodeGreek(theString: string): string;
begin
  {result := UTF8Decode(StringReplace(theString, 'mc', PrefixLabel[4], [rfReplaceAll, rfIgnoreCase]));}
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

procedure ReadPreferences;
var
  Doc: TXMLDocument;
  RootNode, theNode: TDOMNode;
  theFileName, theString: String;
begin
  {assignFile(gPrefsFile, gPrefsFileName);
  try
    reset(gPrefsFile);
    read(gPrefsFile,gPreferences);
    CloseFile(gPrefsFile);
  except
  on Ex: EInOutError do
      with gPreferences do
      begin
        T4Method:=freeHormone;
        T3Method:=freeHormone;
        TSHUnitFactor:=1;
        T4UnitFactor:=1;
        T3UnitFactor:=1;
      end;
   end;}
  theFileName := GetPreferencesFile;
  if FileExists(theFileName) then
  try
    Doc := TXMLDocument.Create();
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

    RootNode := Doc.DocumentElement.FindNode('factors');
    theString := NodeContent(RootNode, 'TSH');
    gPreferences.TSHUnitFactor := StrToFloat(theString);
    theString := NodeContent(RootNode, 'T4');
    gPreferences.T4UnitFactor := StrToFloat(theString);
    theString := NodeContent(RootNode, 'T3');
    gPreferences.T3UnitFactor := StrToFloat(theString);

    RootNode := Doc.DocumentElement.FindNode('unititems');
    theString := NodeContent(RootNode, 'TSH');
    gPreferences.TSHPopUpItem := StrToInt(theString);
    theString := NodeContent(RootNode, 'T4');
    gPreferences.T4PopUpItem := StrToInt(theString);
    theString := NodeContent(RootNode, 'T3');
    gPreferences.T3PopUpItem := StrToInt(theString);

    RootNode := Doc.DocumentElement.FindNode('methoditems');
    theString := NodeContent(RootNode, 'T4');
    gPreferences.T4MethodPopUpItem := StrToInt(theString);
    theString := NodeContent(RootNode, 'T3');
    gPreferences.T3MethodPopUpItem := StrToInt(theString);

  finally
    Doc.Free;
  end
  else  {Standards from dialog, if preference file does not exist}
  begin  {fall-back solution, if file does not exist}
    with gPreferences do
    begin
      T4Method := freeHormone;
      T3Method := freeHormone;
      TSHUnitFactor := 1;
      T4UnitFactor := 1;
      T3UnitFactor := 1;
      TSHPopUpItem := 0;
      T4PopUpItem := 0;
      T3PopUpItem := 0;
      T4MethodPopUpItem := 0;
      T3MethodPopUpItem := 0;
    end;
  end;
end;

procedure GetReferenceValues;
var
  Doc: TXMLDocument;
  RootNode, theNode: TDOMNode;
  theFileName, theString: String;
begin
  theFileName := GetRRFile;
  if FileExists(theFileName) then
  try
    Doc := TXMLDocument.Create();
    ReadXMLFile(Doc, theFileName);
    ;
  finally
    Doc.Free;
  end
  else
  begin  {fall-back solution, if file does not exist}
    with gReferenceRanges do
    begin
      GT.ln := 1.41 / 1e12;
      GT.hn := 8.67 / 1e12;
      GD.ln := 20 / 1e9;
      GD.hn := 40 / 1e9;
    end;
  end;
  gTSHRR := 'N/A';
  gFT4RR := 'N/A';
  gFT3RR := 'N/A';
  gTT4RR := 'N/A';
  gTT3RR := 'N/A';
  gGTRR := FloatToStrF(gReferenceRanges.GT.ln * 1e12, ffFixed, 5, 2) + ' - ' + FloatToStrF(gReferenceRanges.GT.hn * 1e12, ffFixed, 5, 2) + ' pmol/s';
  gGDRR := FloatToStrF(gReferenceRanges.GD.ln * 1e9, ffFixed, 5, 0) + ' - ' + FloatToStrF(gReferenceRanges.GD.hn * 1e9, ffFixed, 5, 0) + ' nmol/s';
end;

procedure SavePreferences;
var
  theFileName, PreferencesFolder: String;
  Doc: TXMLDocument;
  StartComment: TDOMComment;
  RootNode, ElementNode, ItemNode, TextNode: TDOMNode;
begin
  {CreateDir(gPrefsDir);
  assignFile(gPrefsFile, gPrefsFileName);
  rewrite(gPrefsFile);
  write(gPrefsFile, gPreferences);
  CloseFile(gPrefsFile);}
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

    ElementNode:=Doc.CreateElement('factors');
    ElementNode.AppendChild(SimpleNode(Doc, 'TSH', FloatToStr(gPreferences.TSHUnitFactor)));
    ElementNode.AppendChild(SimpleNode(Doc, 'T4', FloatToStr(gPreferences.T4UnitFactor)));
    ElementNode.AppendChild(SimpleNode(Doc, 'T3', FloatToStr(gPreferences.T3UnitFactor)));
    RootNode.AppendChild(ElementNode);

    ElementNode:=Doc.CreateElement('unititems');
    ElementNode.AppendChild(SimpleNode(Doc, 'TSH', FloatToStr(gPreferences.TSHPopUpItem)));
    ElementNode.AppendChild(SimpleNode(Doc, 'T4', FloatToStr(gPreferences.T4PopUpItem)));
    ElementNode.AppendChild(SimpleNode(Doc, 'T3', FloatToStr(gPreferences.T3PopUpItem)));
    RootNode.AppendChild(ElementNode);

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

