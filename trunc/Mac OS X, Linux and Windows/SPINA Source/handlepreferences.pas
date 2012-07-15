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

unit HandlePreferences;

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
{procedure ReadPreferences;}

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

{procedure ReadPreferences; {should not be called before PreferencesDialog has been created}
var
  Doc: TXMLDocument;
  RootNode, theNode: TDOMNode;
  theFileName, theString: String;
begin
  theFileName := GetPreferencesFile;
  if FileExists(theFileName) then
  try
    Doc := TXMLDocument.Create();
    ReadXMLFile(Doc, theFileName);
    RootNode := Doc.DocumentElement.FindNode('formats');
    gNumberFormat := NodeContent(RootNode, 'numbers');
    gDateTimeFormat := NodeContent(RootNode, 'time');
  finally
    Doc.Free;
  end
  else  {Standards from dialog, if preference file does not exist}
  if PreferencesDialog <> nil then begin
    gNumberFormat := PreferencesDialog.NumberFormatEdit.Text;
    gDateTimeFormat := PreferencesDialog.DateTimeFormatEdit.Text;
  end
  else
  begin  {fall-back solution, if neither file nor dialog exist}
    gNumberFormat := '###,###.00##';
    gDateTimeFormat := '"d"D hh:nn:ss';
  end;
end; }

{procedure SavePreferences;
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

    {StartComment := Doc.CreateComment('SimThyr Preferences');
    RootNode.AppendChild(StartComment);}

    RootNode := Doc.CreateElement('preferences');
    Doc.Appendchild(RootNode);
    RootNode:= Doc.DocumentElement;

    ElementNode:=Doc.CreateElement('units');

    ElementNode.AppendChild(SimpleNode(Doc, 'TSH', EncodeGreek(gParameterUnit[pTSH_pos])));
    ElementNode.AppendChild(SimpleNode(Doc, 'TT4', EncodeGreek(gParameterUnit[TT4_pos])));
    ElementNode.AppendChild(SimpleNode(Doc, 'FT4', EncodeGreek(gParameterUnit[FT4_pos])));
    ElementNode.AppendChild(SimpleNode(Doc, 'TT3', EncodeGreek(gParameterUnit[TT3_pos])));
    ElementNode.AppendChild(SimpleNode(Doc, 'FT3', EncodeGreek(gParameterUnit[FT3_pos])));

    RootNode.AppendChild(ElementNode);

    ElementNode:=Doc.CreateElement('formats');

    ElementNode.AppendChild(SimpleNode(Doc, 'numbers', gNumberFormat));
    ElementNode.AppendChild(SimpleNode(Doc, 'time', gDateTimeFormat));

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
end; }

end.

