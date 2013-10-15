unit HandleImpEx;

{ SPINA-Thyr }

{ Application for calculating structure parameters }
{ of thyrotropic feedback control }

{ Programm zur Berechnung von Strukturparametern }
{ des thyreotropen Regelkreises }

{ Version 3.4.0 }

{ (c) J. W. Dietrich, 1994 - 2013 }
{ (c) Ludwig Maximilian University of Munich 1995 - 2002 }
{ (c) University of Ulm Hospitals 2002-2004 }
{ (c) Ruhr University of Bochum 2005 - 2013 }

{ This unit handles import and export of laboratory results and calculations }

{ Source code released under the BSD License }
{ See http://spina.medical-cybernetics.de for details }

{$mode objfpc}

interface

uses
  Classes, SysUtils, SPINA_Engine, SPINA_Types;

procedure SaveResults(caseRecord: tCaseRecord);

implementation

uses spina_toolbar;

procedure SaveStringToPath(theString, filePath: String);
var
  textFile: TFileStream = nil;
  textLength: integer;
  stringBuffer: ^String;
begin
  textLength := length(theString);
  try
    textFile := TFileStream.Create(filePath, fmOpenWrite or fmCreate);
    { write string to stream while avoiding to write the initial length }
    stringBuffer := @theString + 1;
    textFile.WriteBuffer(stringBuffer^, textLength);
  finally
    if textFile <> nil then textFile.Free;
  end;
end;

procedure SaveResults(caseRecord: tCaseRecord);
var
  filePath: String;
  textFile: TFileStream = nil;
  textLength: integer;
  tBuffer: ^String;
begin
  if SPINAToolbar.SaveResultsDialog.Execute then
    SaveStringToPath(gResultString, SPINAToolbar.SaveResultsDialog.FileName);
end;

end.
