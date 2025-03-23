unit HandleImpEx;

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

{ This unit handles import and export of laboratory results and calculations }

{ Source code released under the BSD License }
{ See http://spina.medical-cybernetics.de for details }

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Math, SPINATypes, CaseBroker;

type
  TFileType = (plainTextFile, HL7Message);

procedure SaveCaseRecord(caseRecord: tCaseRecord; filePath: String; fileType: TFileType);

implementation

procedure SaveStringToPath(theString, filePath: string);
var
  textFile:     TFileStream = nil;
  textLength:   integer;
begin
  textLength := length(theString);
  textFile := TFileStream.Create(filePath, fmOpenWrite or fmCreate);
  try
    { write string to stream while avoiding to write the initial length }
    textFile.WriteBuffer(theString[1], textLength);
  finally
    if textFile <> nil then
      textFile.Free;
  end;
end;

procedure SaveAsHL7Message(aCaseRecord: tCaseRecord; filePath: String);
begin

end;

procedure SaveAsTextFile(aCaseRecord: tCaseRecord; filePath: String);
var
  theHeader, theString: ansistring;
  DOBDateString, OBDateString: string;
begin
  if isNaN(aCaseRecord.OBDate) then
    OBDateString := ''
  else
    OBDateString := DateToStr(aCaseRecord.OBDate);
  if isNaN(aCaseRecord.DoBDate) then
    DOBDateString := ''
  else
    DOBDateString := DateToStr(aCaseRecord.DoBDate);
  if aCaseRecord.Name = '' then
    theHeader := ''
  else
    theHeader := aCaseRecord.PID + '/' + aCaseRecord.CaseID + LineEnding +
      kLF + aCaseRecord.Name + ', ' + aCaseRecord.GivenNames + ' *' +
      DOBDateString + LineEnding + OBDateString + ' (' + aCaseRecord.Placer +
      ')' + LineEnding + LineEnding;
  theString := theHeader + aCaseRecord.CombMessage;
  SaveStringToPath(theString, filePath);
end;

procedure SaveCaseRecord(caseRecord: tCaseRecord; filePath: String; fileType: TFileType);
begin
  case fileType of
    HL7Message: SaveAsHL7Message(caseRecord, filePath);
    plainTextFile: SaveAsTextFile(caseRecord, filePath);
  end;
end;

end.

