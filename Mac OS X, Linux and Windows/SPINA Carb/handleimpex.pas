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
  Classes, SysUtils, Dialogs, Math,
  HL7, MSH, MSA, NTE, PID, PV1, OBR, OBX, SPM,
  SPINATypes, CaseBroker;

type
  TFileType = (plainTextFile, HL7Message);
  TLOINCRecord = record
    code, short, long: string;
  end;

const
  ORU_R01_variant1 = 'ORU^R01'; // Unsolicited transmission of an observation
  ORU_R01_variant2 = 'ORU^R01^ORU_R01';
  ORU_R04    = 'ORU^R04'; //Response to query
  ACK_R01    = 'ACK^R01^ACK';
  MDM_T01    = 'MDM^T01';
  OUL_R21    = 'OUL^R21'; // Unsolicited laboratory observation
  OUL_R22    = 'OUL^R22'; // specimen oriented observation
  OUL_R23    = 'OUL^R23'; // specimen container oriented observation
  OUL_R24    = 'OUL^R24'; // (order oriented observation
  NND_ORU_V2_0_profile = 'NND_ORU_v2.0^PHINProfileID^2.16.840.1.114222.4.10.3^ISO~'
      + 'Gen_Case_Map_v1.0^PHINMsgMapID^2.16.840.1.114222.4.10.4^ISO';
  NA_DTM     = '00000000000000';
  LOINC_LABEL = 'LN';

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
var
  oldSeparator, t4substFlag, t3substFlag, tshsubstFlag: char;
  HL7Message: THL7Message;
  theMSH:   tMSH;
  theOBR:   tOBR;
  theOBX:   tOBX;
  thePID:   tPID;
  thePV1:   tPV1;
  theSPM:   tSPM;
  theNTE:   tNTE;
  setIDcounter: integer;
begin
  HL7Message := THL7Message.Create('2.5');
  if HL7Message = nil then
    ShowMessage('HL7 Error')
  else
  begin

  end;
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

