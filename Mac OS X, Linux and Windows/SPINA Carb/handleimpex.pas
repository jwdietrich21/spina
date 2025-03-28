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
  SPINATypes, CaseBroker, EnvironmentInfo;

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
  LOINC_GLUC_1: TLoincRecord = (code: '100746-7'; short: 'Glucose BldMV-sCnc';
    long: 'Glucose [Moles/volume] in Mixed venous blood');
  LOINC_GLUC_2: TLoincRecord = (code: '101476-0'; short: 'Glucose p fast BldV-sCnc';
    long: 'Fasting glucose [Moles/volume] in Venous blood');
  LOINC_GLUC_3: TLoincRecord = (code: '101476-0'; short: 'Glucose p 10h fast SerPl-mCnc';
    long: 'Glucose [Mass/volume] in Serum or Plasma --10 hours fasting');
  LOINC_GLUC_4: TLoincRecord = (code: '101476-0'; short: 'Glucose p 10h fast SerPl-mCnc';
    long: 'Glucose [Mass/volume] in Serum or Plasma --10 hours fasting');
  LOINC_GLUC_5: TLoincRecord = (code: '104597-0'; short: 'Glucose BldV Glucomtr-mCnc';
    long: 'Glucose [Mass/volume] in Venous blood by Glucometer');
  LOINC_GLUC_6: TLoincRecord = (code: '104598-8'; short: 'Glucose BldA Glucomtr-mCnc';
    long: 'Glucose [Mass/volume] in Arterial blood by Glucometer');
  LOINC_GLUC_7: TLoincRecord = (code: '104655-6'; short: 'Glucose BldMV-mCnc';
    long: 'Glucose [Mass/volume] in Mixed venous blood');
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
    oldSeparator := DefaultFormatSettings.DecimalSeparator;
    DefaultFormatSettings.DecimalSeparator := DEC_POINT;

    clearMSH(theMSH);
    theMSH.delimiters   := STANDARD_DELIMITERS;
    theMSH.sendingApp   := 'SPINA Carb';
    theMSH.sendingFac   := gPreferences.MSH_ID;
    theMSH.receivingApp := '';
    theMSH.receivingFac := '';
    theMSH.dateTime     := EncodedDateTime(Now);
    theMSH.messageType  := ORU_R01_variant1;
    theMSH.security     := '';
    theMSH.controlID    := EncodedDateTime(Now) + IntToStr(random(13000));
    theMSH.processingID := 'P^A';
    theMSH.versionID    := ''; // ignored; will be filled-in automatically by PUMA
    theMSH.sequenceNumber := '';
    theMSH.continuationPointer := '';
    theMSH.AccAckType   := '';
    theMSH.AppAckType   := '';
    theMSH.countryCode  := '';
    theMSH.charSet      := '';
    theMSH.messageLanguage := '';
    theMSH.altCharHandlScheme := '';
    theMSH.profileID    := ORU_R01_variant2;
    SetMSH(HL7Message, theMSH, true);

    ClearPID(thePID);
    thePID.SetID := '1';
    if isNaN(aCaseRecord.DoBDate) then
      thePID.BirthDateTime := ''
    else
      thePID.BirthDateTime := EncodedDateTime(aCaseRecord.DoBDate);
    thePID.PatientIDList := aCaseRecord.PID +
      HL7Message.Delimiters.ComponentSeparator + aCaseRecord.CaseID;
    thePID.PatientName := aCaseRecord.Name +
      HL7Message.Delimiters.ComponentSeparator + aCaseRecord.GivenNames;
    SetPID(HL7Message, thePID);

    ClearPV1(thePV1);
    thePV1.SetID := '1';
    thePV1.AssignedPatientLocation := aCaseRecord.Placer;
    SetPV1(Hl7Message, thePV1);

    ClearOBR(theOBR);
    theOBR.SetID    := '1';
    theOBR.PlacOrdNumb := '';
    theOBR.FillOrdNumb := EncodedDateTime(Now) + IntToStr(random(13000));
    theOBR.USI      := 'SPINA Carb';
    theOBR.Priority := '';
    theOBR.ReqDateTime := EncodedDateTime(Now);
    theOBR.ObsDateTime := NA_DTM;
    theOBR.ObsEndDateTime := '';
    SetOBR(HL7Message, theOBR);

    ClearNTE(theNTE);
    theNTE.SetID   := '1';
    theNTE.CommentSource := 'O';
    theNTE.comment := 'Data source: SPINA Carb ' + FileVersion;
    SetNTE(HL7Message, theNTE);

    ClearNTE(theNTE);
    theNTE.SetID   := '2';
    theNTE.CommentSource := 'O';
    SetNTE(HL7Message, theNTE);

    ClearNTE(theNTE);
    theNTE.SetID   := '3';
    theNTE.CommentSource := 'O';
    theNTE.comment := aCaseRecord.Comment;
    SetNTE(HL7Message, theNTE);

    setIDcounter := 1;

    ClearOBX(theOBX);
    theOBX.SetID     := IntToStr(setIDcounter);
    theOBX.ValueType := 'NM';

    //

    ClearSPM(theSPM);
    theSPM.SetID := '1';
    if isNaN(aCaseRecord.OBDate) then
      theSPM.SpecimenCollectionDateTime := ''
    else
      theSPM.SpecimenCollectionDateTime := EncodedDateTime(aCaseRecord.OBDate);
    SetSPM(Hl7Message, theSPM);

    WriteHL7File(HL7Message, filePath);

    DefaultFormatSettings.DecimalSeparator := oldSeparator;
  end;
  if assigned(HL7Message) then
    HL7Message.Destroy;
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

