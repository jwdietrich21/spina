unit HandleImpEx;

{ SPINA-Thyr }

{ Application for calculating structure parameters }
{ of thyrotropic feedback control }

{ Programm zur Berechnung von Strukturparametern }
{ des thyreotropen Regelkreises }

{ Version 3.4.2 }

{ (c) J. W. Dietrich, 1994 - 2014 }
{ (c) Ludwig Maximilian University of Munich 1995 - 2002 }
{ (c) University of Ulm Hospitals 2002-2004 }
{ (c) Ruhr University of Bochum 2005 - 2014 }

{ This unit handles import and export of laboratory results and calculations }

{ Source code released under the BSD License }
{ See http://spina.medical-cybernetics.de for details }

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Dialogs, LCLVersion,
  SPINA_Engine, SPINA_Types, HL7, MSH, MSA, OBR, OBX;

procedure SaveResults(caseRecord: tCaseRecord);

implementation

uses spina_toolbar;

procedure SaveStringToPath(theString, filePath: string);
var
  textFile: TFileStream = nil;
  textLength: integer;
  stringBuffer: ^string;
begin
  textLength := length(theString);
  try
    textFile := TFileStream.Create(filePath, fmOpenWrite or fmCreate);
    { write string to stream while avoiding to write the initial length }
    textFile.WriteBuffer(theString[1], textLength);
  finally
    if textFile <> nil then
      textFile.Free;
  end;
end;

procedure SaveAsHL7file(acaseRecord: tCaseRecord);
var
  HL7Message: THL7Message;
  newSegment: THL7Segment;
  delimiters: str5;
  sendingApp, sendingFac, receivingApp, receivingFac: str227;
  dateTime: str26;
  security: str40;
  messageType: str15;
  controlID: str20;
  processingID: str3;
  versionID: str60;
  sequenceNumber: str15;
  continuationPointer: str180;
  AccAckType, AppAckType: Str2;
  countryCode: str3;
  charSet: str16;
  messageLanguage: str250;
  altCharHandlScheme: str20;
  profileID: str427;
  SetID: str4;
  PlacOrdNumb, FillOrdNumb: str22;
  USI: str250;
  Priority: Str2;
  ReqDateTime, ObsDateTime, ObsEndDateTime: str26;
  ValueType: str2;
  ObsID: str250;
  obsSubID: str20;
  obsValue, obsValue2: ansistring;
  Units: str250;
  RefRange: str60;
  AbnormFlags, probability: str5;
  Nature: str2;
  status: char;
  RRDate: str26;
  UDAC: str20;
  prodID, respObs, observMethod: str250;
  EquipInstID: str22;
  AnalysisDateTime: str26;
begin
  HL7Message := THL7Message.Create('2.5');
  if HL7Message = nil then
    ShowMessage('HL7 Error')
  else
  begin
    delimiters := STANDARD_DELIMITERS;
    sendingApp := 'SPINA Thyr';
    sendingFac := gPreferences.MSH_ID;
    receivingApp := '';
    receivingFac := '';
    dateTime := EncodedDateTime(Now);
    messageType := 'ADT^A04';
    security := '';
    messageType := '';
    controlID := EncodedDateTime(Now) + IntToStr(random(13000));
    processingID := '';
    versionID := '';
    sequenceNumber := '';
    continuationPointer := '';
    AccAckType := '';
    AppAckType := '';
    countryCode := '276';
    charSet := '';
    messageLanguage := '';
    altCharHandlScheme := '';
    profileID := '';
    SetMSH(HL7Message, delimiters, sendingApp,
      sendingFac, receivingApp, receivingFac, dateTime,
      security, messageType, controlID, processingID,
      versionID, sequenceNumber, continuationPointer,
      AccAckType, AppAckType, countryCode, charSet,
      messageLanguage, altCharHandlScheme, profileID);

    SetID := '1';
    PlacOrdNumb := '';
    FillOrdNumb := '';
    USI := 'SPINA Thyr';
    Priority := '';
    ReqDateTime := EncodedDateTime(Now);
    ObsDateTime := '';
    ObsEndDateTime := '';
    SetOBR(HL7Message, SetID, PlacOrdNumb, FillOrdNumb, USI,
      Priority, ReqDateTime, ObsDateTime, ObsEndDateTime);
    SetID := '1';
    ValueType := 'NM';
    ObsID := 'SPINA-GT';
    obsSubID := 'GT';
    obsValue := FloatToStrF(acaseRecord.GT * 1E12, ffNumber, 5, 2);
    Units := 'pmol/s';
    RefRange := FloatToStr(gReferenceRanges.GT.ln * 1E12) + ' - ' +
      FloatToStr(gReferenceRanges.GT.hn * 1E12);
    AbnormFlags := '';
    probability := '';
    Nature := '';
    status := 'F';
    RRDate := '';
    UDAC := '';
    ObsDateTime := '';
    prodID := '';
    respObs := '';
    observMethod := '';
    EquipInstID := '';
    AnalysisDateTime := '';
    SetOBX(HL7Message, SetID, ValueType, ObsID,
      obsSubID, obsValue, Units, RefRange,
      AbnormFlags, probability, Nature, status, RRDate,
      UDAC, ObsDateTime, prodID, respObs, observMethod,
      EquipInstID, AnalysisDateTime);

    SetID := '2';
    ValueType := 'NM';
    ObsID := 'SPINA-GD';
    obsSubID := 'GD';
    obsValue := FloatToStrF(acaseRecord.GD * 1E9, ffNumber, 5, 2);
    Units := 'nmol/s';
    RefRange := FloatToStr(gReferenceRanges.GD.ln * 1E9) + ' - ' +
      FloatToStr(gReferenceRanges.GD.hn * 1E9);
    AbnormFlags := '';
    probability := '';
    Nature := '';
    status := 'F';
    RRDate := '';
    UDAC := '';
    ObsDateTime := '';
    prodID := '';
    respObs := '';
    observMethod := '';
    EquipInstID := '';
    AnalysisDateTime := '';
    SetOBX(HL7Message, SetID, ValueType, ObsID,
      obsSubID, obsValue, Units, RefRange,
      AbnormFlags, probability, Nature, status, RRDate,
      UDAC, ObsDateTime, prodID, respObs, observMethod,
      EquipInstID, AnalysisDateTime);

    SetID := '3';
    ValueType := 'NM';
    ObsID := 'TSHI';
    obsSubID := 'TSHI';
    obsValue := FloatToStrF(acaseRecord.TSHI, ffNumber, 5, 2);
    Units := '';
    RefRange := FloatToStr(gReferenceRanges.TSHI.ln) + ' - ' +
      FloatToStr(gReferenceRanges.TSHI.hn);
    AbnormFlags := '';
    probability := '';
    Nature := '';
    status := 'F';
    RRDate := '';
    UDAC := '';
    ObsDateTime := '';
    prodID := '';
    respObs := '';
    observMethod := '';
    EquipInstID := '';
    AnalysisDateTime := '';
    SetOBX(HL7Message, SetID, ValueType, ObsID,
      obsSubID, obsValue, Units, RefRange,
      AbnormFlags, probability, Nature, status, RRDate,
      UDAC, ObsDateTime, prodID, respObs, observMethod,
      EquipInstID, AnalysisDateTime);

    SetID := '4';
    ValueType := 'NM';
    ObsID := 'TTSI';
    obsSubID := 'TTSI';
    obsValue := FloatToStrF(acaseRecord.TTSI, ffNumber, 5, 2);
    Units := '';
    RefRange := FloatToStr(gReferenceRanges.TTSI.ln) + ' - ' +
      FloatToStr(gReferenceRanges.TTSI.hn);
    AbnormFlags := '';
    probability := '';
    Nature := '';
    status := 'F';
    RRDate := '';
    UDAC := '';
    ObsDateTime := '';
    prodID := '';
    respObs := '';
    observMethod := '';
    EquipInstID := '';
    AnalysisDateTime := '';
    SetOBX(HL7Message, SetID, ValueType, ObsID,
      obsSubID, obsValue, Units, RefRange,
      AbnormFlags, probability, Nature, status, RRDate,
      UDAC, ObsDateTime, prodID, respObs, observMethod,
      EquipInstID, AnalysisDateTime);

    WriteHL7File(HL7Message, SPINAToolbar.SaveResultsDialog.FileName);
  end;
end;

procedure SaveResults(caseRecord: tCaseRecord);
var
  filePath: string;
  theFilterIndex: integer;
begin
  SPINAToolbar.SaveResultsDialog.FilterIndex := 1;
  if SPINAToolbar.SaveResultsDialog.Execute then
  begin
    theFilterIndex := SPINAToolbar.SaveResultsDialog.FilterIndex;
    {$IFDEF LCLcarbon}{compensates for a bug in older versions of carbon widgetset}
      if (lcl_major < 2) and (lcl_minor < 2) then
        theFilterIndex := theFilterIndex + 1;
    {$ENDIF}
    case theFilterIndex of
      1: SaveStringToPath(gResultString, SPINAToolbar.SaveResultsDialog.FileName);
      2: SaveAsHL7file(caseRecord);
    end;
  end;
end;

end.
