unit HandleImpEx;

{ SPINA-Thyr }

 { Application for calculating structure parameters }
 { of thyrotropic feedback control }

 { Programm zur Berechnung von Strukturparametern }
 { des thyreotropen Regelkreises }

{ Version 4.0.0 }

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
  Classes, SysUtils, Dialogs, LCLVersion, Math, FileUtil,
  SPINA_Engine, SPINA_Types, UnitConverter, VersionSupport,
  HL7, MSH, MSA, NTE, PID, PV1, OBR, OBX, SPM;

const
  ORU_R01_variant1 = 'ORU^R01'; // Unsolicited transmission of an observation
  ORU_R01_variant2 = 'ORU^R01^ORU_R01';
  ORU_R04    = 'ORU^R04'; //Response to query
  ACK_R01    = 'ACK^R01^ACK';
  MDM_T01    = 'MDM^T01';
  LT4_CODE   = 'L-T4';
  LT3_CODE   = 'L-T3';
  RHTSH_CODE = 'rh-TSH';

procedure ReadHL7Message(theFile: string; var aCaseRecord: tCaseRecord);
procedure ReadCaseResults(var caseRecord: tCaseRecord);
procedure SaveResults(caseRecord: tCaseRecord);

implementation

uses spina_toolbar;

procedure SaveStringToPath(theString, filePath: string);
var
  textFile:     TFileStream = nil;
  textLength:   integer;
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

procedure SaveAsHL7Message(aCaseRecord: tCaseRecord);
var
  oldSeparator, t4substFlag, t3substFlag, tshsubstFlag: char;
  HL7Message: THL7Message;
  newSegment: THL7Segment;
  delimiters: str5;
  thePID:   tPID;
  thePV1:   tPV1;
  theSPM:   tSPM;
  theNTE:   tNTE;
  setIDcounter: integer;
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
  charSet:  str16;
  messageLanguage: str250;
  altCharHandlScheme: str20;
  profileID: str427;
  SetID:    str4;
  PlacOrdNumb, FillOrdNumb: str22;
  USI:      str250;
  Priority: Str2;
  ReqDateTime, ObsDateTime, ObsEndDateTime: str26;
  ValueType: str2;
  ObsID:    str250;
  obsSubID: str20;
  obsValue, obsValue2: ansistring;
  Units:    str250;
  RefRange: str60;
  AbnormFlags, probability: str5;
  Nature:   str2;
  status:   char;
  RRDate:   str26;
  UDAC:     str20;
  prodID, respObs, observMethod: str250;
  EquipInstID: str22;
  AnalysisDateTime: str26;
begin
  HL7Message := THL7Message.Create('2.5');
  if HL7Message = nil then
    ShowMessage('HL7 Error')
  else
  begin
    oldSeparator := DefaultFormatSettings.DecimalSeparator;
    DefaultFormatSettings.DecimalSeparator := DEC_POINT;
    delimiters   := STANDARD_DELIMITERS;
    sendingApp   := 'SPINA Thyr';
    sendingFac   := gPreferences.MSH_ID;
    receivingApp := '';
    receivingFac := '';
    dateTime     := EncodedDateTime(Now);
    messageType  := ORU_R01_variant1;
    security     := '';
    controlID    := EncodedDateTime(Now) + IntToStr(random(13000));
    processingID := 'P^A';
    versionID    := ''; // ignored; will be filled-in automatically by PUMA
    sequenceNumber := '';
    continuationPointer := '';
    AccAckType   := '';
    AppAckType   := '';
    countryCode  := '';
    charSet      := '';
    messageLanguage := '';
    altCharHandlScheme := '';
    profileID    := '';
    SetMSH(HL7Message, delimiters, sendingApp,
      sendingFac, receivingApp, receivingFac, dateTime,
      security, messageType, controlID, processingID,
      versionID, sequenceNumber, continuationPointer,
      AccAckType, AppAckType, countryCode, charSet,
      messageLanguage, altCharHandlScheme, profileID);

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

    SetID    := '1';
    PlacOrdNumb := '';
    FillOrdNumb := '';
    USI      := 'SPINA Thyr';
    Priority := '';
    ReqDateTime := EncodedDateTime(Now);
    ObsDateTime := '';
    ObsEndDateTime := '';
    SetOBR(HL7Message, SetID, PlacOrdNumb, FillOrdNumb, USI,
      Priority, ReqDateTime, ObsDateTime, ObsEndDateTime);

    ClearNTE(theNTE);
    theNTE.SetID   := '1';
    theNTE.CommentSource := 'O';
    theNTE.comment := 'Data source: SPINA Thyr ' + GetFileVersion;
    SetNTE(HL7Message, theNTE);

    if aCaseRecord.T4Therapy then
      t4substFlag := 'T'
    else
      t4substFlag := 'F';
    if aCaseRecord.T3Therapy then
      t3substFlag := 'T'
    else
      t3substFlag := 'F';
    if aCaseRecord.TSHTherapy then
      tshsubstFlag := 'T'
    else
      tshsubstFlag := 'F';
    ClearNTE(theNTE);
    theNTE.SetID   := '2';
    theNTE.CommentSource := 'O';
    theNTE.comment := LT4_CODE + HL7Message.Delimiters.SubcomponentSeparator +
      t4substFlag + HL7Message.Delimiters.ComponentSeparator + LT3_CODE +
      HL7Message.Delimiters.SubcomponentSeparator + t3substFlag +
      HL7Message.Delimiters.ComponentSeparator + RHTSH_CODE +
      HL7Message.Delimiters.SubcomponentSeparator + tshsubstFlag;
    SetNTE(HL7Message, theNTE);

    setIDcounter := 1;

    SetID     := IntToStr(setIDcounter);
    ValueType := 'NM';
    ObsID     := 'TSH';
    obsSubID  := 'TSH';
    obsValue  := FloatToStrF(acaseRecord.TSH, ffNumber, 5, 2);
    Units     := gReferenceRanges.TSH.UOM;
    RefRange  := FloatToStr(gReferenceRanges.TSH.ln) + ' - ' +
      FloatToStr(gReferenceRanges.TSH.hn);
    AbnormFlags := '';
    probability := '';
    Nature    := '';
    status    := 'F';
    RRDate    := '';
    UDAC      := '';
    if isNaN(aCaseRecord.OBDate) then
      ObsDateTime := ''
    else
      ObsDateTime := EncodedDateTime(aCaseRecord.OBDate);
    prodID := '';
    respObs      := '';
    observMethod := '';
    EquipInstID  := '';
    AnalysisDateTime := '';
    SetOBX(HL7Message, SetID, ValueType, ObsID,
      obsSubID, obsValue, Units, RefRange,
      AbnormFlags, probability, Nature, status, RRDate,
      UDAC, ObsDateTime, prodID, respObs, observMethod,
      EquipInstID, AnalysisDateTime);

    Inc(setIDcounter);
    SetID     := IntToStr(setIDcounter);
    ValueType := 'NM';
    ObsID     := 'FT4';
    obsSubID  := 'FT4';
    obsValue  := FloatToStrF(ConvertedValue(acaseRecord.FT4, T4_MOLAR_MASS,
      'mol/l', gReferenceRanges.FT4.UOM), ffNumber, 5, 2);
    Units     := gReferenceRanges.FT4.UOM;
    RefRange  := FloatToStr(gReferenceRanges.FT4.ln) + ' - ' +
      FloatToStr(gReferenceRanges.FT4.hn);
    AbnormFlags := '';
    probability := '';
    Nature    := '';
    status    := 'F';
    RRDate    := '';
    UDAC      := '';
    if isNaN(aCaseRecord.OBDate) then
      ObsDateTime := ''
    else
      ObsDateTime := EncodedDateTime(aCaseRecord.OBDate);
    prodID := '';
    respObs      := '';
    observMethod := '';
    EquipInstID  := '';
    AnalysisDateTime := '';
    SetOBX(HL7Message, SetID, ValueType, ObsID,
      obsSubID, obsValue, Units, RefRange,
      AbnormFlags, probability, Nature, status, RRDate,
      UDAC, ObsDateTime, prodID, respObs, observMethod,
      EquipInstID, AnalysisDateTime);

    Inc(setIDcounter);
    SetID     := IntToStr(setIDcounter);
    ValueType := 'NM';
    ObsID     := 'FT3';
    obsSubID  := 'FT3';
    obsValue  := FloatToStrF(ConvertedValue(acaseRecord.FT3, T3_MOLAR_MASS,
      'mol/l', gReferenceRanges.FT3.UOM), ffNumber, 5, 2);
    Units     := gReferenceRanges.FT3.UOM;
    ;
    RefRange := FloatToStr(gReferenceRanges.FT3.ln) + ' - ' +
      FloatToStr(gReferenceRanges.FT3.hn);
    AbnormFlags := '';
    probability := '';
    Nature := '';
    status := 'F';
    RRDate := '';
    UDAC := '';
    if isNaN(aCaseRecord.OBDate) then
      ObsDateTime := ''
    else
      ObsDateTime := EncodedDateTime(aCaseRecord.OBDate);
    prodID := '';
    respObs      := '';
    observMethod := '';
    EquipInstID  := '';
    AnalysisDateTime := '';
    SetOBX(HL7Message, SetID, ValueType, ObsID,
      obsSubID, obsValue, Units, RefRange,
      AbnormFlags, probability, Nature, status, RRDate,
      UDAC, ObsDateTime, prodID, respObs, observMethod,
      EquipInstID, AnalysisDateTime);

    Inc(setIDcounter);
    SetID     := IntToStr(setIDcounter);
    ValueType := 'NM';
    ObsID     := 'TT4';
    obsSubID  := 'TT4';
    obsValue  := FloatToStrF(ConvertedValue(acaseRecord.TT4, T4_MOLAR_MASS,
      'mol/l', gReferenceRanges.TT4.UOM), ffNumber, 5, 2);
    Units     := gReferenceRanges.TT4.UOM;
    ;
    RefRange := FloatToStr(gReferenceRanges.TT4.ln) + ' - ' +
      FloatToStr(gReferenceRanges.TT4.hn);
    AbnormFlags := '';
    probability := '';
    Nature := '';
    status := 'F';
    RRDate := '';
    UDAC := '';
    if isNaN(aCaseRecord.OBDate) then
      ObsDateTime := ''
    else
      ObsDateTime := EncodedDateTime(aCaseRecord.OBDate);
    prodID := '';
    respObs      := '';
    observMethod := '';
    EquipInstID  := '';
    AnalysisDateTime := '';
    SetOBX(HL7Message, SetID, ValueType, ObsID,
      obsSubID, obsValue, Units, RefRange,
      AbnormFlags, probability, Nature, status, RRDate,
      UDAC, ObsDateTime, prodID, respObs, observMethod,
      EquipInstID, AnalysisDateTime);

    Inc(setIDcounter);
    SetID     := IntToStr(setIDcounter);
    ValueType := 'NM';
    ObsID     := 'TT3';
    obsSubID  := 'TT3';
    obsValue  := FloatToStrF(ConvertedValue(acaseRecord.TT3, T3_MOLAR_MASS,
      'mol/l', gReferenceRanges.TT3.UOM), ffNumber, 5, 2);
    Units     := gReferenceRanges.TT3.UOM;
    ;
    RefRange := FloatToStr(gReferenceRanges.TT3.ln) + ' - ' +
      FloatToStr(gReferenceRanges.TT3.hn);
    AbnormFlags := '';
    probability := '';
    Nature := '';
    status := 'F';
    RRDate := '';
    UDAC := '';
    if isNaN(aCaseRecord.OBDate) then
      ObsDateTime := ''
    else
      ObsDateTime := EncodedDateTime(aCaseRecord.OBDate);
    prodID := '';
    respObs      := '';
    observMethod := '';
    EquipInstID  := '';
    AnalysisDateTime := '';
    SetOBX(HL7Message, SetID, ValueType, ObsID,
      obsSubID, obsValue, Units, RefRange,
      AbnormFlags, probability, Nature, status, RRDate,
      UDAC, ObsDateTime, prodID, respObs, observMethod,
      EquipInstID, AnalysisDateTime);

    Inc(setIDcounter);
    SetID     := IntToStr(setIDcounter);
    ValueType := 'NM';
    ObsID     := 'SPINA-GT';
    obsSubID  := 'GT';
    obsValue  := FloatToStrF(acaseRecord.GT * 1E12, ffNumber, 5, 2);
    Units     := 'pmol/s';
    RefRange  := FloatToStr(gReferenceRanges.GT.ln * 1E12) + ' - ' +
      FloatToStr(gReferenceRanges.GT.hn * 1E12);
    AbnormFlags := '';
    probability := '';
    Nature    := '';
    status    := 'F';
    RRDate    := '';
    UDAC      := '';
    if isNaN(aCaseRecord.OBDate) then
      ObsDateTime := ''
    else
      ObsDateTime := EncodedDateTime(aCaseRecord.OBDate);
    prodID := '';
    respObs      := '';
    observMethod := '';
    EquipInstID  := '';
    AnalysisDateTime := '';
    SetOBX(HL7Message, SetID, ValueType, ObsID,
      obsSubID, obsValue, Units, RefRange,
      AbnormFlags, probability, Nature, status, RRDate,
      UDAC, ObsDateTime, prodID, respObs, observMethod,
      EquipInstID, AnalysisDateTime);

    Inc(setIDcounter);
    SetID     := IntToStr(setIDcounter);
    ValueType := 'NM';
    ObsID     := 'SPINA-GD';
    obsSubID  := 'GD';
    obsValue  := FloatToStrF(acaseRecord.GD * 1E9, ffNumber, 5, 2);
    Units     := 'nmol/s';
    RefRange  := FloatToStr(gReferenceRanges.GD.ln * 1E9) + ' - ' +
      FloatToStr(gReferenceRanges.GD.hn * 1E9);
    AbnormFlags := '';
    probability := '';
    Nature    := '';
    status    := 'F';
    RRDate    := '';
    UDAC      := '';
    if isNaN(aCaseRecord.OBDate) then
      ObsDateTime := ''
    else
      ObsDateTime := EncodedDateTime(aCaseRecord.OBDate);
    prodID := '';
    respObs      := '';
    observMethod := '';
    EquipInstID  := '';
    AnalysisDateTime := '';
    SetOBX(HL7Message, SetID, ValueType, ObsID,
      obsSubID, obsValue, Units, RefRange,
      AbnormFlags, probability, Nature, status, RRDate,
      UDAC, ObsDateTime, prodID, respObs, observMethod,
      EquipInstID, AnalysisDateTime);

    Inc(setIDcounter);
    SetID     := IntToStr(setIDcounter);
    ValueType := 'NM';
    ObsID     := 'TSHI';
    obsSubID  := 'TSHI';
    obsValue  := FloatToStrF(acaseRecord.TSHI, ffNumber, 5, 2);
    Units     := '';
    RefRange  := FloatToStr(gReferenceRanges.TSHI.ln) + ' - ' +
      FloatToStr(gReferenceRanges.TSHI.hn);
    AbnormFlags := '';
    probability := '';
    Nature    := '';
    status    := 'F';
    RRDate    := '';
    UDAC      := '';
    if isNaN(aCaseRecord.OBDate) then
      ObsDateTime := ''
    else
      ObsDateTime := EncodedDateTime(aCaseRecord.OBDate);
    prodID := '';
    respObs      := '';
    observMethod := '';
    EquipInstID  := '';
    AnalysisDateTime := '';
    SetOBX(HL7Message, SetID, ValueType, ObsID,
      obsSubID, obsValue, Units, RefRange,
      AbnormFlags, probability, Nature, status, RRDate,
      UDAC, ObsDateTime, prodID, respObs, observMethod,
      EquipInstID, AnalysisDateTime);

    Inc(setIDcounter);
    SetID     := IntToStr(setIDcounter);
    ValueType := 'NM';
    ObsID     := 'TTSI';
    obsSubID  := 'TTSI';
    obsValue  := FloatToStrF(acaseRecord.TTSI, ffNumber, 5, 2);
    Units     := '';
    RefRange  := FloatToStr(gReferenceRanges.TTSI.ln) + ' - ' +
      FloatToStr(gReferenceRanges.TTSI.hn);
    AbnormFlags := '';
    probability := '';
    Nature    := '';
    status    := 'F';
    RRDate    := '';
    UDAC      := '';
    if isNaN(aCaseRecord.OBDate) then
      ObsDateTime := ''
    else
      ObsDateTime := EncodedDateTime(aCaseRecord.OBDate);
    prodID := '';
    respObs      := '';
    observMethod := '';
    EquipInstID  := '';
    AnalysisDateTime := '';
    SetOBX(HL7Message, SetID, ValueType, ObsID,
      obsSubID, obsValue, Units, RefRange,
      AbnormFlags, probability, Nature, status, RRDate,
      UDAC, ObsDateTime, prodID, respObs, observMethod,
      EquipInstID, AnalysisDateTime);

    ClearSPM(theSPM);
    theSPM.SetID := '1';
    if isNaN(aCaseRecord.OBDate) then
      theSPM.SpecimenCollectionDateTime := ''
    else
      theSPM.SpecimenCollectionDateTime := EncodedDateTime(aCaseRecord.OBDate);
    SetSPM(Hl7Message, theSPM);

    WriteHL7File(HL7Message, UTF8ToSys(SPINAToolbar.SaveResultsDialog.FileName));
    DefaultFormatSettings.DecimalSeparator := oldSeparator;
  end;
end;

procedure SaveAsTextFile(aCaseRecord: tCaseRecord);
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
    theHeader := aCaseRecord.PID + '/' + aCaseRecord.CaseID + kCR +
      kLF + aCaseRecord.Name + ', ' + aCaseRecord.GivenNames + ' *' +
      DOBDateString + kCR + kLF + OBDateString + ' (' + aCaseRecord.Placer +
      ')' + kCR + kLF + kCR + kLF;
  theString := theHeader + gResultString;
  SaveStringToPath(theString, UTF8ToSys(SPINAToolbar.SaveResultsDialog.FileName));
end;

procedure ReadHL7Message(theFile: string; var aCaseRecord: tCaseRecord);
var
  oldSeparator: char;
  Count:      integer;
  theHL7Message: THL7Message;
  theSegment: THL7Segment;
  theMSHRecord: tMSH;
  thePIDRecord: tPID;
  thePV1Record: tPV1;
  theOBRRecord: tOBR;
  theOBXRecord: tOBX;
  theNTERecord: tNTE;
  theField:   THL7Field;
  theComponent, nextComponent: THL7Component;
  theSubComponent, nextSubComponent: THL7SubComponent;
begin
  oldSeparator := DefaultFormatSettings.DecimalSeparator;
  DefaultFormatSettings.DecimalSeparator := DEC_POINT;
  aCaseRecord.T4Therapy := False;
  aCaseRecord.T3Therapy := False;
  aCaseRecord.TSHTherapy := False;
  ReadHL7File(theHL7Message, theFile);
  theSegment := theHL7Message.FirstSegment;
  while theSegment <> nil do
  begin
    if theSegment.segmentType = 'MSH' then
    begin
      GetMSH(theHL7Message, theMSHRecord);
      theHL7Message.HL7Version := theMSHRecord.versionID;
    end;
    if theSegment.segmentType = 'PID' then
    begin
      Count := 0;
      GetPID(theHL7Message, thePIDRecord);
      aCaseRecord.DoBDate := DecodeDateTime(thePIDRecord.BirthDateTime);
      theField := theSegment.FirstOccurrence.FirstField;
      while theField <> nil do
      begin
        Inc(Count);
        if Count = 4 then
        begin
          theComponent    := theField.FirstComponent;
          aCaseRecord.PID := theComponent.contentString;
          theComponent    := theComponent.nextSibling;
          aCaseRecord.CaseID := theComponent.contentString;
        end;
        if Count = 6 then
        begin
          theComponent     := theField.FirstComponent;
          aCaseRecord.Name := theComponent.contentString;
          theComponent     := theComponent.nextSibling;
          aCaseRecord.GivenNames := theComponent.contentString;
        end;
        theField := theField.nextSibling;
      end;
    end;
    if theSegment.segmentType = 'PV1' then
    begin
      GetPV1(theHL7Message, thePV1Record);
      aCaseRecord.Placer := thePV1Record.AssignedPatientLocation;
    end;
    if theSegment.segmentType = 'NTE' then
    begin
      theField      := nil;
      theComponent  := nil;
      nextComponent := nil;
      theSubComponent := nil;
      nextSubComponent := nil;
      GetNTE(theSegment, theNTERecord);
      if LeftStr(theNTERecord.comment, 4) = LT4_CODE then
      begin
        theField := theSegment.FirstOccurrence.FirstField.nextSibling.
          nextSibling.nextSibling;
        if assigned(theField) then
          theComponent := theField.FirstComponent;
        while assigned(theComponent) do
        begin
          theSubComponent := theComponent.FirstSubComponent;
          if assigned(theSubComponent) then
          begin
            if theSubComponent.contentString = LT4_CODE then
            begin
              nextSubComponent := theSubComponent.nextSibling;
              if assigned(nextSubComponent) then
                if UpperCase(nextSubComponent.contentString) = 'T' then
                  aCaseRecord.T4Therapy := True;
            end
            else if theSubComponent.contentString = LT3_CODE then
            begin
              nextSubComponent := theSubComponent.nextSibling;
              if assigned(nextSubComponent) then
                if UpperCase(nextSubComponent.contentString) = 'T' then
                  aCaseRecord.T3Therapy := True;
            end
            else if theSubComponent.contentString = RHTSH_CODE then
            begin
              nextSubComponent := theSubComponent.nextSibling;
              if assigned(nextSubComponent) then
                if UpperCase(nextSubComponent.contentString) = 'T' then
                  aCaseRecord.TSHTherapy := True;
            end;
          end;
          theComponent := theComponent.nextSibling;
        end;
      end;
    end;
    if theSegment.segmentType = 'OBX' then
    begin
      GetOBX(theSegment, theOBXRecord);
      if theOBXRecord.ObsID = 'TSH' then
      begin
        aCaseRecord.TSH     := StrToFloatDef(theOBXRecord.obsValue, NaN);
        aCaseRecord.TSH_UOM := theOBXRecord.Units;
      end;
      if theOBXRecord.ObsID = 'FT4' then
      begin
        aCaseRecord.FT4     := StrToFloatDef(theOBXRecord.obsValue, NaN);
        aCaseRecord.FT4_UOM := theOBXRecord.Units;
        aCaseRecord.FT4     :=
          ConvertedValue(acaseRecord.FT4, T4_MOLAR_MASS, aCaseRecord.FT4_UOM, 'mol/l');
      end;
      if theOBXRecord.ObsID = 'FT3' then
      begin
        aCaseRecord.FT3     := StrToFloatDef(theOBXRecord.obsValue, NaN);
        aCaseRecord.FT3_UOM := theOBXRecord.Units;
        aCaseRecord.FT3     :=
          ConvertedValue(acaseRecord.FT3, T3_MOLAR_MASS, aCaseRecord.FT3_UOM, 'mol/l');
      end;
      if theOBXRecord.ObsID = 'TT4' then
      begin
        aCaseRecord.TT4     := StrToFloatDef(theOBXRecord.obsValue, NaN);
        aCaseRecord.TT4_UOM := theOBXRecord.Units;
        aCaseRecord.TT4     :=
          ConvertedValue(acaseRecord.TT4, T4_MOLAR_MASS, aCaseRecord.TT4_UOM, 'mol/l');
      end;
      if theOBXRecord.ObsID = 'TT3' then
      begin
        aCaseRecord.TT3     := StrToFloatDef(theOBXRecord.obsValue, NaN);
        aCaseRecord.TT3_UOM := theOBXRecord.Units;
        aCaseRecord.TT3     :=
          ConvertedValue(acaseRecord.TT3, T3_MOLAR_MASS, aCaseRecord.TT3_UOM, 'mol/l');
      end;
      if theOBXRecord.ObsID = 'SPINA-GT' then
      begin
        aCaseRecord.GT     := StrToFloatDef(theOBXRecord.obsValue, NaN);
        aCaseRecord.GT_UOM := theOBXRecord.Units;
      end;
      if theOBXRecord.ObsID = 'SPINA-GD' then
      begin
        aCaseRecord.GD     := StrToFloatDef(theOBXRecord.obsValue, NaN);
        aCaseRecord.GD_UOM := theOBXRecord.Units;
      end;
      if theOBXRecord.ObsID = 'TSHI' then
        aCaseRecord.TSHI := StrToFloatDef(theOBXRecord.obsValue, NaN);
      if theOBXRecord.ObsID = 'TTSI' then
        aCaseRecord.TTSI := StrToFloatDef(theOBXRecord.obsValue, NaN);
      if IsNaN(aCaseRecord.FT4) then
        gPreferences.T4.Method := totalHormone
      else
        gPreferences.T4.Method := freeHormone;
      if IsNaN(aCaseRecord.FT3) then
        gPreferences.T3.Method := totalHormone
      else
        gPreferences.T3.Method := freeHormone;
      aCaseRecord.OBDate := DecodeDateTime(theOBXRecord.ObsDateTime);
    end;
    theSegment := theSegment.nextSibling;
  end;
  DefaultFormatSettings.DecimalSeparator := oldSeparator;
end;

procedure ReadCaseResults(var caseRecord: tCaseRecord);
var
  filePath: string;
  theFilterIndex: integer;
begin
  if SPINAToolbar.OpenCaseDialog.Execute then
  begin
    theFilterIndex := SPINAToolbar.OpenCaseDialog.FilterIndex;
    {$IFDEF LCLcarbon}{compensates for a bug in older versions of carbon widgetset}
    if (lcl_major < 2) and (lcl_minor < 2) then
      theFilterIndex := theFilterIndex + 1;
    {$ENDIF}
    case theFilterIndex of
      1: ReadHL7Message(UTF8ToSys(SPINAToolbar.OpenCaseDialog.FileName), caseRecord);
    end;
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
      1: SaveAsHL7Message(caseRecord);
      2: SaveAsTextFile(caseRecord);
    end;
  end;
end;

end.