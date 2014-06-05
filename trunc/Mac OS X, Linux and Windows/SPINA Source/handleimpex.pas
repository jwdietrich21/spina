unit HandleImpEx;

{ SPINA-Thyr }

{ Application for calculating structure parameters }
{ of thyrotropic feedback control }

{ Programm zur Berechnung von Strukturparametern }
{ des thyreotropen Regelkreises }

{ Version 3.5.0 }

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
  Classes, SysUtils, Dialogs, LCLVersion, Math,
  SPINA_Engine, SPINA_Types, UnitConverter,
  HL7, MSH, MSA, PID, PV1, OBR, OBX, SPM;

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

procedure SaveAsHL7file(aCaseRecord: tCaseRecord);
var
  HL7Message: THL7Message;
  newSegment: THL7Segment;
  delimiters: str5;
  thePID: tPID;
  thePV1: tPV1;
  theSPM: tSPM;
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

    thePID.SetID := '1';
    if isNaN(aCaseRecord.DoBDate) then
      thePID.BirthDateTime := ''
    else
      thePID.BirthDateTime := EncodedDateTime(aCaseRecord.DoBDate);
    thePID.PatientIDList := aCaseRecord.PID +
      HL7Message.Delimiters.ComponentSeparator + aCaseRecord.CaseID;
    thePID.PatientName := aCaseRecord.Name +
      HL7Message.Delimiters.ComponentSeparator + aCaseRecord.GivenNames;
    thePID.AdminSex := '';
    thePID.AltPatID := '';
    thePID.BirthOrder := '';
    thePID.BirthPlace := '';
    thePID.BreedCode := '';
    thePID.BusinessPhone := '';
    thePID.Citizenship := '';
    thePID.CountyCode := '';
    thePID.DriverLicenseNumber := '';
    thePID.EthnicGroup := '';
    thePID.HomePhoe := '';
    thePID.IDReliabilityCode := '';
    thePID.IDReliabilityIndicator := '';
    thePID.IDUnknownIndicator := '';
    thePID.LastUpdateDateTime := '';
    thePID.LastUpdateFacility := '';
    thePID.MaritalStatus := '';
    thePID.MothersID := '';
    thePID.MothersMaidenName := '';
    thePID.MultipleBirthID := '';
    thePID.Nationality := '';
    thePID.SpeciesCode := '';
    thePID.SSNNumber := '';
    thePID.Strain := '';
    thePID.TribalCitizenship := '';
    thePID.VeteransMilitaryStatus := '';
    thePID.PatientID := '';
    thePID.AltPatID := '';
    thePID.PatientAlias := '';
    thePID.PatientAccountNumber := '';
    thePID.PatientAddress := '';
    thePID.PatientDeathDateTime := '';
    thePID.PatientDeathIndicator := '';
    thePID.PatientTelecomInformation :='';
    SetPID(HL7Message, thePID);

    thePV1.SetID := '1';
    thePV1.AssignedPatientLocation := aCaseRecord.Placer;
    thePV1.AccountStatus := '';
    thePV1.AssignedPatientLocation := '';
    thePV1.AdmissionType := '';
    thePV1.AdmitDateTime := '';
    thePV1.AccountStatus := '';
    thePV1.AdmitSource := '';
    thePV1.AdmittingDoctor := '';
    thePV1.AlternateVisitID := '';
    thePV1.AmbulatoryStatus := '';
    thePV1.AttendingDoctor := '';
    thePV1.BadDeptAgencyCode := '';
    thePV1.BadDeptRecoveryAmount := '';
    thePV1.BadDeptTransferAmount := '';
    thePV1.BedStatus := '';
    thePV1.ChargePriceIndicator := '';
    thePV1.ConsultingDoctor := '';
    thePV1.ContractAmount := '';
    thePV1.ContractCode := '';
    thePV1.ContractEffectiveDate := '';
    thePV1.ContractPeriod := '';
    thePV1.CourtesyCode := '';
    thePV1.CreditRate := '';
    thePV1.CurrentPatientBalance := '';
    thePV1.DeleteAccountDate := '';
    thePV1.DeleteAccountIndicator := '';
    thePV1.DietType := '';
    thePV1.DischargeDateTime := '';
    thePV1.DischargeDisposition := '';
    thePV1.DischargedToLocation := '';
    thePV1.FinancialClass := '';
    thePV1.HospitalService := '';
    thePV1.InterestCode := '';
    thePV1.OtherHealthcareProvider := '';
    thePV1.PatientClass := '';
    thePV1.PatientType := '';
    thePV1.PendingLocation := '';
    thePV1.PreadmitNumber := '';
    thePV1.PriorPatientLocation := '';
    thePV1.PreadmitTestIndicator := '';
    thePV1.PriorTemporaryLocation := '';
    thePV1.ReadmissionIndicator := '';
    thePV1.ReferringDoctor := '';
    thePV1.ServiceEpisodeDescription := '';
    thePV1.ServiceEpisodeID := '';
    thePV1.ServicingFacility := '';
    thePV1.TemporaryLocation := '';
    thePV1.TotalAdustments := '';
    thePV1.TotalCharges := '';
    thePV1.TotalPayments := '';
    thePV1.TransferToBadDeptCode := '';
    thePV1.TransferToBadDeptDate := '';
    thePV1.VIPIndicator := '';
    thePV1.VisitIndicator := '';
    thePV1.VisitNumber := '';
    SetPV1(Hl7Message, thePV1);

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

    setIDcounter := 1;

    inc(setIDcounter);
    SetID := IntToStr(setIDcounter);
    ValueType := 'NM';
    ObsID := 'TSH';
    obsSubID := 'TSH';
    obsValue := FloatToStrF(acaseRecord.TSH, ffNumber, 5, 2);
    Units := gReferenceRanges.TSH.UOM;
    RefRange := FloatToStr(gReferenceRanges.TSH.ln) + ' - ' +
      FloatToStr(gReferenceRanges.TSH.hn);
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
    respObs := '';
    observMethod := '';
    EquipInstID := '';
    AnalysisDateTime := '';
    SetOBX(HL7Message, SetID, ValueType, ObsID,
      obsSubID, obsValue, Units, RefRange,
      AbnormFlags, probability, Nature, status, RRDate,
      UDAC, ObsDateTime, prodID, respObs, observMethod,
      EquipInstID, AnalysisDateTime);

    inc(setIDcounter);
    SetID := IntToStr(setIDcounter);
    ValueType := 'NM';
    ObsID := 'FT4';
    obsSubID := 'FT4';
    obsValue := FloatToStrF(ConvertedValue(acaseRecord.FT4, T4_MOLAR_MASS,
      'mol/l', gReferenceRanges.FT4.UOM), ffNumber, 5, 2);
    Units := gReferenceRanges.FT4.UOM;
    RefRange := FloatToStr(gReferenceRanges.FT4.ln) + ' - ' +
      FloatToStr(gReferenceRanges.FT4.hn);
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
    respObs := '';
    observMethod := '';
    EquipInstID := '';
    AnalysisDateTime := '';
    SetOBX(HL7Message, SetID, ValueType, ObsID,
      obsSubID, obsValue, Units, RefRange,
      AbnormFlags, probability, Nature, status, RRDate,
      UDAC, ObsDateTime, prodID, respObs, observMethod,
      EquipInstID, AnalysisDateTime);

    inc(setIDcounter);
    SetID := IntToStr(setIDcounter);
    ValueType := 'NM';
    ObsID := 'FT3';
    obsSubID := 'FT3';
    obsValue := FloatToStrF(ConvertedValue(acaseRecord.FT3, T3_MOLAR_MASS,
      'mol/l', gReferenceRanges.FT3.UOM), ffNumber, 5, 2);
    Units := gReferenceRanges.FT3.UOM;;
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
    respObs := '';
    observMethod := '';
    EquipInstID := '';
    AnalysisDateTime := '';
    SetOBX(HL7Message, SetID, ValueType, ObsID,
      obsSubID, obsValue, Units, RefRange,
      AbnormFlags, probability, Nature, status, RRDate,
      UDAC, ObsDateTime, prodID, respObs, observMethod,
      EquipInstID, AnalysisDateTime);

    inc(setIDcounter);
    SetID := IntToStr(setIDcounter);
    ValueType := 'NM';
    ObsID := 'TT4';
    obsSubID := 'TT4';
    obsValue := FloatToStrF(ConvertedValue(acaseRecord.TT4, T4_MOLAR_MASS,
      'mol/l', gReferenceRanges.TT4.UOM), ffNumber, 5, 2);
    Units := gReferenceRanges.TT4.UOM;;
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
    respObs := '';
    observMethod := '';
    EquipInstID := '';
    AnalysisDateTime := '';
    SetOBX(HL7Message, SetID, ValueType, ObsID,
      obsSubID, obsValue, Units, RefRange,
      AbnormFlags, probability, Nature, status, RRDate,
      UDAC, ObsDateTime, prodID, respObs, observMethod,
      EquipInstID, AnalysisDateTime);

    inc(setIDcounter);
    SetID := IntToStr(setIDcounter);
    ValueType := 'NM';
    ObsID := 'TT3';
    obsSubID := 'TT3';
    obsValue := FloatToStrF(ConvertedValue(acaseRecord.TT3, T3_MOLAR_MASS,
      'mol/l', gReferenceRanges.TT3.UOM), ffNumber, 5, 2);
    Units := gReferenceRanges.TT3.UOM;;
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
    respObs := '';
    observMethod := '';
    EquipInstID := '';
    AnalysisDateTime := '';
    SetOBX(HL7Message, SetID, ValueType, ObsID,
      obsSubID, obsValue, Units, RefRange,
      AbnormFlags, probability, Nature, status, RRDate,
      UDAC, ObsDateTime, prodID, respObs, observMethod,
      EquipInstID, AnalysisDateTime);

    inc(setIDcounter);
    SetID := IntToStr(setIDcounter);
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
    if isNaN(aCaseRecord.OBDate) then
      ObsDateTime := ''
    else
      ObsDateTime := EncodedDateTime(aCaseRecord.OBDate);
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

    inc(setIDcounter);
    SetID := IntToStr(setIDcounter);
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
    if isNaN(aCaseRecord.OBDate) then
      ObsDateTime := ''
    else
      ObsDateTime := EncodedDateTime(aCaseRecord.OBDate);
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

    inc(setIDcounter);
    SetID := IntToStr(setIDcounter);
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
    if isNaN(aCaseRecord.OBDate) then
      ObsDateTime := ''
    else
      ObsDateTime := EncodedDateTime(aCaseRecord.OBDate);
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

    inc(setIDcounter);
    SetID := IntToStr(setIDcounter);
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
    if isNaN(aCaseRecord.OBDate) then
      ObsDateTime := ''
    else
      ObsDateTime := EncodedDateTime(aCaseRecord.OBDate);
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

    theSPM.SetID := '1';
    theSPM.AccessionID := '';
    theSPM.ContainerCondition := '';
    theSPM.ContainerType := '';
    theSPM.GroupedSpecimenCount := '';
    theSPM.NumberOfSpecimenContainers := '';
    theSPM.otherSpecimenID := '';
    theSPM.ShipmentID := '';
    theSPM.SpecimenAdditives := '';
    theSPM.SpecimenAppropriateness := '';
    theSPM.SpecimenAvailability := '';
    theSPM.SpecimenChildRole := '';
    theSPM.SpecimenCollectionAmount := '';
    if isNaN(aCaseRecord.OBDate) then
      theSPM.SpecimenCollectionDateTime := ''
    else
      theSPM.SpecimenCollectionDateTime := EncodedDateTime(aCaseRecord.OBDate);
    theSPM.SpecimenCollectionMethods := '';
    theSPM.SpecimenCollectionSite := '';
    theSPM.SpecimenCondition := '';
    theSPM.SpecimenCurrentQuality := '';
    theSPM.SpecimenDescription := '';
    theSPM.SpecimenExpirDateTime := '';
    theSPM.SpecimenHandlingCode := '';
    theSPM.SpecimenID := '';
    theSPM.SpecimenParentID := '';
    theSPM.SpecimenQuality := '';
    theSPM.SpecimenReceivedDateTime := '';
    theSPM.SpecimenRejectReason := '';
    theSPM.SpecimenRiskCode := '';
    theSPM.SpecimenRole := '';
    theSPM.SpecimenSourceSite := '';
    theSPM.SpecimenSourceSiteMod := '';
    theSPM.SpecimenType := '';
    theSPM.SpecimenTypeMod := '';
    SetSPM(Hl7Message, theSPM);

    WriteHL7File(HL7Message, SPINAToolbar.SaveResultsDialog.FileName);
  end;
end;

procedure SaveAsTextFile(aCaseRecord: tCaseRecord);
var
  theHeader, theString: ANSIString;
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
    theHeader := aCaseRecord.PID + '/' + aCaseRecord.CaseID + kCR + kLF +
      aCaseRecord.Name + ', ' + aCaseRecord.GivenNames + ' *' +
      DOBDateString + kCR + kLF +
      OBDateString + ' (' + aCaseRecord.Placer + ')'+
      kCR + kLF + kCR + kLF;
  theString := theHeader + gResultString;
  SaveStringToPath(theString, SPINAToolbar.SaveResultsDialog.FileName);
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
      1: SaveAsTextFile(caseRecord);
      2: SaveAsHL7file(caseRecord);
    end;
  end;
end;

end.
