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
  HL7, MSH, MSA, NTE, PID, PV1, OBR, OBX, SPM, UnitConverter,
  SPINATypes, SPINA_Engine, CaseBroker, EnvironmentInfo;

type
  TFileType = (plainTextFile, HL7Message);

  TLOINCRecord = record
    code, short, long: string;
  end;

const
  ORU_R01_variant1 = 'ORU^R01'; // Unsolicited transmission of an observation
  ORU_R01_variant2 = 'ORU^R01^ORU_R01';
  ORU_R04 = 'ORU^R04'; //Response to query
  ACK_R01 = 'ACK^R01^ACK';
  MDM_T01 = 'MDM^T01';
  OUL_R21 = 'OUL^R21'; // Unsolicited laboratory observation
  OUL_R22 = 'OUL^R22'; // specimen oriented observation
  OUL_R23 = 'OUL^R23'; // specimen container oriented observation
  OUL_R24 = 'OUL^R24'; // (order oriented observation
  NND_ORU_V2_0_profile = 'NND_ORU_v2.0^PHINProfileID^2.16.840.1.114222.4.10.3^ISO~' +
    'Gen_Case_Map_v1.0^PHINMsgMapID^2.16.840.1.114222.4.10.4^ISO';
  NA_DTM = '00000000000000';

  LOINC_GLUC_1: TLoincRecord = (code: '101476-0'; short: 'Glucose p fast BldV-sCnc';
    long: 'Fasting glucose [Moles/volume] in Venous blood');
  LOINC_GLUC_2: TLoincRecord = (code: '1557-8'; short: 'Glucose p fast BldV-mCnc';
    long: 'Fasting glucose [Mass/volume] in Venous blood');
  LOINC_GLUC_3: TLoincRecord = (code: '14770-2';
    short: 'Glucose p fast BldC Glucomtr-sCnc';
    long: 'Fasting glucose [Moles/volume] in Capillary blood by Glucometer');
  LOINC_GLUC_4: TLoincRecord = (code: '41604-0';
    short: 'Glucose p fast BldC Glucomtr-mCnc';
    long: 'Fasting glucose [Mass/volume] in Capillary blood by Glucometer');
  LOINC_GLUC_5: TLoincRecord = (code: '14771-0'; short: 'Glucose p fast SerPl-sCnc';
    long: 'Fasting glucose [Moles/volume] in Serum or Plasma');
  LOINC_GLUC_6: TLoincRecord = (code: '1558-6'; short: 'Glucose p fast SerPl-mCnc';
    long: 'Fasting glucose [Mass/volume] in Serum or Plasma');
  LOINC_GLUC_7: TLoincRecord = (code: '1556-0'; short: 'Glucose p fast BldC-mCnc';
    long: 'Fasting glucose [Mass/volume] in Capillary blood');
  LOINC_GLUC_8: TLoincRecord = (code: '76629-5'; short: 'Glucose p fast Bld-sCnc';
    long: 'Fasting glucose [Moles/volume] in Blood');
  LOINC_GLUC_9: TLoincRecord = (code: '77145-1'; short: 'Glucose p fast SerPlBld-sCnc';
    long: 'Fasting glucose [Moles/volume] in Serum; Plasma or Blood');

  LOINC_INSU_1: TLoincRecord = (code: '59179-2'; short: 'Insulin p fast SerPl-sCnc';
    long: 'Insulin [Moles/volume] in Serum or Plasma --fasting');
  LOINC_INSU_2: TLoincRecord = (code: '27873-9'; short: 'Insulin p fast SerPl-aCnc';
    long: 'Insulin [Units/volume] in Serum or Plasma --fasting');
  LOINC_INSU_3: TLoincRecord = (code: '14293-5'; short: 'Insulin BS SerPl-sCnc';
    long: 'Insulin [Moles/volume] in Serum or Plasma --baseline');
  LOINC_INSU_4: TLoincRecord = (code: '1570-1'; short: 'Insulin BS SerPl-mCnc';
    long: 'Insulin [Mass/volume] in Serum or Plasma --baseline');
  LOINC_INSU_5: TLoincRecord = (code: '56482-3'; short: 'Insulin BS SerPl-aCnc';
    long: 'Insulin [Units/volume] in Serum or Plasma --baseline');
  LOINC_INSU_6: TLoincRecord = (code: '14796-7'; short: 'Insulin SerPl-sCnc';
    long: 'Insulin [Moles/volume] in Serum or Plasma');
  LOINC_INSU_7: TLoincRecord = (code: '3695-4'; short: 'Insulin SerPl-mCnc';
    long: 'Insulin [Mass/volume] in Serum or Plasma');
  LOINC_INSU_8: TLoincRecord = (code: '20448-7'; short: 'Insulin SerPl-aCnc';
    long: 'Insulin [Units/volume] in Serum or Plasma');

  LOINC_CPEP_1: TLoincRecord = (code: '42180-0'; short: 'C peptide p fast SerPl-sCnc';
    long: 'C peptide [Moles/volume] in Serum or Plasma --fasting');
  LOINC_CPEP_2: TLoincRecord = (code: '13037-7'; short: 'C peptide p fast SerPl-mCnc';
    long: 'C peptide [Mass/volume] in Serum or Plasma --fasting');

  LOINC_LABEL = 'LN';

procedure OpenCaseRecord(var caseRecord: tCaseRecord; filePath: string;
  fileType: TFileType);
procedure SaveCaseRecord(caseRecord: tCaseRecord; filePath: string; fileType: TFileType);

implementation

procedure SaveStringToPath(theString, filePath: string);
var
  textFile: TFileStream = nil;
  textLength: integer;
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

procedure SaveAsHL7Message(aCaseRecord: tCaseRecord; filePath: string);
var
  oldSeparator: char;
  HL7Message: THL7Message;
  theMSH: tMSH;
  theOBR: tOBR;
  theOBX: tOBX;
  thePID: tPID;
  thePV1: tPV1;
  theSPM: tSPM;
  theNTE: tNTE;
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
    theMSH.delimiters := STANDARD_DELIMITERS;
    theMSH.sendingApp := 'SPINA Carb';
    theMSH.sendingFac := gPreferences.MSH_ID;
    theMSH.receivingApp := '';
    theMSH.receivingFac := '';
    theMSH.dateTime := EncodedDateTime(Now);
    theMSH.messageType := ORU_R01_variant1;
    theMSH.security := '';
    theMSH.controlID := EncodedDateTime(Now) + IntToStr(random(13000));
    theMSH.processingID := 'P^A';
    theMSH.versionID := ''; // ignored; will be filled-in automatically by PUMA
    theMSH.sequenceNumber := '';
    theMSH.continuationPointer := '';
    theMSH.AccAckType := '';
    theMSH.AppAckType := '';
    theMSH.countryCode := '';
    theMSH.charSet := '';
    theMSH.messageLanguage := '';
    theMSH.altCharHandlScheme := '';
    theMSH.profileID := ORU_R01_variant2;
    SetMSH(HL7Message, theMSH, True);

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
    theOBR.SetID := '1';
    theOBR.PlacOrdNumb := '';
    theOBR.FillOrdNumb := EncodedDateTime(Now) + IntToStr(random(13000));
    theOBR.USI := 'SPINA Carb';
    theOBR.Priority := '';
    theOBR.ReqDateTime := EncodedDateTime(Now);
    theOBR.ObsDateTime := NA_DTM;
    theOBR.ObsEndDateTime := '';
    SetOBR(HL7Message, theOBR);

    ClearNTE(theNTE);
    theNTE.SetID := '1';
    theNTE.CommentSource := 'O';
    theNTE.comment := 'Data source: SPINA Carb ' + FileVersion;
    SetNTE(HL7Message, theNTE);

    ClearNTE(theNTE);
    theNTE.SetID := '2';
    theNTE.CommentSource := 'O';
    SetNTE(HL7Message, theNTE);

    ClearNTE(theNTE);
    theNTE.SetID := '3';
    theNTE.CommentSource := 'O';
    theNTE.comment := aCaseRecord.Comment;
    SetNTE(HL7Message, theNTE);

    setIDcounter := 1;         // Glucose
    ClearOBX(theOBX);
    theOBX.SetID := IntToStr(setIDcounter);
    theOBX.ValueType := 'NM';
    if gPreferences.exportLOINC then
    begin
      if pos('mol', gPreferences.ReferenceValues.Glucose.UoM) > 0 then  // SI unit
        theOBX.ObsID := LOINC_GLUC_1.code +
          HL7Message.Delimiters.ComponentSeparator + 'Glucose' +
          HL7Message.Delimiters.ComponentSeparator + LOINC_LABEL
      else
        theOBX.ObsID := LOINC_GLUC_2.code +
          HL7Message.Delimiters.ComponentSeparator + 'Glucose' +
          HL7Message.Delimiters.ComponentSeparator + LOINC_LABEL;
    end
    else
      theOBX.ObsID := 'Glucose';
    theOBX.obsSubID := '1';
    theOBX.obsValue := FloatToStrF(ConvertedValue(aCaseRecord.LabRecord.Glucose,
      kGlucoseMolarMass, 'mmol/l', gPreferences.ReferenceValues.Glucose.UoM),
      ffNumber, 5, 13);
    theOBX.Units := gPreferences.ReferenceValues.Glucose.UoM;
    theOBX.RefRange := FloatToStr(gPreferences.ReferenceValues.Glucose.ln) +
      ' - ' + FloatToStr(gPreferences.ReferenceValues.Glucose.hn);
    theOBX.AbnormFlags := '';
    theOBX.probability := '';
    theOBX.Nature := '';
    theOBX.status := 'F';
    theOBX.RRDate := '';
    theOBX.UDAC := '';
    if isNaN(aCaseRecord.OBDate) then
      theOBX.ObsDateTime := ''
    else
      theOBX.ObsDateTime := EncodedDateTime(aCaseRecord.OBDate);
    theOBX.prodID := '';
    theOBX.respObs := '';
    theOBX.observMethod := '';
    theOBX.EquipInstID := '';
    theOBX.AnalysisDateTime := '';
    SetOBX(HL7Message, theOBX);

    Inc(setIDcounter);         // Insulin
    ClearOBX(theOBX);
    theOBX.SetID := IntToStr(setIDcounter);
    theOBX.ValueType := 'NM';
    if gPreferences.exportLOINC then
    begin
      if pos('mol', gPreferences.ReferenceValues.Insulin.UoM) > 0 then  // SI unit
        theOBX.ObsID := LOINC_INSU_1.code +
          HL7Message.Delimiters.ComponentSeparator + 'Insulin' +
          HL7Message.Delimiters.ComponentSeparator + LOINC_LABEL
      else
        theOBX.ObsID := LOINC_INSU_2.code +
          HL7Message.Delimiters.ComponentSeparator + 'Insulin' +
          HL7Message.Delimiters.ComponentSeparator + LOINC_LABEL;
    end
    else
      theOBX.ObsID := 'Insulin';
    theOBX.obsSubID := '1';
    theOBX.obsValue := FloatToStrF(ConvertedValue(aCaseRecord.LabRecord.Insulin,
      kInsulinConversionFactor, 'pmol/l', gPreferences.ReferenceValues.Insulin.UoM),
      ffNumber, 5, 13);
    theOBX.Units := gPreferences.ReferenceValues.Insulin.UoM;
    theOBX.RefRange := FloatToStr(gPreferences.ReferenceValues.Insulin.ln) +
      ' - ' + FloatToStr(gPreferences.ReferenceValues.Insulin.hn);
    theOBX.AbnormFlags := '';
    theOBX.probability := '';
    theOBX.Nature := '';
    theOBX.status := 'F';
    theOBX.RRDate := '';
    theOBX.UDAC := '';
    if isNaN(aCaseRecord.OBDate) then
      theOBX.ObsDateTime := ''
    else
      theOBX.ObsDateTime := EncodedDateTime(aCaseRecord.OBDate);
    theOBX.prodID := '';
    theOBX.respObs := '';
    theOBX.observMethod := '';
    theOBX.EquipInstID := '';
    theOBX.AnalysisDateTime := '';
    SetOBX(HL7Message, theOBX);

    Inc(setIDcounter);         // C-Peptide
    ClearOBX(theOBX);
    theOBX.SetID := IntToStr(setIDcounter);
    theOBX.ValueType := 'NM';
    if gPreferences.exportLOINC then
    begin
      if pos('mol', gPreferences.ReferenceValues.CPeptide.UoM) > 0 then  // SI unit
        theOBX.ObsID := LOINC_CPEP_1.code +
          HL7Message.Delimiters.ComponentSeparator + 'C-Peptide' +
          HL7Message.Delimiters.ComponentSeparator + LOINC_LABEL
      else
        theOBX.ObsID := LOINC_CPEP_2.code +
          HL7Message.Delimiters.ComponentSeparator + 'C-Peptide' +
          HL7Message.Delimiters.ComponentSeparator + LOINC_LABEL;
    end
    else
      theOBX.ObsID := 'C-Peptide';
    theOBX.obsSubID := '1';
    theOBX.obsValue := FloatToStrF(ConvertedValue(aCaseRecord.LabRecord.CPeptide,
      kCPeptideMolarMass, 'nmol/l', gPreferences.ReferenceValues.CPeptide.UoM),
      ffNumber, 5, 13);
    theOBX.Units := gPreferences.ReferenceValues.CPeptide.UoM;
    theOBX.RefRange := FloatToStr(gPreferences.ReferenceValues.CPeptide.ln) +
      ' – ' + FloatToStr(gPreferences.ReferenceValues.CPeptide.hn);
    theOBX.AbnormFlags := '';
    theOBX.probability := '';
    theOBX.Nature := '';
    theOBX.status := 'F';
    theOBX.RRDate := '';
    theOBX.UDAC := '';
    if isNaN(aCaseRecord.OBDate) then
      theOBX.ObsDateTime := ''
    else
      theOBX.ObsDateTime := EncodedDateTime(aCaseRecord.OBDate);
    theOBX.prodID := '';
    theOBX.respObs := '';
    theOBX.observMethod := '';
    theOBX.EquipInstID := '';
    theOBX.AnalysisDateTime := '';
    SetOBX(HL7Message, theOBX);

    Inc(setIDcounter);         // SPINA-GBeta
    ClearOBX(theOBX);
    theOBX.SetID := IntToStr(setIDcounter);
    theOBX.ValueType := 'NM';
    theOBX.ObsID := 'SPINA-GBeta';
    theOBX.obsSubID := '1';
    theOBX.obsValue := FloatToStrF(aCaseRecord.LabRecord.SPINA_GBeta, ffNumber, 5, 13);
    theOBX.Units := 'pmol/s';
    theOBX.RefRange := FloatToStr(gPreferences.ReferenceValues.SPINA_GBeta.ln) +
      ' – ' + FloatToStr(gPreferences.ReferenceValues.SPINA_GBeta.hn);
    theOBX.AbnormFlags := '';
    theOBX.probability := '';
    theOBX.Nature := '';
    theOBX.status := 'F';
    theOBX.RRDate := '';
    theOBX.UDAC := '';
    if isNaN(aCaseRecord.OBDate) then
      theOBX.ObsDateTime := ''
    else
      theOBX.ObsDateTime := EncodedDateTime(aCaseRecord.OBDate);
    theOBX.prodID := '';
    theOBX.respObs := '';
    theOBX.observMethod := '';
    theOBX.EquipInstID := '';
    theOBX.AnalysisDateTime := '';
    SetOBX(HL7Message, theOBX);

    Inc(setIDcounter);         // SPINA-GR
    ClearOBX(theOBX);
    theOBX.SetID := IntToStr(setIDcounter);
    theOBX.ValueType := 'NM';
    theOBX.ObsID := 'SPINA-GR';
    theOBX.obsSubID := '1';
    theOBX.obsValue := FloatToStrF(aCaseRecord.LabRecord.SPINA_GR, ffNumber, 5, 13);
    theOBX.Units := 'mol/s';
    theOBX.RefRange := FloatToStr(gPreferences.ReferenceValues.SPINA_GR.ln) +
      ' – ' + FloatToStr(gPreferences.ReferenceValues.SPINA_GR.hn);
    theOBX.AbnormFlags := '';
    theOBX.probability := '';
    theOBX.Nature := '';
    theOBX.status := 'F';
    theOBX.RRDate := '';
    theOBX.UDAC := '';
    if isNaN(aCaseRecord.OBDate) then
      theOBX.ObsDateTime := ''
    else
      theOBX.ObsDateTime := EncodedDateTime(aCaseRecord.OBDate);
    theOBX.prodID := '';
    theOBX.respObs := '';
    theOBX.observMethod := '';
    theOBX.EquipInstID := '';
    theOBX.AnalysisDateTime := '';
    SetOBX(HL7Message, theOBX);

    Inc(setIDcounter);         // SPINA-DI
    ClearOBX(theOBX);
    theOBX.SetID := IntToStr(setIDcounter);
    theOBX.ValueType := 'NM';
    theOBX.ObsID := 'SPINA-DI';
    theOBX.obsSubID := '1';
    theOBX.obsValue := FloatToStrF(aCaseRecord.LabRecord.SPINA_DI, ffNumber, 5, 13);
    theOBX.Units := 'mol/s';
    theOBX.RefRange := FloatToStr(gPreferences.ReferenceValues.SPINA_DI.ln) +
      ' – ' + FloatToStr(gPreferences.ReferenceValues.SPINA_DI.hn);
    theOBX.AbnormFlags := '';
    theOBX.probability := '';
    theOBX.Nature := '';
    theOBX.status := 'F';
    theOBX.RRDate := '';
    theOBX.UDAC := '';
    if isNaN(aCaseRecord.OBDate) then
      theOBX.ObsDateTime := ''
    else
      theOBX.ObsDateTime := EncodedDateTime(aCaseRecord.OBDate);
    theOBX.prodID := '';
    theOBX.respObs := '';
    theOBX.observMethod := '';
    theOBX.EquipInstID := '';
    theOBX.AnalysisDateTime := '';
    SetOBX(HL7Message, theOBX);

    Inc(setIDcounter);         // HOMA-Beta
    ClearOBX(theOBX);
    theOBX.SetID := IntToStr(setIDcounter);
    theOBX.ValueType := 'NM';
    theOBX.ObsID := 'HOMA-Beta';
    theOBX.obsSubID := '1';
    theOBX.obsValue := FloatToStrF(aCaseRecord.LabRecord.HOMA_Beta, ffNumber, 5, 13);
    theOBX.Units := '%';
    theOBX.RefRange := FloatToStr(gPreferences.ReferenceValues.HOMA_Beta.ln) +
      ' – ' + FloatToStr(gPreferences.ReferenceValues.HOMA_Beta.hn);
    theOBX.AbnormFlags := '';
    theOBX.probability := '';
    theOBX.Nature := '';
    theOBX.status := 'F';
    theOBX.RRDate := '';
    theOBX.UDAC := '';
    if isNaN(aCaseRecord.OBDate) then
      theOBX.ObsDateTime := ''
    else
      theOBX.ObsDateTime := EncodedDateTime(aCaseRecord.OBDate);
    theOBX.prodID := '';
    theOBX.respObs := '';
    theOBX.observMethod := '';
    theOBX.EquipInstID := '';
    theOBX.AnalysisDateTime := '';
    SetOBX(HL7Message, theOBX);

    Inc(setIDcounter);         // HOMA-IR
    ClearOBX(theOBX);
    theOBX.SetID := IntToStr(setIDcounter);
    theOBX.ValueType := 'NM';
    theOBX.ObsID := 'HOMA-IR';
    theOBX.obsSubID := '1';
    theOBX.obsValue := FloatToStrF(aCaseRecord.LabRecord.HOMA_IR, ffNumber, 5, 13);
    theOBX.Units := '';
    theOBX.RefRange := FloatToStr(gPreferences.ReferenceValues.HOMA_IR.ln) +
      ' – ' + FloatToStr(gPreferences.ReferenceValues.HOMA_IR.hn);
    theOBX.AbnormFlags := '';
    theOBX.probability := '';
    theOBX.Nature := '';
    theOBX.status := 'F';
    theOBX.RRDate := '';
    theOBX.UDAC := '';
    if isNaN(aCaseRecord.OBDate) then
      theOBX.ObsDateTime := ''
    else
      theOBX.ObsDateTime := EncodedDateTime(aCaseRecord.OBDate);
    theOBX.prodID := '';
    theOBX.respObs := '';
    theOBX.observMethod := '';
    theOBX.EquipInstID := '';
    theOBX.AnalysisDateTime := '';
    SetOBX(HL7Message, theOBX);

    Inc(setIDcounter);         // HOMA-IS
    ClearOBX(theOBX);
    theOBX.SetID := IntToStr(setIDcounter);
    theOBX.ValueType := 'NM';
    theOBX.ObsID := 'HOMA-IS';
    theOBX.obsSubID := '1';
    theOBX.obsValue := FloatToStrF(aCaseRecord.LabRecord.HOMA_IS, ffNumber, 5, 13);
    theOBX.Units := '';
    theOBX.RefRange := FloatToStr(gPreferences.ReferenceValues.HOMA_IS.ln) +
      ' – ' + FloatToStr(gPreferences.ReferenceValues.HOMA_IS.hn);
    theOBX.AbnormFlags := '';
    theOBX.probability := '';
    theOBX.Nature := '';
    theOBX.status := 'F';
    theOBX.RRDate := '';
    theOBX.UDAC := '';
    if isNaN(aCaseRecord.OBDate) then
      theOBX.ObsDateTime := ''
    else
      theOBX.ObsDateTime := EncodedDateTime(aCaseRecord.OBDate);
    theOBX.prodID := '';
    theOBX.respObs := '';
    theOBX.observMethod := '';
    theOBX.EquipInstID := '';
    theOBX.AnalysisDateTime := '';
    SetOBX(HL7Message, theOBX);

    Inc(setIDcounter);         // QUICKI
    ClearOBX(theOBX);
    theOBX.SetID := IntToStr(setIDcounter);
    theOBX.ValueType := 'NM';
    theOBX.ObsID := 'QUICKI';
    theOBX.obsSubID := '1';
    theOBX.obsValue := FloatToStrF(aCaseRecord.LabRecord.QUICKI, ffNumber, 5, 13);
    theOBX.Units := '';
    theOBX.RefRange := FloatToStr(gPreferences.ReferenceValues.QUICKI.ln) +
      ' – ' + FloatToStr(gPreferences.ReferenceValues.QUICKI.hn);
    theOBX.AbnormFlags := '';
    theOBX.probability := '';
    theOBX.Nature := '';
    theOBX.status := 'F';
    theOBX.RRDate := '';
    theOBX.UDAC := '';
    if isNaN(aCaseRecord.OBDate) then
      theOBX.ObsDateTime := ''
    else
      theOBX.ObsDateTime := EncodedDateTime(aCaseRecord.OBDate);
    theOBX.prodID := '';
    theOBX.respObs := '';
    theOBX.observMethod := '';
    theOBX.EquipInstID := '';
    theOBX.AnalysisDateTime := '';
    SetOBX(HL7Message, theOBX);

    Inc(setIDcounter);         // AIGR
    ClearOBX(theOBX);
    theOBX.SetID := IntToStr(setIDcounter);
    theOBX.ValueType := 'NM';
    theOBX.ObsID := 'AIGR';
    theOBX.obsSubID := '1';
    theOBX.obsValue := FloatToStrF(aCaseRecord.LabRecord.AIGR, ffNumber, 5, 13);
    theOBX.Units := 'pmol/mmol';
    theOBX.RefRange := FloatToStr(gPreferences.ReferenceValues.AIGR.ln) +
      ' – ' + FloatToStr(gPreferences.ReferenceValues.AIGR.hn);
    theOBX.AbnormFlags := '';
    theOBX.probability := '';
    theOBX.Nature := '';
    theOBX.status := 'F';
    theOBX.RRDate := '';
    theOBX.UDAC := '';
    if isNaN(aCaseRecord.OBDate) then
      theOBX.ObsDateTime := ''
    else
      theOBX.ObsDateTime := EncodedDateTime(aCaseRecord.OBDate);
    theOBX.prodID := '';
    theOBX.respObs := '';
    theOBX.observMethod := '';
    theOBX.EquipInstID := '';
    theOBX.AnalysisDateTime := '';
    SetOBX(HL7Message, theOBX);

    Inc(setIDcounter);         // CGR
    ClearOBX(theOBX);
    theOBX.SetID := IntToStr(setIDcounter);
    theOBX.ValueType := 'NM';
    theOBX.ObsID := 'CGR';
    theOBX.obsSubID := '1';
    theOBX.obsValue := FloatToStrF(aCaseRecord.LabRecord.CGR, ffNumber, 5, 13);
    theOBX.Units := '';
    theOBX.RefRange := FloatToStr(gPreferences.ReferenceValues.CGR.ln) +
      ' – ' + FloatToStr(gPreferences.ReferenceValues.CGR.hn);
    theOBX.AbnormFlags := '';
    theOBX.probability := '';
    theOBX.Nature := '';
    theOBX.status := 'F';
    theOBX.RRDate := '';
    theOBX.UDAC := '';
    if isNaN(aCaseRecord.OBDate) then
      theOBX.ObsDateTime := ''
    else
      theOBX.ObsDateTime := EncodedDateTime(aCaseRecord.OBDate);
    theOBX.prodID := '';
    theOBX.respObs := '';
    theOBX.observMethod := '';
    theOBX.EquipInstID := '';
    theOBX.AnalysisDateTime := '';
    SetOBX(HL7Message, theOBX);

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

procedure SaveAsTextFile(aCaseRecord: tCaseRecord; filePath: string);
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
    theHeader := aCaseRecord.PID + '/' + aCaseRecord.CaseID +
      LineEnding + kLF + aCaseRecord.Name + ', ' + aCaseRecord.GivenNames +
      ' *' + DOBDateString + LineEnding + OBDateString + ' (' +
      aCaseRecord.Placer + ')' + LineEnding + LineEnding;
  theString := theHeader + aCaseRecord.CombMessage;
  SaveStringToPath(theString, filePath);
end;

procedure ReadHL7Message(theFile: string; var aCaseRecord: tCaseRecord);
var
  oldSeparator: char;
  theHL7Message: THL7Message;
  theSegment: THL7Segment;
  theMSHRecord: tMSH;
  thePIDRecord: tPID;
  thePV1Record: tPV1;
  theOBXRecord: tOBX;
  theNTERecord: tNTE;
  theField: THL7Field;
  theComponent: THL7Component;
  theSubComponent, nextSubComponent: THL7SubComponent;
begin
  assert(theFile <> '');
  oldSeparator := DefaultFormatSettings.DecimalSeparator;
  DefaultFormatSettings.DecimalSeparator := DEC_POINT;
  NewCaseRecord(aCaseRecord);
  ReadHL7File(theHL7Message, theFile);
  assert(assigned(theHL7Message));
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
      GetPID(theHL7Message, thePIDRecord);
      aCaseRecord.DoBDate := DecodeDateTime(thePIDRecord.BirthDateTime);
      theField := THL7Field.Create(nil, thePIDRecord.PatientIDList);
      theComponent := theField.FirstComponent;
      if theComponent <> nil then
      begin
        aCaseRecord.PID := theComponent.contentString;
        theComponent := theComponent.nextSibling;
        if theComponent <> nil then
          aCaseRecord.CaseID := theComponent.contentString;
      end;
      theField.Destroy;
      theField := THL7Field.Create(nil, thePIDRecord.PatientName);
      theComponent := theField.FirstComponent;
      if theComponent <> nil then
      begin
        aCaseRecord.Name := theComponent.contentString;
        theComponent := theComponent.nextSibling;
        if theComponent <> nil then
          aCaseRecord.GivenNames := theComponent.contentString;
      end;
    end;
    if theSegment.segmentType = 'PV1' then
    begin
      GetPV1(theHL7Message, thePV1Record);
      aCaseRecord.Placer := thePV1Record.AssignedPatientLocation;
    end;
    if theSegment.segmentType = 'NTE' then
    begin
      theField := nil;
      theComponent := nil;
      theSubComponent := nil;
      nextSubComponent := nil;
      GetNTE(theSegment, theNTERecord);
      if theNTERecord.SetID = '3' then
      begin
        aCaseRecord.Comment := theNTERecord.comment;
      end;
    end;
    if theSegment.segmentType = 'OBX' then
    begin
      GetOBX(theSegment, theOBXRecord);

      if pos('Glucose', theOBXRecord.ObsID) > 0 then
        { #todo : Insert code for LOINC here }
      begin
        aCaseRecord.LabRecord.Glucose :=
          ConvertedValue(StrToFloatDef(theOBXRecord.obsValue, NaN),
          kGlucoseMolarMass, theOBXRecord.Units,
          gPreferences.ReferenceValues.Glucose.UoM);
      end;

      if pos('Insulin', theOBXRecord.ObsID) > 0 then
        { #todo : Insert code for LOINC here }
      begin
        aCaseRecord.LabRecord.Insulin :=
          ConvertedValue(StrToFloatDef(theOBXRecord.obsValue, NaN),
          kInsulinConversionFactor, theOBXRecord.Units, gPreferences.ReferenceValues.Insulin.UoM);
      end;

      if pos('C-Peptide', theOBXRecord.ObsID) > 0 then
        { #todo : Insert code for LOINC here }
      begin
        aCaseRecord.LabRecord.CPeptide :=
          ConvertedValue(StrToFloatDef(theOBXRecord.obsValue, NaN), kCPeptideMolarMass,
          theOBXRecord.Units, gPreferences.ReferenceValues.CPeptide.UoM);
      end;

      if pos('SPINA-GBeta', theOBXRecord.ObsID) > 0 then
        { #todo : Insert code for LOINC here }
      begin
        aCaseRecord.LabRecord.SPINA_GBeta := StrToFloatDef(theOBXRecord.obsValue, NaN);
      end;

      if pos('SPINA-GR', theOBXRecord.ObsID) > 0 then
        { #todo : Insert code for LOINC here }
      begin
        aCaseRecord.LabRecord.SPINA_GR := StrToFloatDef(theOBXRecord.obsValue, NaN);
      end;

      if pos('SPINA-DI', theOBXRecord.ObsID) > 0 then
        { #todo : Insert code for LOINC here }
      begin
        aCaseRecord.LabRecord.SPINA_DI := StrToFloatDef(theOBXRecord.obsValue, NaN);
      end;

      if pos('HOMA-Beta', theOBXRecord.ObsID) > 0 then
        { #todo : Insert code for LOINC here }
      begin
        aCaseRecord.LabRecord.HOMA_Beta := StrToFloatDef(theOBXRecord.obsValue, NaN);
      end;

      if pos('HOMA-IR', theOBXRecord.ObsID) > 0 then
        { #todo : Insert code for LOINC here }
      begin
        aCaseRecord.LabRecord.HOMA_IR := StrToFloatDef(theOBXRecord.obsValue, NaN);
      end;

      if pos('HOMA-IS', theOBXRecord.ObsID) > 0 then
        { #todo : Insert code for LOINC here }
      begin
        aCaseRecord.LabRecord.HOMA_IS := StrToFloatDef(theOBXRecord.obsValue, NaN);
      end;

      if pos('QUICKI', theOBXRecord.ObsID) > 0 then
        { #todo : Insert code for LOINC here }
      begin
        aCaseRecord.LabRecord.QUICKI := StrToFloatDef(theOBXRecord.obsValue, NaN);
      end;

      if pos('AIGR', theOBXRecord.ObsID) > 0 then
        { #todo : Insert code for LOINC here }
      begin
        aCaseRecord.LabRecord.AIGR := StrToFloatDef(theOBXRecord.obsValue, NaN);
      end;

      if pos('CGR', theOBXRecord.ObsID) > 0 then
        { #todo : Insert code for LOINC here }
      begin
        aCaseRecord.LabRecord.CGR := StrToFloatDef(theOBXRecord.obsValue, NaN);
      end;

    end;
    theSegment := theSegment.nextSibling;
  end;

  DefaultFormatSettings.DecimalSeparator := oldSeparator;
end;

procedure OpenCaseRecord(var caseRecord: tCaseRecord; filePath: string;
  fileType: TFileType);
begin
  case fileType of
    HL7Message: ReadHL7Message(filePath, caseRecord);
  end;

end;

procedure SaveCaseRecord(caseRecord: tCaseRecord; filePath: string; fileType: TFileType);
begin
  case fileType of
    HL7Message: SaveAsHL7Message(caseRecord, filePath);
    plainTextFile: SaveAsTextFile(caseRecord, filePath);
  end;
end;

end.
