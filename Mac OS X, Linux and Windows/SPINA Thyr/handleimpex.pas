unit HandleImpEx;

{ SPINA-Thyr }

{ Application for calculating structure parameters }
{ of thyrotropic feedback control }

{ Programm zur Berechnung von Strukturparametern }
{ des thyreotropen Regelkreises }

{ Version 4.2.1 (Kontinuum) }

{ (c) J. W. Dietrich, 1994 - 2024 }
{ (c) Ludwig Maximilian University of Munich 1995 - 2002 }
{ (c) University of Ulm Hospitals 2002 - 2004 }
{ (c) Ruhr University of Bochum 2005 - 2024 }

{ This unit handles import and export of laboratory results and calculations }

{ Source code released under the BSD License }
{ See http://spina.medical-cybernetics.de for details }

{$mode objfpc}
{$H+}
//{$J}
{$ASSERTIONS ON}

interface

uses
  Classes, SysUtils, Dialogs, LCLVersion, Math, FileUtil,
  SPINA_Engine, SPINA_Types, UnitConverter, EnvironmentInfo,
  HL7, MSH, MSA, NTE, PID, PV1, OBR, OBX, SPM;

type
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
  LT4_CODE   = 'L-T4';    // substitution therapy with L-thyroxine
  LT3_CODE   = 'L-T3';    // substitution therapy with liothyronine
  RHTSH_CODE = 'rh-TSH';
  LOINC_TSH_1: TLoincRecord = (code: '3016-3'; short: 'TSH SerPl-aCnc';
    long: 'Thyrotropin [Units/volume] in Serum or Plasma'); // 1st gen assays
  LOINC_TSH_2: TLoincRecord = (code: '11579-0'; short: 'TSH SerPl DL<=0.005 mU/L aCnc';
    long: 'Thyrotropin [Units/volume] in Serum or Plasma by Detection limit <= 0.05 mIU/L');
  LOINC_TSH_3: TLoincRecord = (code: '11580-8'; short: 'TSH SerPl DL<=0.005 mU/L aCnc';
    long: 'Thyrotropin [Units/volume] in Serum or Plasma by Detection limit <= 0.005 mU/L');
  LOINC_FT4_1: TLoincRecord = (code: '3024-7'; short: 'T4 Free SerPl-mCnc';
    long: 'Thyroxine (T4) free [Mass/volume] in Serum or Plasma');
  LOINC_FT4_2: TLoincRecord = (code: '14920-3'; short: 'Free T4 SerPl-sCnc';
    long: 'Thyroxine (T4) free [Moles/volume] in Serum or Plasma');
  LOINC_TT4_1: TLoincRecord = (code: '3026-2'; short: 'T4 SerPl-mCnc';
    long: 'Thyroxine (T4) [Mass/volume] in Serum or Plasma');
  LOINC_TT4_2: TLoincRecord = (code: '14921-1'; short: 'T4 SerPl-sCnc';
    long: 'Thyroxine (T4) [Moles/volume] in Serum or Plasma');
  LOINC_FT3_1: TLoincRecord = (code: '3051-0'; short: 'T3Free SerPl-mCnc';
    long: 'Triiodothyronine (T3) Free [Mass/volume] in Serum or Plasma');
  LOINC_FT3_2: TLoincRecord = (code: '14928-6'; short: 'T3Free SerPl-sCnc';
    long: 'Triiodothyronine (T3) Free [Moles/volume] in Serum or Plasma');
  LOINC_TT3_1: TLoincRecord = (code: '3053-6'; short: 'T3 SerPl-mCnc';
    long: 'Triiodothyronine (T3) [Mass/volume] in Serum or Plasma');
  LOINC_TT3_2: TLoincRecord = (code: '14930-2'; short: 'T3 SerPl-sCnc';
    long: 'Triiodothyronine (T3) [Moles/volume] in Serum or Plasma');
  LOINC_SPINA_GT: TLoincRecord = (code: '82368-2'; short: 'Thyr secr cap SerPl Calc SPINA-sRate';
    long: 'Thyroid secretory capacity [Moles/time] in Serum or Plasma by Calculated.SPINA');
  LOINC_SPINA_GD: TLoincRecord = (code: '82367-4'; short: 'Per deiod act SerPl Calc SPINA-sRate';
    long: 'Peripheral deiodinase activity [Moles/time] in Serum or Plasma by Calculated.SPINA');
  LOINC_LABEL = 'LN';

procedure ReadHL7Message(theFile: string; var aCaseRecord: tCaseRecord);
procedure OpenCaseResults(var caseRecord: tCaseRecord);
procedure SaveResults(caseRecord: tCaseRecord);

implementation

uses spina_toolbar;

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

procedure SaveAsHL7Message(aCaseRecord: tCaseRecord);
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
    theMSH.sendingApp   := 'SPINA Thyr';
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
    theOBR.USI      := 'SPINA Thyr';
    theOBR.Priority := '';
    theOBR.ReqDateTime := EncodedDateTime(Now);
    theOBR.ObsDateTime := NA_DTM;
    theOBR.ObsEndDateTime := '';
    SetOBR(HL7Message, theOBR);

    ClearNTE(theNTE);
    theNTE.SetID   := '1';
    theNTE.CommentSource := 'O';
    theNTE.comment := 'Data source: SPINA Thyr ' + FileVersion;
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

    ClearNTE(theNTE);
    theNTE.SetID   := '3';
    theNTE.CommentSource := 'O';
    theNTE.comment := aCaseRecord.Comment;
    SetNTE(HL7Message, theNTE);

    setIDcounter := 1;

    ClearOBX(theOBX);
    theOBX.SetID     := IntToStr(setIDcounter);
    theOBX.ValueType := 'NM';
    if gPreferences.exportLOINC then
      theOBX.ObsID        := LOINC_TSH_3.code
                          + HL7Message.Delimiters.ComponentSeparator + 'TSH'
                          + HL7Message.Delimiters.ComponentSeparator
                          + LOINC_LABEL
    else
      theOBX.ObsID        := 'TSH';
    theOBX.obsSubID  := '1';
    theOBX.obsValue  := FloatToStrF(acaseRecord.TSH, ffNumber, 5, 2);
    //theOBX.Units     := gReferenceRanges.TSH.UOM;
    theOBX.Units     := aCaseRecord.TSH_UOM;
    theOBX.RefRange  := FloatToStr(gReferenceRanges.TSH.ln) + ' - ' +
      FloatToStr(gReferenceRanges.TSH.hn);
    theOBX.AbnormFlags := '';
    theOBX.probability := '';
    theOBX.Nature    := '';
    theOBX.status    := 'F';
    theOBX.RRDate    := '';
    theOBX.UDAC      := '';
    if isNaN(aCaseRecord.OBDate) then
      theOBX.ObsDateTime := ''
    else
      theOBX.ObsDateTime := EncodedDateTime(aCaseRecord.OBDate);
    theOBX.prodID := '';
    theOBX.respObs      := '';
    theOBX.observMethod := '';
    theOBX.EquipInstID  := '';
    theOBX.AnalysisDateTime := '';
    SetOBX(HL7Message, theOBX);

    Inc(setIDcounter);
    ClearOBX(theOBX);
    theOBX.SetID     := IntToStr(setIDcounter);
    theOBX.ValueType := 'NM';
    if gPreferences.exportLOINC then
      begin
      if pos('mol', gReferenceRanges.FT4.UOM) > 0 then  // SI unit
        theOBX.ObsID      := LOINC_FT4_2.code
                          + HL7Message.Delimiters.ComponentSeparator + 'FT4'
                          + HL7Message.Delimiters.ComponentSeparator
                          + LOINC_LABEL
      else
        theOBX.ObsID      := LOINC_FT4_1.code
                          + HL7Message.Delimiters.ComponentSeparator + 'FT4'
                          + HL7Message.Delimiters.ComponentSeparator
                          + LOINC_LABEL
      end
    else
      theOBX.ObsID     := 'FT4';
    theOBX.obsSubID  := '1';
    //theOBX.obsValue  := FloatToStrF(ConvertedValue(acaseRecord.FT4, T4_MOLAR_MASS,
    //  'mol/l', gReferenceRanges.FT4.UOM), ffNumber, 5, 2);
    //theOBX.Units     := gReferenceRanges.FT4.UOM;
    theOBX.obsValue  := FloatToStrF(ConvertedValue(acaseRecord.FT4, T4_MOLAR_MASS,
      'mol/l', gT4Unit), ffNumber, 5, 2);
    theOBX.Units     := gT4Unit;
    theOBX.RefRange  := FloatToStr(gReferenceRanges.FT4.ln) + ' - ' +
      FloatToStr(gReferenceRanges.FT4.hn);
    theOBX.AbnormFlags := '';
    theOBX.probability := '';
    theOBX.Nature    := '';
    theOBX.status    := 'F';
    theOBX.RRDate    := '';
    theOBX.UDAC      := '';
    if isNaN(aCaseRecord.OBDate) then
      theOBX.ObsDateTime := ''
    else
      theOBX.ObsDateTime := EncodedDateTime(aCaseRecord.OBDate);
    theOBX.prodID := '';
    theOBX.respObs      := '';
    theOBX.observMethod := '';
    theOBX.EquipInstID  := '';
    theOBX.AnalysisDateTime := '';
    SetOBX(HL7Message, theOBX);

    Inc(setIDcounter);
    ClearOBX(theOBX);
    theOBX.SetID     := IntToStr(setIDcounter);
    theOBX.ValueType := 'NM';
    if gPreferences.exportLOINC then
      begin
      if pos('mol', gReferenceRanges.FT3.UOM) > 0 then  // SI unit
        theOBX.ObsID      := LOINC_FT3_2.code
                          + HL7Message.Delimiters.ComponentSeparator + 'FT3'
                          + HL7Message.Delimiters.ComponentSeparator
                          + LOINC_LABEL
      else
        theOBX.ObsID      := LOINC_FT3_1.code
                          + HL7Message.Delimiters.ComponentSeparator + 'FT3'
                          + HL7Message.Delimiters.ComponentSeparator
                          + LOINC_LABEL
      end
    else
      theOBX.ObsID     := 'FT3';
    theOBX.obsSubID  := '1';
    //theOBX.obsValue  := FloatToStrF(ConvertedValue(acaseRecord.FT3, T3_MOLAR_MASS,
    //  'mol/l', gReferenceRanges.FT3.UOM), ffNumber, 5, 2);
    //theOBX.Units     := gReferenceRanges.FT3.UOM;
    theOBX.obsValue  := FloatToStrF(ConvertedValue(acaseRecord.FT3, T3_MOLAR_MASS,
      'mol/l', gT3Unit), ffNumber, 5, 2);
    theOBX.Units     := gT3Unit;
    ;
    theOBX.RefRange := FloatToStr(gReferenceRanges.FT3.ln) + ' - ' +
      FloatToStr(gReferenceRanges.FT3.hn);
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
    theOBX.respObs      := '';
    theOBX.observMethod := '';
    theOBX.EquipInstID  := '';
    theOBX.AnalysisDateTime := '';
    SetOBX(HL7Message, theOBX);

    Inc(setIDcounter);
    ClearOBX(theOBX);
    theOBX.SetID     := IntToStr(setIDcounter);
    theOBX.ValueType := 'NM';
    if gPreferences.exportLOINC then
      begin
      if pos('mol', gReferenceRanges.TT4.UOM) > 0 then  // SI unit
        theOBX.ObsID      := LOINC_FT4_2.code
                          + HL7Message.Delimiters.ComponentSeparator + 'TT4'
                          + HL7Message.Delimiters.ComponentSeparator
                          + LOINC_LABEL
      else
        theOBX.ObsID      := LOINC_TT4_1.code
                          + HL7Message.Delimiters.ComponentSeparator + 'TT4'
                          + HL7Message.Delimiters.ComponentSeparator
                          + LOINC_LABEL
      end
    else
      theOBX.ObsID     := 'TT4';
    theOBX.obsSubID  := '1';
    //theOBX.obsValue  := FloatToStrF(ConvertedValue(acaseRecord.TT4, T4_MOLAR_MASS,
    //  'mol/l', gReferenceRanges.TT4.UOM), ffNumber, 5, 2);
    //theOBX.Units     := gReferenceRanges.TT4.UOM;
    theOBX.obsValue  := FloatToStrF(ConvertedValue(acaseRecord.TT4, T4_MOLAR_MASS,
      'mol/l', gT4Unit), ffNumber, 5, 2);
    theOBX.Units     := gT4Unit;
    theOBX.RefRange := FloatToStr(gReferenceRanges.TT4.ln) + ' - ' +
      FloatToStr(gReferenceRanges.TT4.hn);
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
    theOBX.respObs      := '';
    theOBX.observMethod := '';
    theOBX.EquipInstID  := '';
    theOBX.AnalysisDateTime := '';
    SetOBX(HL7Message, theOBX);

    Inc(setIDcounter);
    ClearOBX(theOBX);
    theOBX.SetID     := IntToStr(setIDcounter);
    theOBX.ValueType := 'NM';
    if gPreferences.exportLOINC then
      begin
      if pos('mol', gReferenceRanges.TT3.UOM) > 0 then  // SI unit
        theOBX.ObsID      := LOINC_TT3_2.code
                          + HL7Message.Delimiters.ComponentSeparator + 'TT3'
                          + HL7Message.Delimiters.ComponentSeparator
                          + LOINC_LABEL
      else
        theOBX.ObsID      := LOINC_TT3_1.code
                          + HL7Message.Delimiters.ComponentSeparator + 'TT3'
                          + HL7Message.Delimiters.ComponentSeparator
                          + LOINC_LABEL
      end
    else
      theOBX.ObsID     := 'TT3';
    theOBX.obsSubID  := '1';
    //theOBX.obsValue  := FloatToStrF(ConvertedValue(acaseRecord.TT3, T3_MOLAR_MASS,
    //  'mol/l', gReferenceRanges.TT3.UOM), ffNumber, 5, 2);
    //theOBX.Units     := gReferenceRanges.TT3.UOM;
    theOBX.obsValue  := FloatToStrF(ConvertedValue(acaseRecord.TT3, T3_MOLAR_MASS,
      'mol/l', gT3Unit), ffNumber, 5, 2);
    theOBX.Units     := gT3Unit;
    ;
    theOBX.RefRange := FloatToStr(gReferenceRanges.TT3.ln) + ' - ' +
      FloatToStr(gReferenceRanges.TT3.hn);
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
    theOBX.respObs      := '';
    theOBX.observMethod := '';
    theOBX.EquipInstID  := '';
    theOBX.AnalysisDateTime := '';
    SetOBX(HL7Message, theOBX);

    Inc(setIDcounter);
    ClearOBX(theOBX);
    theOBX.SetID     := IntToStr(setIDcounter);
    theOBX.ValueType := 'NM';
    if gPreferences.exportLOINC then
      theOBX.ObsID        := LOINC_SPINA_GT.code
                          + HL7Message.Delimiters.ComponentSeparator + 'SPINA-GT'
                          + HL7Message.Delimiters.ComponentSeparator
                          + LOINC_LABEL
    else
      theOBX.ObsID        := 'SPINA-GT';
    theOBX.obsSubID  := '1';
    theOBX.obsValue  := FloatToStrF(acaseRecord.GT * 1E12, ffNumber, 5, 2);
    theOBX.Units     := 'pmol/s';
    theOBX.RefRange  := FloatToStr(gReferenceRanges.GT.ln * 1E12) + ' - ' +
      FloatToStr(gReferenceRanges.GT.hn * 1E12);
    theOBX.AbnormFlags := '';
    theOBX.probability := '';
    theOBX.Nature    := '';
    theOBX.status    := 'F';
    theOBX.RRDate    := '';
    theOBX.UDAC      := '';
    if isNaN(aCaseRecord.OBDate) then
      theOBX.ObsDateTime := ''
    else
      theOBX.ObsDateTime := EncodedDateTime(aCaseRecord.OBDate);
    theOBX.prodID := '';
    theOBX.respObs      := '';
    theOBX.observMethod := '';
    theOBX.EquipInstID  := '';
    theOBX.AnalysisDateTime := '';
    SetOBX(HL7Message, theOBX);

    Inc(setIDcounter);
    ClearOBX(theOBX);
    theOBX.SetID     := IntToStr(setIDcounter);
    theOBX.ValueType := 'NM';
    if gPreferences.exportLOINC then
      theOBX.ObsID        := LOINC_SPINA_GD.code
                          + HL7Message.Delimiters.ComponentSeparator + 'SPINA-GD'
                          + HL7Message.Delimiters.ComponentSeparator
                          + LOINC_LABEL
    else
      theOBX.ObsID        := 'SPINA-GD';
    theOBX.obsSubID  := '1';
    theOBX.obsValue  := FloatToStrF(acaseRecord.GD * 1E9, ffNumber, 5, 2);
    theOBX.Units     := 'nmol/s';
    theOBX.RefRange  := FloatToStr(gReferenceRanges.GD.ln * 1E9) + ' - ' +
      FloatToStr(gReferenceRanges.GD.hn * 1E9);
    theOBX.AbnormFlags := '';
    theOBX.probability := '';
    theOBX.Nature    := '';
    theOBX.status    := 'F';
    theOBX.RRDate    := '';
    theOBX.UDAC      := '';
    if isNaN(aCaseRecord.OBDate) then
      theOBX.ObsDateTime := ''
    else
      theOBX.ObsDateTime := EncodedDateTime(aCaseRecord.OBDate);
    theOBX.prodID := '';
    theOBX.respObs      := '';
    theOBX.observMethod := '';
    theOBX.EquipInstID  := '';
    theOBX.AnalysisDateTime := '';
    SetOBX(HL7Message, theOBX);

    Inc(setIDcounter);
    ClearOBX(theOBX);
    theOBX.SetID     := IntToStr(setIDcounter);
    theOBX.ValueType := 'NM';
    theOBX.ObsID     := 'SPINA-sGD';
    theOBX.obsSubID  := '1';
    theOBX.obsValue  := FloatToStrF(acaseRecord.sGD, ffNumber, 5, 2);
    theOBX.Units     := '';
    theOBX.RefRange  := FloatToStr(gReferenceRanges.sGD.ln) + ' - +' +
      FloatToStr(gReferenceRanges.sGD.hn);
    theOBX.AbnormFlags := '';
    theOBX.probability := '';
    theOBX.Nature    := '';
    theOBX.status    := 'F';
    theOBX.RRDate    := '';
    theOBX.UDAC      := '';
    if isNaN(aCaseRecord.OBDate) then
      theOBX.ObsDateTime := ''
    else
      theOBX.ObsDateTime := EncodedDateTime(aCaseRecord.OBDate);
    theOBX.prodID := '';
    theOBX.respObs      := '';
    theOBX.observMethod := '';
    theOBX.EquipInstID  := '';
    theOBX.AnalysisDateTime := '';
    SetOBX(HL7Message, theOBX);

    Inc(setIDcounter);
    ClearOBX(theOBX);
    theOBX.SetID     := IntToStr(setIDcounter);
    theOBX.ValueType := 'NM';
    theOBX.ObsID     := 'TSHI';
    theOBX.obsSubID  := '1';
    theOBX.obsValue  := FloatToStrF(acaseRecord.TSHI, ffNumber, 5, 2);
    theOBX.Units     := '';
    theOBX.RefRange  := FloatToStr(gReferenceRanges.TSHI.ln) + ' - ' +
      FloatToStr(gReferenceRanges.TSHI.hn);
    theOBX.AbnormFlags := '';
    theOBX.probability := '';
    theOBX.Nature    := '';
    theOBX.status    := 'F';
    theOBX.RRDate    := '';
    theOBX.UDAC      := '';
    if isNaN(aCaseRecord.OBDate) then
      theOBX.ObsDateTime := ''
    else
      theOBX.ObsDateTime := EncodedDateTime(aCaseRecord.OBDate);
    theOBX.prodID := '';
    theOBX.respObs      := '';
    theOBX.observMethod := '';
    theOBX.EquipInstID  := '';
    theOBX.AnalysisDateTime := '';
    SetOBX(HL7Message, theOBX);

    Inc(setIDcounter);
    ClearOBX(theOBX);
    theOBX.SetID     := IntToStr(setIDcounter);
    theOBX.ValueType := 'NM';
    theOBX.ObsID     := 'sTSHI';
    theOBX.obsSubID  := '1';
    theOBX.obsValue  := FloatToStrF(acaseRecord.sTSHI, ffNumber, 5, 2);
    theOBX.Units     := '';
    theOBX.RefRange  := FloatToStr(gReferenceRanges.sTSHI.ln) + ' - +' +
      FloatToStr(gReferenceRanges.sTSHI.hn);
    theOBX.AbnormFlags := '';
    theOBX.probability := '';
    theOBX.Nature    := '';
    theOBX.status    := 'F';
    theOBX.RRDate    := '';
    theOBX.UDAC      := '';
    if isNaN(aCaseRecord.OBDate) then
      theOBX.ObsDateTime := ''
    else
      theOBX.ObsDateTime := EncodedDateTime(aCaseRecord.OBDate);
    theOBX.prodID := '';
    theOBX.respObs      := '';
    theOBX.observMethod := '';
    theOBX.EquipInstID  := '';
    theOBX.AnalysisDateTime := '';
    SetOBX(HL7Message, theOBX);

    Inc(setIDcounter);
    ClearOBX(theOBX);
    theOBX.SetID     := IntToStr(setIDcounter);
    theOBX.ValueType := 'NM';
    theOBX.ObsID     := 'TTSI';
    theOBX.obsSubID  := '1';
    theOBX.obsValue  := FloatToStrF(acaseRecord.TTSI, ffNumber, 5, 2);
    theOBX.Units     := '';
    theOBX.RefRange  := FloatToStr(gReferenceRanges.TTSI.ln) + ' - ' +
      FloatToStr(gReferenceRanges.TTSI.hn);
    theOBX.AbnormFlags := '';
    theOBX.probability := '';
    theOBX.Nature    := '';
    theOBX.status    := 'F';
    theOBX.RRDate    := '';
    theOBX.UDAC      := '';
    if isNaN(aCaseRecord.OBDate) then
      theOBX.ObsDateTime := ''
    else
      theOBX.ObsDateTime := EncodedDateTime(aCaseRecord.OBDate);
    theOBX.prodID := '';
    theOBX.respObs      := '';
    theOBX.observMethod := '';
    theOBX.EquipInstID  := '';
    theOBX.AnalysisDateTime := '';
    SetOBX(HL7Message, theOBX);

    ClearSPM(theSPM);
    theSPM.SetID := '1';
    if isNaN(aCaseRecord.OBDate) then
      theSPM.SpecimenCollectionDateTime := ''
    else
      theSPM.SpecimenCollectionDateTime := EncodedDateTime(aCaseRecord.OBDate);
    SetSPM(Hl7Message, theSPM);

    WriteHL7File(HL7Message, SPINAToolbar.SaveResultsDialog.FileName);
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
  SaveStringToPath(theString, SPINAToolbar.SaveResultsDialog.FileName);
end;

function isLOINCTerm(ObsID: string; LOINCTerm: TLoincRecord): boolean;
begin
  if obsID = '' then result := false
  else if (pos(LOINCTerm.code, ObsID) > 0) or
  (pos(LOINCTerm.long, ObsID) > 0) or
  (pos(LOINCTerm.short, ObsID) > 0) or
  (pos(LOINCTerm.code, ObsID) > 0) or
  (pos(LOINCTerm.long, ObsID) > 0) or
  (pos(LOINCTerm.short, ObsID) > 0) then
    result := true
  else
    result := false;
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
  theField:   THL7Field;
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
      theField     := THL7Field.Create(nil, thePIDRecord.PatientIDList);
      theComponent := theField.FirstComponent;
      if theComponent <> nil then
      begin
        aCaseRecord.PID := theComponent.contentString;
        theComponent    := theComponent.nextSibling;
        if theComponent <> nil then
          aCaseRecord.CaseID := theComponent.contentString;
      end;
      theField.Destroy;
      theField     := THL7Field.Create(nil, thePIDRecord.PatientName);
      theComponent := theField.FirstComponent;
      if theComponent <> nil then
      begin
        aCaseRecord.Name := theComponent.contentString;
        theComponent     := theComponent.nextSibling;
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
      theField      := nil;
      theComponent  := nil;
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
      end
      else if theNTERecord.SetID = '3' then
      begin
        aCaseRecord.Comment := theNTERecord.comment;
      end;
    end;
    if theSegment.segmentType = 'OBX' then
    begin
      GetOBX(theSegment, theOBXRecord);
      if (pos('TSH', theOBXRecord.ObsID) > 0) and
        (pos('TSHI', theOBXRecord.ObsID) = 0) or
        isLOINCTerm(theOBXRecord.ObsID, LOINC_TSH_1) or
        isLOINCTerm(theOBXRecord.ObsID, LOINC_TSH_2) then
      begin
        aCaseRecord.TSH     := StrToFloatDef(theOBXRecord.obsValue, NaN);
        aCaseRecord.TSH_UOM := theOBXRecord.Units;
      end;
      if (pos('FT4', theOBXRecord.ObsID) > 0) or
        isLOINCTerm(theOBXRecord.ObsID, LOINC_FT4_1) or
        isLOINCTerm(theOBXRecord.ObsID, LOINC_FT4_2) then
      begin
        aCaseRecord.FT4     := StrToFloatDef(theOBXRecord.obsValue, NaN);
        aCaseRecord.FT4_UOM := theOBXRecord.Units;
        aCaseRecord.FT4     :=
          ConvertedValue(acaseRecord.FT4, T4_MOLAR_MASS, aCaseRecord.FT4_UOM, 'mol/l');
      end;
      if (pos('FT3', theOBXRecord.ObsID) > 0) or
        isLOINCTerm(theOBXRecord.ObsID, LOINC_FT3_1) or
        isLOINCTerm(theOBXRecord.ObsID, LOINC_FT3_2) then
      begin
        aCaseRecord.FT3     := StrToFloatDef(theOBXRecord.obsValue, NaN);
        aCaseRecord.FT3_UOM := theOBXRecord.Units;
        aCaseRecord.FT3     :=
          ConvertedValue(acaseRecord.FT3, T3_MOLAR_MASS, aCaseRecord.FT3_UOM, 'mol/l');
      end;
      if (pos('TT4', theOBXRecord.ObsID) > 0) or
        isLOINCTerm(theOBXRecord.ObsID, LOINC_TT4_1) or
        isLOINCTerm(theOBXRecord.ObsID, LOINC_TT4_2) then
      begin
        aCaseRecord.TT4     := StrToFloatDef(theOBXRecord.obsValue, NaN);
        aCaseRecord.TT4_UOM := theOBXRecord.Units;
        aCaseRecord.TT4     :=
          ConvertedValue(acaseRecord.TT4, T4_MOLAR_MASS, aCaseRecord.TT4_UOM, 'mol/l');
      end;
      if (pos('TT3', theOBXRecord.ObsID) > 0) or
        isLOINCTerm(theOBXRecord.ObsID, LOINC_TT3_1) or
        isLOINCTerm(theOBXRecord.ObsID, LOINC_TT3_2) then
      begin
        aCaseRecord.TT3     := StrToFloatDef(theOBXRecord.obsValue, NaN);
        aCaseRecord.TT3_UOM := theOBXRecord.Units;
        aCaseRecord.TT3     :=
          ConvertedValue(acaseRecord.TT3, T3_MOLAR_MASS, aCaseRecord.TT3_UOM, 'mol/l');
      end;
      if pos('SPINA-GT', theOBXRecord.ObsID) > 0 then
      begin
        aCaseRecord.GT     := StrToFloatDef(theOBXRecord.obsValue, NaN);
        aCaseRecord.GT_UOM := theOBXRecord.Units;
      end;
      if pos('SPINA-GD', theOBXRecord.ObsID) > 0 then
      begin
        aCaseRecord.GD     := StrToFloatDef(theOBXRecord.obsValue, NaN);
        aCaseRecord.GD_UOM := theOBXRecord.Units;
      end;
      if pos('SPINA-sGD', theOBXRecord.ObsID) > 0 then
      begin
        aCaseRecord.sGD    := StrToFloatDef(theOBXRecord.obsValue, NaN);
      end;
      if pos('TSHI', theOBXRecord.ObsID) > 0 then
        aCaseRecord.TSHI := StrToFloatDef(theOBXRecord.obsValue, NaN);
      if pos('sTSHI', theOBXRecord.ObsID) > 0 then
        aCaseRecord.sTSHI := StrToFloatDef(theOBXRecord.obsValue, NaN);
      if pos('TTSI', theOBXRecord.ObsID) > 0 then
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

procedure OpenCaseResults(var caseRecord: tCaseRecord);
var
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
      1: ReadHL7Message(SPINAToolbar.OpenCaseDialog.FileName, caseRecord);
    end;
  end;
end;

procedure SaveResults(caseRecord: tCaseRecord);
var
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
