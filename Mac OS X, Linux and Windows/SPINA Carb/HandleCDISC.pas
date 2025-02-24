unit HandleCDISC;

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

{ This unit handles CDISC XML files for reference ranges }

{ Source code released under the BSD License }
{ See http://spina.medical-cybernetics.de for details }

{$mode objfpc}

{Return codes of procedure SaveStandardCDISC_RRFile:
0: No Error.
6: Error saving file.
}

interface

uses
  Classes, SysUtils, FileUtil, DOM, XMLRead, XMLWrite, StrUtils, DateUtils,
  Math, SPINATypes, UnitConverter;

procedure SaveCDISC_RRFile(theFileName: string; const ReferenceRanges: tReferenceValues; var returnCode: integer);
procedure SaveStandardCDISC_RRFile(theFileName: string; var returnCode: integer);

implementation

procedure AddSubExNodes(Doc: TXMLDocument; theRoot: TDOMNode;
  ExclusionDefinitions: tReferenceExDefinitions);
var
  theNode, FlagUOMNode: TDOMNode;
  SubjectCharsNode, AgeNode, ExclusionsNode, TextNode: TDOMNode;
begin
  with ExclusionDefinitions do
  begin
    SubjectCharsNode := Doc.CreateElement('SubjectCharacteristics');
    theNode := Doc.CreateElement('Sex');
    TDOMElement(theNode).SetAttribute('CodeListID', 'HL7 Gender Vocabulary Domain V2.4');
    TDOMElement(theNode).SetAttribute('Value', Sex);
    SubjectCharsNode.Appendchild(theNode);
    AgeNode := Doc.CreateElement('Age');
    TDOMElement(AgeNode).SetAttribute('BoundaryType', 'L');
    SubjectCharsNode.Appendchild(AgeNode);
    theNode := Doc.CreateElement('LowerLimit');
    TDOMElement(theNode).SetAttribute('UOM', 'Y');
    TDOMElement(theNode).SetAttribute('Value', IntToStr(AgeL));
    AgeNode.Appendchild(theNode);
    theNode := Doc.CreateElement('UpperLimit');
    TDOMElement(theNode).SetAttribute('UOM', 'Y');
    TDOMElement(theNode).SetAttribute('Value', IntToStr(AgeH));
    AgeNode.Appendchild(theNode);
    FlagUOMNode := Doc.CreateElement('FlagUOM');
    TDOMElement(FlagUOMNode).SetAttribute('ResultClass', 'S');
    theNode := Doc.CreateElement('ResultUnits');
    TDOMElement(theNode).SetAttribute('CodeListID', 'ISO 1000');
    TDOMElement(theNode).SetAttribute('Value', UOMS);
    FlagUOMNode.Appendchild(theNode);
    theNode := Doc.CreateElement('Deltas');
    FlagUOMNode.Appendchild(theNode);
    ExclusionsNode := Doc.CreateElement('Exclusions');
    TDOMElement(ExclusionsNode).SetAttribute('StartDateTime', startDateTime);
    FlagUOMNode.Appendchild(ExclusionsNode);
    theNode := Doc.CreateElement('ExclusionDefinition');
    TDOMElement(theNode).SetAttribute('ExclusionLevel', 'LX');
    TDOMElement(theNode).SetAttribute('Value', '<' + FloatToStrF(LXS, ffGeneral, 5, 2));
    ExclusionsNode.Appendchild(theNode);
    theNode := Doc.CreateElement('ExclusionDefinition');
    TDOMElement(theNode).SetAttribute('ExclusionLevel', 'HX');
    TDOMElement(theNode).SetAttribute('Value', '>' + FloatToStrF(HXS, ffGeneral, 5, 2));
    ExclusionsNode.Appendchild(theNode);
    theNode  := Doc.CreateElement('TransactionType');
    TextNode := Doc.CreateTextNode('I');
    theNode.AppendChild(TextNode);
    ExclusionsNode.Appendchild(theNode);
    SubjectCharsNode.Appendchild(FlagUOMNode);
    FlagUOMNode := Doc.CreateElement('FlagUOM');
    TDOMElement(FlagUOMNode).SetAttribute('ResultClass', 'C');
    theNode := Doc.CreateElement('ResultUnits');
    TDOMElement(theNode).SetAttribute('CodeListID', 'ISO 1000');
    TDOMElement(theNode).SetAttribute('Value', UOMC);
    FlagUOMNode.Appendchild(theNode);
    theNode := Doc.CreateElement('Deltas');
    FlagUOMNode.Appendchild(theNode);
    ExclusionsNode := Doc.CreateElement('Exclusions');
    TDOMElement(ExclusionsNode).SetAttribute('StartDateTime', startDateTime);
    FlagUOMNode.Appendchild(ExclusionsNode);
    theNode := Doc.CreateElement('ExclusionDefinition');
    TDOMElement(theNode).SetAttribute('ExclusionLevel', 'LX');
    TDOMElement(theNode).SetAttribute('Value', '<' + FloatToStrF(LXC, ffGeneral, 5, 2));
    ExclusionsNode.Appendchild(theNode);
    theNode := Doc.CreateElement('ExclusionDefinition');
    TDOMElement(theNode).SetAttribute('ExclusionLevel', 'HX');
    TDOMElement(theNode).SetAttribute('Value', '>' + FloatToStrF(HXC, ffGeneral, 5, 2));
    ExclusionsNode.Appendchild(theNode);
    theNode  := Doc.CreateElement('TransactionType');
    TextNode := Doc.CreateTextNode('I');
    theNode.AppendChild(TextNode);
    ExclusionsNode.Appendchild(theNode);
    SubjectCharsNode.Appendchild(FlagUOMNode);
    theRoot.Appendchild(SubjectCharsNode);
  end;
end;

procedure AddSubNormNodes(Doc: TXMLDocument; theRoot: TDOMNode;
  NormDefinitions: tReferenceNormDefinitions);
var
  RootNode, parentNode, theNode, BatteryNode, BaseTestNode, FlagUOMNode: TDOMNode;
  LabTestNode, NormalNode, AlertNode, UnitsNode, NormalDefinitionNode: TDOMNode;
  SubjectCharsNode, AgeNode, ExclusionsNode, TextNode: TDOMNode;
  oldSeparator: char;
begin
  oldSeparator := DefaultFormatSettings.DecimalSeparator;
  DefaultFormatSettings.DecimalSeparator := DEC_POINT;
  with NormDefinitions do
  begin
    SubjectCharsNode := Doc.CreateElement('SubjectCharacteristics');
    theNode := Doc.CreateElement('Sex');
    TDOMElement(theNode).SetAttribute('CodeListID', 'HL7 Gender Vocabulary Domain V2.4');
    TDOMElement(theNode).SetAttribute('Value', Sex);
    SubjectCharsNode.Appendchild(theNode);
    AgeNode := Doc.CreateElement('Age');
    TDOMElement(AgeNode).SetAttribute('BoundaryType', 'L');
    SubjectCharsNode.Appendchild(AgeNode);
    theNode := Doc.CreateElement('LowerLimit');
    TDOMElement(theNode).SetAttribute('UOM', 'Y');
    TDOMElement(theNode).SetAttribute('Value', IntToStr(AgeL));
    AgeNode.Appendchild(theNode);
    theNode := Doc.CreateElement('UpperLimit');
    TDOMElement(theNode).SetAttribute('UOM', 'Y');
    TDOMElement(theNode).SetAttribute('Value', IntToStr(AgeH));
    AgeNode.Appendchild(theNode);
    FlagUOMNode := Doc.CreateElement('FlagUOM');
    TDOMElement(FlagUOMNode).SetAttribute('ResultClass', 'S');
    theNode := Doc.CreateElement('ResultUnits');
    TDOMElement(theNode).SetAttribute('CodeListID', 'ISO 1000');
    TDOMElement(theNode).SetAttribute('Value', UOMS);
    FlagUOMNode.Appendchild(theNode);
    NormalNode := Doc.CreateElement('Normal');
    TDOMElement(NormalNode).SetAttribute('StartDateTime', startDateTime);
    FlagUOMNode.Appendchild(NormalNode);
    theNode := Doc.CreateElement('NormalDefinition');
    TDOMElement(theNode).SetAttribute('NormalLevel', 'L');
    TDOMElement(theNode).SetAttribute('Value', FloatToStrF(LS, ffGeneral, 5, 2));
    NormalNode.Appendchild(theNode);
    theNode := Doc.CreateElement('NormalDefinition');
    TDOMElement(theNode).SetAttribute('NormalLevel', 'H');
    TDOMElement(theNode).SetAttribute('Value', FloatToStrF(HS, ffGeneral, 5, 2));
    NormalNode.Appendchild(theNode);
    theNode  := Doc.CreateElement('TransactionType');
    TextNode := Doc.CreateTextNode('I');
    theNode.AppendChild(TextNode);
    NormalNode.Appendchild(theNode);
    AlertNode := Doc.CreateElement('Alerts');
    TDOMElement(AlertNode).SetAttribute('StartDateTime', startDateTime);
    FlagUOMNode.Appendchild(AlertNode);
    theNode := Doc.CreateElement('AlertDefinition');
    TDOMElement(theNode).SetAttribute('AlertLevel', 'LN');
    TDOMElement(theNode).SetAttribute('Value', FloatToStrF(LS, ffGeneral, 5, 2));
    AlertNode.Appendchild(theNode);
    theNode := Doc.CreateElement('AlertDefinition');
    TDOMElement(theNode).SetAttribute('AlertLevel', 'HN');
    TDOMElement(theNode).SetAttribute('Value', FloatToStrF(HS, ffGeneral, 5, 2));
    AlertNode.Appendchild(theNode);
    if not IsNan(LTS) then
    begin
      theNode := Doc.CreateElement('AlertDefinition');
      TDOMElement(theNode).SetAttribute('AlertLevel', 'LT');
      TDOMElement(theNode).SetAttribute('Value', '<' + FloatToStrF(LTS, ffGeneral, 5, 2));
      AlertNode.Appendchild(theNode);
    end;
    if not IsNan(HTS) then
    begin
      theNode := Doc.CreateElement('AlertDefinition');
      TDOMElement(theNode).SetAttribute('AlertLevel', 'HT');
      TDOMElement(theNode).SetAttribute('Value', '>' + FloatToStrF(HTS, ffGeneral, 5, 2));
      AlertNode.Appendchild(theNode);
    end;
    if not IsNan(LPS) then
    begin
      theNode := Doc.CreateElement('AlertDefinition');
      TDOMElement(theNode).SetAttribute('AlertLevel', 'LP');
      TDOMElement(theNode).SetAttribute('Value', '<' + FloatToStrF(LPS, ffGeneral, 5, 2));
      AlertNode.Appendchild(theNode);
    end;
    if not IsNan(HPS) then
    begin
      theNode := Doc.CreateElement('AlertDefinition');
      TDOMElement(theNode).SetAttribute('AlertLevel', 'HP');
      TDOMElement(theNode).SetAttribute('Value', '>' + FloatToStrF(HPS, ffGeneral, 5, 2));
      AlertNode.Appendchild(theNode);
    end;
    theNode  := Doc.CreateElement('TransactionType');
    TextNode := Doc.CreateTextNode('I');
    theNode.AppendChild(TextNode);
    AlertNode.Appendchild(theNode);
    SubjectCharsNode.Appendchild(FlagUOMNode);
    FlagUOMNode := Doc.CreateElement('FlagUOM');
    TDOMElement(FlagUOMNode).SetAttribute('ResultClass', 'C');
    theNode := Doc.CreateElement('ResultUnits');
    TDOMElement(theNode).SetAttribute('CodeListID', 'ISO 1000');
    TDOMElement(theNode).SetAttribute('Value', UOMC);
    FlagUOMNode.Appendchild(theNode);
    NormalNode := Doc.CreateElement('Normal');
    TDOMElement(NormalNode).SetAttribute('StartDateTime', startDateTime);
    FlagUOMNode.Appendchild(NormalNode);
    theNode := Doc.CreateElement('NormalDefinition');
    TDOMElement(theNode).SetAttribute('NormalLevel', 'L');
    TDOMElement(theNode).SetAttribute('Value', FloatToStrF(LC, ffGeneral, 5, 2));
    NormalNode.Appendchild(theNode);
    theNode := Doc.CreateElement('NormalDefinition');
    TDOMElement(theNode).SetAttribute('NormalLevel', 'H');
    TDOMElement(theNode).SetAttribute('Value', FloatToStrF(HC, ffGeneral, 5, 2));
    NormalNode.Appendchild(theNode);
    theNode  := Doc.CreateElement('TransactionType');
    TextNode := Doc.CreateTextNode('I');
    theNode.AppendChild(TextNode);
    NormalNode.Appendchild(theNode);
    AlertNode := Doc.CreateElement('Alerts');
    TDOMElement(AlertNode).SetAttribute('StartDateTime', startDateTime);
    FlagUOMNode.Appendchild(AlertNode);
    theNode := Doc.CreateElement('AlertDefinition');
    TDOMElement(theNode).SetAttribute('AlertLevel', 'LN');
    TDOMElement(theNode).SetAttribute('Value', FloatToStrF(LC, ffGeneral, 5, 2));
    AlertNode.Appendchild(theNode);
    theNode := Doc.CreateElement('AlertDefinition');
    TDOMElement(theNode).SetAttribute('AlertLevel', 'HN');
    TDOMElement(theNode).SetAttribute('Value', FloatToStrF(HC, ffGeneral, 5, 2));
    AlertNode.Appendchild(theNode);
    if not IsNan(LTC) then
    begin
      theNode := Doc.CreateElement('AlertDefinition');
      TDOMElement(theNode).SetAttribute('AlertLevel', 'LT');
      TDOMElement(theNode).SetAttribute('Value', '<' + FloatToStrF(LTC, ffGeneral, 5, 2));
      AlertNode.Appendchild(theNode);
    end;
    if not IsNan(HTC) then
    begin
      theNode := Doc.CreateElement('AlertDefinition');
      TDOMElement(theNode).SetAttribute('AlertLevel', 'HT');
      TDOMElement(theNode).SetAttribute('Value', '>' + FloatToStrF(HTC, ffGeneral, 5, 2));
      AlertNode.Appendchild(theNode);
    end;
    if not IsNan(LPC) then
    begin
      theNode := Doc.CreateElement('AlertDefinition');
      TDOMElement(theNode).SetAttribute('AlertLevel', 'LP');
      TDOMElement(theNode).SetAttribute('Value', '<' + FloatToStrF(LPC, ffGeneral, 5, 2));
      AlertNode.Appendchild(theNode);
    end;
    if not IsNan(HPC) then
    begin
      theNode := Doc.CreateElement('AlertDefinition');
      TDOMElement(theNode).SetAttribute('AlertLevel', 'HP');
      TDOMElement(theNode).SetAttribute('Value', '>' + FloatToStrF(HPC, ffGeneral, 5, 2));
      AlertNode.Appendchild(theNode);
    end;
    theNode  := Doc.CreateElement('TransactionType');
    TextNode := Doc.CreateTextNode('I');
    theNode.AppendChild(TextNode);
    AlertNode.Appendchild(theNode);
    SubjectCharsNode.Appendchild(FlagUOMNode);
    theRoot.Appendchild(SubjectCharsNode);
  end;
  DefaultFormatSettings.DecimalSeparator := oldSeparator;
end;

function ISO8601Date(const AValue: TDateTime): String;
begin
  result := FormatDateTime('YYYY-MM-DD"T"hh:nn:ss', AValue);
end;

function ISO8601ToDateTime(const ADate: String): TDateTime;
begin
  result := ScanDateTime('YYYY-MM-DD"T"hh:nn:ss', ADate, 1);
end;

procedure SaveCDISC_RRFile(theFileName: string; const ReferenceRanges: tReferenceValues; var returnCode: integer);
{Saves edited reference ranges as CDISC file}
var
  Doc: TXMLDocument;
  RootNode, parentNode, theNode, BatteryNode, BaseTestNode, FlagUOMNode: TDOMNode;
  LabTestNode, NormalNode, AlertNode, UnitsNode, NormalDefinitionNode: TDOMNode;
  SubjectCharsNode, AgeNode, ExclusionsNode, TextNode: TDOMNode;
  ExclusionDefinitions: tReferenceExDefinitions;
  NormDefinitions: tReferenceNormDefinitions;
  TestString: String;
begin
  returnCode := 6;
  try
    Doc     := TXMLDocument.Create;
    theNode := Doc.CreateComment('SPINA Reference Values');
    Doc.AppendChild(theNode);
    theNode := Doc.CreateComment('adapted to CDISC LAB MODEL 1.0.1');
    Doc.AppendChild(theNode);
    RootNode := Doc.CreateElement('GTP');
    TDOMElement(RootNode).SetAttribute('CreationDateTime', ISO8601Date(now));
    TDOMElement(RootNode).SetAttribute('ModelVersion', '01-0-01');
    Doc.Appendchild(RootNode);
    RootNode   := Doc.DocumentElement;
    parentNode := Doc.CreateElement('TransmissionSource');
    TDOMElement(parentNode).SetAttribute('ID', gPreferences.MSH_ID);
    TDOMElement(parentNode).SetAttribute('Name', gPreferences.MSH_ID);
    RootNode.Appendchild(parentNode);
    parentNode := Doc.CreateElement('Study');
    TDOMElement(parentNode).SetAttribute('ID', 'SPIc');
    TDOMElement(parentNode).SetAttribute('Name', 'Reference Ranges for SPINA Carb');
    TDOMElement(parentNode).SetAttribute('TransmissionType', 'C');
    RootNode.Appendchild(parentNode);

    {Standard hormone and metabolite concentrations:}

    BatteryNode := Doc.CreateElement('BaseBattery');
    TDOMElement(BatteryNode).SetAttribute('ID', 'Hormones_metabolites');
    TDOMElement(BatteryNode).SetAttribute('Name', 'SPINA Carb');
    parentNode.Appendchild(BatteryNode);

    {Glucose:}

    BaseTestNode := Doc.CreateElement('BaseTest');
    TDOMElement(BaseTestNode).SetAttribute('DefiningEntity', 'C');
    theNode := Doc.CreateComment('Definitions for Glucose:');
    BaseTestNode.AppendChild(theNode);
    BatteryNode.Appendchild(BaseTestNode);
    LabTestNode := Doc.CreateElement('LabTest');
    TDOMElement(LabTestNode).SetAttribute('ID', 'Glucose');
    TDOMElement(LabTestNode).SetAttribute('Name', 'Glucose');
    BaseTestNode.Appendchild(LabTestNode);

    ExclusionDefinitions.Sex  := 'F';
    ExclusionDefinitions.AgeL := 0;
    ExclusionDefinitions.AgeH := 999;
    ExclusionDefinitions.UOMS := 'mmol/L';
    ExclusionDefinitions.UOMC := 'mg/dL';
    ExclusionDefinitions.LXS  := 0;
    ExclusionDefinitions.HXS  := 10000;
    ExclusionDefinitions.LXC  := 0;
    ExclusionDefinitions.HXC  := 10000;
    ExclusionDefinitions.startDateTime := '2000-01-01T12:00:00+01:00';
    AddSubExNodes(Doc, BaseTestNode, ExclusionDefinitions);
    NormDefinitions.Sex  := 'F';
    NormDefinitions.AgeL := 0;
    NormDefinitions.AgeH := 130;
    NormDefinitions.UOMS := 'mIU/L';
    NormDefinitions.UOMC := 'mIU/L';
    NormDefinitions.LS  := ReferenceRanges.Glucose.ln;
    NormDefinitions.HS  := ReferenceRanges.Glucose.hn;
    NormDefinitions.LTS := ReferenceRanges.Glucose.lt;
    NormDefinitions.HTS := ReferenceRanges.Glucose.ht;
    NormDefinitions.LPS := ReferenceRanges.Glucose.lp;
    NormDefinitions.HPS := ReferenceRanges.Glucose.hp;
    NormDefinitions.LC  := ReferenceRanges.Glucose.ln;
    NormDefinitions.HC  := ReferenceRanges.Glucose.hn;
    NormDefinitions.LTC := ReferenceRanges.Glucose.lt;
    NormDefinitions.HTC := ReferenceRanges.Glucose.ht;
    NormDefinitions.LPC := ReferenceRanges.Glucose.lp;
    NormDefinitions.HPC := ReferenceRanges.Glucose.hp;
    NormDefinitions.startDateTime := ISO8601Date(now);
    AddSubNormNodes(Doc, BaseTestNode, NormDefinitions);

    theNode := Doc.CreateComment('Add additional age classes here');
    BaseTestNode.AppendChild(theNode);

    ExclusionDefinitions.Sex  := 'M';
    ExclusionDefinitions.AgeL := 0;
    ExclusionDefinitions.AgeH := 999;
    ExclusionDefinitions.UOMS := 'mmol/L';
    ExclusionDefinitions.UOMC := 'mg/dL';
    ExclusionDefinitions.LXS  := 0;
    ExclusionDefinitions.HXS  := 10000;
    ExclusionDefinitions.LXC  := 0;
    ExclusionDefinitions.HXC  := 10000;
    ExclusionDefinitions.startDateTime := '2000-01-01T12:00:00+01:00';
    AddSubExNodes(Doc, BaseTestNode, ExclusionDefinitions);
    NormDefinitions.Sex  := 'M';
    NormDefinitions.AgeL := 0;
    NormDefinitions.AgeH := 130;
    NormDefinitions.UOMS := 'mmol/L';
    NormDefinitions.UOMC := 'mg/dL';
    NormDefinitions.LS  := ReferenceRanges.Glucose.ln;
    NormDefinitions.HS  := ReferenceRanges.Glucose.hn;
    NormDefinitions.LTS := ReferenceRanges.Glucose.lt;
    NormDefinitions.HTS := ReferenceRanges.Glucose.ht;
    NormDefinitions.LPS := ReferenceRanges.Glucose.lp;
    NormDefinitions.HPS := ReferenceRanges.Glucose.hp;
    NormDefinitions.LC  := ReferenceRanges.Glucose.ln;
    NormDefinitions.HC  := ReferenceRanges.Glucose.hn;
    NormDefinitions.LTC := ReferenceRanges.Glucose.lt;
    NormDefinitions.HTC := ReferenceRanges.Glucose.ht;
    NormDefinitions.LPC := ReferenceRanges.Glucose.lp;
    NormDefinitions.HPC := ReferenceRanges.Glucose.hp;
    NormDefinitions.startDateTime := ISO8601Date(now);
    AddSubNormNodes(Doc, BaseTestNode, NormDefinitions);

    {Insulin:}

    { #todo -oJWD : Insert handlers for insulin and c-peptide here }

    {SPINA:}

    BatteryNode := Doc.CreateElement('BaseBattery');
    TDOMElement(BatteryNode).SetAttribute('ID', 'SPIc');
    TDOMElement(BatteryNode).SetAttribute('Name', 'SPINA Carb');
    parentNode.Appendchild(BatteryNode);

    {SPINA-GBeta:}

    BaseTestNode := Doc.CreateElement('BaseTest');
    TDOMElement(BaseTestNode).SetAttribute('DefiningEntity', 'C');
    theNode := Doc.CreateComment('Definitions for SPINA-GBeta:');
    BaseTestNode.AppendChild(theNode);
    BatteryNode.Appendchild(BaseTestNode);
    LabTestNode := Doc.CreateElement('LabTest');
    TDOMElement(LabTestNode).SetAttribute('ID', 'GBeta');
    TDOMElement(LabTestNode).SetAttribute('Name', 'Beta cells'' Secretory Capacity');
    BaseTestNode.Appendchild(LabTestNode);

    ExclusionDefinitions.Sex  := 'F';
    ExclusionDefinitions.AgeL := 0;
    ExclusionDefinitions.AgeH := 999;
    ExclusionDefinitions.UOMS := 'pmol/s';
    ExclusionDefinitions.UOMC := 'pmol/s';
    ExclusionDefinitions.LXS  := 0;
    ExclusionDefinitions.HXS  := 10000;
    ExclusionDefinitions.LXC  := 0;
    ExclusionDefinitions.HXC  := 10000;
    ExclusionDefinitions.startDateTime := '2000-01-01T12:00:00+01:00';
    AddSubExNodes(Doc, BaseTestNode, ExclusionDefinitions);
    NormDefinitions.Sex  := 'F';
    NormDefinitions.AgeL := 0;
    NormDefinitions.AgeH := 130;
    NormDefinitions.UOMS := 'pmol/s';
    NormDefinitions.UOMC := 'pmol/s';
    NormDefinitions.LS  := ReferenceRanges.SPINA_GBeta.ln;
    NormDefinitions.HS  := ReferenceRanges.SPINA_GBeta.hn;
    NormDefinitions.LTS := ReferenceRanges.SPINA_GBeta.lt;
    NormDefinitions.HTS := ReferenceRanges.SPINA_GBeta.ht;
    NormDefinitions.LPS := ReferenceRanges.SPINA_GBeta.lp;
    NormDefinitions.HPS := ReferenceRanges.SPINA_GBeta.hp;
    NormDefinitions.LC  := ReferenceRanges.SPINA_GBeta.ln;
    NormDefinitions.HC  := ReferenceRanges.SPINA_GBeta.hn;
    NormDefinitions.LTC := ReferenceRanges.SPINA_GBeta.lt;
    NormDefinitions.HTC := ReferenceRanges.SPINA_GBeta.ht;
    NormDefinitions.LPC := ReferenceRanges.SPINA_GBeta.lp;
    NormDefinitions.HPC := ReferenceRanges.SPINA_GBeta.hp;
    NormDefinitions.startDateTime := ISO8601Date(now);
    AddSubNormNodes(Doc, BaseTestNode, NormDefinitions);

    theNode := Doc.CreateComment('Add additional age classes here');
    BaseTestNode.AppendChild(theNode);

    ExclusionDefinitions.Sex  := 'M';
    ExclusionDefinitions.AgeL := 0;
    ExclusionDefinitions.AgeH := 999;
    ExclusionDefinitions.UOMS := 'pmol/s';
    ExclusionDefinitions.UOMC := 'pmol/s';
    ExclusionDefinitions.LXS  := 0;
    ExclusionDefinitions.HXS  := 10000;
    ExclusionDefinitions.LXC  := 0;
    ExclusionDefinitions.HXC  := 10000;
    ExclusionDefinitions.startDateTime := '2000-01-01T12:00:00+01:00';
    AddSubExNodes(Doc, BaseTestNode, ExclusionDefinitions);
    NormDefinitions.Sex  := 'M';
    NormDefinitions.AgeL := 0;
    NormDefinitions.AgeH := 130;
    NormDefinitions.UOMS := 'pmol/s';
    NormDefinitions.UOMC := 'pmol/s';
    NormDefinitions.LS  := ReferenceRanges.SPINA_GBeta.ln;
    NormDefinitions.HS  := ReferenceRanges.SPINA_GBeta.hn;
    NormDefinitions.LTS := ReferenceRanges.SPINA_GBeta.lt;
    NormDefinitions.HTS := ReferenceRanges.SPINA_GBeta.ht;
    NormDefinitions.LPS := ReferenceRanges.SPINA_GBeta.lp;
    NormDefinitions.HPS := ReferenceRanges.SPINA_GBeta.hp;
    NormDefinitions.LC  := ReferenceRanges.SPINA_GBeta.ln;
    NormDefinitions.HC  := ReferenceRanges.SPINA_GBeta.hn;
    NormDefinitions.LTC := ReferenceRanges.SPINA_GBeta.lt;
    NormDefinitions.HTC := ReferenceRanges.SPINA_GBeta.ht;
    NormDefinitions.LPC := ReferenceRanges.SPINA_GBeta.lp;
    NormDefinitions.HPC := ReferenceRanges.SPINA_GBeta.hp;
    NormDefinitions.startDateTime := ISO8601Date(now);
    AddSubNormNodes(Doc, BaseTestNode, NormDefinitions);

    {SPINA-GR:}

    { #todo -oJWD : Insert handlers for structure parameters here }

    writeXMLFile(Doc, theFileName);  // was writeXMLFile(Doc, UTF8ToSys(theFileName));

    returnCode := 0;
  finally
    Doc.Free;
  end;
end;

procedure SaveStandardCDISC_RRFile(theFileName: string; var returnCode: integer);
{saves a file with standard reference ranges}
var
  Doc: TXMLDocument;
  RootNode, parentNode, theNode, BatteryNode, BaseTestNode, FlagUOMNode: TDOMNode;
  LabTestNode, NormalNode, AlertNode, UnitsNode, NormalDefinitionNode: TDOMNode;
  SubjectCharsNode, AgeNode, ExclusionsNode, TextNode: TDOMNode;
  ExclusionDefinitions: tReferenceExDefinitions;
  NormDefinitions: tReferenceNormDefinitions;
  TestString: String;
begin
  returnCode := 6;
  try
    Doc     := TXMLDocument.Create;
    theNode := Doc.CreateComment('Example for SPINA Reference Values');
    Doc.AppendChild(theNode);
    theNode := Doc.CreateComment('adapted to CDISC LAB MODEL 1.0.1');
    Doc.AppendChild(theNode);
    theNode := Doc.CreateComment('(c) J. W. Dietrich, 1994 - 2025');
    Doc.AppendChild(theNode);
    theNode := Doc.CreateComment(
      '(c) Ludwig Maximilian University of Munich 1995 - 2002');
    Doc.AppendChild(theNode);
    theNode := Doc.CreateComment('(c) University of Ulm Hospitals 2002-2004');
    Doc.AppendChild(theNode);
    theNode := Doc.CreateComment('(c) Ruhr University of Bochum 2005 - 2025');
    Doc.AppendChild(theNode);
    theNode := Doc.CreateComment('(released under the BSD License');
    Doc.AppendChild(theNode);
    RootNode := Doc.CreateElement('GTP');
    TDOMElement(RootNode).SetAttribute('CreationDateTime', '2025-02-07T00:13:00+01:00');
    TDOMElement(RootNode).SetAttribute('ModelVersion', '01-0-01');
    Doc.Appendchild(RootNode);
    RootNode   := Doc.DocumentElement;
    parentNode := Doc.CreateElement('TransmissionSource');
    TDOMElement(parentNode).SetAttribute('ID', 'www.medizin.ruhr-uni-bochum.de');
    TDOMElement(parentNode).SetAttribute('Name', 'UK RUB');
    RootNode.Appendchild(parentNode);
    parentNode := Doc.CreateElement('Study');
    TDOMElement(parentNode).SetAttribute('ID', 'SPIc');
    TDOMElement(parentNode).SetAttribute('Name', 'Reference Ranges for SPINA Carb');
    TDOMElement(parentNode).SetAttribute('TransmissionType', 'C');
    RootNode.Appendchild(parentNode);

    {Standard hormone and metabolite concentrations:}

    BatteryNode := Doc.CreateElement('BaseBattery');
    TDOMElement(BatteryNode).SetAttribute('ID', 'Hormones_metabolites');
    TDOMElement(BatteryNode).SetAttribute('Name', 'SPINA Carb');
    parentNode.Appendchild(BatteryNode);

    {Glucose:}

    BaseTestNode := Doc.CreateElement('BaseTest');
    TDOMElement(BaseTestNode).SetAttribute('DefiningEntity', 'C');
    theNode := Doc.CreateComment('Definitions for Glucose:');
    BaseTestNode.AppendChild(theNode);
    BatteryNode.Appendchild(BaseTestNode);
    LabTestNode := Doc.CreateElement('LabTest');
    TDOMElement(LabTestNode).SetAttribute('ID', 'Glucose');
    TDOMElement(LabTestNode).SetAttribute('Name', 'Glucose');
    BaseTestNode.Appendchild(LabTestNode);

    ExclusionDefinitions.Sex  := 'F';
    ExclusionDefinitions.AgeL := 0;
    ExclusionDefinitions.AgeH := 999;
    ExclusionDefinitions.UOMS := 'mmol/L';
    ExclusionDefinitions.UOMC := 'mg/dL';
    ExclusionDefinitions.LXS  := 0;
    ExclusionDefinitions.HXS  := 10000;
    ExclusionDefinitions.LXC  := 0;
    ExclusionDefinitions.HXC  := 10000;
    ExclusionDefinitions.startDateTime := '2000-01-01T12:00:00+01:00';
    AddSubExNodes(Doc, BaseTestNode, ExclusionDefinitions);
    NormDefinitions.Sex  := 'F';
    NormDefinitions.AgeL := 0;
    NormDefinitions.AgeH := 130;
    NormDefinitions.UOMS := 'mmol/L';
    NormDefinitions.UOMC := 'mg/L';
    NormDefinitions.LS  := 3.89;
    NormDefinitions.HS  := 5.56;
    NormDefinitions.LTS := 3;
    NormDefinitions.HTS := 10;
    NormDefinitions.LPS := 2.22;
    NormDefinitions.HPS := 13.89;
    NormDefinitions.LC  := 70;
    NormDefinitions.HC  := 100;
    NormDefinitions.LTC := 54;
    NormDefinitions.HTC := 180;
    NormDefinitions.LPC := 40;
    NormDefinitions.HPC := 250;
    NormDefinitions.startDateTime := '2012-08-01T12:00:00+01:00';
    AddSubNormNodes(Doc, BaseTestNode, NormDefinitions);

    theNode := Doc.CreateComment('Add additional age classes here');
    BaseTestNode.AppendChild(theNode);

    ExclusionDefinitions.Sex  := 'M';
    ExclusionDefinitions.AgeL := 0;
    ExclusionDefinitions.AgeH := 999;
    ExclusionDefinitions.UOMS := 'mmol/L';
    ExclusionDefinitions.UOMC := 'mg/dL';
    ExclusionDefinitions.LXS  := 0;
    ExclusionDefinitions.HXS  := 10000;
    ExclusionDefinitions.LXC  := 0;
    ExclusionDefinitions.HXC  := 10000;
    ExclusionDefinitions.startDateTime := '2000-01-01T12:00:00+01:00';
    AddSubExNodes(Doc, BaseTestNode, ExclusionDefinitions);
    NormDefinitions.Sex  := 'M';
    NormDefinitions.AgeL := 0;
    NormDefinitions.AgeH := 130;
    NormDefinitions.UOMS := 'mmol/L';
    NormDefinitions.UOMC := 'mg/dL';
    NormDefinitions.LS  := 3.89;
     NormDefinitions.HS  := 5.56;
     NormDefinitions.LTS := 3;
     NormDefinitions.HTS := 10;
     NormDefinitions.LPS := 2.22;
     NormDefinitions.HPS := 13.89;
     NormDefinitions.LC  := 70;
     NormDefinitions.HC  := 100;
     NormDefinitions.LTC := 54;
     NormDefinitions.HTC := 180;
     NormDefinitions.LPC := 40;
     NormDefinitions.HPC := 250;
      NormDefinitions.startDateTime := '2012-08-01T12:00:00+01:00';
    AddSubNormNodes(Doc, BaseTestNode, NormDefinitions);

    {Insulin:}

    { #todo -oJWD : Insert handlers for insulin and c-peptide here }

    {SPINA:}

    BatteryNode := Doc.CreateElement('BaseBattery');
    TDOMElement(BatteryNode).SetAttribute('ID', 'SPIc');
    TDOMElement(BatteryNode).SetAttribute('Name', 'SPINA Carb');
    parentNode.Appendchild(BatteryNode);

    {SPINA-GBeta:}

    BaseTestNode := Doc.CreateElement('BaseTest');
    TDOMElement(BaseTestNode).SetAttribute('DefiningEntity', 'C');
    theNode := Doc.CreateComment('Definitions for SPINA-GBeta:');
    BaseTestNode.AppendChild(theNode);
    BatteryNode.Appendchild(BaseTestNode);
    LabTestNode := Doc.CreateElement('LabTest');
    TDOMElement(LabTestNode).SetAttribute('ID', 'GBeta');
    TDOMElement(LabTestNode).SetAttribute('Name', 'Beta cells'' Secretory Capacity');
    BaseTestNode.Appendchild(LabTestNode);

    ExclusionDefinitions.Sex  := 'F';
    ExclusionDefinitions.AgeL := 0;
    ExclusionDefinitions.AgeH := 999;
    ExclusionDefinitions.UOMS := 'pmol/s';
    ExclusionDefinitions.UOMC := 'pmol/s';
    ExclusionDefinitions.LXS  := 0;
    ExclusionDefinitions.HXS  := 10000;
    ExclusionDefinitions.LXC  := 0;
    ExclusionDefinitions.HXC  := 10000;
    ExclusionDefinitions.startDateTime := '2000-01-01T12:00:00+01:00';
    AddSubExNodes(Doc, BaseTestNode, ExclusionDefinitions);
    NormDefinitions.Sex  := 'F';
    NormDefinitions.AgeL := 0;
    NormDefinitions.AgeH := 130;
    NormDefinitions.UOMS := 'pmol/s';
    NormDefinitions.UOMC := 'pmol/s';
    NormDefinitions.LS  := 0.64;
    NormDefinitions.HS  := 3.73;
    NormDefinitions.LTS := 0.3;
    NormDefinitions.HTS := 6;
    NormDefinitions.LPS := 0.1;
    NormDefinitions.HPS := 10;
    NormDefinitions.LC  := 0.64;
    NormDefinitions.HC  := 3.73;
    NormDefinitions.LTC := 0.3;
    NormDefinitions.HTC := 6;
    NormDefinitions.LPC := 0.1;
    NormDefinitions.HPC := 10;
    NormDefinitions.startDateTime := '2012-08-01T12:00:00+01:00';
    AddSubNormNodes(Doc, BaseTestNode, NormDefinitions);

    theNode := Doc.CreateComment('Add additional age classes here');
    BaseTestNode.AppendChild(theNode);

    ExclusionDefinitions.Sex  := 'M';
    ExclusionDefinitions.AgeL := 0;
    ExclusionDefinitions.AgeH := 999;
    ExclusionDefinitions.UOMS := 'pmol/s';
    ExclusionDefinitions.UOMC := 'pmol/s';
    ExclusionDefinitions.LXS  := 0;
    ExclusionDefinitions.HXS  := 10000;
    ExclusionDefinitions.LXC  := 0;
    ExclusionDefinitions.HXC  := 10000;
    ExclusionDefinitions.startDateTime := '2000-01-01T12:00:00+01:00';
    AddSubExNodes(Doc, BaseTestNode, ExclusionDefinitions);
    NormDefinitions.Sex  := 'M';
    NormDefinitions.AgeL := 0;
    NormDefinitions.AgeH := 130;
    NormDefinitions.UOMS := 'pmol/s';
    NormDefinitions.UOMC := 'pmol/s';
    NormDefinitions.LS  := 0.64;
    NormDefinitions.HS  := 3.73;
    NormDefinitions.LTS := 0.3;
    NormDefinitions.HTS := 6;
    NormDefinitions.LPS := 0.1;
    NormDefinitions.HPS := 10;
    NormDefinitions.LC  := 0.64;
    NormDefinitions.HC  := 3.73;
    NormDefinitions.LTC := 0.3;
    NormDefinitions.HTC := 6;
    NormDefinitions.LPC := 0.1;
    NormDefinitions.HPC := 10;
    NormDefinitions.startDateTime := '2012-08-01T12:00:00+01:00';
    AddSubNormNodes(Doc, BaseTestNode, NormDefinitions);

    {SPINA-GR:}

    { #todo -oJWD : Insert Handlers for structure parameters here }

    writeXMLFile(Doc, theFileName);

    returnCode := 0;
  finally
    Doc.Free;
  end;
end;

end.
