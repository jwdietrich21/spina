unit CDISC;

{ SPINA-Thyr }

{ Application for calculating structure parameters }
{ of thyrotropic feedback control }

{ Programm zur Berechnung von Strukturparametern }
{ des thyreotropen Regelkreises }

{ Version 4.0.0 (Mercator) }

{ (c) J. W. Dietrich, 1994 - 2014 }
{ (c) Ludwig Maximilian University of Munich 1995 - 2002 }
{ (c) University of Ulm Hospitals 2002-2004 }
{ (c) Ruhr University of Bochum 2005 - 2014 }

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
  Math, SPINA_Types, UnitConverter;

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
    TDOMElement(parentNode).SetAttribute('ID', 'SPIt');
    TDOMElement(parentNode).SetAttribute('Name', 'Reference Ranges for SPINA Thyr');
    TDOMElement(parentNode).SetAttribute('TransmissionType', 'C');
    RootNode.Appendchild(parentNode);

    {Standard hormone levels:}

    BatteryNode := Doc.CreateElement('BaseBattery');
    TDOMElement(BatteryNode).SetAttribute('ID', 'Thyroid_Hormones');
    TDOMElement(BatteryNode).SetAttribute('Name', 'SPINA Thyr');
    parentNode.Appendchild(BatteryNode);

    {TSH:}

    BaseTestNode := Doc.CreateElement('BaseTest');
    TDOMElement(BaseTestNode).SetAttribute('DefiningEntity', 'C');
    theNode := Doc.CreateComment('Definitions for TSH:');
    BaseTestNode.AppendChild(theNode);
    BatteryNode.Appendchild(BaseTestNode);
    LabTestNode := Doc.CreateElement('LabTest');
    TDOMElement(LabTestNode).SetAttribute('ID', 'TSH');
    TDOMElement(LabTestNode).SetAttribute('Name', 'Thyrotropin');
    BaseTestNode.Appendchild(LabTestNode);

    ExclusionDefinitions.Sex  := 'F';
    ExclusionDefinitions.AgeL := 0;
    ExclusionDefinitions.AgeH := 999;
    ExclusionDefinitions.UOMS := 'mIU/L';
    ExclusionDefinitions.UOMC := 'mIU/L';
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
    NormDefinitions.LS  := ReferenceRanges.TSH.ln;
    NormDefinitions.HS  := ReferenceRanges.TSH.hn;
    NormDefinitions.LTS := ReferenceRanges.TSH.lt;
    NormDefinitions.HTS := ReferenceRanges.TSH.ht;
    NormDefinitions.LPS := ReferenceRanges.TSH.lp;
    NormDefinitions.HPS := ReferenceRanges.TSH.hp;
    NormDefinitions.LC  := ReferenceRanges.TSH.ln;
    NormDefinitions.HC  := ReferenceRanges.TSH.hn;
    NormDefinitions.LTC := ReferenceRanges.TSH.lt;
    NormDefinitions.HTC := ReferenceRanges.TSH.ht;
    NormDefinitions.LPC := ReferenceRanges.TSH.lp;
    NormDefinitions.HPC := ReferenceRanges.TSH.hp;
    NormDefinitions.startDateTime := ISO8601Date(now);
    AddSubNormNodes(Doc, BaseTestNode, NormDefinitions);

    theNode := Doc.CreateComment('Add additional age classes here');
    BaseTestNode.AppendChild(theNode);

    ExclusionDefinitions.Sex  := 'M';
    ExclusionDefinitions.AgeL := 0;
    ExclusionDefinitions.AgeH := 999;
    ExclusionDefinitions.UOMS := 'mIU/L';
    ExclusionDefinitions.UOMC := 'mIU/L';
    ExclusionDefinitions.LXS  := 0;
    ExclusionDefinitions.HXS  := 10000;
    ExclusionDefinitions.LXC  := 0;
    ExclusionDefinitions.HXC  := 10000;
    ExclusionDefinitions.startDateTime := '2000-01-01T12:00:00+01:00';
    AddSubExNodes(Doc, BaseTestNode, ExclusionDefinitions);
    NormDefinitions.Sex  := 'M';
    NormDefinitions.AgeL := 0;
    NormDefinitions.AgeH := 130;
    NormDefinitions.UOMS := 'mIU/L';
    NormDefinitions.UOMC := 'mIU/L';
    NormDefinitions.LS  := ReferenceRanges.TSH.ln;
    NormDefinitions.HS  := ReferenceRanges.TSH.hn;
    NormDefinitions.LTS := ReferenceRanges.TSH.lt;
    NormDefinitions.HTS := ReferenceRanges.TSH.ht;
    NormDefinitions.LPS := ReferenceRanges.TSH.lp;
    NormDefinitions.HPS := ReferenceRanges.TSH.hp;
    NormDefinitions.LC  := ReferenceRanges.TSH.ln;
    NormDefinitions.HC  := ReferenceRanges.TSH.hn;
    NormDefinitions.LTC := ReferenceRanges.TSH.lt;
    NormDefinitions.HTC := ReferenceRanges.TSH.ht;
    NormDefinitions.LPC := ReferenceRanges.TSH.lp;
    NormDefinitions.HPC := ReferenceRanges.TSH.hp;
    NormDefinitions.startDateTime := ISO8601Date(now);
    AddSubNormNodes(Doc, BaseTestNode, NormDefinitions);

    {FT4:}

    BaseTestNode := Doc.CreateElement('BaseTest');
    TDOMElement(BaseTestNode).SetAttribute('DefiningEntity', 'C');
    theNode := Doc.CreateComment('Definitions for FT4:');
    BaseTestNode.AppendChild(theNode);
    BatteryNode.Appendchild(BaseTestNode);
    LabTestNode := Doc.CreateElement('LabTest');
    TDOMElement(LabTestNode).SetAttribute('ID', 'FT4');
    TDOMElement(LabTestNode).SetAttribute('Name', 'Free T4');
    BaseTestNode.Appendchild(LabTestNode);

    ExclusionDefinitions.Sex  := 'F';
    ExclusionDefinitions.AgeL := 0;
    ExclusionDefinitions.AgeH := 999;
    ExclusionDefinitions.UOMS := 'pmol/L';
    ExclusionDefinitions.UOMC := 'ng/L';
    ExclusionDefinitions.LXS  := 0;
    ExclusionDefinitions.HXS  := 1287;
    ExclusionDefinitions.LXC  := 0;
    ExclusionDefinitions.HXC  := 1000;
    ExclusionDefinitions.startDateTime := '2000-01-01T12:00:00+01:00';
    AddSubExNodes(Doc, BaseTestNode, ExclusionDefinitions);
    NormDefinitions.Sex  := 'F';
    NormDefinitions.AgeL := 0;
    NormDefinitions.AgeH := 130;
    if pos(UpCase('mol'), UpCase(ReferenceRanges.FT4.UOM)) > 0 then
    begin
      NormDefinitions.UOMS := ReferenceRanges.FT4.UOM;
      NormDefinitions.UOMC := 'N/A';
      NormDefinitions.LS  := ReferenceRanges.FT4.ln;
      NormDefinitions.HS  := ReferenceRanges.FT4.hn;
      NormDefinitions.LTS := ReferenceRanges.FT4.lt;
      NormDefinitions.HTS := ReferenceRanges.FT4.ht;
      NormDefinitions.LPS := ReferenceRanges.FT4.lp;
      NormDefinitions.HPS := ReferenceRanges.FT4.hp;
      NormDefinitions.LC  := ConvertedValue(ReferenceRanges.FT4.ln, T4_MOLAR_MASS, ReferenceRanges.FT4.UOM, 'ng/L');
      NormDefinitions.HC  := ConvertedValue(ReferenceRanges.FT4.hn, T4_MOLAR_MASS, ReferenceRanges.FT4.UOM, 'ng/L');
      NormDefinitions.LTC := ConvertedValue(ReferenceRanges.FT4.lt, T4_MOLAR_MASS, ReferenceRanges.FT4.UOM, 'ng/L');
      NormDefinitions.HTC := ConvertedValue(ReferenceRanges.FT4.ht, T4_MOLAR_MASS, ReferenceRanges.FT4.UOM, 'ng/L');
      NormDefinitions.LPC := ConvertedValue(ReferenceRanges.FT4.lp, T4_MOLAR_MASS, ReferenceRanges.FT4.UOM, 'ng/L');
      NormDefinitions.HPC := ConvertedValue(ReferenceRanges.FT4.hp, T4_MOLAR_MASS, ReferenceRanges.FT4.UOM, 'ng/L');
    end
    else
    begin
      NormDefinitions.UOMS := 'N/A';
      NormDefinitions.UOMC := ReferenceRanges.FT4.UOM;
      NormDefinitions.LS  := ConvertedValue(ReferenceRanges.FT4.ln, T4_MOLAR_MASS, ReferenceRanges.FT4.UOM, 'pmol/L');
      NormDefinitions.HS  := ConvertedValue(ReferenceRanges.FT4.hn, T4_MOLAR_MASS, ReferenceRanges.FT4.UOM, 'pmol/L');
      NormDefinitions.LTS := ConvertedValue(ReferenceRanges.FT4.lt, T4_MOLAR_MASS, ReferenceRanges.FT4.UOM, 'pmol/L');
      NormDefinitions.HTS := ConvertedValue(ReferenceRanges.FT4.ht, T4_MOLAR_MASS, ReferenceRanges.FT4.UOM, 'pmol/L');
      NormDefinitions.LPS := ConvertedValue(ReferenceRanges.FT4.lp, T4_MOLAR_MASS, ReferenceRanges.FT4.UOM, 'pmol/L');
      NormDefinitions.HPS := ConvertedValue(ReferenceRanges.FT4.hp, T4_MOLAR_MASS, ReferenceRanges.FT4.UOM, 'pmol/L');
      NormDefinitions.LC  := ReferenceRanges.FT4.ln;
      NormDefinitions.HC  := ReferenceRanges.FT4.hn;
      NormDefinitions.LTC := ReferenceRanges.FT4.lt;
      NormDefinitions.HTC := ReferenceRanges.FT4.ht;
      NormDefinitions.LPC := ReferenceRanges.FT4.lp;
      NormDefinitions.HPC := ReferenceRanges.FT4.hp;
    end;
    NormDefinitions.startDateTime := ISO8601Date(now);
    AddSubNormNodes(Doc, BaseTestNode, NormDefinitions);

    theNode := Doc.CreateComment('Add additional age classes here');
    BaseTestNode.AppendChild(theNode);

    ExclusionDefinitions.Sex  := 'M';
    ExclusionDefinitions.AgeL := 0;
    ExclusionDefinitions.AgeH := 999;
    ExclusionDefinitions.UOMS := 'pmol/L';
    ExclusionDefinitions.UOMC := 'ng/L';
    ExclusionDefinitions.LXS  := 0;
    ExclusionDefinitions.HXS  := 1287;
    ExclusionDefinitions.LXC  := 0;
    ExclusionDefinitions.HXC  := 1000;
    ExclusionDefinitions.startDateTime := '2000-01-01T12:00:00+01:00';
    AddSubExNodes(Doc, BaseTestNode, ExclusionDefinitions);
    NormDefinitions.Sex  := 'M';
    NormDefinitions.AgeL := 0;
    NormDefinitions.AgeH := 130;
    NormDefinitions.UOMS := 'pmol/L';
    NormDefinitions.UOMC := 'ng/L';
    if pos(UpCase('mol'), UpCase(ReferenceRanges.FT4.UOM)) > 0 then
    begin
      NormDefinitions.UOMS := ReferenceRanges.FT4.UOM;
      NormDefinitions.UOMC := 'N/A';
      NormDefinitions.LS  := ReferenceRanges.FT4.ln;
      NormDefinitions.HS  := ReferenceRanges.FT4.hn;
      NormDefinitions.LTS := ReferenceRanges.FT4.lt;
      NormDefinitions.HTS := ReferenceRanges.FT4.ht;
      NormDefinitions.LPS := ReferenceRanges.FT4.lp;
      NormDefinitions.HPS := ReferenceRanges.FT4.hp;
      NormDefinitions.LC  := ConvertedValue(ReferenceRanges.FT4.ln, T4_MOLAR_MASS, ReferenceRanges.FT4.UOM, 'ng/L');
      NormDefinitions.HC  := ConvertedValue(ReferenceRanges.FT4.hn, T4_MOLAR_MASS, ReferenceRanges.FT4.UOM, 'ng/L');
      NormDefinitions.LTC := ConvertedValue(ReferenceRanges.FT4.lt, T4_MOLAR_MASS, ReferenceRanges.FT4.UOM, 'ng/L');
      NormDefinitions.HTC := ConvertedValue(ReferenceRanges.FT4.ht, T4_MOLAR_MASS, ReferenceRanges.FT4.UOM, 'ng/L');
      NormDefinitions.LPC := ConvertedValue(ReferenceRanges.FT4.lp, T4_MOLAR_MASS, ReferenceRanges.FT4.UOM, 'ng/L');
      NormDefinitions.HPC := ConvertedValue(ReferenceRanges.FT4.hp, T4_MOLAR_MASS, ReferenceRanges.FT4.UOM, 'ng/L');
    end
    else
    begin
      NormDefinitions.UOMS := 'N/A';
      NormDefinitions.UOMC := ReferenceRanges.FT4.UOM;
      NormDefinitions.LS  := ConvertedValue(ReferenceRanges.FT4.ln, T4_MOLAR_MASS, ReferenceRanges.FT4.UOM, 'pmol/L');
      NormDefinitions.HS  := ConvertedValue(ReferenceRanges.FT4.hn, T4_MOLAR_MASS, ReferenceRanges.FT4.UOM, 'pmol/L');
      NormDefinitions.LTS := ConvertedValue(ReferenceRanges.FT4.lt, T4_MOLAR_MASS, ReferenceRanges.FT4.UOM, 'pmol/L');
      NormDefinitions.HTS := ConvertedValue(ReferenceRanges.FT4.ht, T4_MOLAR_MASS, ReferenceRanges.FT4.UOM, 'pmol/L');
      NormDefinitions.LPS := ConvertedValue(ReferenceRanges.FT4.lp, T4_MOLAR_MASS, ReferenceRanges.FT4.UOM, 'pmol/L');
      NormDefinitions.HPS := ConvertedValue(ReferenceRanges.FT4.hp, T4_MOLAR_MASS, ReferenceRanges.FT4.UOM, 'pmol/L');
      NormDefinitions.LC  := ReferenceRanges.FT4.ln;
      NormDefinitions.HC  := ReferenceRanges.FT4.hn;
      NormDefinitions.LTC := ReferenceRanges.FT4.lt;
      NormDefinitions.HTC := ReferenceRanges.FT4.ht;
      NormDefinitions.LPC := ReferenceRanges.FT4.lp;
      NormDefinitions.HPC := ReferenceRanges.FT4.hp;
    end;
    NormDefinitions.startDateTime := ISO8601Date(now);
    AddSubNormNodes(Doc, BaseTestNode, NormDefinitions);

    {FT3:}

    BaseTestNode := Doc.CreateElement('BaseTest');
    TDOMElement(BaseTestNode).SetAttribute('DefiningEntity', 'C');
    theNode := Doc.CreateComment('Definitions for FT3:');
    BaseTestNode.AppendChild(theNode);
    BatteryNode.Appendchild(BaseTestNode);
    LabTestNode := Doc.CreateElement('LabTest');
    TDOMElement(LabTestNode).SetAttribute('ID', 'FT3');
    TDOMElement(LabTestNode).SetAttribute('Name', 'Free T3');
    BaseTestNode.Appendchild(LabTestNode);

    ExclusionDefinitions.Sex  := 'F';
    ExclusionDefinitions.AgeL := 0;
    ExclusionDefinitions.AgeH := 999;
    ExclusionDefinitions.UOMS := 'pmol/L';
    ExclusionDefinitions.UOMC := 'ng/L';
    ExclusionDefinitions.LXS  := 0;
    ExclusionDefinitions.HXS  := 100;
    ExclusionDefinitions.LXC  := 0;
    ExclusionDefinitions.HXC  := 65;
    ExclusionDefinitions.startDateTime := '2000-01-01T12:00:00+01:00';
    AddSubExNodes(Doc, BaseTestNode, ExclusionDefinitions);
    NormDefinitions.Sex  := 'F';
    NormDefinitions.AgeL := 0;
    NormDefinitions.AgeH := 130;
    NormDefinitions.UOMS := 'pmol/L';
    NormDefinitions.UOMC := 'ng/L';
    if pos(UpCase('mol'), UpCase(ReferenceRanges.FT3.UOM)) > 0 then
    begin
      NormDefinitions.UOMS := ReferenceRanges.FT3.UOM;
      NormDefinitions.UOMC := 'N/A';
      NormDefinitions.LS  := ReferenceRanges.FT3.ln;
      NormDefinitions.HS  := ReferenceRanges.FT3.hn;
      NormDefinitions.LTS := ReferenceRanges.FT3.lt;
      NormDefinitions.HTS := ReferenceRanges.FT3.ht;
      NormDefinitions.LPS := ReferenceRanges.FT3.lp;
      NormDefinitions.HPS := ReferenceRanges.FT3.hp;
      NormDefinitions.LC  := ConvertedValue(ReferenceRanges.FT3.ln, T3_MOLAR_MASS, ReferenceRanges.FT3.UOM, 'pg/mL');
      NormDefinitions.HC  := ConvertedValue(ReferenceRanges.FT3.hn, T3_MOLAR_MASS, ReferenceRanges.FT3.UOM, 'pg/ml');
      NormDefinitions.LTC := ConvertedValue(ReferenceRanges.FT3.lt, T3_MOLAR_MASS, ReferenceRanges.FT3.UOM, 'pg/ml');
      NormDefinitions.HTC := ConvertedValue(ReferenceRanges.FT3.ht, T3_MOLAR_MASS, ReferenceRanges.FT3.UOM, 'pg/ml');
      NormDefinitions.LPC := ConvertedValue(ReferenceRanges.FT3.lp, T3_MOLAR_MASS, ReferenceRanges.FT3.UOM, 'pg/ml');
      NormDefinitions.HPC := ConvertedValue(ReferenceRanges.FT3.hp, T3_MOLAR_MASS, ReferenceRanges.FT3.UOM, 'pg/ml');
    end
    else
    begin
      NormDefinitions.UOMS := 'N/A';
      NormDefinitions.UOMC := ReferenceRanges.FT3.UOM;
      NormDefinitions.LS  := ConvertedValue(ReferenceRanges.FT3.ln, T3_MOLAR_MASS, ReferenceRanges.FT3.UOM, 'pmol/L');
      NormDefinitions.HS  := ConvertedValue(ReferenceRanges.FT3.hn, T3_MOLAR_MASS, ReferenceRanges.FT3.UOM, 'pmol/L');
      NormDefinitions.LTS := ConvertedValue(ReferenceRanges.FT3.lt, T3_MOLAR_MASS, ReferenceRanges.FT3.UOM, 'pmol/L');
      NormDefinitions.HTS := ConvertedValue(ReferenceRanges.FT3.ht, T3_MOLAR_MASS, ReferenceRanges.FT3.UOM, 'pmol/L');
      NormDefinitions.LPS := ConvertedValue(ReferenceRanges.FT3.lp, T3_MOLAR_MASS, ReferenceRanges.FT3.UOM, 'pmol/L');
      NormDefinitions.HPS := ConvertedValue(ReferenceRanges.FT3.hp, T3_MOLAR_MASS, ReferenceRanges.FT3.UOM, 'pmol/L');
      NormDefinitions.LC  := ReferenceRanges.FT3.ln;
      NormDefinitions.HC  := ReferenceRanges.FT3.hn;
      NormDefinitions.LTC := ReferenceRanges.FT3.lt;
      NormDefinitions.HTC := ReferenceRanges.FT3.ht;
      NormDefinitions.LPC := ReferenceRanges.FT3.lp;
      NormDefinitions.HPC := ReferenceRanges.FT3.hp;
    end;
    NormDefinitions.startDateTime := ISO8601Date(now);
    AddSubNormNodes(Doc, BaseTestNode, NormDefinitions);

    theNode := Doc.CreateComment('Add additional age classes here');
    BaseTestNode.AppendChild(theNode);

    ExclusionDefinitions.Sex  := 'M';
    ExclusionDefinitions.AgeL := 0;
    ExclusionDefinitions.AgeH := 999;
    ExclusionDefinitions.UOMS := 'pmol/L';
    ExclusionDefinitions.UOMC := 'ng/L';
    ExclusionDefinitions.LXS  := 0;
    ExclusionDefinitions.HXS  := 100;
    ExclusionDefinitions.LXC  := 0;
    ExclusionDefinitions.HXC  := 65;
    ExclusionDefinitions.startDateTime := '2000-01-01T12:00:00+01:00';
    AddSubExNodes(Doc, BaseTestNode, ExclusionDefinitions);
    NormDefinitions.Sex  := 'M';
    NormDefinitions.AgeL := 0;
    NormDefinitions.AgeH := 130;
    NormDefinitions.UOMS := 'pmol/L';
    NormDefinitions.UOMC := 'ng/L';
    if pos(UpCase('mol'), UpCase(ReferenceRanges.FT3.UOM)) > 0 then
    begin
      NormDefinitions.UOMS := ReferenceRanges.FT3.UOM;
      NormDefinitions.UOMC := 'N/A';
      NormDefinitions.LS  := ReferenceRanges.FT3.ln;
      NormDefinitions.HS  := ReferenceRanges.FT3.hn;
      NormDefinitions.LTS := ReferenceRanges.FT3.lt;
      NormDefinitions.HTS := ReferenceRanges.FT3.ht;
      NormDefinitions.LPS := ReferenceRanges.FT3.lp;
      NormDefinitions.HPS := ReferenceRanges.FT3.hp;
      NormDefinitions.LC  := ConvertedValue(ReferenceRanges.FT3.ln, T3_MOLAR_MASS, ReferenceRanges.FT3.UOM, 'pg/mL');
      NormDefinitions.HC  := ConvertedValue(ReferenceRanges.FT3.hn, T3_MOLAR_MASS, ReferenceRanges.FT3.UOM, 'pg/ml');
      NormDefinitions.LTC := ConvertedValue(ReferenceRanges.FT3.lt, T3_MOLAR_MASS, ReferenceRanges.FT3.UOM, 'pg/ml');
      NormDefinitions.HTC := ConvertedValue(ReferenceRanges.FT3.ht, T3_MOLAR_MASS, ReferenceRanges.FT3.UOM, 'pg/ml');
      NormDefinitions.LPC := ConvertedValue(ReferenceRanges.FT3.lp, T3_MOLAR_MASS, ReferenceRanges.FT3.UOM, 'pg/ml');
      NormDefinitions.HPC := ConvertedValue(ReferenceRanges.FT3.hp, T3_MOLAR_MASS, ReferenceRanges.FT3.UOM, 'pg/ml');
    end
    else
    begin
      NormDefinitions.UOMS := 'N/A';
      NormDefinitions.UOMC := ReferenceRanges.FT3.UOM;
      NormDefinitions.LS  := ConvertedValue(ReferenceRanges.FT3.ln, T3_MOLAR_MASS, ReferenceRanges.FT3.UOM, 'pmol/L');
      NormDefinitions.HS  := ConvertedValue(ReferenceRanges.FT3.hn, T3_MOLAR_MASS, ReferenceRanges.FT3.UOM, 'pmol/L');
      NormDefinitions.LTS := ConvertedValue(ReferenceRanges.FT3.lt, T3_MOLAR_MASS, ReferenceRanges.FT3.UOM, 'pmol/L');
      NormDefinitions.HTS := ConvertedValue(ReferenceRanges.FT3.ht, T3_MOLAR_MASS, ReferenceRanges.FT3.UOM, 'pmol/L');
      NormDefinitions.LPS := ConvertedValue(ReferenceRanges.FT3.lp, T3_MOLAR_MASS, ReferenceRanges.FT3.UOM, 'pmol/L');
      NormDefinitions.HPS := ConvertedValue(ReferenceRanges.FT3.hp, T3_MOLAR_MASS, ReferenceRanges.FT3.UOM, 'pmol/L');
      NormDefinitions.LC  := ReferenceRanges.FT3.ln;
      NormDefinitions.HC  := ReferenceRanges.FT3.hn;
      NormDefinitions.LTC := ReferenceRanges.FT3.lt;
      NormDefinitions.HTC := ReferenceRanges.FT3.ht;
      NormDefinitions.LPC := ReferenceRanges.FT3.lp;
      NormDefinitions.HPC := ReferenceRanges.FT3.hp;
    end;
    NormDefinitions.startDateTime := ISO8601Date(now);
    AddSubNormNodes(Doc, BaseTestNode, NormDefinitions);

    {TT4:}

    BaseTestNode := Doc.CreateElement('BaseTest');
    TDOMElement(BaseTestNode).SetAttribute('DefiningEntity', 'C');
    theNode := Doc.CreateComment('Definitions for TT4:');
    BaseTestNode.AppendChild(theNode);
    BatteryNode.Appendchild(BaseTestNode);
    LabTestNode := Doc.CreateElement('LabTest');
    TDOMElement(LabTestNode).SetAttribute('ID', 'TT4');
    TDOMElement(LabTestNode).SetAttribute('Name', 'Total T4');
    BaseTestNode.Appendchild(LabTestNode);

    ExclusionDefinitions.Sex  := 'F';
    ExclusionDefinitions.AgeL := 0;
    ExclusionDefinitions.AgeH := 999;
    ExclusionDefinitions.UOMS := 'nmol/L';
    ExclusionDefinitions.UOMC := 'mcg/L';
    ExclusionDefinitions.LXS  := 0;
    ExclusionDefinitions.HXS  := 1287;
    ExclusionDefinitions.LXC  := 0;
    ExclusionDefinitions.HXC  := 1000;
    ExclusionDefinitions.startDateTime := '2000-01-01T12:00:00+01:00';
    AddSubExNodes(Doc, BaseTestNode, ExclusionDefinitions);
    NormDefinitions.Sex  := 'F';
    NormDefinitions.AgeL := 0;
    NormDefinitions.AgeH := 130;
    NormDefinitions.UOMS := 'nmol/L';
    NormDefinitions.UOMC := 'mcg/L';
    if pos(UpCase('mol'), UpCase(ReferenceRanges.TT4.UOM)) > 0 then
    begin
      NormDefinitions.UOMS := ReferenceRanges.TT4.UOM;
      NormDefinitions.UOMC := 'N/A';
      NormDefinitions.LS  := ReferenceRanges.TT4.ln;
      NormDefinitions.HS  := ReferenceRanges.TT4.hn;
      NormDefinitions.LTS := ReferenceRanges.TT4.lt;
      NormDefinitions.HTS := ReferenceRanges.TT4.ht;
      NormDefinitions.LPS := ReferenceRanges.TT4.lp;
      NormDefinitions.HPS := ReferenceRanges.TT4.hp;
      NormDefinitions.LC  := ConvertedValue(ReferenceRanges.TT4.ln, T4_MOLAR_MASS, ReferenceRanges.TT4.UOM, 'mcg/L');
      NormDefinitions.HC  := ConvertedValue(ReferenceRanges.TT4.hn, T4_MOLAR_MASS, ReferenceRanges.TT4.UOM, 'mcg/L');
      NormDefinitions.LTC := ConvertedValue(ReferenceRanges.TT4.lt, T4_MOLAR_MASS, ReferenceRanges.TT4.UOM, 'mcg/L');
      NormDefinitions.HTC := ConvertedValue(ReferenceRanges.TT4.ht, T4_MOLAR_MASS, ReferenceRanges.TT4.UOM, 'mcg/L');
      NormDefinitions.LPC := ConvertedValue(ReferenceRanges.TT4.lp, T4_MOLAR_MASS, ReferenceRanges.TT4.UOM, 'mcg/L');
      NormDefinitions.HPC := ConvertedValue(ReferenceRanges.TT4.hp, T4_MOLAR_MASS, ReferenceRanges.TT4.UOM, 'mcg/L');
    end
    else
    begin
      NormDefinitions.UOMS := 'N/A';
      NormDefinitions.UOMC := ReferenceRanges.TT4.UOM;
      NormDefinitions.LS  := ConvertedValue(ReferenceRanges.TT4.ln, T4_MOLAR_MASS, ReferenceRanges.TT4.UOM, 'pmol/L');
      NormDefinitions.HS  := ConvertedValue(ReferenceRanges.TT4.hn, T4_MOLAR_MASS, ReferenceRanges.TT4.UOM, 'pmol/L');
      NormDefinitions.LTS := ConvertedValue(ReferenceRanges.TT4.lt, T4_MOLAR_MASS, ReferenceRanges.TT4.UOM, 'pmol/L');
      NormDefinitions.HTS := ConvertedValue(ReferenceRanges.TT4.ht, T4_MOLAR_MASS, ReferenceRanges.TT4.UOM, 'pmol/L');
      NormDefinitions.LPS := ConvertedValue(ReferenceRanges.TT4.lp, T4_MOLAR_MASS, ReferenceRanges.TT4.UOM, 'pmol/L');
      NormDefinitions.HPS := ConvertedValue(ReferenceRanges.TT4.hp, T4_MOLAR_MASS, ReferenceRanges.TT4.UOM, 'pmol/L');
      NormDefinitions.LC  := ReferenceRanges.TT4.ln;
      NormDefinitions.HC  := ReferenceRanges.TT4.hn;
      NormDefinitions.LTC := ReferenceRanges.TT4.lt;
      NormDefinitions.HTC := ReferenceRanges.TT4.ht;
      NormDefinitions.LPC := ReferenceRanges.TT4.lp;
      NormDefinitions.HPC := ReferenceRanges.TT4.hp;
    end;
    NormDefinitions.startDateTime := ISO8601Date(now);
    AddSubNormNodes(Doc, BaseTestNode, NormDefinitions);

    theNode := Doc.CreateComment('Add additional age classes here');
    BaseTestNode.AppendChild(theNode);

    ExclusionDefinitions.Sex  := 'M';
    ExclusionDefinitions.AgeL := 0;
    ExclusionDefinitions.AgeH := 999;
    ExclusionDefinitions.UOMS := 'nmol/L';
    ExclusionDefinitions.UOMC := 'mcg/L';
    ExclusionDefinitions.LXS  := 0;
    ExclusionDefinitions.HXS  := 1287;
    ExclusionDefinitions.LXC  := 0;
    ExclusionDefinitions.HXC  := 1000;
    ExclusionDefinitions.startDateTime := '2000-01-01T12:00:00+01:00';
    AddSubExNodes(Doc, BaseTestNode, ExclusionDefinitions);
    NormDefinitions.Sex  := 'M';
    NormDefinitions.AgeL := 0;
    NormDefinitions.AgeH := 130;
    NormDefinitions.UOMS := 'nmol/L';
    NormDefinitions.UOMC := 'mcg/L';
    if pos(UpCase('mol'), UpCase(ReferenceRanges.TT4.UOM)) > 0 then
    begin
      NormDefinitions.UOMS := ReferenceRanges.TT4.UOM;
      NormDefinitions.UOMC := 'N/A';
      NormDefinitions.LS  := ReferenceRanges.TT4.ln;
      NormDefinitions.HS  := ReferenceRanges.TT4.hn;
      NormDefinitions.LTS := ReferenceRanges.TT4.lt;
      NormDefinitions.HTS := ReferenceRanges.TT4.ht;
      NormDefinitions.LPS := ReferenceRanges.TT4.lp;
      NormDefinitions.HPS := ReferenceRanges.TT4.hp;
      NormDefinitions.LC  := ConvertedValue(ReferenceRanges.TT4.ln, T4_MOLAR_MASS, ReferenceRanges.TT4.UOM, 'mcg/L');
      NormDefinitions.HC  := ConvertedValue(ReferenceRanges.TT4.hn, T4_MOLAR_MASS, ReferenceRanges.TT4.UOM, 'mcg/L');
      NormDefinitions.LTC := ConvertedValue(ReferenceRanges.TT4.lt, T4_MOLAR_MASS, ReferenceRanges.TT4.UOM, 'mcg/L');
      NormDefinitions.HTC := ConvertedValue(ReferenceRanges.TT4.ht, T4_MOLAR_MASS, ReferenceRanges.TT4.UOM, 'mcg/L');
      NormDefinitions.LPC := ConvertedValue(ReferenceRanges.TT4.lp, T4_MOLAR_MASS, ReferenceRanges.TT4.UOM, 'mcg/L');
      NormDefinitions.HPC := ConvertedValue(ReferenceRanges.TT4.hp, T4_MOLAR_MASS, ReferenceRanges.TT4.UOM, 'mcg/L');
    end
    else
    begin
      NormDefinitions.UOMS := 'N/A';
      NormDefinitions.UOMC := ReferenceRanges.TT4.UOM;
      NormDefinitions.LS  := ConvertedValue(ReferenceRanges.TT4.ln, T4_MOLAR_MASS, ReferenceRanges.TT4.UOM, 'pmol/L');
      NormDefinitions.HS  := ConvertedValue(ReferenceRanges.TT4.hn, T4_MOLAR_MASS, ReferenceRanges.TT4.UOM, 'pmol/L');
      NormDefinitions.LTS := ConvertedValue(ReferenceRanges.TT4.lt, T4_MOLAR_MASS, ReferenceRanges.TT4.UOM, 'pmol/L');
      NormDefinitions.HTS := ConvertedValue(ReferenceRanges.TT4.ht, T4_MOLAR_MASS, ReferenceRanges.TT4.UOM, 'pmol/L');
      NormDefinitions.LPS := ConvertedValue(ReferenceRanges.TT4.lp, T4_MOLAR_MASS, ReferenceRanges.TT4.UOM, 'pmol/L');
      NormDefinitions.HPS := ConvertedValue(ReferenceRanges.TT4.hp, T4_MOLAR_MASS, ReferenceRanges.TT4.UOM, 'pmol/L');
      NormDefinitions.LC  := ReferenceRanges.TT4.ln;
      NormDefinitions.HC  := ReferenceRanges.TT4.hn;
      NormDefinitions.LTC := ReferenceRanges.TT4.lt;
      NormDefinitions.HTC := ReferenceRanges.TT4.ht;
      NormDefinitions.LPC := ReferenceRanges.TT4.lp;
      NormDefinitions.HPC := ReferenceRanges.TT4.hp;
    end;
    NormDefinitions.startDateTime := ISO8601Date(now);
    AddSubNormNodes(Doc, BaseTestNode, NormDefinitions);

    {TT3:}

    BaseTestNode := Doc.CreateElement('BaseTest');
    TDOMElement(BaseTestNode).SetAttribute('DefiningEntity', 'C');
    theNode := Doc.CreateComment('Definitions for TT3:');
    BaseTestNode.AppendChild(theNode);
    BatteryNode.Appendchild(BaseTestNode);
    LabTestNode := Doc.CreateElement('LabTest');
    TDOMElement(LabTestNode).SetAttribute('ID', 'TT3');
    TDOMElement(LabTestNode).SetAttribute('Name', 'Total T3');
    BaseTestNode.Appendchild(LabTestNode);

    ExclusionDefinitions.Sex  := 'F';
    ExclusionDefinitions.AgeL := 0;
    ExclusionDefinitions.AgeH := 999;
    ExclusionDefinitions.UOMS := 'nmol/L';
    ExclusionDefinitions.UOMC := 'mcg/L';
    ExclusionDefinitions.LXS  := 0;
    ExclusionDefinitions.HXS  := 154;
    ExclusionDefinitions.LXC  := 0;
    ExclusionDefinitions.HXC  := 100;
    ExclusionDefinitions.startDateTime := '2000-01-01T12:00:00+01:00';
    AddSubExNodes(Doc, BaseTestNode, ExclusionDefinitions);
    NormDefinitions.Sex  := 'F';
    NormDefinitions.AgeL := 0;
    NormDefinitions.AgeH := 130;
    NormDefinitions.UOMS := 'nmol/L';
    NormDefinitions.UOMC := 'mcg/L';
    if pos(UpCase('mol'), UpCase(ReferenceRanges.TT3.UOM)) > 0 then
    begin
      NormDefinitions.UOMS := ReferenceRanges.TT3.UOM;
      NormDefinitions.UOMC := 'N/A';
      NormDefinitions.LS  := ReferenceRanges.TT3.ln;
      NormDefinitions.HS  := ReferenceRanges.TT3.hn;
      NormDefinitions.LTS := ReferenceRanges.TT3.lt;
      NormDefinitions.HTS := ReferenceRanges.TT3.ht;
      NormDefinitions.LPS := ReferenceRanges.TT3.lp;
      NormDefinitions.HPS := ReferenceRanges.TT3.hp;
      NormDefinitions.LC  := ConvertedValue(ReferenceRanges.TT3.ln, T3_MOLAR_MASS, ReferenceRanges.TT3.UOM, 'mcg/L');
      NormDefinitions.HC  := ConvertedValue(ReferenceRanges.TT3.hn, T3_MOLAR_MASS, ReferenceRanges.TT3.UOM, 'mcg/L');
      NormDefinitions.LTC := ConvertedValue(ReferenceRanges.TT3.lt, T3_MOLAR_MASS, ReferenceRanges.TT3.UOM, 'mcg/L');
      NormDefinitions.HTC := ConvertedValue(ReferenceRanges.TT3.ht, T3_MOLAR_MASS, ReferenceRanges.TT3.UOM, 'mcg/L');
      NormDefinitions.LPC := ConvertedValue(ReferenceRanges.TT3.lp, T3_MOLAR_MASS, ReferenceRanges.TT3.UOM, 'mcg/L');
      NormDefinitions.HPC := ConvertedValue(ReferenceRanges.TT3.hp, T3_MOLAR_MASS, ReferenceRanges.TT3.UOM, 'mcg/L');
    end
    else
    begin
      NormDefinitions.UOMS := 'N/A';
      NormDefinitions.UOMC := ReferenceRanges.TT3.UOM;
      NormDefinitions.LS  := ConvertedValue(ReferenceRanges.TT3.ln, T3_MOLAR_MASS, ReferenceRanges.TT3.UOM, 'pmol/L');
      NormDefinitions.HS  := ConvertedValue(ReferenceRanges.TT3.hn, T3_MOLAR_MASS, ReferenceRanges.TT3.UOM, 'pmol/L');
      NormDefinitions.LTS := ConvertedValue(ReferenceRanges.TT3.lt, T3_MOLAR_MASS, ReferenceRanges.TT3.UOM, 'pmol/L');
      NormDefinitions.HTS := ConvertedValue(ReferenceRanges.TT3.ht, T3_MOLAR_MASS, ReferenceRanges.TT3.UOM, 'pmol/L');
      NormDefinitions.LPS := ConvertedValue(ReferenceRanges.TT3.lp, T3_MOLAR_MASS, ReferenceRanges.TT3.UOM, 'pmol/L');
      NormDefinitions.HPS := ConvertedValue(ReferenceRanges.TT3.hp, T3_MOLAR_MASS, ReferenceRanges.TT3.UOM, 'pmol/L');
      NormDefinitions.LC  := ReferenceRanges.TT3.ln;
      NormDefinitions.HC  := ReferenceRanges.TT3.hn;
      NormDefinitions.LTC := ReferenceRanges.TT3.lt;
      NormDefinitions.HTC := ReferenceRanges.TT3.ht;
      NormDefinitions.LPC := ReferenceRanges.TT3.lp;
      NormDefinitions.HPC := ReferenceRanges.TT3.hp;
    end;
    NormDefinitions.startDateTime := ISO8601Date(now);
    AddSubNormNodes(Doc, BaseTestNode, NormDefinitions);

    theNode := Doc.CreateComment('Add additional age classes here');
    BaseTestNode.AppendChild(theNode);

    ExclusionDefinitions.Sex  := 'M';
    ExclusionDefinitions.AgeL := 0;
    ExclusionDefinitions.AgeH := 999;
    ExclusionDefinitions.UOMS := 'nmol/L';
    ExclusionDefinitions.UOMC := 'mcg/L';
    ExclusionDefinitions.LXS  := 0;
    ExclusionDefinitions.HXS  := 154;
    ExclusionDefinitions.LXC  := 0;
    ExclusionDefinitions.HXC  := 100;
    ExclusionDefinitions.startDateTime := '2000-01-01T12:00:00+01:00';
    AddSubExNodes(Doc, BaseTestNode, ExclusionDefinitions);
    NormDefinitions.Sex  := 'M';
    NormDefinitions.AgeL := 0;
    NormDefinitions.AgeH := 130;
    NormDefinitions.UOMS := 'nmol/L';
    NormDefinitions.UOMC := 'mcg/L';
    if pos(UpCase('mol'), UpCase(ReferenceRanges.TT3.UOM)) > 0 then
    begin
      NormDefinitions.UOMS := ReferenceRanges.TT3.UOM;
      NormDefinitions.UOMC := 'N/A';
      NormDefinitions.LS  := ReferenceRanges.TT3.ln;
      NormDefinitions.HS  := ReferenceRanges.TT3.hn;
      NormDefinitions.LTS := ReferenceRanges.TT3.lt;
      NormDefinitions.HTS := ReferenceRanges.TT3.ht;
      NormDefinitions.LPS := ReferenceRanges.TT3.lp;
      NormDefinitions.HPS := ReferenceRanges.TT3.hp;
      NormDefinitions.LC  := ConvertedValue(ReferenceRanges.TT3.ln, T3_MOLAR_MASS, ReferenceRanges.TT3.UOM, 'mcg/L');
      NormDefinitions.HC  := ConvertedValue(ReferenceRanges.TT3.hn, T3_MOLAR_MASS, ReferenceRanges.TT3.UOM, 'mcg/L');
      NormDefinitions.LTC := ConvertedValue(ReferenceRanges.TT3.lt, T3_MOLAR_MASS, ReferenceRanges.TT3.UOM, 'mcg/L');
      NormDefinitions.HTC := ConvertedValue(ReferenceRanges.TT3.ht, T3_MOLAR_MASS, ReferenceRanges.TT3.UOM, 'mcg/L');
      NormDefinitions.LPC := ConvertedValue(ReferenceRanges.TT3.lp, T3_MOLAR_MASS, ReferenceRanges.TT3.UOM, 'mcg/L');
      NormDefinitions.HPC := ConvertedValue(ReferenceRanges.TT3.hp, T3_MOLAR_MASS, ReferenceRanges.TT3.UOM, 'mcg/L');
    end
    else
    begin
      NormDefinitions.UOMS := 'N/A';
      NormDefinitions.UOMC := ReferenceRanges.TT3.UOM;
      NormDefinitions.LS  := ConvertedValue(ReferenceRanges.TT3.ln, T3_MOLAR_MASS, ReferenceRanges.TT3.UOM, 'pmol/L');
      NormDefinitions.HS  := ConvertedValue(ReferenceRanges.TT3.hn, T3_MOLAR_MASS, ReferenceRanges.TT3.UOM, 'pmol/L');
      NormDefinitions.LTS := ConvertedValue(ReferenceRanges.TT3.lt, T3_MOLAR_MASS, ReferenceRanges.TT3.UOM, 'pmol/L');
      NormDefinitions.HTS := ConvertedValue(ReferenceRanges.TT3.ht, T3_MOLAR_MASS, ReferenceRanges.TT3.UOM, 'pmol/L');
      NormDefinitions.LPS := ConvertedValue(ReferenceRanges.TT3.lp, T3_MOLAR_MASS, ReferenceRanges.TT3.UOM, 'pmol/L');
      NormDefinitions.HPS := ConvertedValue(ReferenceRanges.TT3.hp, T3_MOLAR_MASS, ReferenceRanges.TT3.UOM, 'pmol/L');
      NormDefinitions.LC  := ReferenceRanges.TT3.ln;
      NormDefinitions.HC  := ReferenceRanges.TT3.hn;
      NormDefinitions.LTC := ReferenceRanges.TT3.lt;
      NormDefinitions.HTC := ReferenceRanges.TT3.ht;
      NormDefinitions.LPC := ReferenceRanges.TT3.lp;
      NormDefinitions.HPC := ReferenceRanges.TT3.hp;
    end;
    NormDefinitions.startDateTime := ISO8601Date(now);
    AddSubNormNodes(Doc, BaseTestNode, NormDefinitions);

    {SPINA:}

    BatteryNode := Doc.CreateElement('BaseBattery');
    TDOMElement(BatteryNode).SetAttribute('ID', 'SPIt');
    TDOMElement(BatteryNode).SetAttribute('Name', 'SPINA Thyr');
    parentNode.Appendchild(BatteryNode);

    {SPINA-GT:}

    BaseTestNode := Doc.CreateElement('BaseTest');
    TDOMElement(BaseTestNode).SetAttribute('DefiningEntity', 'C');
    theNode := Doc.CreateComment('Definitions for SPINA-GT:');
    BaseTestNode.AppendChild(theNode);
    BatteryNode.Appendchild(BaseTestNode);
    LabTestNode := Doc.CreateElement('LabTest');
    TDOMElement(LabTestNode).SetAttribute('ID', 'GT');
    TDOMElement(LabTestNode).SetAttribute('Name', 'Thyroid''s Secretory Capacity');
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
    NormDefinitions.LS  := ReferenceRanges.GT.ln;
    NormDefinitions.HS  := ReferenceRanges.GT.hn;
    NormDefinitions.LTS := ReferenceRanges.GT.lt;
    NormDefinitions.HTS := ReferenceRanges.GT.ht;
    NormDefinitions.LPS := ReferenceRanges.GT.lp;
    NormDefinitions.HPS := ReferenceRanges.GT.hp;
    NormDefinitions.LC  := ReferenceRanges.GT.ln;
    NormDefinitions.HC  := ReferenceRanges.GT.hn;
    NormDefinitions.LTC := ReferenceRanges.GT.lt;
    NormDefinitions.HTC := ReferenceRanges.GT.ht;
    NormDefinitions.LPC := ReferenceRanges.GT.lp;
    NormDefinitions.HPC := ReferenceRanges.GT.hp;
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
    NormDefinitions.LS  := ReferenceRanges.GT.ln;
    NormDefinitions.HS  := ReferenceRanges.GT.hn;
    NormDefinitions.LTS := ReferenceRanges.GT.lt;
    NormDefinitions.HTS := ReferenceRanges.GT.ht;
    NormDefinitions.LPS := ReferenceRanges.GT.lp;
    NormDefinitions.HPS := ReferenceRanges.GT.hp;
    NormDefinitions.LC  := ReferenceRanges.GT.ln;
    NormDefinitions.HC  := ReferenceRanges.GT.hn;
    NormDefinitions.LTC := ReferenceRanges.GT.lt;
    NormDefinitions.HTC := ReferenceRanges.GT.ht;
    NormDefinitions.LPC := ReferenceRanges.GT.lp;
    NormDefinitions.HPC := ReferenceRanges.GT.hp;
    NormDefinitions.startDateTime := ISO8601Date(now);
    AddSubNormNodes(Doc, BaseTestNode, NormDefinitions);

    {SPINA-GD:}

    BaseTestNode := Doc.CreateElement('BaseTest');
    TDOMElement(BaseTestNode).SetAttribute('DefiningEntity', 'C');
    theNode := Doc.CreateComment('Definitions for SPINA-GD:');
    BaseTestNode.AppendChild(theNode);
    BatteryNode.Appendchild(BaseTestNode);
    LabTestNode := Doc.CreateElement('LabTest');
    TDOMElement(LabTestNode).SetAttribute('ID', 'GD');
    TDOMElement(LabTestNode).SetAttribute('Name',
      'Sum activity of peripheral type 1 deiodinase');
    BaseTestNode.Appendchild(LabTestNode);

    ExclusionDefinitions.Sex  := 'F';
    ExclusionDefinitions.AgeL := 0;
    ExclusionDefinitions.AgeH := 999;
    ExclusionDefinitions.UOMS := 'nmol/s';
    ExclusionDefinitions.UOMC := 'nmol/s';
    ExclusionDefinitions.LXS  := 0;
    ExclusionDefinitions.HXS  := 1000;
    ExclusionDefinitions.LXC  := 0;
    ExclusionDefinitions.HXC  := 1000;
    AddSubExNodes(Doc, BaseTestNode, ExclusionDefinitions);
    ExclusionDefinitions.startDateTime := '2000-01-01T12:00:00+01:00';
    NormDefinitions.Sex  := 'F';
    NormDefinitions.AgeL := 0;
    NormDefinitions.AgeH := 130;
    NormDefinitions.UOMS := 'nmol/s';
    NormDefinitions.UOMC := 'nmol/s';
    NormDefinitions.LS  := ReferenceRanges.GD.ln;
    NormDefinitions.HS  := ReferenceRanges.GD.hn;
    NormDefinitions.LTS := ReferenceRanges.GD.lt;
    NormDefinitions.HTS := ReferenceRanges.GD.ht;
    NormDefinitions.LPS := ReferenceRanges.GD.lp;
    NormDefinitions.HPS := ReferenceRanges.GD.hp;
    NormDefinitions.LC  := ReferenceRanges.GD.ln;
    NormDefinitions.HC  := ReferenceRanges.GD.hn;
    NormDefinitions.LTC := ReferenceRanges.GD.lt;
    NormDefinitions.HTC := ReferenceRanges.GD.ht;
    NormDefinitions.LPC := ReferenceRanges.GD.lp;
    NormDefinitions.HPC := ReferenceRanges.GD.hp;
    NormDefinitions.startDateTime := ISO8601Date(now);
    AddSubNormNodes(Doc, BaseTestNode, NormDefinitions);

    theNode := Doc.CreateComment('Add additional age classes here');
    BaseTestNode.AppendChild(theNode);

    ExclusionDefinitions.Sex  := 'M';
    ExclusionDefinitions.AgeL := 0;
    ExclusionDefinitions.AgeH := 999;
    ExclusionDefinitions.UOMS := 'nmol/s';
    ExclusionDefinitions.UOMC := 'nmol/s';
    ExclusionDefinitions.LXS  := 0;
    ExclusionDefinitions.HXS  := 1000;
    ExclusionDefinitions.LXC  := 0;
    ExclusionDefinitions.HXC  := 1000;
    ExclusionDefinitions.startDateTime := '2000-01-01T12:00:00+01:00';
    AddSubExNodes(Doc, BaseTestNode, ExclusionDefinitions);
    NormDefinitions.Sex  := 'M';
    NormDefinitions.AgeL := 0;
    NormDefinitions.AgeH := 130;
    NormDefinitions.UOMS := 'nmol/s';
    NormDefinitions.UOMC := 'nmol/s';
    NormDefinitions.LS  := ReferenceRanges.GD.ln;
    NormDefinitions.HS  := ReferenceRanges.GD.hn;
    NormDefinitions.LTS := ReferenceRanges.GD.lt;
    NormDefinitions.HTS := ReferenceRanges.GD.ht;
    NormDefinitions.LPS := ReferenceRanges.GD.lp;
    NormDefinitions.HPS := ReferenceRanges.GD.hp;
    NormDefinitions.LC  := ReferenceRanges.GD.ln;
    NormDefinitions.HC  := ReferenceRanges.GD.hn;
    NormDefinitions.LTC := ReferenceRanges.GD.lt;
    NormDefinitions.HTC := ReferenceRanges.GD.ht;
    NormDefinitions.LPC := ReferenceRanges.GD.lp;
    NormDefinitions.HPC := ReferenceRanges.GD.hp;
    NormDefinitions.startDateTime := ISO8601Date(now);
    AddSubNormNodes(Doc, BaseTestNode, NormDefinitions);

    {Other structure parameters:}

    BatteryNode := Doc.CreateElement('BaseBattery');
    TDOMElement(BatteryNode).SetAttribute('ID', 'Other');
    TDOMElement(BatteryNode).SetAttribute('Name', 'Other structure parameters');
    parentNode.Appendchild(BatteryNode);

    {TSHI:}

    BaseTestNode := Doc.CreateElement('BaseTest');
    TDOMElement(BaseTestNode).SetAttribute('DefiningEntity', 'C');
    theNode := Doc.CreateComment('Definitions for TSHI:');
    BaseTestNode.AppendChild(theNode);
    BatteryNode.Appendchild(BaseTestNode);
    LabTestNode := Doc.CreateElement('LabTest');
    TDOMElement(LabTestNode).SetAttribute('ID', 'TSHI');
    TDOMElement(LabTestNode).SetAttribute('Name', 'Jostel''s TSH index');
    BaseTestNode.Appendchild(LabTestNode);

    ExclusionDefinitions.Sex  := 'F';
    ExclusionDefinitions.AgeL := 0;
    ExclusionDefinitions.AgeH := 999;
    ExclusionDefinitions.UOMS := '';
    ExclusionDefinitions.UOMC := '';
    ExclusionDefinitions.LXS  := 0;
    ExclusionDefinitions.HXS  := 100;
    ExclusionDefinitions.LXC  := 0;
    ExclusionDefinitions.HXC  := 100;
    ExclusionDefinitions.startDateTime := '2012-08-01T12:00:00+01:00';
    AddSubExNodes(Doc, BaseTestNode, ExclusionDefinitions);
    NormDefinitions.Sex  := 'F';
    NormDefinitions.AgeL := 0;
    NormDefinitions.AgeH := 130;
    NormDefinitions.UOMS := '';
    NormDefinitions.UOMC := '';
    NormDefinitions.LS  := ReferenceRanges.TSHI.ln;
    NormDefinitions.HS  := ReferenceRanges.TSHI.hn;
    NormDefinitions.LTS := ReferenceRanges.TSHI.lt;
    NormDefinitions.HTS := ReferenceRanges.TSHI.ht;
    NormDefinitions.LPS := ReferenceRanges.TSHI.lp;
    NormDefinitions.HPS := ReferenceRanges.TSHI.hp;
    NormDefinitions.LC  := ReferenceRanges.TSHI.ln;
    NormDefinitions.HC  := ReferenceRanges.TSHI.hn;
    NormDefinitions.LTC := ReferenceRanges.TSHI.lt;
    NormDefinitions.HTC := ReferenceRanges.TSHI.ht;
    NormDefinitions.LPC := ReferenceRanges.TSHI.lp;
    NormDefinitions.HPC := ReferenceRanges.TSHI.hp;
    NormDefinitions.startDateTime := ISO8601Date(now);
    AddSubNormNodes(Doc, BaseTestNode, NormDefinitions);

    theNode := Doc.CreateComment('Add additional age classes here');
    BaseTestNode.AppendChild(theNode);

    ExclusionDefinitions.Sex  := 'M';
    ExclusionDefinitions.AgeL := 0;
    ExclusionDefinitions.AgeH := 999;
    ExclusionDefinitions.UOMS := '';
    ExclusionDefinitions.UOMC := '';
    ExclusionDefinitions.LXS  := 0;
    ExclusionDefinitions.HXS  := 100;
    ExclusionDefinitions.LXC  := 0;
    ExclusionDefinitions.HXC  := 100;
    ExclusionDefinitions.startDateTime := '2012-08-01T12:00:00+01:00';
    AddSubExNodes(Doc, BaseTestNode, ExclusionDefinitions);
    NormDefinitions.Sex  := 'M';
    NormDefinitions.AgeL := 0;
    NormDefinitions.AgeH := 130;
    NormDefinitions.UOMS := '';
    NormDefinitions.UOMC := '';
    NormDefinitions.LS  := ReferenceRanges.TSHI.ln;
    NormDefinitions.HS  := ReferenceRanges.TSHI.hn;
    NormDefinitions.LTS := ReferenceRanges.TSHI.lt;
    NormDefinitions.HTS := ReferenceRanges.TSHI.ht;
    NormDefinitions.LPS := ReferenceRanges.TSHI.lp;
    NormDefinitions.HPS := ReferenceRanges.TSHI.hp;
    NormDefinitions.LC  := ReferenceRanges.TSHI.ln;
    NormDefinitions.HC  := ReferenceRanges.TSHI.hn;
    NormDefinitions.LTC := ReferenceRanges.TSHI.lt;
    NormDefinitions.HTC := ReferenceRanges.TSHI.ht;
    NormDefinitions.LPC := ReferenceRanges.TSHI.lp;
    NormDefinitions.HPC := ReferenceRanges.TSHI.hp;
    NormDefinitions.startDateTime := ISO8601Date(now);
    AddSubNormNodes(Doc, BaseTestNode, NormDefinitions);

    {TTSI:}

    BaseTestNode := Doc.CreateElement('BaseTest');
    TDOMElement(BaseTestNode).SetAttribute('DefiningEntity', 'C');
    theNode := Doc.CreateComment('Definitions for TTSI:');
    BaseTestNode.AppendChild(theNode);
    BatteryNode.Appendchild(BaseTestNode);
    LabTestNode := Doc.CreateElement('LabTest');
    TDOMElement(LabTestNode).SetAttribute('ID', 'TTSI');
    TDOMElement(LabTestNode).SetAttribute('Name',
      'Thyrotroph thyroid hormone sensitivity index');
    BaseTestNode.Appendchild(LabTestNode);

    ExclusionDefinitions.Sex  := 'F';
    ExclusionDefinitions.AgeL := 0;
    ExclusionDefinitions.AgeH := 999;
    ExclusionDefinitions.UOMS := '';
    ExclusionDefinitions.UOMC := '';
    ExclusionDefinitions.LXS  := 0;
    ExclusionDefinitions.HXS  := 1000;
    ExclusionDefinitions.LXC  := 0;
    ExclusionDefinitions.HXC  := 1000;
    ExclusionDefinitions.startDateTime := '2012-08-01T12:00:00+01:00';
    AddSubExNodes(Doc, BaseTestNode, ExclusionDefinitions);
    NormDefinitions.Sex  := 'F';
    NormDefinitions.AgeL := 0;
    NormDefinitions.AgeH := 130;
    NormDefinitions.UOMS := '';
    NormDefinitions.UOMC := '';
    NormDefinitions.LS  := ReferenceRanges.TTSI.ln;
    NormDefinitions.HS  := ReferenceRanges.TTSI.hn;
    NormDefinitions.LTS := ReferenceRanges.TTSI.lt;
    NormDefinitions.HTS := ReferenceRanges.TTSI.ht;
    NormDefinitions.LPS := ReferenceRanges.TTSI.lp;
    NormDefinitions.HPS := ReferenceRanges.TTSI.hp;
    NormDefinitions.LC  := ReferenceRanges.TTSI.ln;
    NormDefinitions.HC  := ReferenceRanges.TTSI.hn;
    NormDefinitions.LTC := ReferenceRanges.TTSI.lt;
    NormDefinitions.HTC := ReferenceRanges.TTSI.ht;
    NormDefinitions.LPC := ReferenceRanges.TTSI.lp;
    NormDefinitions.HPC := ReferenceRanges.TTSI.hp;
    NormDefinitions.startDateTime := ISO8601Date(now);
    AddSubNormNodes(Doc, BaseTestNode, NormDefinitions);

    theNode := Doc.CreateComment('Add additional age classes here');
    BaseTestNode.AppendChild(theNode);

    ExclusionDefinitions.Sex  := 'F';
    ExclusionDefinitions.AgeL := 0;
    ExclusionDefinitions.AgeH := 999;
    ExclusionDefinitions.UOMS := '';
    ExclusionDefinitions.UOMC := '';
    ExclusionDefinitions.LXS  := 0;
    ExclusionDefinitions.HXS  := 1000;
    ExclusionDefinitions.LXC  := 0;
    ExclusionDefinitions.HXC  := 1000;
    ExclusionDefinitions.startDateTime := '2012-08-01T12:00:00+01:00';
    AddSubExNodes(Doc, BaseTestNode, ExclusionDefinitions);
    NormDefinitions.Sex  := 'F';
    NormDefinitions.AgeL := 0;
    NormDefinitions.AgeH := 130;
    NormDefinitions.UOMS := '';
    NormDefinitions.UOMC := '';
    NormDefinitions.LS  := ReferenceRanges.TTSI.ln;
    NormDefinitions.HS  := ReferenceRanges.TTSI.hn;
    NormDefinitions.LTS := ReferenceRanges.TTSI.lt;
    NormDefinitions.HTS := ReferenceRanges.TTSI.ht;
    NormDefinitions.LPS := ReferenceRanges.TTSI.lp;
    NormDefinitions.HPS := ReferenceRanges.TTSI.hp;
    NormDefinitions.LC  := ReferenceRanges.TTSI.ln;
    NormDefinitions.HC  := ReferenceRanges.TTSI.hn;
    NormDefinitions.LTC := ReferenceRanges.TTSI.lt;
    NormDefinitions.HTC := ReferenceRanges.TTSI.ht;
    NormDefinitions.LPC := ReferenceRanges.TTSI.lp;
    NormDefinitions.HPC := ReferenceRanges.TTSI.hp;
    NormDefinitions.startDateTime := ISO8601Date(now);
    AddSubNormNodes(Doc, BaseTestNode, NormDefinitions);

    writeXMLFile(Doc, UTF8ToSys(theFileName));

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
    theNode := Doc.CreateComment('(c) J. W. Dietrich, 1994 - 2014');
    Doc.AppendChild(theNode);
    theNode := Doc.CreateComment(
      '(c) Ludwig Maximilian University of Munich 1995 - 2002');
    Doc.AppendChild(theNode);
    theNode := Doc.CreateComment('(c) University of Ulm Hospitals 2002-2004');
    Doc.AppendChild(theNode);
    theNode := Doc.CreateComment('(c) Ruhr University of Bochum 2005 - 2014');
    Doc.AppendChild(theNode);
    theNode := Doc.CreateComment('(released under the BSD License');
    Doc.AppendChild(theNode);
    RootNode := Doc.CreateElement('GTP');
    TDOMElement(RootNode).SetAttribute('CreationDateTime', '2014-03-05T13:13:13+01:00');
    TDOMElement(RootNode).SetAttribute('ModelVersion', '01-0-01');
    Doc.Appendchild(RootNode);
    RootNode   := Doc.DocumentElement;
    parentNode := Doc.CreateElement('TransmissionSource');
    TDOMElement(parentNode).SetAttribute('ID', 'www.ruhr-uni-bochum.de/bergmannsheil/');
    TDOMElement(parentNode).SetAttribute('Name', 'Bergmannsheil University Hospitals');
    RootNode.Appendchild(parentNode);
    parentNode := Doc.CreateElement('Study');
    TDOMElement(parentNode).SetAttribute('ID', 'SPIt');
    TDOMElement(parentNode).SetAttribute('Name', 'Reference Ranges for SPINA Thyr');
    TDOMElement(parentNode).SetAttribute('TransmissionType', 'C');
    RootNode.Appendchild(parentNode);

    {Standard hormone levels:}

    BatteryNode := Doc.CreateElement('BaseBattery');
    TDOMElement(BatteryNode).SetAttribute('ID', 'Thyroid_Hormones');
    TDOMElement(BatteryNode).SetAttribute('Name', 'SPINA Thyr');
    parentNode.Appendchild(BatteryNode);

    {TSH:}

    BaseTestNode := Doc.CreateElement('BaseTest');
    TDOMElement(BaseTestNode).SetAttribute('DefiningEntity', 'C');
    theNode := Doc.CreateComment('Definitions for TSH:');
    BaseTestNode.AppendChild(theNode);
    BatteryNode.Appendchild(BaseTestNode);
    LabTestNode := Doc.CreateElement('LabTest');
    TDOMElement(LabTestNode).SetAttribute('ID', 'TSH');
    TDOMElement(LabTestNode).SetAttribute('Name', 'Thyrotropin');
    BaseTestNode.Appendchild(LabTestNode);

    ExclusionDefinitions.Sex  := 'F';
    ExclusionDefinitions.AgeL := 0;
    ExclusionDefinitions.AgeH := 999;
    ExclusionDefinitions.UOMS := 'mIU/L';
    ExclusionDefinitions.UOMC := 'mIU/L';
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
    NormDefinitions.LS  := 0.35;
    NormDefinitions.HS  := 3.5;
    NormDefinitions.LTS := 0.1;
    NormDefinitions.HTS := 100;
    NormDefinitions.LPS := NaN;
    NormDefinitions.HPS := 500;
    NormDefinitions.LC  := 0.35;
    NormDefinitions.HC  := 3.5;
    NormDefinitions.LTC := 0.1;
    NormDefinitions.HTC := 100;
    NormDefinitions.LPC := NaN;
    NormDefinitions.HPC := 500;
    NormDefinitions.startDateTime := '2012-08-01T12:00:00+01:00';
    AddSubNormNodes(Doc, BaseTestNode, NormDefinitions);

    theNode := Doc.CreateComment('Add additional age classes here');
    BaseTestNode.AppendChild(theNode);

    ExclusionDefinitions.Sex  := 'M';
    ExclusionDefinitions.AgeL := 0;
    ExclusionDefinitions.AgeH := 999;
    ExclusionDefinitions.UOMS := 'mIU/L';
    ExclusionDefinitions.UOMC := 'mIU/L';
    ExclusionDefinitions.LXS  := 0;
    ExclusionDefinitions.HXS  := 10000;
    ExclusionDefinitions.LXC  := 0;
    ExclusionDefinitions.HXC  := 10000;
    ExclusionDefinitions.startDateTime := '2000-01-01T12:00:00+01:00';
    AddSubExNodes(Doc, BaseTestNode, ExclusionDefinitions);
    NormDefinitions.Sex  := 'M';
    NormDefinitions.AgeL := 0;
    NormDefinitions.AgeH := 130;
    NormDefinitions.UOMS := 'mIU/L';
    NormDefinitions.UOMC := 'mIU/L';
    NormDefinitions.LS  := 0.35;
    NormDefinitions.HS  := 3.5;
    NormDefinitions.LTS := 0.1;
    NormDefinitions.HTS := 100;
    NormDefinitions.LPS := NaN;
    NormDefinitions.HPS := 500;
    NormDefinitions.LC  := 0.35;
    NormDefinitions.HC  := 3.5;
    NormDefinitions.LTC := 0.1;
    NormDefinitions.HTC := 100;
    NormDefinitions.LPC := NaN;
    NormDefinitions.HPC := 500;
    NormDefinitions.startDateTime := '2012-08-01T12:00:00+01:00';
    AddSubNormNodes(Doc, BaseTestNode, NormDefinitions);

    {FT4:}

    BaseTestNode := Doc.CreateElement('BaseTest');
    TDOMElement(BaseTestNode).SetAttribute('DefiningEntity', 'C');
    theNode := Doc.CreateComment('Definitions for FT4:');
    BaseTestNode.AppendChild(theNode);
    BatteryNode.Appendchild(BaseTestNode);
    LabTestNode := Doc.CreateElement('LabTest');
    TDOMElement(LabTestNode).SetAttribute('ID', 'FT4');
    TDOMElement(LabTestNode).SetAttribute('Name', 'Free T4');
    BaseTestNode.Appendchild(LabTestNode);

    ExclusionDefinitions.Sex  := 'F';
    ExclusionDefinitions.AgeL := 0;
    ExclusionDefinitions.AgeH := 999;
    ExclusionDefinitions.UOMS := 'pmol/L';
    ExclusionDefinitions.UOMC := 'ng/L';
    ExclusionDefinitions.LXS  := 0;
    ExclusionDefinitions.HXS  := 1287;
    ExclusionDefinitions.LXC  := 0;
    ExclusionDefinitions.HXC  := 1000;
    ExclusionDefinitions.startDateTime := '2000-01-01T12:00:00+01:00';
    AddSubExNodes(Doc, BaseTestNode, ExclusionDefinitions);
    NormDefinitions.Sex  := 'F';
    NormDefinitions.AgeL := 0;
    NormDefinitions.AgeH := 130;
    NormDefinitions.UOMS := 'pmol/L';
    NormDefinitions.UOMC := 'ng/L';
    NormDefinitions.LS  := 7.7;
    NormDefinitions.HS  := 18;
    NormDefinitions.LTS := 7.7;
    NormDefinitions.HTS := 18;
    NormDefinitions.LPS := 7.7;
    NormDefinitions.HPS := 18;
    NormDefinitions.LC  := 6;
    NormDefinitions.HC  := 14;
    NormDefinitions.LTC := 6;
    NormDefinitions.HTC := 14;
    NormDefinitions.LPC := 6;
    NormDefinitions.HPC := 14;
    NormDefinitions.startDateTime := '2012-08-01T12:00:00+01:00';
    AddSubNormNodes(Doc, BaseTestNode, NormDefinitions);

    theNode := Doc.CreateComment('Add additional age classes here');
    BaseTestNode.AppendChild(theNode);

    ExclusionDefinitions.Sex  := 'M';
    ExclusionDefinitions.AgeL := 0;
    ExclusionDefinitions.AgeH := 999;
    ExclusionDefinitions.UOMS := 'pmol/L';
    ExclusionDefinitions.UOMC := 'ng/L';
    ExclusionDefinitions.LXS  := 0;
    ExclusionDefinitions.HXS  := 1287;
    ExclusionDefinitions.LXC  := 0;
    ExclusionDefinitions.HXC  := 1000;
    ExclusionDefinitions.startDateTime := '2000-01-01T12:00:00+01:00';
    AddSubExNodes(Doc, BaseTestNode, ExclusionDefinitions);
    NormDefinitions.Sex  := 'M';
    NormDefinitions.AgeL := 0;
    NormDefinitions.AgeH := 130;
    NormDefinitions.UOMS := 'pmol/L';
    NormDefinitions.UOMC := 'ng/L';
    NormDefinitions.LS  := 7.7;
    NormDefinitions.HS  := 18;
    NormDefinitions.LTS := 7.7;
    NormDefinitions.HTS := 18;
    NormDefinitions.LPS := 7.7;
    NormDefinitions.HPS := 18;
    NormDefinitions.LC  := 6;
    NormDefinitions.HC  := 14;
    NormDefinitions.LTC := 6;
    NormDefinitions.HTC := 14;
    NormDefinitions.LPC := 6;
    NormDefinitions.HPC := 14;
    NormDefinitions.startDateTime := '2012-08-01T12:00:00+01:00';
    AddSubNormNodes(Doc, BaseTestNode, NormDefinitions);

    {FT3:}

    BaseTestNode := Doc.CreateElement('BaseTest');
    TDOMElement(BaseTestNode).SetAttribute('DefiningEntity', 'C');
    theNode := Doc.CreateComment('Definitions for FT3:');
    BaseTestNode.AppendChild(theNode);
    BatteryNode.Appendchild(BaseTestNode);
    LabTestNode := Doc.CreateElement('LabTest');
    TDOMElement(LabTestNode).SetAttribute('ID', 'FT3');
    TDOMElement(LabTestNode).SetAttribute('Name', 'Free T3');
    BaseTestNode.Appendchild(LabTestNode);

    ExclusionDefinitions.Sex  := 'F';
    ExclusionDefinitions.AgeL := 0;
    ExclusionDefinitions.AgeH := 999;
    ExclusionDefinitions.UOMS := 'pmol/L';
    ExclusionDefinitions.UOMC := 'ng/L';
    ExclusionDefinitions.LXS  := 0;
    ExclusionDefinitions.HXS  := 100;
    ExclusionDefinitions.LXC  := 0;
    ExclusionDefinitions.HXC  := 65;
    ExclusionDefinitions.startDateTime := '2000-01-01T12:00:00+01:00';
    AddSubExNodes(Doc, BaseTestNode, ExclusionDefinitions);
    NormDefinitions.Sex  := 'F';
    NormDefinitions.AgeL := 0;
    NormDefinitions.AgeH := 130;
    NormDefinitions.UOMS := 'pmol/L';
    NormDefinitions.UOMC := 'ng/L';
    NormDefinitions.LS  := 3.5;
    NormDefinitions.HS  := 6.3;
    NormDefinitions.LTS := 3.5;
    NormDefinitions.HTS := 6.3;
    NormDefinitions.LPS := 3.5;
    NormDefinitions.HPS := 6.3;
    NormDefinitions.LC  := 2.3;
    NormDefinitions.HC  := 4.1;
    NormDefinitions.LTC := 2.3;
    NormDefinitions.HTC := 4.1;
    NormDefinitions.LPC := 2.3;
    NormDefinitions.HPC := 4.1;
    NormDefinitions.startDateTime := '2012-08-01T12:00:00+01:00';
    AddSubNormNodes(Doc, BaseTestNode, NormDefinitions);

    theNode := Doc.CreateComment('Add additional age classes here');
    BaseTestNode.AppendChild(theNode);

    ExclusionDefinitions.Sex  := 'M';
    ExclusionDefinitions.AgeL := 0;
    ExclusionDefinitions.AgeH := 999;
    ExclusionDefinitions.UOMS := 'pmol/L';
    ExclusionDefinitions.UOMC := 'ng/L';
    ExclusionDefinitions.LXS  := 0;
    ExclusionDefinitions.HXS  := 100;
    ExclusionDefinitions.LXC  := 0;
    ExclusionDefinitions.HXC  := 65;
    ExclusionDefinitions.startDateTime := '2000-01-01T12:00:00+01:00';
    AddSubExNodes(Doc, BaseTestNode, ExclusionDefinitions);
    NormDefinitions.Sex  := 'M';
    NormDefinitions.AgeL := 0;
    NormDefinitions.AgeH := 130;
    NormDefinitions.UOMS := 'pmol/L';
    NormDefinitions.UOMC := 'ng/L';
    NormDefinitions.LS  := 3.5;
    NormDefinitions.HS  := 6.3;
    NormDefinitions.LTS := 3.5;
    NormDefinitions.HTS := 6.3;
    NormDefinitions.LPS := 3.5;
    NormDefinitions.HPS := 6.3;
    NormDefinitions.LC  := 2.3;
    NormDefinitions.HC  := 4.1;
    NormDefinitions.LTC := 2.3;
    NormDefinitions.HTC := 4.1;
    NormDefinitions.LPC := 2.3;
    NormDefinitions.HPC := 4.1;
    NormDefinitions.startDateTime := '2012-08-01T12:00:00+01:00';
    AddSubNormNodes(Doc, BaseTestNode, NormDefinitions);

    {TT4:}

    BaseTestNode := Doc.CreateElement('BaseTest');
    TDOMElement(BaseTestNode).SetAttribute('DefiningEntity', 'C');
    theNode := Doc.CreateComment('Definitions for TT4:');
    BaseTestNode.AppendChild(theNode);
    BatteryNode.Appendchild(BaseTestNode);
    LabTestNode := Doc.CreateElement('LabTest');
    TDOMElement(LabTestNode).SetAttribute('ID', 'TT4');
    TDOMElement(LabTestNode).SetAttribute('Name', 'Total T4');
    BaseTestNode.Appendchild(LabTestNode);

    ExclusionDefinitions.Sex  := 'F';
    ExclusionDefinitions.AgeL := 0;
    ExclusionDefinitions.AgeH := 999;
    ExclusionDefinitions.UOMS := 'nmol/L';
    ExclusionDefinitions.UOMC := 'mcg/L';
    ExclusionDefinitions.LXS  := 0;
    ExclusionDefinitions.HXS  := 1287;
    ExclusionDefinitions.LXC  := 0;
    ExclusionDefinitions.HXC  := 1000;
    ExclusionDefinitions.startDateTime := '2000-01-01T12:00:00+01:00';
    AddSubExNodes(Doc, BaseTestNode, ExclusionDefinitions);
    NormDefinitions.Sex  := 'F';
    NormDefinitions.AgeL := 0;
    NormDefinitions.AgeH := 130;
    NormDefinitions.UOMS := 'nmol/L';
    NormDefinitions.UOMC := 'mcg/L';
    NormDefinitions.LS  := 64.4;
    NormDefinitions.HS  := 144.4;
    NormDefinitions.LTS := 64.4;
    NormDefinitions.HTS := 144.4;
    NormDefinitions.LPS := 64.4;
    NormDefinitions.HPS := 144.4;
    NormDefinitions.LC  := 50;
    NormDefinitions.HC  := 120;
    NormDefinitions.LTC := 50;
    NormDefinitions.HTC := 120;
    NormDefinitions.LPC := 50;
    NormDefinitions.HPC := 120;
    NormDefinitions.startDateTime := '2012-08-01T12:00:00+01:00';
    AddSubNormNodes(Doc, BaseTestNode, NormDefinitions);

    theNode := Doc.CreateComment('Add additional age classes here');
    BaseTestNode.AppendChild(theNode);

    ExclusionDefinitions.Sex  := 'M';
    ExclusionDefinitions.AgeL := 0;
    ExclusionDefinitions.AgeH := 999;
    ExclusionDefinitions.UOMS := 'nmol/L';
    ExclusionDefinitions.UOMC := 'mcg/L';
    ExclusionDefinitions.LXS  := 0;
    ExclusionDefinitions.HXS  := 1287;
    ExclusionDefinitions.LXC  := 0;
    ExclusionDefinitions.HXC  := 1000;
    ExclusionDefinitions.startDateTime := '2000-01-01T12:00:00+01:00';
    AddSubExNodes(Doc, BaseTestNode, ExclusionDefinitions);
    NormDefinitions.Sex  := 'M';
    NormDefinitions.AgeL := 0;
    NormDefinitions.AgeH := 130;
    NormDefinitions.UOMS := 'nmol/L';
    NormDefinitions.UOMC := 'mcg/L';
    NormDefinitions.LS  := 64.4;
    NormDefinitions.HS  := 144.4;
    NormDefinitions.LTS := 64.4;
    NormDefinitions.HTS := 144.4;
    NormDefinitions.LPS := 64.4;
    NormDefinitions.HPS := 144.4;
    NormDefinitions.LC  := 50;
    NormDefinitions.HC  := 120;
    NormDefinitions.LTC := 50;
    NormDefinitions.HTC := 120;
    NormDefinitions.LPC := 50;
    NormDefinitions.HPC := 120;
    NormDefinitions.startDateTime := '2012-08-01T12:00:00+01:00';
    AddSubNormNodes(Doc, BaseTestNode, NormDefinitions);

    {TT3:}

    BaseTestNode := Doc.CreateElement('BaseTest');
    TDOMElement(BaseTestNode).SetAttribute('DefiningEntity', 'C');
    theNode := Doc.CreateComment('Definitions for TT3:');
    BaseTestNode.AppendChild(theNode);
    BatteryNode.Appendchild(BaseTestNode);
    LabTestNode := Doc.CreateElement('LabTest');
    TDOMElement(LabTestNode).SetAttribute('ID', 'TT3');
    TDOMElement(LabTestNode).SetAttribute('Name', 'Total T3');
    BaseTestNode.Appendchild(LabTestNode);

    ExclusionDefinitions.Sex  := 'F';
    ExclusionDefinitions.AgeL := 0;
    ExclusionDefinitions.AgeH := 999;
    ExclusionDefinitions.UOMS := 'nmol/L';
    ExclusionDefinitions.UOMC := 'mcg/L';
    ExclusionDefinitions.LXS  := 0;
    ExclusionDefinitions.HXS  := 154;
    ExclusionDefinitions.LXC  := 0;
    ExclusionDefinitions.HXC  := 100;
    ExclusionDefinitions.startDateTime := '2000-01-01T12:00:00+01:00';
    AddSubExNodes(Doc, BaseTestNode, ExclusionDefinitions);
    NormDefinitions.Sex  := 'F';
    NormDefinitions.AgeL := 0;
    NormDefinitions.AgeH := 130;
    NormDefinitions.UOMS := 'nmol/L';
    NormDefinitions.UOMC := 'mcg/L';
    NormDefinitions.LS  := 1.1;
    NormDefinitions.HS  := 2.8;
    NormDefinitions.LTS := 1.1;
    NormDefinitions.HTS := 2.8;
    NormDefinitions.LPS := 1.1;
    NormDefinitions.HPS := 2.8;
    NormDefinitions.LC  := 0.7;
    NormDefinitions.HC  := 1.8;
    NormDefinitions.LTC := 0.7;
    NormDefinitions.HTC := 1.8;
    NormDefinitions.LPC := 0.7;
    NormDefinitions.HPC := 1.8;
    NormDefinitions.startDateTime := '2012-08-01T12:00:00+01:00';
    AddSubNormNodes(Doc, BaseTestNode, NormDefinitions);

    theNode := Doc.CreateComment('Add additional age classes here');
    BaseTestNode.AppendChild(theNode);

    ExclusionDefinitions.Sex  := 'M';
    ExclusionDefinitions.AgeL := 0;
    ExclusionDefinitions.AgeH := 999;
    ExclusionDefinitions.UOMS := 'nmol/L';
    ExclusionDefinitions.UOMC := 'mcg/L';
    ExclusionDefinitions.LXS  := 0;
    ExclusionDefinitions.HXS  := 154;
    ExclusionDefinitions.LXC  := 0;
    ExclusionDefinitions.HXC  := 100;
    ExclusionDefinitions.startDateTime := '2000-01-01T12:00:00+01:00';
    AddSubExNodes(Doc, BaseTestNode, ExclusionDefinitions);
    NormDefinitions.Sex  := 'M';
    NormDefinitions.AgeL := 0;
    NormDefinitions.AgeH := 130;
    NormDefinitions.UOMS := 'nmol/L';
    NormDefinitions.UOMC := 'mcg/L';
    NormDefinitions.LS  := 1.1;
    NormDefinitions.HS  := 2.8;
    NormDefinitions.LTS := 1.1;
    NormDefinitions.HTS := 2.8;
    NormDefinitions.LPS := 1.1;
    NormDefinitions.HPS := 2.8;
    NormDefinitions.LC  := 0.7;
    NormDefinitions.HC  := 1.8;
    NormDefinitions.LTC := 0.7;
    NormDefinitions.HTC := 1.8;
    NormDefinitions.LPC := 0.7;
    NormDefinitions.HPC := 1.8;
    NormDefinitions.startDateTime := '2012-08-01T12:00:00+01:00';
    AddSubNormNodes(Doc, BaseTestNode, NormDefinitions);

    {SPINA:}

    BatteryNode := Doc.CreateElement('BaseBattery');
    TDOMElement(BatteryNode).SetAttribute('ID', 'SPIt');
    TDOMElement(BatteryNode).SetAttribute('Name', 'SPINA Thyr');
    parentNode.Appendchild(BatteryNode);

    {SPINA-GT:}

    BaseTestNode := Doc.CreateElement('BaseTest');
    TDOMElement(BaseTestNode).SetAttribute('DefiningEntity', 'C');
    theNode := Doc.CreateComment('Definitions for SPINA-GT:');
    BaseTestNode.AppendChild(theNode);
    BatteryNode.Appendchild(BaseTestNode);
    LabTestNode := Doc.CreateElement('LabTest');
    TDOMElement(LabTestNode).SetAttribute('ID', 'GT');
    TDOMElement(LabTestNode).SetAttribute('Name', 'Thyroid''s Secretory Capacity');
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
    NormDefinitions.LS  := 1.41;
    NormDefinitions.HS  := 8.67;
    NormDefinitions.LTS := 1.00;
    NormDefinitions.HTS := 25;
    NormDefinitions.LPS := 0.7;
    NormDefinitions.HPS := 100;
    NormDefinitions.LC  := 1.41;
    NormDefinitions.HC  := 8.67;
    NormDefinitions.LTC := 1.00;
    NormDefinitions.HTC := 25;
    NormDefinitions.LPC := 0.7;
    NormDefinitions.HPC := 100;
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
    NormDefinitions.LS  := 1.41;
    NormDefinitions.HS  := 8.67;
    NormDefinitions.LTS := 1.00;
    NormDefinitions.HTS := 25;
    NormDefinitions.LPS := 0.7;
    NormDefinitions.HPS := 100;
    NormDefinitions.LC  := 1.41;
    NormDefinitions.HC  := 8.67;
    NormDefinitions.LTC := 1.00;
    NormDefinitions.HTC := 25;
    NormDefinitions.LPC := 0.7;
    NormDefinitions.HPC := 100;
    NormDefinitions.startDateTime := '2012-08-01T12:00:00+01:00';
    AddSubNormNodes(Doc, BaseTestNode, NormDefinitions);

    {SPINA-GD:}

    BaseTestNode := Doc.CreateElement('BaseTest');
    TDOMElement(BaseTestNode).SetAttribute('DefiningEntity', 'C');
    theNode := Doc.CreateComment('Definitions for SPINA-GD:');
    BaseTestNode.AppendChild(theNode);
    BatteryNode.Appendchild(BaseTestNode);
    LabTestNode := Doc.CreateElement('LabTest');
    TDOMElement(LabTestNode).SetAttribute('ID', 'GD');
    TDOMElement(LabTestNode).SetAttribute('Name',
      'Sum activity of peripheral type 1 deiodinase');
    BaseTestNode.Appendchild(LabTestNode);

    ExclusionDefinitions.Sex  := 'F';
    ExclusionDefinitions.AgeL := 0;
    ExclusionDefinitions.AgeH := 999;
    ExclusionDefinitions.UOMS := 'nmol/s';
    ExclusionDefinitions.UOMC := 'nmol/s';
    ExclusionDefinitions.LXS  := 0;
    ExclusionDefinitions.HXS  := 1000;
    ExclusionDefinitions.LXC  := 0;
    ExclusionDefinitions.HXC  := 1000;
    AddSubExNodes(Doc, BaseTestNode, ExclusionDefinitions);
    ExclusionDefinitions.startDateTime := '2000-01-01T12:00:00+01:00';
    NormDefinitions.Sex  := 'F';
    NormDefinitions.AgeL := 0;
    NormDefinitions.AgeH := 130;
    NormDefinitions.UOMS := 'nmol/s';
    NormDefinitions.UOMC := 'nmol/s';
    NormDefinitions.LS  := 20;
    NormDefinitions.HS  := 40;
    NormDefinitions.LTS := 10;
    NormDefinitions.HTS := 60;
    NormDefinitions.LPS := 5;
    NormDefinitions.HPS := 100;
    NormDefinitions.LC  := 20;
    NormDefinitions.HC  := 40;
    NormDefinitions.LTC := 10;
    NormDefinitions.HTC := 60;
    NormDefinitions.LPC := 5;
    NormDefinitions.HPC := 100;
    NormDefinitions.startDateTime := '2012-08-01T12:00:00+01:00';
    AddSubNormNodes(Doc, BaseTestNode, NormDefinitions);

    theNode := Doc.CreateComment('Add additional age classes here');
    BaseTestNode.AppendChild(theNode);

    ExclusionDefinitions.Sex  := 'M';
    ExclusionDefinitions.AgeL := 0;
    ExclusionDefinitions.AgeH := 999;
    ExclusionDefinitions.UOMS := 'nmol/s';
    ExclusionDefinitions.UOMC := 'nmol/s';
    ExclusionDefinitions.LXS  := 0;
    ExclusionDefinitions.HXS  := 1000;
    ExclusionDefinitions.LXC  := 0;
    ExclusionDefinitions.HXC  := 1000;
    ExclusionDefinitions.startDateTime := '2000-01-01T12:00:00+01:00';
    AddSubExNodes(Doc, BaseTestNode, ExclusionDefinitions);
    NormDefinitions.Sex  := 'M';
    NormDefinitions.AgeL := 0;
    NormDefinitions.AgeH := 130;
    NormDefinitions.UOMS := 'nmol/s';
    NormDefinitions.UOMC := 'nmol/s';
    NormDefinitions.LS  := 20;
    NormDefinitions.HS  := 40;
    NormDefinitions.LTS := 10;
    NormDefinitions.HTS := 60;
    NormDefinitions.LPS := 5;
    NormDefinitions.HPS := 100;
    NormDefinitions.LC  := 20;
    NormDefinitions.HC  := 40;
    NormDefinitions.LTC := 10;
    NormDefinitions.HTC := 60;
    NormDefinitions.LPC := 5;
    NormDefinitions.HPC := 100;
    NormDefinitions.startDateTime := '2012-08-01T12:00:00+01:00';
    AddSubNormNodes(Doc, BaseTestNode, NormDefinitions);

    {Other structure parameters:}

    BatteryNode := Doc.CreateElement('BaseBattery');
    TDOMElement(BatteryNode).SetAttribute('ID', 'Other');
    TDOMElement(BatteryNode).SetAttribute('Name', 'Other structure parameters');
    parentNode.Appendchild(BatteryNode);

    {TSHI:}

    BaseTestNode := Doc.CreateElement('BaseTest');
    TDOMElement(BaseTestNode).SetAttribute('DefiningEntity', 'C');
    theNode := Doc.CreateComment('Definitions for TSHI:');
    BaseTestNode.AppendChild(theNode);
    BatteryNode.Appendchild(BaseTestNode);
    LabTestNode := Doc.CreateElement('LabTest');
    TDOMElement(LabTestNode).SetAttribute('ID', 'TSHI');
    TDOMElement(LabTestNode).SetAttribute('Name', 'Jostel''s TSH index');
    BaseTestNode.Appendchild(LabTestNode);

    ExclusionDefinitions.Sex  := 'F';
    ExclusionDefinitions.AgeL := 0;
    ExclusionDefinitions.AgeH := 999;
    ExclusionDefinitions.UOMS := '';
    ExclusionDefinitions.UOMC := '';
    ExclusionDefinitions.LXS  := 0;
    ExclusionDefinitions.HXS  := 100;
    ExclusionDefinitions.LXC  := 0;
    ExclusionDefinitions.HXC  := 100;
    ExclusionDefinitions.startDateTime := '2012-08-01T12:00:00+01:00';
    AddSubExNodes(Doc, BaseTestNode, ExclusionDefinitions);
    NormDefinitions.Sex  := 'F';
    NormDefinitions.AgeL := 0;
    NormDefinitions.AgeH := 130;
    NormDefinitions.UOMS := '';
    NormDefinitions.UOMC := '';
    NormDefinitions.LS  := 1.3;
    NormDefinitions.HS  := 4.1;
    NormDefinitions.LTS := 1;
    NormDefinitions.HTS := 6;
    NormDefinitions.LPS := 0.2;
    NormDefinitions.HPS := 10;
    NormDefinitions.LC  := 1.3;
    NormDefinitions.HC  := 4.1;
    NormDefinitions.LTC := 1;
    NormDefinitions.HTC := 6;
    NormDefinitions.LPC := 0.2;
    NormDefinitions.HPC := 10;
    NormDefinitions.startDateTime := '2012-08-01T12:00:00+01:00';
    AddSubNormNodes(Doc, BaseTestNode, NormDefinitions);

    theNode := Doc.CreateComment('Add additional age classes here');
    BaseTestNode.AppendChild(theNode);

    ExclusionDefinitions.Sex  := 'M';
    ExclusionDefinitions.AgeL := 0;
    ExclusionDefinitions.AgeH := 999;
    ExclusionDefinitions.UOMS := '';
    ExclusionDefinitions.UOMC := '';
    ExclusionDefinitions.LXS  := 0;
    ExclusionDefinitions.HXS  := 100;
    ExclusionDefinitions.LXC  := 0;
    ExclusionDefinitions.HXC  := 100;
    ExclusionDefinitions.startDateTime := '2012-08-01T12:00:00+01:00';
    AddSubExNodes(Doc, BaseTestNode, ExclusionDefinitions);
    NormDefinitions.Sex  := 'M';
    NormDefinitions.AgeL := 0;
    NormDefinitions.AgeH := 130;
    NormDefinitions.UOMS := '';
    NormDefinitions.UOMC := '';
    NormDefinitions.LS  := 1.3;
    NormDefinitions.HS  := 4.1;
    NormDefinitions.LTS := 1;
    NormDefinitions.HTS := 6;
    NormDefinitions.LPS := 0.2;
    NormDefinitions.HPS := 10;
    NormDefinitions.LC  := 1.3;
    NormDefinitions.HC  := 4.1;
    NormDefinitions.LTC := 1;
    NormDefinitions.HTC := 6;
    NormDefinitions.LPC := 0.2;
    NormDefinitions.HPC := 10;
    AddSubNormNodes(Doc, BaseTestNode, NormDefinitions);

    {TTSI:}

    BaseTestNode := Doc.CreateElement('BaseTest');
    TDOMElement(BaseTestNode).SetAttribute('DefiningEntity', 'C');
    theNode := Doc.CreateComment('Definitions for TTSI:');
    BaseTestNode.AppendChild(theNode);
    BatteryNode.Appendchild(BaseTestNode);
    LabTestNode := Doc.CreateElement('LabTest');
    TDOMElement(LabTestNode).SetAttribute('ID', 'TTSI');
    TDOMElement(LabTestNode).SetAttribute('Name',
      'Thyrotroph thyroid hormone sensitivity index');
    BaseTestNode.Appendchild(LabTestNode);

    ExclusionDefinitions.Sex  := 'F';
    ExclusionDefinitions.AgeL := 0;
    ExclusionDefinitions.AgeH := 999;
    ExclusionDefinitions.UOMS := '';
    ExclusionDefinitions.UOMC := '';
    ExclusionDefinitions.LXS  := 0;
    ExclusionDefinitions.HXS  := 1000;
    ExclusionDefinitions.LXC  := 0;
    ExclusionDefinitions.HXC  := 1000;
    ExclusionDefinitions.startDateTime := '2012-08-01T12:00:00+01:00';
    AddSubExNodes(Doc, BaseTestNode, ExclusionDefinitions);
    NormDefinitions.Sex  := 'F';
    NormDefinitions.AgeL := 0;
    NormDefinitions.AgeH := 130;
    NormDefinitions.UOMS := '';
    NormDefinitions.UOMC := '';
    NormDefinitions.LS  := 100;
    NormDefinitions.HS  := 150;
    NormDefinitions.LTS := 50;
    NormDefinitions.HTS := 200;
    NormDefinitions.LPS := 10;
    NormDefinitions.HPS := 300;
    NormDefinitions.LC  := 100;
    NormDefinitions.HC  := 150;
    NormDefinitions.LTC := 50;
    NormDefinitions.HTC := 200;
    NormDefinitions.LPC := 10;
    NormDefinitions.HPC := 300;
    NormDefinitions.startDateTime := '2012-08-01T12:00:00+01:00';
    AddSubNormNodes(Doc, BaseTestNode, NormDefinitions);

    theNode := Doc.CreateComment('Add additional age classes here');
    BaseTestNode.AppendChild(theNode);

    ExclusionDefinitions.Sex  := 'F';
    ExclusionDefinitions.AgeL := 0;
    ExclusionDefinitions.AgeH := 999;
    ExclusionDefinitions.UOMS := '';
    ExclusionDefinitions.UOMC := '';
    ExclusionDefinitions.LXS  := 0;
    ExclusionDefinitions.HXS  := 1000;
    ExclusionDefinitions.LXC  := 0;
    ExclusionDefinitions.HXC  := 1000;
    ExclusionDefinitions.startDateTime := '2012-08-01T12:00:00+01:00';
    AddSubExNodes(Doc, BaseTestNode, ExclusionDefinitions);
    NormDefinitions.Sex  := 'F';
    NormDefinitions.AgeL := 0;
    NormDefinitions.AgeH := 130;
    NormDefinitions.UOMS := '';
    NormDefinitions.UOMC := '';
    NormDefinitions.LS  := 100;
    NormDefinitions.HS  := 150;
    NormDefinitions.LTS := 50;
    NormDefinitions.HTS := 200;
    NormDefinitions.LPS := 10;
    NormDefinitions.HPS := 300;
    NormDefinitions.LC  := 100;
    NormDefinitions.HC  := 150;
    NormDefinitions.LTC := 50;
    NormDefinitions.HTC := 200;
    NormDefinitions.LPC := 10;
    NormDefinitions.HPC := 300;
    NormDefinitions.startDateTime := '2012-08-01T12:00:00+01:00';
    AddSubNormNodes(Doc, BaseTestNode, NormDefinitions);

    writeXMLFile(Doc, theFileName);

    returnCode := 0;
  finally
    Doc.Free;
  end;
end;

end.