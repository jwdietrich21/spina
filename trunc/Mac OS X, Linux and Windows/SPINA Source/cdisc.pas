unit CDISC;

{ SPINA-Thyr }

{ Application for calculating structure parameters }
{ of thyrotropic feedback control }

{ Programm zur Berechnung von Strukturparametern }
{ des thyreotropen Regelkreises }

{ Version 3.4.0 }

{ (c) J. W. Dietrich, 1994 - 2014 }
{ (c) Ludwig Maximilian University of Munich 1995 - 2002 }
{ (c) University of Ulm Hospitals 2002-2004 }
{ (c) Ruhr University of Bochum 2005 - 2013 }

{ This unit handles global application preferences }

{ Source code released under the BSD License }
{ See http://spina.medical-cybernetics.de for details }

{$mode objfpc}

{Return codes of procedure SaveStandardCDISC_RRFile:
0: No Error.
6: Error saving file.
}

interface

uses
  Classes, SysUtils, FileUtil, DOM, XMLRead, XMLWrite, StrUtils;

procedure SaveStandardCDISC_RRFile(theFileName: String; var returnCode: integer);

implementation

procedure AddSubExNodes(Doc: TXMLDocument; theRoot: TDOMNode; Sex: char; AgeL, AgeH: integer; UOMS, UOMC, LXS, HXS, LXC, HXC, startDateTime: string);
var
  theNode, FlagUOMNode: TDOMNode;
  SubjectCharsNode, AgeNode, ExclusionsNode, TextNode: TDOMNode;
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
  TDOMElement(theNode).SetAttribute('Value', LXS);
  ExclusionsNode.Appendchild(theNode);
  theNode := Doc.CreateElement('ExclusionDefinition');
  TDOMElement(theNode).SetAttribute('ExclusionLevel', 'HX');
  TDOMElement(theNode).SetAttribute('Value', HXS);
  ExclusionsNode.Appendchild(theNode);
  theNode := Doc.CreateElement('TransactionType');
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
  TDOMElement(theNode).SetAttribute('Value', LXC);
  ExclusionsNode.Appendchild(theNode);
  theNode := Doc.CreateElement('ExclusionDefinition');
  TDOMElement(theNode).SetAttribute('ExclusionLevel', 'HX');
  TDOMElement(theNode).SetAttribute('Value', HXC);
  ExclusionsNode.Appendchild(theNode);
  theNode := Doc.CreateElement('TransactionType');
  TextNode := Doc.CreateTextNode('I');
  theNode.AppendChild(TextNode);
  ExclusionsNode.Appendchild(theNode);
  SubjectCharsNode.Appendchild(FlagUOMNode);
  theRoot.Appendchild(SubjectCharsNode);
end;

procedure AddSubNormNodes(Doc: TXMLDocument; theRoot: TDOMNode; Sex: char; AgeL, AgeH: integer; UOMS, UOMC, LS, HS, LTS, HTS, LPS, HPS, LC, HC, lTC, HTC, LPC, HPC, startDateTime: string);
var
  RootNode, parentNode, theNode, BatteryNode, BaseTestNode, FlagUOMNode: TDOMNode;
  LabTestNode, NormalNode, AlertNode, UnitsNode, NormalDefinitionNode: TDOMNode;
  SubjectCharsNode, AgeNode, ExclusionsNode, TextNode: TDOMNode;
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
  TDOMElement(theNode).SetAttribute('Value', LS);
  NormalNode.Appendchild(theNode);
  theNode := Doc.CreateElement('NormalDefinition');
  TDOMElement(theNode).SetAttribute('NormalLevel', 'H');
  TDOMElement(theNode).SetAttribute('Value', HS);
  NormalNode.Appendchild(theNode);
  theNode := Doc.CreateElement('TransactionType');
  TextNode := Doc.CreateTextNode('I');
  theNode.AppendChild(TextNode);
  NormalNode.Appendchild(theNode);
  AlertNode := Doc.CreateElement('Alerts');
  TDOMElement(AlertNode).SetAttribute('StartDateTime', startDateTime);
  FlagUOMNode.Appendchild(AlertNode);
  theNode := Doc.CreateElement('AlertDefinition');
  TDOMElement(theNode).SetAttribute('AlertLevel', 'LN');
  TDOMElement(theNode).SetAttribute('Value', LS);
  AlertNode.Appendchild(theNode);
  theNode := Doc.CreateElement('AlertDefinition');
  TDOMElement(theNode).SetAttribute('AlertLevel', 'HN');
  TDOMElement(theNode).SetAttribute('Value', HS);
  AlertNode.Appendchild(theNode);
  if LTS <> '' then
  begin
    theNode := Doc.CreateElement('AlertDefinition');
    TDOMElement(theNode).SetAttribute('AlertLevel', 'LT');
    TDOMElement(theNode).SetAttribute('Value', LTS);
    AlertNode.Appendchild(theNode);
  end;
  if HTS <> '' then
  begin
    theNode := Doc.CreateElement('AlertDefinition');
    TDOMElement(theNode).SetAttribute('AlertLevel', 'HT');
    TDOMElement(theNode).SetAttribute('Value', HTS);
    AlertNode.Appendchild(theNode);
  end;
  if LPS <> '' then
  begin
    theNode := Doc.CreateElement('AlertDefinition');
    TDOMElement(theNode).SetAttribute('AlertLevel', 'LP');
    TDOMElement(theNode).SetAttribute('Value', LPS);
    AlertNode.Appendchild(theNode);
  end;
  if HPS <> '' then
  begin
    theNode := Doc.CreateElement('AlertDefinition');
    TDOMElement(theNode).SetAttribute('AlertLevel', 'HP');
    TDOMElement(theNode).SetAttribute('Value', HPS);
    AlertNode.Appendchild(theNode);
  end;
  theNode := Doc.CreateElement('TransactionType');
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
  TDOMElement(theNode).SetAttribute('Value', LC);
  NormalNode.Appendchild(theNode);
  theNode := Doc.CreateElement('NormalDefinition');
  TDOMElement(theNode).SetAttribute('NormalLevel', 'H');
  TDOMElement(theNode).SetAttribute('Value', HC);
  NormalNode.Appendchild(theNode);
  theNode := Doc.CreateElement('TransactionType');
  TextNode := Doc.CreateTextNode('I');
  theNode.AppendChild(TextNode);
  NormalNode.Appendchild(theNode);
  AlertNode := Doc.CreateElement('Alerts');
  TDOMElement(AlertNode).SetAttribute('StartDateTime', startDateTime);
  FlagUOMNode.Appendchild(AlertNode);
  theNode := Doc.CreateElement('AlertDefinition');
  TDOMElement(theNode).SetAttribute('AlertLevel', 'LN');
  TDOMElement(theNode).SetAttribute('Value', LC);
  AlertNode.Appendchild(theNode);
  theNode := Doc.CreateElement('AlertDefinition');
  TDOMElement(theNode).SetAttribute('AlertLevel', 'HN');
  TDOMElement(theNode).SetAttribute('Value', HC);
  AlertNode.Appendchild(theNode);
  if LTC <> '' then
  begin
    theNode := Doc.CreateElement('AlertDefinition');
    TDOMElement(theNode).SetAttribute('AlertLevel', 'LT');
    TDOMElement(theNode).SetAttribute('Value', LTC);
    AlertNode.Appendchild(theNode);
  end;
  if HTC <> '' then
  begin
    theNode := Doc.CreateElement('AlertDefinition');
    TDOMElement(theNode).SetAttribute('AlertLevel', 'HT');
    TDOMElement(theNode).SetAttribute('Value', HTC);
    AlertNode.Appendchild(theNode);
  end;
  if LPC <> '' then
  begin
    theNode := Doc.CreateElement('AlertDefinition');
    TDOMElement(theNode).SetAttribute('AlertLevel', 'LP');
    TDOMElement(theNode).SetAttribute('Value', LPC);
    AlertNode.Appendchild(theNode);
  end;
  if HPC <> '' then
  begin
    theNode := Doc.CreateElement('AlertDefinition');
    TDOMElement(theNode).SetAttribute('AlertLevel', 'HP');
    TDOMElement(theNode).SetAttribute('Value', HPC);
    AlertNode.Appendchild(theNode);
  end;
  theNode := Doc.CreateElement('TransactionType');
  TextNode := Doc.CreateTextNode('I');
  theNode.AppendChild(TextNode);
  AlertNode.Appendchild(theNode);
  SubjectCharsNode.Appendchild(FlagUOMNode);
  theRoot.Appendchild(SubjectCharsNode);
end;

procedure SaveStandardCDISC_RRFile(theFileName: String; var returnCode: integer);
{saves a minimal standard file}
var
  Doc: TXMLDocument;
  RootNode, parentNode, theNode, BatteryNode, BaseTestNode, FlagUOMNode: TDOMNode;
  LabTestNode, NormalNode, AlertNode, UnitsNode, NormalDefinitionNode: TDOMNode;
  SubjectCharsNode, AgeNode, ExclusionsNode, TextNode: TDOMNode;
begin
  returnCode := 6;
  try
    Doc := TXMLDocument.Create;
    theNode := Doc.CreateComment('Example for SPINA Reference Values');
    Doc.AppendChild(theNode);
    theNode := Doc.CreateComment('adapted to CDISC LAB MODEL 1.0.1');
    Doc.AppendChild(theNode);
    theNode := Doc.CreateComment('(c) J. W. Dietrich, 1994 - 2012');
    Doc.AppendChild(theNode);
    theNode := Doc.CreateComment('(c) Ludwig Maximilian University of Munich 1995 - 2002');
    Doc.AppendChild(theNode);
    theNode := Doc.CreateComment('(c) University of Ulm Hospitals 2002-2004');
    Doc.AppendChild(theNode);
    theNode := Doc.CreateComment('(c) Ruhr University of Bochum 2005 - 2012');
    Doc.AppendChild(theNode);
    theNode := Doc.CreateComment('(released under the BSD License');
    Doc.AppendChild(theNode);
    RootNode := Doc.CreateElement('GTP');
    TDOMElement(RootNode).SetAttribute('CreationDateTime', '2012-12-30T13:13:13+01:00');
    TDOMElement(RootNode).SetAttribute('ModelVersion', '01-0-01');
    Doc.Appendchild(RootNode);
    RootNode:= Doc.DocumentElement;
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

    AddSubExNodes(Doc, BaseTestNode, 'F', 0, 999, 'mIU/L', 'mIU/L', '<0', '>10000', '<0', '>10000', '2000-01-01T12:00:00+01:00');
    AddSubNormNodes(Doc, BaseTestNode, 'F', 0, 130, 'mIU/L', 'mIU/L', '0.35', '3.5', '', '>100', '', '>500', '0.35', '3.5', '', '>100', '', '>500', '2012-08-01T12:00:00+01:00');

    theNode := Doc.CreateComment('Add additional age classes here');
    BaseTestNode.AppendChild(theNode);

    AddSubExNodes(Doc, BaseTestNode, 'M', 0, 999, 'mIU/L', 'mIU/L', '<0', '>10000', '<0', '>10000', '2012-08-01T12:00:00+01:00');
    AddSubNormNodes(Doc, BaseTestNode, 'M', 0, 130, 'mIU/L', 'mIU/L', '0.35', '3.5', '', '>100', '', '>500', '0.35', '3.5', '', '>100', '', '>500', '2012-08-01T12:00:00+01:00');

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

    AddSubExNodes(Doc, BaseTestNode, 'F', 0, 999, 'pmol/L', 'ng/L', '0', '>1287', '<0', '>1000', '2012-08-01T12:00:00+01:00');
    AddSubNormNodes(Doc, BaseTestNode, 'F', 0, 130, 'pmol/L', 'ng/L', '7.7', '18', '<7.7', '>18', '<7.7', '>18', '6', '14', '6', '14', '6', '14', '2012-08-01T12:00:00+01:00');

    theNode := Doc.CreateComment('Add additional age classes here');
    BaseTestNode.AppendChild(theNode);

    AddSubExNodes(Doc, BaseTestNode, 'M', 0, 999, 'pmol/L', 'ng/L', '<0', '>1287', '<0', '>1000', '2012-08-01T12:00:00+01:00');
    AddSubNormNodes(Doc, BaseTestNode, 'M', 0, 130, 'pmol/L', 'ng/L', '7.7', '18', '<7.7', '>18', '<7.7', '>18', '6', '14', '6', '14', '6', '14', '2012-08-01T12:00:00+01:00');

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
    AddSubExNodes(Doc, BaseTestNode, 'F', 0, 999, 'pmol/L', 'ng/L', '<0', '>100', '<0', '>65', '2012-08-01T12:00:00+01:00');
    AddSubNormNodes(Doc, BaseTestNode, 'F', 0, 130, 'pmol/L', 'ng/L', '3.5', '6.3', '<3.5', '>6.3', '<3.5', '>6.3', '2.3', '4.1', '<2.3', '>4.1', '<2.3', '>4.1', '2012-08-01T12:00:00+01:00');

    theNode := Doc.CreateComment('Add additional age classes here');
    BaseTestNode.AppendChild(theNode);

    AddSubExNodes(Doc, BaseTestNode, 'M', 0, 999, 'pmol/L', 'ng/L', '<0', '>100', '<0', '>65', '2012-08-01T12:00:00+01:00');
    AddSubNormNodes(Doc, BaseTestNode, 'M', 0, 130, 'pmol/L', 'ng/L', '3.5', '6.3', '<3.5', '>6.3', '<3.5', '>6.3', '2.3', '4.1', '<2.3', '>4.1', '<2.3', '>4.1', '2012-08-01T12:00:00+01:00');

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
    AddSubExNodes(Doc, BaseTestNode, 'F', 0, 999, 'nmol/L', 'mcg/L', '<0', '>1287', '<0', '>1000', '2012-08-01T12:00:00+01:00');
    AddSubNormNodes(Doc, BaseTestNode, 'F', 0, 130, 'nmol/L', 'mcg/L', '64.4', '144.4', '<64.4', '>144.4', '<64.4', '>144.4', '50', '120', '<50', '>120', '<50', '>120', '2012-08-01T12:00:00+01:00');

    theNode := Doc.CreateComment('Add additional age classes here');
    BaseTestNode.AppendChild(theNode);

    AddSubExNodes(Doc, BaseTestNode, 'M', 0, 999, 'nmol/L', 'mcg/L', '<0', '>1287', '<0', '>1000', '2012-08-01T12:00:00+01:00');
    AddSubNormNodes(Doc, BaseTestNode, 'M', 0, 130, 'nmol/L', 'mcg/L', '64.4', '144.4', '<64.4', '>144.4', '<64.4', '>144.4', '50', '120', '<50', '>120', '<50', '>120', '2012-08-01T12:00:00+01:00');

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
    AddSubExNodes(Doc, BaseTestNode, 'F', 0, 999, 'nmol/L', 'mcg/L', '<0', '>154', '<0', '>100', '2012-08-01T12:00:00+01:00');
    AddSubNormNodes(Doc, BaseTestNode, 'F', 0, 130, 'nmol/L', 'mcg/L', '1.1', '2.8', '<1.1', '>2.8', '<1.1', '>2.8', '0.7', '1.8', '<0.7', '>1.8', '<0.7', '>1.8', '2012-08-01T12:00:00+01:00');

    theNode := Doc.CreateComment('Add additional age classes here');
    BaseTestNode.AppendChild(theNode);

    AddSubExNodes(Doc, BaseTestNode, 'M', 0, 999, 'nmol/L', 'mcg/L', '<0', '>154', '<0', '>100', '2012-08-01T12:00:00+01:00');
    AddSubNormNodes(Doc, BaseTestNode, 'M', 0, 130, 'nmol/L', 'mcg/L', '1.1', '2.8', '<1.1', '>2.8', '<1.1', '>2.8', '0.7', '1.8', '<0.7', '>1.8', '<0.7', '>1.8', '2012-08-01T12:00:00+01:00');

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
    AddSubExNodes(Doc, BaseTestNode, 'F', 0, 999, 'pmol/s', 'pmol/s', '<0', '>10000', '<0', '>10000', '2012-08-01T12:00:00+01:00');
    AddSubNormNodes(Doc, BaseTestNode, 'F', 0, 130, 'pmol/s', 'pmol/s', '1.41', '8.67', '', '>100', '', '>500', '1.41', '8.67', '', '>100', '', '>500', '2012-08-01T12:00:00+01:00');

    theNode := Doc.CreateComment('Add additional age classes here');
    BaseTestNode.AppendChild(theNode);

    AddSubExNodes(Doc, BaseTestNode, 'M', 0, 999, 'pmol/s', 'pmol/s', '<0', '>10000', '<0', '>10000', '2012-08-01T12:00:00+01:00');
    AddSubNormNodes(Doc, BaseTestNode, 'M', 0, 130, 'pmol/s', 'pmol/s', '1.41', '8.67', '', '>100', '', '>500', '1.41', '8.67', '', '>100', '', '>500', '2012-08-01T12:00:00+01:00');

    {SPINA-GD:}

    BaseTestNode := Doc.CreateElement('BaseTest');
    TDOMElement(BaseTestNode).SetAttribute('DefiningEntity', 'C');
    theNode := Doc.CreateComment('Definitions for SPINA-GD:');
    BaseTestNode.AppendChild(theNode);
    BatteryNode.Appendchild(BaseTestNode);
    LabTestNode := Doc.CreateElement('LabTest');
    TDOMElement(LabTestNode).SetAttribute('ID', 'GD');
    TDOMElement(LabTestNode).SetAttribute('Name', 'Sum activity of peripheral type 1 deiodinase');
    BaseTestNode.Appendchild(LabTestNode);
    AddSubExNodes(Doc, BaseTestNode, 'F', 0, 999, 'nmol/s', 'nmol/s', '<0', '>1000', '<0', '>1000', '2012-08-01T12:00:00+01:00');
    AddSubNormNodes(Doc, BaseTestNode, 'F', 0, 130, 'nmol/s', 'nmol/s', '20', '40', '<10', '>60', '<5', '>100', '20', '40', '<10', '>60', '<5', '>100', '2012-08-01T12:00:00+01:00');

    theNode := Doc.CreateComment('Add additional age classes here');
    BaseTestNode.AppendChild(theNode);

    AddSubExNodes(Doc, BaseTestNode, 'M', 0, 999, 'nmol/s', 'nmol/s', '<0', '>1000', '<0', '>1000', '2012-08-01T12:00:00+01:00');
    AddSubNormNodes(Doc, BaseTestNode, 'M', 0, 130, 'nmol/s', 'nmol/s', '20', '40', '<10', '>60', '<5', '>100', '20', '40', '<10', '>60', '<5', '>100', '2012-08-01T12:00:00+01:00');

    writeXMLFile(Doc, theFileName);

    returnCode := 0;
  finally
    Doc.Free;
  end;
end;

end.

