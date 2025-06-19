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

{Return codes of procedures ReadCDISC_RRFile and SaveCDISC_RRFile:
0:  No Error.
2:  Root node with ID "SPIc" not found
6:  Error reading or saving file.
10: New file created.
}

interface

uses
  Classes, SysUtils, FileUtil, DOM, XMLRead, XMLWrite, StrUtils, DateUtils,
  Math, SPINATypes, SPINA_Engine, UnitConverter;

procedure ReadCDISC_RRFile(theFileName: string; var ReferenceRanges: tReferenceValues;
  out returnCode: integer);
procedure SaveCDISC_RRFile(theFileName: string; const ReferenceRanges: tReferenceValues;
  var returnCode: integer);

implementation

function AttributeValue(theNode: TDOMNode; theName: string): string;
  {this functions finds an attribute of an XML tag and delivers its value}
var
  i: integer;
  foundValue: string;
begin
  foundValue := '';
  if assigned(theNode) then
    for i := 0 to theNode.Attributes.Length - 1 do
    begin
      if theNode.Attributes[i].NodeName = theName then
        foundValue := theNode.Attributes[i].NodeValue;
    end;
  Result := foundValue;
end;

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
    TDOMElement(theNode).SetAttribute('Value', '<' + FloatToStrF(LXC, ffGeneral, 5, 2));
    ExclusionsNode.Appendchild(theNode);
    theNode := Doc.CreateElement('ExclusionDefinition');
    TDOMElement(theNode).SetAttribute('ExclusionLevel', 'HX');
    TDOMElement(theNode).SetAttribute('Value', '>' + FloatToStrF(HXC, ffGeneral, 5, 2));
    ExclusionsNode.Appendchild(theNode);
    theNode := Doc.CreateElement('TransactionType');
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
    theNode := Doc.CreateElement('TransactionType');
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
      TDOMElement(theNode).SetAttribute('Value', '<' +
        FloatToStrF(LTS, ffGeneral, 5, 2));
      AlertNode.Appendchild(theNode);
    end;
    if not IsNan(HTS) then
    begin
      theNode := Doc.CreateElement('AlertDefinition');
      TDOMElement(theNode).SetAttribute('AlertLevel', 'HT');
      TDOMElement(theNode).SetAttribute('Value', '>' +
        FloatToStrF(HTS, ffGeneral, 5, 2));
      AlertNode.Appendchild(theNode);
    end;
    if not IsNan(LPS) then
    begin
      theNode := Doc.CreateElement('AlertDefinition');
      TDOMElement(theNode).SetAttribute('AlertLevel', 'LP');
      TDOMElement(theNode).SetAttribute('Value', '<' +
        FloatToStrF(LPS, ffGeneral, 5, 2));
      AlertNode.Appendchild(theNode);
    end;
    if not IsNan(HPS) then
    begin
      theNode := Doc.CreateElement('AlertDefinition');
      TDOMElement(theNode).SetAttribute('AlertLevel', 'HP');
      TDOMElement(theNode).SetAttribute('Value', '>' +
        FloatToStrF(HPS, ffGeneral, 5, 2));
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
    TDOMElement(theNode).SetAttribute('Value', FloatToStrF(LC, ffGeneral, 5, 2));
    NormalNode.Appendchild(theNode);
    theNode := Doc.CreateElement('NormalDefinition');
    TDOMElement(theNode).SetAttribute('NormalLevel', 'H');
    TDOMElement(theNode).SetAttribute('Value', FloatToStrF(HC, ffGeneral, 5, 2));
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
      TDOMElement(theNode).SetAttribute('Value', '<' +
        FloatToStrF(LTC, ffGeneral, 5, 2));
      AlertNode.Appendchild(theNode);
    end;
    if not IsNan(HTC) then
    begin
      theNode := Doc.CreateElement('AlertDefinition');
      TDOMElement(theNode).SetAttribute('AlertLevel', 'HT');
      TDOMElement(theNode).SetAttribute('Value', '>' +
        FloatToStrF(HTC, ffGeneral, 5, 2));
      AlertNode.Appendchild(theNode);
    end;
    if not IsNan(LPC) then
    begin
      theNode := Doc.CreateElement('AlertDefinition');
      TDOMElement(theNode).SetAttribute('AlertLevel', 'LP');
      TDOMElement(theNode).SetAttribute('Value', '<' +
        FloatToStrF(LPC, ffGeneral, 5, 2));
      AlertNode.Appendchild(theNode);
    end;
    if not IsNan(HPC) then
    begin
      theNode := Doc.CreateElement('AlertDefinition');
      TDOMElement(theNode).SetAttribute('AlertLevel', 'HP');
      TDOMElement(theNode).SetAttribute('Value', '>' +
        FloatToStrF(HPC, ffGeneral, 5, 2));
      AlertNode.Appendchild(theNode);
    end;
    theNode := Doc.CreateElement('TransactionType');
    TextNode := Doc.CreateTextNode('I');
    theNode.AppendChild(TextNode);
    AlertNode.Appendchild(theNode);
    SubjectCharsNode.Appendchild(FlagUOMNode);
    theRoot.Appendchild(SubjectCharsNode);
  end;
  DefaultFormatSettings.DecimalSeparator := oldSeparator;
end;

function ISO8601Date(const AValue: TDateTime): string;
begin
  Result := FormatDateTime('YYYY-MM-DD"T"hh:nn:ss', AValue);
end;

function ISO8601ToDateTime(const ADate: string): TDateTime;
begin
  Result := ScanDateTime('YYYY-MM-DD"T"hh:nn:ss', ADate, 1);
end;

procedure ReadCDISC_RRFile(theFileName: string; var ReferenceRanges: tReferenceValues;
  out returnCode: integer);
{reads reference values from a CDISC LAB model-compliant XML file.}
{This version of the procedure ignores sex- and age-specific reference values}
const
  MaxTries = 1000;
var
  Doc: TXMLDocument;
  RootNode, theNode, BatteryNode, BaseTestNode, FlagUOMNode, NormalNode,
  UnitsNode, NormalDefinitionNode: TDOMNode;
  theStream: TStringStream;
  oldSeparator: char;
  SI: boolean;
  breakCounter: integer;
begin
  returnCode := 6;
  breakCounter := 0;          // fall-back upper bound for loops
  oldSeparator := DefaultFormatSettings.DecimalSeparator;
  DefaultFormatSettings.DecimalSeparator := DEC_POINT;
  if not FileExists(theFileName) then
  begin
    SaveCDISC_RRFile(theFileName, sReferenceValues, returnCode);
    {saves a minimal standard file}
    if returnCode = 0 then    {no error,}
      returnCode := 10;       {therefore new file created}
  end;
  if FileExists(theFileName) then
    {could this file be created (or did it already exist)?}
  try
    ReadXMLFile(Doc, theFileName);
    assert(assigned(Doc));
    RootNode := Doc.DocumentElement.FindNode('Study');
    if assigned(RootNode) then
      if AttributeValue(RootNode, 'ID') = 'SPIc' then
      begin
        if returnCode < 10 then
          returnCode := 0;
        BatteryNode := RootNode.FindNode('BaseBattery');
        while (assigned(BatteryNode)) and (breakCounter < MaxTries) do
        begin
          Inc(breakCounter);

          {Standard hormone and metabolite concentrations:}

          if AttributeValue(BatteryNode, 'ID') = 'Hormones_metabolites' then
          begin
            BaseTestNode := BatteryNode.FindNode('BaseTest');
            while assigned(BaseTestNode) do
            begin
              theNode := BaseTestNode.FindNode('LabTest');
              if assigned(theNode) then

                {Glucose:}

                if AttributeValue(theNode, 'ID') = 'Glucose' then
                begin
                  theNode := theNode.NextSibling;
                  while assigned(theNode) do
                  begin
                    if theNode.NodeName = 'SubjectCharacteristics' then
                    begin
                      FlagUOMNode := theNode.FindNode('FlagUOM');
                      if assigned(FlagUOMNode) then
                      begin
                        UnitsNode := FlagUOMNode.FindNode('ResultUnits');
                        if assigned(UnitsNode) then
                          ReferenceRanges.Glucose.UoM := AttributeValue(UnitsNode, 'Value');
                        NormalNode := FlagUOMNode.FindNode('Normal');
                        if assigned(NormalNode) then
                        begin
                          NormalDefinitionNode :=
                            NormalNode.FindNode('NormalDefinition');
                          while assigned(NormalDefinitionNode) do
                          begin
                            if (AttributeValue(NormalDefinitionNode, 'NormalLevel') =
                              'L') or
                              (AttributeValue(NormalDefinitionNode,
                              'AlertLevel') = 'LN') then
                              ReferenceRanges.Glucose.ln :=
                                StrToFloatDef(
                                AttributeValue(NormalDefinitionNode, 'Value'),
                                Math.Nan);
                            if (AttributeValue(
                              NormalDefinitionNode, 'NormalLevel') = 'H') or
                              (AttributeValue(NormalDefinitionNode,
                              'AlertLevel') = 'HN') then
                              ReferenceRanges.Glucose.hn :=
                                StrToFloatDef(
                                AttributeValue(NormalDefinitionNode, 'Value'),
                                Math.Nan);
                            NormalDefinitionNode :=
                              NormalDefinitionNode.NextSibling;
                          end;
                        end;
                      end;
                    end;
                    theNode := theNode.NextSibling;
                  end;
                end;

              {Insulin:}

              if AttributeValue(theNode, 'ID') = 'Insulin' then
              begin
                theNode := theNode.NextSibling;
                while assigned(theNode) do
                begin
                  if theNode.NodeName = 'SubjectCharacteristics' then
                  begin
                    FlagUOMNode := theNode.FindNode('FlagUOM');
                    if assigned(FlagUOMNode) then
                    begin
                      UnitsNode := FlagUOMNode.FindNode('ResultUnits');
                        if assigned(UnitsNode) then
                          ReferenceRanges.Insulin.UoM := AttributeValue(UnitsNode, 'Value');
                      NormalNode := FlagUOMNode.FindNode('Normal');
                      if assigned(NormalNode) then {skips exclusion definition}
                      begin
                        NormalDefinitionNode :=
                          NormalNode.FindNode('NormalDefinition');
                        while assigned(NormalDefinitionNode) do
                        begin
                          if (AttributeValue(NormalDefinitionNode, 'NormalLevel') =
                            'L') or
                            (AttributeValue(NormalDefinitionNode,
                            'AlertLevel') = 'LN') then
                            ReferenceRanges.Insulin.ln :=
                              StrToFloatDef(
                              AttributeValue(NormalDefinitionNode, 'Value'),
                              Math.Nan);
                          if (AttributeValue(
                            NormalDefinitionNode, 'NormalLevel') = 'H') or
                            (AttributeValue(NormalDefinitionNode,
                            'AlertLevel') = 'HN') then
                            ReferenceRanges.Insulin.hn :=
                              StrToFloatDef(
                              AttributeValue(NormalDefinitionNode, 'Value'),
                              Math.Nan);
                          NormalDefinitionNode :=
                            NormalDefinitionNode.NextSibling;
                        end;
                      end;
                    end;
                  end;
                  theNode := theNode.NextSibling;
                end;
              end;

              {C-Peptide:}

              if AttributeValue(theNode, 'ID') = 'C-Peptide' then
              begin
                theNode := theNode.NextSibling;
                while assigned(theNode) do
                begin
                  if theNode.NodeName = 'SubjectCharacteristics' then
                  begin
                    FlagUOMNode := theNode.FindNode('FlagUOM');
                    if assigned(FlagUOMNode) then
                    begin
                      UnitsNode := FlagUOMNode.FindNode('ResultUnits');
                        if assigned(UnitsNode) then
                          ReferenceRanges.CPeptide.UoM := AttributeValue(UnitsNode, 'Value');
                      NormalNode := FlagUOMNode.FindNode('Normal');
                      if assigned(NormalNode) then {skips exclusion definition}
                      begin
                        NormalDefinitionNode :=
                          NormalNode.FindNode('NormalDefinition');
                        while assigned(NormalDefinitionNode) do
                        begin
                          if (AttributeValue(NormalDefinitionNode, 'NormalLevel') =
                            'L') or
                            (AttributeValue(NormalDefinitionNode,
                            'AlertLevel') = 'LN') then
                            ReferenceRanges.CPeptide.ln :=
                              StrToFloatDef(
                              AttributeValue(NormalDefinitionNode, 'Value'),
                              Math.Nan);
                          if (AttributeValue(
                            NormalDefinitionNode, 'NormalLevel') = 'H') or
                            (AttributeValue(NormalDefinitionNode,
                            'AlertLevel') = 'HN') then
                            ReferenceRanges.CPeptide.hn :=
                              StrToFloatDef(
                              AttributeValue(NormalDefinitionNode, 'Value'),
                              Math.Nan);
                          NormalDefinitionNode :=
                            NormalDefinitionNode.NextSibling;
                        end;
                      end;
                    end;
                  end;
                  theNode := theNode.NextSibling;
                end;
              end;

              BaseTestNode := BaseTestNode.NextSibling;
            end;
          end;
          if BatteryNode.NextSibling <> nil then
            BatteryNode := BatteryNode.NextSibling
          else
            BatteryNode := nil;

          {Calculated biomarkers: SPINA Carb etc:}

          if AttributeValue(BatteryNode, 'ID') = 'SPIc' then
          begin
            BaseTestNode := BatteryNode.FindNode('BaseTest');
            while assigned(BaseTestNode) do
            begin
              theNode := BaseTestNode.FindNode('LabTest');
              if assigned(theNode) then

                {SPINA-GBeta:}

                if AttributeValue(theNode, 'ID') = 'GBeta' then
                begin
                  theNode := theNode.NextSibling;
                  while assigned(theNode) do
                  begin
                    if theNode.NodeName = 'SubjectCharacteristics' then
                    begin
                      FlagUOMNode := theNode.FindNode('FlagUOM');
                      if assigned(FlagUOMNode) then
                      begin
                        NormalNode := FlagUOMNode.FindNode('Normal');
                        if assigned(NormalNode) then {skips exclusion definition}
                        begin
                          NormalDefinitionNode :=
                            NormalNode.FindNode('NormalDefinition');
                          while assigned(NormalDefinitionNode) do
                          begin
                            if (AttributeValue(NormalDefinitionNode, 'NormalLevel') =
                              'L') or
                              (AttributeValue(NormalDefinitionNode,
                              'AlertLevel') = 'LN') then
                              ReferenceRanges.SPINA_GBeta.ln :=
                                StrToFloatDef(
                                AttributeValue(NormalDefinitionNode, 'Value'),
                                Math.Nan);
                            if (AttributeValue(
                              NormalDefinitionNode, 'NormalLevel') = 'H') or
                              (AttributeValue(NormalDefinitionNode,
                              'AlertLevel') = 'HN') then
                              ReferenceRanges.SPINA_GBeta.hn :=
                                StrToFloatDef(
                                AttributeValue(NormalDefinitionNode, 'Value'),
                                Math.Nan);
                            NormalDefinitionNode :=
                              NormalDefinitionNode.NextSibling;
                          end;
                        end;
                      end;
                    end;
                    theNode := theNode.NextSibling;
                  end;
                end;

              {SPINA-GR:}

              if AttributeValue(theNode, 'ID') = 'GR' then
              begin
                theNode := theNode.NextSibling;
                while assigned(theNode) do
                begin
                  if theNode.NodeName = 'SubjectCharacteristics' then
                  begin
                    FlagUOMNode := theNode.FindNode('FlagUOM');
                    if assigned(FlagUOMNode) then
                    begin
                      NormalNode := FlagUOMNode.FindNode('Normal');
                      if assigned(NormalNode) then {skips exclusion definition}
                      begin
                        NormalDefinitionNode :=
                          NormalNode.FindNode('NormalDefinition');
                        while assigned(NormalDefinitionNode) do
                        begin
                          if (AttributeValue(NormalDefinitionNode, 'NormalLevel') =
                            'L') or
                            (AttributeValue(NormalDefinitionNode,
                            'AlertLevel') = 'LN') then
                            ReferenceRanges.SPINA_GR.ln :=
                              StrToFloatDef(
                              AttributeValue(NormalDefinitionNode, 'Value'),
                              Math.Nan);
                          if (AttributeValue(
                            NormalDefinitionNode, 'NormalLevel') = 'H') or
                            (AttributeValue(NormalDefinitionNode,
                            'AlertLevel') = 'HN') then
                            ReferenceRanges.SPINA_GR.hn :=
                              StrToFloatDef(
                              AttributeValue(NormalDefinitionNode, 'Value'),
                              Math.Nan);
                          NormalDefinitionNode :=
                            NormalDefinitionNode.NextSibling;
                        end;
                      end;
                    end;
                  end;
                  theNode := theNode.NextSibling;
                end;
              end;

              {SPINA-DI:}

              if AttributeValue(theNode, 'ID') = 'DI' then
              begin
                theNode := theNode.NextSibling;
                while assigned(theNode) do
                begin
                  if theNode.NodeName = 'SubjectCharacteristics' then
                  begin
                    FlagUOMNode := theNode.FindNode('FlagUOM');
                    if assigned(FlagUOMNode) then
                    begin
                      NormalNode := FlagUOMNode.FindNode('Normal');
                      if assigned(NormalNode) then {skips exclusion definition}
                      begin
                        NormalDefinitionNode :=
                          NormalNode.FindNode('NormalDefinition');
                        while assigned(NormalDefinitionNode) do
                        begin
                          if (AttributeValue(NormalDefinitionNode, 'NormalLevel') =
                            'L') or
                            (AttributeValue(NormalDefinitionNode,
                            'AlertLevel') = 'LN') then
                            ReferenceRanges.SPINA_DI.ln :=
                              StrToFloatDef(
                              AttributeValue(NormalDefinitionNode, 'Value'),
                              Math.Nan);
                          if (AttributeValue(
                            NormalDefinitionNode, 'NormalLevel') = 'H') or
                            (AttributeValue(NormalDefinitionNode,
                            'AlertLevel') = 'HN') then
                            ReferenceRanges.SPINA_DI.hn :=
                              StrToFloatDef(
                              AttributeValue(NormalDefinitionNode, 'Value'),
                              Math.Nan);
                          NormalDefinitionNode :=
                            NormalDefinitionNode.NextSibling;
                        end;
                      end;
                    end;
                  end;
                  theNode := theNode.NextSibling;
                end;
              end;

              {HOMA-Beta:}

              if AttributeValue(theNode, 'ID') = 'HOMA-Beta' then
              begin
                theNode := theNode.NextSibling;
                while assigned(theNode) do
                begin
                  if theNode.NodeName = 'SubjectCharacteristics' then
                  begin
                    FlagUOMNode := theNode.FindNode('FlagUOM');
                    if assigned(FlagUOMNode) then
                    begin
                      NormalNode := FlagUOMNode.FindNode('Normal');
                      if assigned(NormalNode) then {skips exclusion definition}
                      begin
                        NormalDefinitionNode :=
                          NormalNode.FindNode('NormalDefinition');
                        while assigned(NormalDefinitionNode) do
                        begin
                          if (AttributeValue(NormalDefinitionNode, 'NormalLevel') =
                            'L') or
                            (AttributeValue(NormalDefinitionNode,
                            'AlertLevel') = 'LN') then
                            ReferenceRanges.HOMA_Beta.ln :=
                              StrToFloatDef(
                              AttributeValue(NormalDefinitionNode, 'Value'),
                              Math.Nan);
                          if (AttributeValue(
                            NormalDefinitionNode, 'NormalLevel') = 'H') or
                            (AttributeValue(NormalDefinitionNode,
                            'AlertLevel') = 'HN') then
                            ReferenceRanges.HOMA_Beta.hn :=
                              StrToFloatDef(
                              AttributeValue(NormalDefinitionNode, 'Value'),
                              Math.Nan);
                          NormalDefinitionNode :=
                            NormalDefinitionNode.NextSibling;
                        end;
                      end;
                    end;
                  end;
                  theNode := theNode.NextSibling;
                end;
              end;

              {HOMA-IR:}

              if AttributeValue(theNode, 'ID') = 'HOMA-IR' then
              begin
                theNode := theNode.NextSibling;
                while assigned(theNode) do
                begin
                  if theNode.NodeName = 'SubjectCharacteristics' then
                  begin
                    FlagUOMNode := theNode.FindNode('FlagUOM');
                    if assigned(FlagUOMNode) then
                    begin
                      NormalNode := FlagUOMNode.FindNode('Normal');
                      if assigned(NormalNode) then {skips exclusion definition}
                      begin
                        NormalDefinitionNode :=
                          NormalNode.FindNode('NormalDefinition');
                        while assigned(NormalDefinitionNode) do
                        begin
                          if (AttributeValue(NormalDefinitionNode, 'NormalLevel') =
                            'L') or
                            (AttributeValue(NormalDefinitionNode,
                            'AlertLevel') = 'LN') then
                            ReferenceRanges.HOMA_IR.ln :=
                              StrToFloatDef(
                              AttributeValue(NormalDefinitionNode, 'Value'),
                              Math.Nan);
                          if (AttributeValue(
                            NormalDefinitionNode, 'NormalLevel') = 'H') or
                            (AttributeValue(NormalDefinitionNode,
                            'AlertLevel') = 'HN') then
                            ReferenceRanges.HOMA_IR.hn :=
                              StrToFloatDef(
                              AttributeValue(NormalDefinitionNode, 'Value'),
                              Math.Nan);
                          NormalDefinitionNode :=
                            NormalDefinitionNode.NextSibling;
                        end;
                      end;
                    end;
                  end;
                  theNode := theNode.NextSibling;
                end;
              end;

              {HOMA-IS:}

              if AttributeValue(theNode, 'ID') = 'HOMA-IS' then
              begin
                theNode := theNode.NextSibling;
                while assigned(theNode) do
                begin
                  if theNode.NodeName = 'SubjectCharacteristics' then
                  begin
                    FlagUOMNode := theNode.FindNode('FlagUOM');
                    if assigned(FlagUOMNode) then
                    begin
                      NormalNode := FlagUOMNode.FindNode('Normal');
                      if assigned(NormalNode) then {skips exclusion definition}
                      begin
                        NormalDefinitionNode :=
                          NormalNode.FindNode('NormalDefinition');
                        while assigned(NormalDefinitionNode) do
                        begin
                          if (AttributeValue(NormalDefinitionNode, 'NormalLevel') =
                            'L') or
                            (AttributeValue(NormalDefinitionNode,
                            'AlertLevel') = 'LN') then
                            ReferenceRanges.HOMA_IS.ln :=
                              StrToFloatDef(
                              AttributeValue(NormalDefinitionNode, 'Value'),
                              Math.Nan);
                          if (AttributeValue(
                            NormalDefinitionNode, 'NormalLevel') = 'H') or
                            (AttributeValue(NormalDefinitionNode,
                            'AlertLevel') = 'HN') then
                            ReferenceRanges.HOMA_IS.hn :=
                              StrToFloatDef(
                              AttributeValue(NormalDefinitionNode, 'Value'),
                              Math.Nan);
                          NormalDefinitionNode :=
                            NormalDefinitionNode.NextSibling;
                        end;
                      end;
                    end;
                  end;
                  theNode := theNode.NextSibling;
                end;
              end;

              {QUICKI:}

              if AttributeValue(theNode, 'ID') = 'QUICKI' then
              begin
                theNode := theNode.NextSibling;
                while assigned(theNode) do
                begin
                  if theNode.NodeName = 'SubjectCharacteristics' then
                  begin
                    FlagUOMNode := theNode.FindNode('FlagUOM');
                    if assigned(FlagUOMNode) then
                    begin
                      NormalNode := FlagUOMNode.FindNode('Normal');
                      if assigned(NormalNode) then {skips exclusion definition}
                      begin
                        NormalDefinitionNode :=
                          NormalNode.FindNode('NormalDefinition');
                        while assigned(NormalDefinitionNode) do
                        begin
                          if (AttributeValue(NormalDefinitionNode, 'NormalLevel') =
                            'L') or
                            (AttributeValue(NormalDefinitionNode,
                            'AlertLevel') = 'LN') then
                            ReferenceRanges.QUICKI.ln :=
                              StrToFloatDef(
                              AttributeValue(NormalDefinitionNode, 'Value'),
                              Math.Nan);
                          if (AttributeValue(
                            NormalDefinitionNode, 'NormalLevel') = 'H') or
                            (AttributeValue(NormalDefinitionNode,
                            'AlertLevel') = 'HN') then
                            ReferenceRanges.QUICKI.hn :=
                              StrToFloatDef(
                              AttributeValue(NormalDefinitionNode, 'Value'),
                              Math.Nan);
                          NormalDefinitionNode :=
                            NormalDefinitionNode.NextSibling;
                        end;
                      end;
                    end;
                  end;
                  theNode := theNode.NextSibling;
                end;
              end;

              {AIGR:}

              if AttributeValue(theNode, 'ID') = 'AIGR' then
              begin
                theNode := theNode.NextSibling;
                while assigned(theNode) do
                begin
                  if theNode.NodeName = 'SubjectCharacteristics' then
                  begin
                    FlagUOMNode := theNode.FindNode('FlagUOM');
                    if assigned(FlagUOMNode) then
                    begin
                      UnitsNode := FlagUOMNode.FindNode('ResultUnits');
                      if assigned(UnitsNode) then
                        ReferenceRanges.AIGR.UoM := AttributeValue(UnitsNode, 'Value');
                      NormalNode := FlagUOMNode.FindNode('Normal');
                      if assigned(NormalNode) then {skips exclusion definition}
                      begin
                        NormalDefinitionNode :=
                          NormalNode.FindNode('NormalDefinition');
                        while assigned(NormalDefinitionNode) do
                        begin
                          if (AttributeValue(NormalDefinitionNode, 'NormalLevel') =
                            'L') or
                            (AttributeValue(NormalDefinitionNode,
                            'AlertLevel') = 'LN') then
                            ReferenceRanges.AIGR.ln :=
                              StrToFloatDef(
                              AttributeValue(NormalDefinitionNode, 'Value'),
                              Math.Nan);
                          if (AttributeValue(
                            NormalDefinitionNode, 'NormalLevel') = 'H') or
                            (AttributeValue(NormalDefinitionNode,
                            'AlertLevel') = 'HN') then
                            ReferenceRanges.AIGR.hn :=
                              StrToFloatDef(
                              AttributeValue(NormalDefinitionNode, 'Value'),
                              Math.Nan);
                          NormalDefinitionNode :=
                            NormalDefinitionNode.NextSibling;
                        end;
                      end;
                    end;
                  end;
                  theNode := theNode.NextSibling;
                end;
              end;

              {CGR:}

              if AttributeValue(theNode, 'ID') = 'CGR' then
              begin
                theNode := theNode.NextSibling;
                while assigned(theNode) do
                begin
                  if theNode.NodeName = 'SubjectCharacteristics' then
                  begin
                    FlagUOMNode := theNode.FindNode('FlagUOM');
                    if assigned(FlagUOMNode) then
                    begin
                      NormalNode := FlagUOMNode.FindNode('Normal');
                      if assigned(NormalNode) then {skips exclusion definition}
                      begin
                        NormalDefinitionNode :=
                          NormalNode.FindNode('NormalDefinition');
                        while assigned(NormalDefinitionNode) do
                        begin
                          if (AttributeValue(NormalDefinitionNode, 'NormalLevel') =
                            'L') or
                            (AttributeValue(NormalDefinitionNode,
                            'AlertLevel') = 'LN') then
                            ReferenceRanges.CGR.ln :=
                              StrToFloatDef(
                              AttributeValue(NormalDefinitionNode, 'Value'),
                              Math.Nan);
                          if (AttributeValue(
                            NormalDefinitionNode, 'NormalLevel') = 'H') or
                            (AttributeValue(NormalDefinitionNode,
                            'AlertLevel') = 'HN') then
                            ReferenceRanges.CGR.hn :=
                              StrToFloatDef(
                              AttributeValue(NormalDefinitionNode, 'Value'),
                              Math.Nan);
                          NormalDefinitionNode :=
                            NormalDefinitionNode.NextSibling;
                        end;
                      end;
                    end;
                  end;
                  theNode := theNode.NextSibling;
                end;
              end;

              BaseTestNode := BaseTestNode.NextSibling;
            end;
          end;
          if BatteryNode.NextSibling <> nil then
            BatteryNode := BatteryNode.NextSibling
          else
            BatteryNode := nil;
        end;
      end
      else
        returnCode := 2;
  finally
    Doc.Free;
  end
  else
  begin                         {fall-back solution, if file does not exist}
    ReferenceRanges := sReferenceValues;
    returnCode := 6;
  end;
  DefaultFormatSettings.DecimalSeparator := oldSeparator;
end;

procedure SaveCDISC_RRFile(theFileName: string; const ReferenceRanges: tReferenceValues;
  var returnCode: integer);
{Saves edited reference ranges as CDISC file}
var
  Doc: TXMLDocument;
  RootNode, parentNode, theNode, BatteryNode, BaseTestNode, FlagUOMNode: TDOMNode;
  LabTestNode, NormalNode, AlertNode, UnitsNode, NormalDefinitionNode: TDOMNode;
  SubjectCharsNode, AgeNode, ExclusionsNode, TextNode: TDOMNode;
  ExclusionDefinitions: tReferenceExDefinitions;
  NormDefinitions: tReferenceNormDefinitions;
  TestString: string;
begin
  returnCode := 6;
  try
    Doc := TXMLDocument.Create;
    theNode := Doc.CreateComment('SPINA Reference Values');
    Doc.AppendChild(theNode);
    theNode := Doc.CreateComment('adapted to CDISC LAB MODEL 1.0.1');
    Doc.AppendChild(theNode);
    RootNode := Doc.CreateElement('GTP');
    TDOMElement(RootNode).SetAttribute('CreationDateTime', ISO8601Date(now));
    TDOMElement(RootNode).SetAttribute('ModelVersion', '01-0-01');
    Doc.Appendchild(RootNode);
    RootNode := Doc.DocumentElement;
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

    ExclusionDefinitions.Sex := 'F';
    ExclusionDefinitions.AgeL := 0;
    ExclusionDefinitions.AgeH := 999;
    ExclusionDefinitions.UOMS := 'mmol/L';
    ExclusionDefinitions.UOMC := 'mg/dL';
    ExclusionDefinitions.LXS := 0;
    ExclusionDefinitions.HXS := 10000;
    ExclusionDefinitions.LXC := 0;
    ExclusionDefinitions.HXC := 10000;
    ExclusionDefinitions.startDateTime := '2000-01-01T12:00:00+01:00';
    AddSubExNodes(Doc, BaseTestNode, ExclusionDefinitions);
    NormDefinitions.Sex := 'F';
    NormDefinitions.AgeL := 0;
    NormDefinitions.AgeH := 130;
    NormDefinitions.UOMS := 'mmol/L';
    NormDefinitions.UOMC := 'mg/dL';
    if pos(UpCase('mol'), UpCase(ReferenceRanges.Glucose.UoM)) > 0 then
    begin
      NormDefinitions.LS := ReferenceRanges.Glucose.ln;
      NormDefinitions.HS := ReferenceRanges.Glucose.hn;
      NormDefinitions.LTS := ReferenceRanges.Glucose.lt;
      NormDefinitions.HTS := ReferenceRanges.Glucose.ht;
      NormDefinitions.LPS := ReferenceRanges.Glucose.lp;
      NormDefinitions.HPS := ReferenceRanges.Glucose.hp;
      NormDefinitions.LC := ConvertedValue(ReferenceRanges.Glucose.ln,
        kGlucoseMolarMass, ReferenceRanges.Glucose.UoM, 'mg/dL');
      NormDefinitions.HC := ConvertedValue(ReferenceRanges.Glucose.hn,
        kGlucoseMolarMass, ReferenceRanges.Glucose.UoM, 'mg/dL');
      NormDefinitions.LTC := ConvertedValue(ReferenceRanges.Glucose.lt,
        kGlucoseMolarMass, ReferenceRanges.Glucose.UoM, 'mg/dL');
      NormDefinitions.HTC := ConvertedValue(ReferenceRanges.Glucose.ht,
        kGlucoseMolarMass, ReferenceRanges.Glucose.UoM, 'mg/dL');
      NormDefinitions.LPC := ConvertedValue(ReferenceRanges.Glucose.lp,
        kGlucoseMolarMass, ReferenceRanges.Glucose.UoM, 'mg/dL');
      NormDefinitions.HPC := ConvertedValue(ReferenceRanges.Glucose.hp,
        kGlucoseMolarMass, ReferenceRanges.Glucose.UoM, 'mg/dL');
    end
    else
    begin
      NormDefinitions.LS := ConvertedValue(ReferenceRanges.Glucose.ln,
        kGlucoseMolarMass, ReferenceRanges.Glucose.UoM, 'mmol/L');
      NormDefinitions.HS := ConvertedValue(ReferenceRanges.Glucose.hn,
        kGlucoseMolarMass, ReferenceRanges.Glucose.UoM, 'mmol/L');
      NormDefinitions.LTS := ConvertedValue(ReferenceRanges.Glucose.lt,
        kGlucoseMolarMass, ReferenceRanges.Glucose.UoM, 'mmol/L');
      NormDefinitions.HTS := ConvertedValue(ReferenceRanges.Glucose.ht,
        kGlucoseMolarMass, ReferenceRanges.Glucose.UoM, 'mmol/L');
      NormDefinitions.LPS := ConvertedValue(ReferenceRanges.Glucose.lp,
        kGlucoseMolarMass, ReferenceRanges.Glucose.UoM, 'mmol/L');
      NormDefinitions.HPS := ConvertedValue(ReferenceRanges.Glucose.hp,
        kGlucoseMolarMass, ReferenceRanges.Glucose.UoM, 'mmol/L');
      NormDefinitions.LC := ReferenceRanges.Glucose.ln;
      NormDefinitions.HC := ReferenceRanges.Glucose.hn;
      NormDefinitions.LTC := ReferenceRanges.Glucose.lt;
      NormDefinitions.HTC := ReferenceRanges.Glucose.ht;
      NormDefinitions.LPC := ReferenceRanges.Glucose.lp;
      NormDefinitions.HPC := ReferenceRanges.Glucose.hp;
    end;
    NormDefinitions.startDateTime := ISO8601Date(now);
    AddSubNormNodes(Doc, BaseTestNode, NormDefinitions);

    theNode := Doc.CreateComment('Add additional age classes here');
    BaseTestNode.AppendChild(theNode);

    ExclusionDefinitions.Sex := 'M';
    ExclusionDefinitions.AgeL := 0;
    ExclusionDefinitions.AgeH := 999;
    ExclusionDefinitions.UOMS := 'mmol/L';
    ExclusionDefinitions.UOMC := 'mg/dL';
    ExclusionDefinitions.LXS := 0;
    ExclusionDefinitions.HXS := 10000;
    ExclusionDefinitions.LXC := 0;
    ExclusionDefinitions.HXC := 10000;
    ExclusionDefinitions.startDateTime := '2000-01-01T12:00:00+01:00';
    AddSubExNodes(Doc, BaseTestNode, ExclusionDefinitions);
    NormDefinitions.Sex := 'M';
    NormDefinitions.AgeL := 0;
    NormDefinitions.AgeH := 130;
    NormDefinitions.UOMS := 'mmol/L';
    NormDefinitions.UOMC := 'mg/dL';
    if pos(UpCase('mol'), UpCase(ReferenceRanges.Glucose.UoM)) > 0 then
    begin
      NormDefinitions.LS := ReferenceRanges.Glucose.ln;
      NormDefinitions.HS := ReferenceRanges.Glucose.hn;
      NormDefinitions.LTS := ReferenceRanges.Glucose.lt;
      NormDefinitions.HTS := ReferenceRanges.Glucose.ht;
      NormDefinitions.LPS := ReferenceRanges.Glucose.lp;
      NormDefinitions.HPS := ReferenceRanges.Glucose.hp;
      NormDefinitions.LC := ConvertedValue(ReferenceRanges.Glucose.ln,
        kGlucoseMolarMass, ReferenceRanges.Glucose.UoM, 'mg/dL');
      NormDefinitions.HC := ConvertedValue(ReferenceRanges.Glucose.hn,
        kGlucoseMolarMass, ReferenceRanges.Glucose.UoM, 'mg/dL');
      NormDefinitions.LTC := ConvertedValue(ReferenceRanges.Glucose.lt,
        kGlucoseMolarMass, ReferenceRanges.Glucose.UoM, 'mg/dL');
      NormDefinitions.HTC := ConvertedValue(ReferenceRanges.Glucose.ht,
        kGlucoseMolarMass, ReferenceRanges.Glucose.UoM, 'mg/dL');
      NormDefinitions.LPC := ConvertedValue(ReferenceRanges.Glucose.lp,
        kGlucoseMolarMass, ReferenceRanges.Glucose.UoM, 'mg/dL');
      NormDefinitions.HPC := ConvertedValue(ReferenceRanges.Glucose.hp,
        kGlucoseMolarMass, ReferenceRanges.Glucose.UoM, 'mg/dL');
    end
    else
    begin
      NormDefinitions.LS := ConvertedValue(ReferenceRanges.Glucose.ln,
        kGlucoseMolarMass, ReferenceRanges.Glucose.UoM, 'mmol/L');
      NormDefinitions.HS := ConvertedValue(ReferenceRanges.Glucose.hn,
        kGlucoseMolarMass, ReferenceRanges.Glucose.UoM, 'mmol/L');
      NormDefinitions.LTS := ConvertedValue(ReferenceRanges.Glucose.lt,
        kGlucoseMolarMass, ReferenceRanges.Glucose.UoM, 'mmol/L');
      NormDefinitions.HTS := ConvertedValue(ReferenceRanges.Glucose.ht,
        kGlucoseMolarMass, ReferenceRanges.Glucose.UoM, 'mmol/L');
      NormDefinitions.LPS := ConvertedValue(ReferenceRanges.Glucose.lp,
        kGlucoseMolarMass, ReferenceRanges.Glucose.UoM, 'mmol/L');
      NormDefinitions.HPS := ConvertedValue(ReferenceRanges.Glucose.hp,
        kGlucoseMolarMass, ReferenceRanges.Glucose.UoM, 'mmol/L');
      NormDefinitions.LC := ReferenceRanges.Glucose.ln;
      NormDefinitions.HC := ReferenceRanges.Glucose.hn;
      NormDefinitions.LTC := ReferenceRanges.Glucose.lt;
      NormDefinitions.HTC := ReferenceRanges.Glucose.ht;
      NormDefinitions.LPC := ReferenceRanges.Glucose.lp;
      NormDefinitions.HPC := ReferenceRanges.Glucose.hp;
    end;
    NormDefinitions.startDateTime := ISO8601Date(now);
    AddSubNormNodes(Doc, BaseTestNode, NormDefinitions);

    {Insulin:}

    BaseTestNode := Doc.CreateElement('BaseTest');
    TDOMElement(BaseTestNode).SetAttribute('DefiningEntity', 'C');
    theNode := Doc.CreateComment('Definitions for Insulin:');
    BaseTestNode.AppendChild(theNode);
    BatteryNode.Appendchild(BaseTestNode);
    LabTestNode := Doc.CreateElement('LabTest');
    TDOMElement(LabTestNode).SetAttribute('ID', 'Insulin');
    TDOMElement(LabTestNode).SetAttribute('Name', 'Insulin');
    BaseTestNode.Appendchild(LabTestNode);

    ExclusionDefinitions.Sex := 'F';
    ExclusionDefinitions.AgeL := 0;
    ExclusionDefinitions.AgeH := 999;
    ExclusionDefinitions.UOMS := 'pmol/L';
    ExclusionDefinitions.UOMC := 'mIU/L';
    ExclusionDefinitions.LXS := 0;
    ExclusionDefinitions.HXS := 10000;
    ExclusionDefinitions.LXC := 0;
    ExclusionDefinitions.HXC := 10000;
    ExclusionDefinitions.startDateTime := '2000-01-01T12:00:00+01:00';
    AddSubExNodes(Doc, BaseTestNode, ExclusionDefinitions);
    NormDefinitions.Sex := 'F';
    NormDefinitions.AgeL := 0;
    NormDefinitions.AgeH := 130;
    NormDefinitions.UOMS := 'pmol/L';
    NormDefinitions.UOMC := 'mIU/L';
    if pos(UpCase('mol'), UpCase(ReferenceRanges.Insulin.UoM)) > 0 then
    begin
      NormDefinitions.LS := ReferenceRanges.Insulin.ln;
      NormDefinitions.HS := ReferenceRanges.Insulin.hn;
      NormDefinitions.LTS := ReferenceRanges.Insulin.lt;
      NormDefinitions.HTS := ReferenceRanges.Insulin.ht;
      NormDefinitions.LPS := ReferenceRanges.Insulin.lp;
      NormDefinitions.HPS := ReferenceRanges.Insulin.hp;
      NormDefinitions.LC := ReferenceRanges.Insulin.ln / kInsulinConversionFactor;
      NormDefinitions.HC := ReferenceRanges.Insulin.hn / kInsulinConversionFactor;
      NormDefinitions.LTC := ReferenceRanges.Insulin.lt /
        kInsulinConversionFactor;
      NormDefinitions.HTC := ReferenceRanges.Insulin.ht /
        kInsulinConversionFactor;
      NormDefinitions.LPC := ReferenceRanges.Insulin.lp /
        kInsulinConversionFactor;
      NormDefinitions.HPC := ReferenceRanges.Insulin.hp /
        kInsulinConversionFactor;
    end
    else
    begin
      NormDefinitions.LS := ReferenceRanges.Insulin.ln * kInsulinConversionFactor;
      NormDefinitions.HS := ReferenceRanges.Insulin.hn * kInsulinConversionFactor;
      NormDefinitions.LTS := ReferenceRanges.Insulin.lt *
        kInsulinConversionFactor;
      NormDefinitions.HTS := ReferenceRanges.Insulin.ht *
        kInsulinConversionFactor;
      NormDefinitions.LPS := ReferenceRanges.Insulin.lp *
        kInsulinConversionFactor;
      NormDefinitions.HPS := ReferenceRanges.Insulin.hp *
        kInsulinConversionFactor;
      NormDefinitions.LC := ReferenceRanges.Insulin.ln;
      NormDefinitions.HC := ReferenceRanges.Insulin.hn;
      NormDefinitions.LTC := ReferenceRanges.Insulin.lt;
      NormDefinitions.HTC := ReferenceRanges.Insulin.ht;
      NormDefinitions.LPC := ReferenceRanges.Insulin.lp;
      NormDefinitions.HPC := ReferenceRanges.Insulin.hp;
    end;
    NormDefinitions.startDateTime := ISO8601Date(now);
    AddSubNormNodes(Doc, BaseTestNode, NormDefinitions);

    theNode := Doc.CreateComment('Add additional age classes here');
    BaseTestNode.AppendChild(theNode);

    ExclusionDefinitions.Sex := 'M';
    ExclusionDefinitions.AgeL := 0;
    ExclusionDefinitions.AgeH := 999;
    ExclusionDefinitions.UOMS := 'pmol/L';
    ExclusionDefinitions.UOMC := 'mIU/L';
    ExclusionDefinitions.LXS := 0;
    ExclusionDefinitions.HXS := 10000;
    ExclusionDefinitions.LXC := 0;
    ExclusionDefinitions.HXC := 10000;
    ExclusionDefinitions.startDateTime := '2000-01-01T12:00:00+01:00';
    AddSubExNodes(Doc, BaseTestNode, ExclusionDefinitions);
    NormDefinitions.Sex := 'M';
    NormDefinitions.AgeL := 0;
    NormDefinitions.AgeH := 130;
    NormDefinitions.UOMS := 'pmol/L';
    NormDefinitions.UOMC := 'mIU/L';
    if pos(UpCase('mol'), UpCase(ReferenceRanges.Insulin.UoM)) > 0 then
    begin
      NormDefinitions.LS := ReferenceRanges.Insulin.ln;
      NormDefinitions.HS := ReferenceRanges.Insulin.hn;
      NormDefinitions.LTS := ReferenceRanges.Insulin.lt;
      NormDefinitions.HTS := ReferenceRanges.Insulin.ht;
      NormDefinitions.LPS := ReferenceRanges.Insulin.lp;
      NormDefinitions.HPS := ReferenceRanges.Insulin.hp;
      NormDefinitions.LC := ReferenceRanges.Insulin.ln / kInsulinConversionFactor;
      NormDefinitions.HC := ReferenceRanges.Insulin.hn / kInsulinConversionFactor;
      NormDefinitions.LTC := ReferenceRanges.Insulin.lt /
        kInsulinConversionFactor;
      NormDefinitions.HTC := ReferenceRanges.Insulin.ht /
        kInsulinConversionFactor;
      NormDefinitions.LPC := ReferenceRanges.Insulin.lp /
        kInsulinConversionFactor;
      NormDefinitions.HPC := ReferenceRanges.Insulin.hp /
        kInsulinConversionFactor;
    end
    else
    begin
      NormDefinitions.LS := ReferenceRanges.Insulin.ln * kInsulinConversionFactor;
      NormDefinitions.HS := ReferenceRanges.Insulin.hn * kInsulinConversionFactor;
      NormDefinitions.LTS := ReferenceRanges.Insulin.lt *
        kInsulinConversionFactor;
      NormDefinitions.HTS := ReferenceRanges.Insulin.ht *
        kInsulinConversionFactor;
      NormDefinitions.LPS := ReferenceRanges.Insulin.lp *
        kInsulinConversionFactor;
      NormDefinitions.HPS := ReferenceRanges.Insulin.hp *
        kInsulinConversionFactor;
      NormDefinitions.LC := ReferenceRanges.Insulin.ln;
      NormDefinitions.HC := ReferenceRanges.Insulin.hn;
      NormDefinitions.LTC := ReferenceRanges.Insulin.lt;
      NormDefinitions.HTC := ReferenceRanges.Insulin.ht;
      NormDefinitions.LPC := ReferenceRanges.Insulin.lp;
      NormDefinitions.HPC := ReferenceRanges.Insulin.hp;
    end;
    NormDefinitions.startDateTime := ISO8601Date(now);
    AddSubNormNodes(Doc, BaseTestNode, NormDefinitions);

    {C-Peptide:}

    BaseTestNode := Doc.CreateElement('BaseTest');
    TDOMElement(BaseTestNode).SetAttribute('DefiningEntity', 'C');
    theNode := Doc.CreateComment('Definitions for C-Peptide:');
    BaseTestNode.AppendChild(theNode);
    BatteryNode.Appendchild(BaseTestNode);
    LabTestNode := Doc.CreateElement('LabTest');
    TDOMElement(LabTestNode).SetAttribute('ID', 'C-Peptide');
    TDOMElement(LabTestNode).SetAttribute('Name', 'C-Peptide');
    BaseTestNode.Appendchild(LabTestNode);

    ExclusionDefinitions.Sex := 'F';
    ExclusionDefinitions.AgeL := 0;
    ExclusionDefinitions.AgeH := 999;
    ExclusionDefinitions.UOMS := 'nmol/L';
    ExclusionDefinitions.UOMC := 'ng/mL';
    ExclusionDefinitions.LXS := 0;
    ExclusionDefinitions.HXS := 10000;
    ExclusionDefinitions.LXC := 0;
    ExclusionDefinitions.HXC := 10000;
    ExclusionDefinitions.startDateTime := '2000-01-01T12:00:00+01:00';
    AddSubExNodes(Doc, BaseTestNode, ExclusionDefinitions);
    NormDefinitions.Sex := 'F';
    NormDefinitions.AgeL := 0;
    NormDefinitions.AgeH := 130;
    NormDefinitions.UOMS := 'nmol/L';
    NormDefinitions.UOMC := 'ng/mL';
    if pos(UpCase('mol'), UpCase(ReferenceRanges.CPeptide.UoM)) > 0 then
    begin
      NormDefinitions.LS := ReferenceRanges.CPeptide.ln;
      NormDefinitions.HS := ReferenceRanges.CPeptide.hn;
      NormDefinitions.LTS := ReferenceRanges.CPeptide.lt;
      NormDefinitions.HTS := ReferenceRanges.CPeptide.ht;
      NormDefinitions.LPS := ReferenceRanges.CPeptide.lp;
      NormDefinitions.HPS := ReferenceRanges.CPeptide.hp;
      NormDefinitions.LC := ConvertedValue(ReferenceRanges.CPeptide.ln,
        kCPeptideMolarMass, ReferenceRanges.CPeptide.UoM, 'ng/mL');
      NormDefinitions.HC := ConvertedValue(ReferenceRanges.CPeptide.hn,
        kCPeptideMolarMass, ReferenceRanges.CPeptide.UoM, 'ng/mL');
      NormDefinitions.LTC := ConvertedValue(ReferenceRanges.CPeptide.lt,
        kCPeptideMolarMass, ReferenceRanges.CPeptide.UoM, 'ng/mL');
      NormDefinitions.HTC := ConvertedValue(ReferenceRanges.CPeptide.ht,
        kCPeptideMolarMass, ReferenceRanges.CPeptide.UoM, 'ng/mL');
      NormDefinitions.LPC := ConvertedValue(ReferenceRanges.CPeptide.lp,
        kCPeptideMolarMass, ReferenceRanges.CPeptide.UoM, 'ng/mL');
      NormDefinitions.HPC := ConvertedValue(ReferenceRanges.CPeptide.hp,
        kCPeptideMolarMass, ReferenceRanges.CPeptide.UoM, 'ng/mL');
    end
    else
    begin
      NormDefinitions.LS := ConvertedValue(ReferenceRanges.CPeptide.ln,
        kCPeptideMolarMass, ReferenceRanges.CPeptide.UoM, 'nmol/L');
      NormDefinitions.HS := ConvertedValue(ReferenceRanges.CPeptide.hn,
        kCPeptideMolarMass, ReferenceRanges.CPeptide.UoM, 'nmol/L');
      NormDefinitions.LTS := ConvertedValue(ReferenceRanges.CPeptide.lt,
        kCPeptideMolarMass, ReferenceRanges.CPeptide.UoM, 'nmol/L');
      NormDefinitions.HTS := ConvertedValue(ReferenceRanges.CPeptide.ht,
        kCPeptideMolarMass, ReferenceRanges.CPeptide.UoM, 'nmol/L');
      NormDefinitions.LPS := ConvertedValue(ReferenceRanges.CPeptide.lp,
        kCPeptideMolarMass, ReferenceRanges.CPeptide.UoM, 'nmol/L');
      NormDefinitions.HPS := ConvertedValue(ReferenceRanges.CPeptide.hp,
        kCPeptideMolarMass, ReferenceRanges.CPeptide.UoM, 'nmol/L');
      NormDefinitions.LC := ReferenceRanges.CPeptide.ln;
      NormDefinitions.HC := ReferenceRanges.CPeptide.hn;
      NormDefinitions.LTC := ReferenceRanges.CPeptide.lt;
      NormDefinitions.HTC := ReferenceRanges.CPeptide.ht;
      NormDefinitions.LPC := ReferenceRanges.CPeptide.lp;
      NormDefinitions.HPC := ReferenceRanges.CPeptide.hp;
    end;
    NormDefinitions.startDateTime := ISO8601Date(now);
    AddSubNormNodes(Doc, BaseTestNode, NormDefinitions);

    theNode := Doc.CreateComment('Add additional age classes here');
    BaseTestNode.AppendChild(theNode);

    ExclusionDefinitions.Sex := 'M';
    ExclusionDefinitions.AgeL := 0;
    ExclusionDefinitions.AgeH := 999;
    ExclusionDefinitions.UOMS := 'nmol/L';
    ExclusionDefinitions.UOMC := 'ng/mL';
    ExclusionDefinitions.LXS := 0;
    ExclusionDefinitions.HXS := 10000;
    ExclusionDefinitions.LXC := 0;
    ExclusionDefinitions.HXC := 10000;
    ExclusionDefinitions.startDateTime := '2000-01-01T12:00:00+01:00';
    AddSubExNodes(Doc, BaseTestNode, ExclusionDefinitions);
    NormDefinitions.Sex := 'M';
    NormDefinitions.AgeL := 0;
    NormDefinitions.AgeH := 130;
    NormDefinitions.UOMS := 'nmol/L';
    NormDefinitions.UOMC := 'ng/mL';
    if pos(UpCase('mol'), UpCase(ReferenceRanges.CPeptide.UoM)) > 0 then
    begin
      NormDefinitions.LS := ReferenceRanges.CPeptide.ln;
      NormDefinitions.HS := ReferenceRanges.CPeptide.hn;
      NormDefinitions.LTS := ReferenceRanges.CPeptide.lt;
      NormDefinitions.HTS := ReferenceRanges.CPeptide.ht;
      NormDefinitions.LPS := ReferenceRanges.CPeptide.lp;
      NormDefinitions.HPS := ReferenceRanges.CPeptide.hp;
      NormDefinitions.LC := ConvertedValue(ReferenceRanges.CPeptide.ln,
        kCPeptideMolarMass, ReferenceRanges.CPeptide.UoM, 'ng/mL');
      NormDefinitions.HC := ConvertedValue(ReferenceRanges.CPeptide.hn,
        kCPeptideMolarMass, ReferenceRanges.CPeptide.UoM, 'ng/mL');
      NormDefinitions.LTC := ConvertedValue(ReferenceRanges.CPeptide.lt,
        kCPeptideMolarMass, ReferenceRanges.CPeptide.UoM, 'ng/mL');
      NormDefinitions.HTC := ConvertedValue(ReferenceRanges.CPeptide.ht,
        kCPeptideMolarMass, ReferenceRanges.CPeptide.UoM, 'ng/mL');
      NormDefinitions.LPC := ConvertedValue(ReferenceRanges.CPeptide.lp,
        kCPeptideMolarMass, ReferenceRanges.CPeptide.UoM, 'ng/mL');
      NormDefinitions.HPC := ConvertedValue(ReferenceRanges.CPeptide.hp,
        kCPeptideMolarMass, ReferenceRanges.CPeptide.UoM, 'ng/mL');
    end
    else
    begin
      NormDefinitions.LS := ConvertedValue(ReferenceRanges.CPeptide.ln,
        kCPeptideMolarMass, ReferenceRanges.CPeptide.UoM, 'nmol/L');
      NormDefinitions.HS := ConvertedValue(ReferenceRanges.CPeptide.hn,
        kCPeptideMolarMass, ReferenceRanges.CPeptide.UoM, 'nmol/L');
      NormDefinitions.LTS := ConvertedValue(ReferenceRanges.CPeptide.lt,
        kCPeptideMolarMass, ReferenceRanges.CPeptide.UoM, 'nmol/L');
      NormDefinitions.HTS := ConvertedValue(ReferenceRanges.CPeptide.ht,
        kCPeptideMolarMass, ReferenceRanges.CPeptide.UoM, 'nmol/L');
      NormDefinitions.LPS := ConvertedValue(ReferenceRanges.CPeptide.lp,
        kCPeptideMolarMass, ReferenceRanges.CPeptide.UoM, 'nmol/L');
      NormDefinitions.HPS := ConvertedValue(ReferenceRanges.CPeptide.hp,
        kCPeptideMolarMass, ReferenceRanges.CPeptide.UoM, 'nmol/L');
      NormDefinitions.LC := ReferenceRanges.CPeptide.ln;
      NormDefinitions.HC := ReferenceRanges.CPeptide.hn;
      NormDefinitions.LTC := ReferenceRanges.CPeptide.lt;
      NormDefinitions.HTC := ReferenceRanges.CPeptide.ht;
      NormDefinitions.LPC := ReferenceRanges.CPeptide.lp;
      NormDefinitions.HPC := ReferenceRanges.CPeptide.hp;
    end;
    NormDefinitions.startDateTime := ISO8601Date(now);
    AddSubNormNodes(Doc, BaseTestNode, NormDefinitions);

    {SPINA Carb etc.:}

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
    TDOMElement(LabTestNode).SetAttribute('Name',
      'Beta cells'' Secretory Capacity (SPINA-GBeta)');
    BaseTestNode.Appendchild(LabTestNode);

    ExclusionDefinitions.Sex := 'F';
    ExclusionDefinitions.AgeL := 0;
    ExclusionDefinitions.AgeH := 999;
    ExclusionDefinitions.UOMS := 'pmol/s';
    ExclusionDefinitions.UOMC := 'pmol/s';
    ExclusionDefinitions.LXS := 0;
    ExclusionDefinitions.HXS := 10000;
    ExclusionDefinitions.LXC := 0;
    ExclusionDefinitions.HXC := 10000;
    ExclusionDefinitions.startDateTime := '2000-01-01T12:00:00+01:00';
    AddSubExNodes(Doc, BaseTestNode, ExclusionDefinitions);
    NormDefinitions.Sex := 'F';
    NormDefinitions.AgeL := 0;
    NormDefinitions.AgeH := 130;
    NormDefinitions.UOMS := 'pmol/s';
    NormDefinitions.UOMC := 'pmol/s';
    NormDefinitions.LS := ReferenceRanges.SPINA_GBeta.ln;
    NormDefinitions.HS := ReferenceRanges.SPINA_GBeta.hn;
    NormDefinitions.LTS := ReferenceRanges.SPINA_GBeta.lt;
    NormDefinitions.HTS := ReferenceRanges.SPINA_GBeta.ht;
    NormDefinitions.LPS := ReferenceRanges.SPINA_GBeta.lp;
    NormDefinitions.HPS := ReferenceRanges.SPINA_GBeta.hp;
    NormDefinitions.LC := ReferenceRanges.SPINA_GBeta.ln;
    NormDefinitions.HC := ReferenceRanges.SPINA_GBeta.hn;
    NormDefinitions.LTC := ReferenceRanges.SPINA_GBeta.lt;
    NormDefinitions.HTC := ReferenceRanges.SPINA_GBeta.ht;
    NormDefinitions.LPC := ReferenceRanges.SPINA_GBeta.lp;
    NormDefinitions.HPC := ReferenceRanges.SPINA_GBeta.hp;
    NormDefinitions.startDateTime := ISO8601Date(now);
    AddSubNormNodes(Doc, BaseTestNode, NormDefinitions);

    theNode := Doc.CreateComment('Add additional age classes here');
    BaseTestNode.AppendChild(theNode);

    ExclusionDefinitions.Sex := 'M';
    ExclusionDefinitions.AgeL := 0;
    ExclusionDefinitions.AgeH := 999;
    ExclusionDefinitions.UOMS := 'pmol/s';
    ExclusionDefinitions.UOMC := 'pmol/s';
    ExclusionDefinitions.LXS := 0;
    ExclusionDefinitions.HXS := 10000;
    ExclusionDefinitions.LXC := 0;
    ExclusionDefinitions.HXC := 10000;
    ExclusionDefinitions.startDateTime := '2000-01-01T12:00:00+01:00';
    AddSubExNodes(Doc, BaseTestNode, ExclusionDefinitions);
    NormDefinitions.Sex := 'M';
    NormDefinitions.AgeL := 0;
    NormDefinitions.AgeH := 130;
    NormDefinitions.UOMS := 'pmol/s';
    NormDefinitions.UOMC := 'pmol/s';
    NormDefinitions.LS := ReferenceRanges.SPINA_GBeta.ln;
    NormDefinitions.HS := ReferenceRanges.SPINA_GBeta.hn;
    NormDefinitions.LTS := ReferenceRanges.SPINA_GBeta.lt;
    NormDefinitions.HTS := ReferenceRanges.SPINA_GBeta.ht;
    NormDefinitions.LPS := ReferenceRanges.SPINA_GBeta.lp;
    NormDefinitions.HPS := ReferenceRanges.SPINA_GBeta.hp;
    NormDefinitions.LC := ReferenceRanges.SPINA_GBeta.ln;
    NormDefinitions.HC := ReferenceRanges.SPINA_GBeta.hn;
    NormDefinitions.LTC := ReferenceRanges.SPINA_GBeta.lt;
    NormDefinitions.HTC := ReferenceRanges.SPINA_GBeta.ht;
    NormDefinitions.LPC := ReferenceRanges.SPINA_GBeta.lp;
    NormDefinitions.HPC := ReferenceRanges.SPINA_GBeta.hp;
    NormDefinitions.startDateTime := ISO8601Date(now);
    AddSubNormNodes(Doc, BaseTestNode, NormDefinitions);

    {SPINA-GR:}

    BaseTestNode := Doc.CreateElement('BaseTest');
    TDOMElement(BaseTestNode).SetAttribute('DefiningEntity', 'C');
    theNode := Doc.CreateComment('Definitions for SPINA-GR:');
    BaseTestNode.AppendChild(theNode);
    BatteryNode.Appendchild(BaseTestNode);
    LabTestNode := Doc.CreateElement('LabTest');
    TDOMElement(LabTestNode).SetAttribute('ID', 'GR');
    TDOMElement(LabTestNode).SetAttribute('Name', 'Insulin Receptor Gain (SPINA-GR)');
    BaseTestNode.Appendchild(LabTestNode);

    ExclusionDefinitions.Sex := 'F';
    ExclusionDefinitions.AgeL := 0;
    ExclusionDefinitions.AgeH := 999;
    ExclusionDefinitions.UOMS := 'mol/s';
    ExclusionDefinitions.UOMC := 'mol/s';
    ExclusionDefinitions.LXS := 0;
    ExclusionDefinitions.HXS := 10000;
    ExclusionDefinitions.LXC := 0;
    ExclusionDefinitions.HXC := 10000;
    ExclusionDefinitions.startDateTime := '2000-01-01T12:00:00+01:00';
    AddSubExNodes(Doc, BaseTestNode, ExclusionDefinitions);
    NormDefinitions.Sex := 'F';
    NormDefinitions.AgeL := 0;
    NormDefinitions.AgeH := 130;
    NormDefinitions.UOMS := 'mol/s';
    NormDefinitions.UOMC := 'mol/s';
    NormDefinitions.LS := ReferenceRanges.SPINA_GR.ln;
    NormDefinitions.HS := ReferenceRanges.SPINA_GR.hn;
    NormDefinitions.LTS := ReferenceRanges.SPINA_GR.lt;
    NormDefinitions.HTS := ReferenceRanges.SPINA_GR.ht;
    NormDefinitions.LPS := ReferenceRanges.SPINA_GR.lp;
    NormDefinitions.HPS := ReferenceRanges.SPINA_GR.hp;
    NormDefinitions.LC := ReferenceRanges.SPINA_GR.ln;
    NormDefinitions.HC := ReferenceRanges.SPINA_GR.hn;
    NormDefinitions.LTC := ReferenceRanges.SPINA_GR.lt;
    NormDefinitions.HTC := ReferenceRanges.SPINA_GR.ht;
    NormDefinitions.LPC := ReferenceRanges.SPINA_GR.lp;
    NormDefinitions.HPC := ReferenceRanges.SPINA_GR.hp;
    NormDefinitions.startDateTime := ISO8601Date(now);
    AddSubNormNodes(Doc, BaseTestNode, NormDefinitions);

    theNode := Doc.CreateComment('Add additional age classes here');
    BaseTestNode.AppendChild(theNode);

    ExclusionDefinitions.Sex := 'M';
    ExclusionDefinitions.AgeL := 0;
    ExclusionDefinitions.AgeH := 999;
    ExclusionDefinitions.UOMS := 'mol/s';
    ExclusionDefinitions.UOMC := 'mol/s';
    ExclusionDefinitions.LXS := 0;
    ExclusionDefinitions.HXS := 10000;
    ExclusionDefinitions.LXC := 0;
    ExclusionDefinitions.HXC := 10000;
    ExclusionDefinitions.startDateTime := '2000-01-01T12:00:00+01:00';
    AddSubExNodes(Doc, BaseTestNode, ExclusionDefinitions);
    NormDefinitions.Sex := 'M';
    NormDefinitions.AgeL := 0;
    NormDefinitions.AgeH := 130;
    NormDefinitions.UOMS := 'mol/s';
    NormDefinitions.UOMC := 'mol/s';
    NormDefinitions.LS := ReferenceRanges.SPINA_GR.ln;
    NormDefinitions.HS := ReferenceRanges.SPINA_GR.hn;
    NormDefinitions.LTS := ReferenceRanges.SPINA_GR.lt;
    NormDefinitions.HTS := ReferenceRanges.SPINA_GR.ht;
    NormDefinitions.LPS := ReferenceRanges.SPINA_GR.lp;
    NormDefinitions.HPS := ReferenceRanges.SPINA_GR.hp;
    NormDefinitions.LC := ReferenceRanges.SPINA_GR.ln;
    NormDefinitions.HC := ReferenceRanges.SPINA_GR.hn;
    NormDefinitions.LTC := ReferenceRanges.SPINA_GR.lt;
    NormDefinitions.HTC := ReferenceRanges.SPINA_GR.ht;
    NormDefinitions.LPC := ReferenceRanges.SPINA_GR.lp;
    NormDefinitions.HPC := ReferenceRanges.SPINA_GR.hp;
    NormDefinitions.startDateTime := ISO8601Date(now);
    AddSubNormNodes(Doc, BaseTestNode, NormDefinitions);

    {SPINA-DI:}

    BaseTestNode := Doc.CreateElement('BaseTest');
    TDOMElement(BaseTestNode).SetAttribute('DefiningEntity', 'C');
    theNode := Doc.CreateComment('Definitions for SPINA-DI:');
    BaseTestNode.AppendChild(theNode);
    BatteryNode.Appendchild(BaseTestNode);
    LabTestNode := Doc.CreateElement('LabTest');
    TDOMElement(LabTestNode).SetAttribute('ID', 'DI');
    TDOMElement(LabTestNode).SetAttribute('Name', 'Static Disposition Index (SPINA-DI)');
    BaseTestNode.Appendchild(LabTestNode);

    ExclusionDefinitions.Sex := 'F';
    ExclusionDefinitions.AgeL := 0;
    ExclusionDefinitions.AgeH := 999;
    ExclusionDefinitions.UOMS := '';
    ExclusionDefinitions.UOMC := '';
    ExclusionDefinitions.LXS := 0;
    ExclusionDefinitions.HXS := 10000;
    ExclusionDefinitions.LXC := 0;
    ExclusionDefinitions.HXC := 10000;
    ExclusionDefinitions.startDateTime := '2000-01-01T12:00:00+01:00';
    AddSubExNodes(Doc, BaseTestNode, ExclusionDefinitions);
    NormDefinitions.Sex := 'F';
    NormDefinitions.AgeL := 0;
    NormDefinitions.AgeH := 130;
    NormDefinitions.UOMS := '';
    NormDefinitions.UOMC := '';
    NormDefinitions.LS := ReferenceRanges.SPINA_DI.ln;
    NormDefinitions.HS := ReferenceRanges.SPINA_DI.hn;
    NormDefinitions.LTS := ReferenceRanges.SPINA_DI.lt;
    NormDefinitions.HTS := ReferenceRanges.SPINA_DI.ht;
    NormDefinitions.LPS := ReferenceRanges.SPINA_DI.lp;
    NormDefinitions.HPS := ReferenceRanges.SPINA_DI.hp;
    NormDefinitions.LC := ReferenceRanges.SPINA_DI.ln;
    NormDefinitions.HC := ReferenceRanges.SPINA_DI.hn;
    NormDefinitions.LTC := ReferenceRanges.SPINA_DI.lt;
    NormDefinitions.HTC := ReferenceRanges.SPINA_DI.ht;
    NormDefinitions.LPC := ReferenceRanges.SPINA_DI.lp;
    NormDefinitions.HPC := ReferenceRanges.SPINA_DI.hp;
    NormDefinitions.startDateTime := ISO8601Date(now);
    AddSubNormNodes(Doc, BaseTestNode, NormDefinitions);

    theNode := Doc.CreateComment('Add additional age classes here');
    BaseTestNode.AppendChild(theNode);

    ExclusionDefinitions.Sex := 'M';
    ExclusionDefinitions.AgeL := 0;
    ExclusionDefinitions.AgeH := 999;
    ExclusionDefinitions.UOMS := '';
    ExclusionDefinitions.UOMC := '';
    ExclusionDefinitions.LXS := 0;
    ExclusionDefinitions.HXS := 10000;
    ExclusionDefinitions.LXC := 0;
    ExclusionDefinitions.HXC := 10000;
    ExclusionDefinitions.startDateTime := '2000-01-01T12:00:00+01:00';
    AddSubExNodes(Doc, BaseTestNode, ExclusionDefinitions);
    NormDefinitions.Sex := 'M';
    NormDefinitions.AgeL := 0;
    NormDefinitions.AgeH := 130;
    NormDefinitions.UOMS := '';
    NormDefinitions.UOMC := '';
    NormDefinitions.LS := ReferenceRanges.SPINA_DI.ln;
    NormDefinitions.HS := ReferenceRanges.SPINA_DI.hn;
    NormDefinitions.LTS := ReferenceRanges.SPINA_DI.lt;
    NormDefinitions.HTS := ReferenceRanges.SPINA_DI.ht;
    NormDefinitions.LPS := ReferenceRanges.SPINA_DI.lp;
    NormDefinitions.HPS := ReferenceRanges.SPINA_DI.hp;
    NormDefinitions.LC := ReferenceRanges.SPINA_DI.ln;
    NormDefinitions.HC := ReferenceRanges.SPINA_DI.hn;
    NormDefinitions.LTC := ReferenceRanges.SPINA_DI.lt;
    NormDefinitions.HTC := ReferenceRanges.SPINA_DI.ht;
    NormDefinitions.LPC := ReferenceRanges.SPINA_DI.lp;
    NormDefinitions.HPC := ReferenceRanges.SPINA_DI.hp;
    NormDefinitions.startDateTime := ISO8601Date(now);
    AddSubNormNodes(Doc, BaseTestNode, NormDefinitions);

    {HOMA-Beta:}

    BaseTestNode := Doc.CreateElement('BaseTest');
    TDOMElement(BaseTestNode).SetAttribute('DefiningEntity', 'C');
    theNode := Doc.CreateComment('Definitions for HOMA-Beta:');
    BaseTestNode.AppendChild(theNode);
    BatteryNode.Appendchild(BaseTestNode);
    LabTestNode := Doc.CreateElement('LabTest');
    TDOMElement(LabTestNode).SetAttribute('ID', 'HOMA-Beta');
    TDOMElement(LabTestNode).SetAttribute('Name',
      'Homeostasis Model Assessment for beta cell function');
    BaseTestNode.Appendchild(LabTestNode);

    ExclusionDefinitions.Sex := 'F';
    ExclusionDefinitions.AgeL := 0;
    ExclusionDefinitions.AgeH := 999;
    ExclusionDefinitions.UOMS := '%';
    ExclusionDefinitions.UOMC := '%';
    ExclusionDefinitions.LXS := 0;
    ExclusionDefinitions.HXS := 10000;
    ExclusionDefinitions.LXC := 0;
    ExclusionDefinitions.HXC := 10000;
    ExclusionDefinitions.startDateTime := '2000-01-01T12:00:00+01:00';
    AddSubExNodes(Doc, BaseTestNode, ExclusionDefinitions);
    NormDefinitions.Sex := 'F';
    NormDefinitions.AgeL := 0;
    NormDefinitions.AgeH := 130;
    NormDefinitions.UOMS := '%';
    NormDefinitions.UOMC := '%';
    NormDefinitions.LS := ReferenceRanges.HOMA_Beta.ln;
    NormDefinitions.HS := ReferenceRanges.HOMA_Beta.hn;
    NormDefinitions.LTS := ReferenceRanges.HOMA_Beta.lt;
    NormDefinitions.HTS := ReferenceRanges.HOMA_Beta.ht;
    NormDefinitions.LPS := ReferenceRanges.HOMA_Beta.lp;
    NormDefinitions.HPS := ReferenceRanges.HOMA_Beta.hp;
    NormDefinitions.LC := ReferenceRanges.HOMA_Beta.ln;
    NormDefinitions.HC := ReferenceRanges.HOMA_Beta.hn;
    NormDefinitions.LTC := ReferenceRanges.HOMA_Beta.lt;
    NormDefinitions.HTC := ReferenceRanges.HOMA_Beta.ht;
    NormDefinitions.LPC := ReferenceRanges.HOMA_Beta.lp;
    NormDefinitions.HPC := ReferenceRanges.HOMA_Beta.hp;
    NormDefinitions.startDateTime := ISO8601Date(now);
    AddSubNormNodes(Doc, BaseTestNode, NormDefinitions);

    theNode := Doc.CreateComment('Add additional age classes here');
    BaseTestNode.AppendChild(theNode);

    ExclusionDefinitions.Sex := 'M';
    ExclusionDefinitions.AgeL := 0;
    ExclusionDefinitions.AgeH := 999;
    ExclusionDefinitions.UOMS := '%';
    ExclusionDefinitions.UOMC := '%';
    ExclusionDefinitions.LXS := 0;
    ExclusionDefinitions.HXS := 10000;
    ExclusionDefinitions.LXC := 0;
    ExclusionDefinitions.HXC := 10000;
    ExclusionDefinitions.startDateTime := '2000-01-01T12:00:00+01:00';
    AddSubExNodes(Doc, BaseTestNode, ExclusionDefinitions);
    NormDefinitions.Sex := 'M';
    NormDefinitions.AgeL := 0;
    NormDefinitions.AgeH := 130;
    NormDefinitions.UOMS := '%';
    NormDefinitions.UOMC := '%';
    NormDefinitions.LS := ReferenceRanges.HOMA_Beta.ln;
    NormDefinitions.HS := ReferenceRanges.HOMA_Beta.hn;
    NormDefinitions.LTS := ReferenceRanges.HOMA_Beta.lt;
    NormDefinitions.HTS := ReferenceRanges.HOMA_Beta.ht;
    NormDefinitions.LPS := ReferenceRanges.HOMA_Beta.lp;
    NormDefinitions.HPS := ReferenceRanges.HOMA_Beta.hp;
    NormDefinitions.LC := ReferenceRanges.HOMA_Beta.ln;
    NormDefinitions.HC := ReferenceRanges.HOMA_Beta.hn;
    NormDefinitions.LTC := ReferenceRanges.HOMA_Beta.lt;
    NormDefinitions.HTC := ReferenceRanges.HOMA_Beta.ht;
    NormDefinitions.LPC := ReferenceRanges.HOMA_Beta.lp;
    NormDefinitions.HPC := ReferenceRanges.HOMA_Beta.hp;
    NormDefinitions.startDateTime := ISO8601Date(now);
    AddSubNormNodes(Doc, BaseTestNode, NormDefinitions);

    {HOMA-IR:}

    BaseTestNode := Doc.CreateElement('BaseTest');
    TDOMElement(BaseTestNode).SetAttribute('DefiningEntity', 'C');
    theNode := Doc.CreateComment('Definitions for HOMA-IR:');
    BaseTestNode.AppendChild(theNode);
    BatteryNode.Appendchild(BaseTestNode);
    LabTestNode := Doc.CreateElement('LabTest');
    TDOMElement(LabTestNode).SetAttribute('ID', 'HOMA-IR');
    TDOMElement(LabTestNode).SetAttribute('Name',
      'Homeostasis Model Assessment for insulin resistance');
    BaseTestNode.Appendchild(LabTestNode);

    ExclusionDefinitions.Sex := 'F';
    ExclusionDefinitions.AgeL := 0;
    ExclusionDefinitions.AgeH := 999;
    ExclusionDefinitions.UOMS := '';
    ExclusionDefinitions.UOMC := '';
    ExclusionDefinitions.LXS := 0;
    ExclusionDefinitions.HXS := 10000;
    ExclusionDefinitions.LXC := 0;
    ExclusionDefinitions.HXC := 10000;
    ExclusionDefinitions.startDateTime := '2000-01-01T12:00:00+01:00';
    AddSubExNodes(Doc, BaseTestNode, ExclusionDefinitions);
    NormDefinitions.Sex := 'F';
    NormDefinitions.AgeL := 0;
    NormDefinitions.AgeH := 130;
    NormDefinitions.UOMS := '';
    NormDefinitions.UOMC := '';
    NormDefinitions.LS := ReferenceRanges.HOMA_IR.ln;
    NormDefinitions.HS := ReferenceRanges.HOMA_IR.hn;
    NormDefinitions.LTS := ReferenceRanges.HOMA_IR.lt;
    NormDefinitions.HTS := ReferenceRanges.HOMA_IR.ht;
    NormDefinitions.LPS := ReferenceRanges.HOMA_IR.lp;
    NormDefinitions.HPS := ReferenceRanges.HOMA_IR.hp;
    NormDefinitions.LC := ReferenceRanges.HOMA_IR.ln;
    NormDefinitions.HC := ReferenceRanges.HOMA_IR.hn;
    NormDefinitions.LTC := ReferenceRanges.HOMA_IR.lt;
    NormDefinitions.HTC := ReferenceRanges.HOMA_IR.ht;
    NormDefinitions.LPC := ReferenceRanges.HOMA_IR.lp;
    NormDefinitions.HPC := ReferenceRanges.HOMA_IR.hp;
    NormDefinitions.startDateTime := ISO8601Date(now);
    AddSubNormNodes(Doc, BaseTestNode, NormDefinitions);

    theNode := Doc.CreateComment('Add additional age classes here');
    BaseTestNode.AppendChild(theNode);

    ExclusionDefinitions.Sex := 'M';
    ExclusionDefinitions.AgeL := 0;
    ExclusionDefinitions.AgeH := 999;
    ExclusionDefinitions.UOMS := '';
    ExclusionDefinitions.UOMC := '';
    ExclusionDefinitions.LXS := 0;
    ExclusionDefinitions.HXS := 10000;
    ExclusionDefinitions.LXC := 0;
    ExclusionDefinitions.HXC := 10000;
    ExclusionDefinitions.startDateTime := '2000-01-01T12:00:00+01:00';
    AddSubExNodes(Doc, BaseTestNode, ExclusionDefinitions);
    NormDefinitions.Sex := 'M';
    NormDefinitions.AgeL := 0;
    NormDefinitions.AgeH := 130;
    NormDefinitions.UOMS := '';
    NormDefinitions.UOMC := '';
    NormDefinitions.LS := ReferenceRanges.HOMA_IR.ln;
    NormDefinitions.HS := ReferenceRanges.HOMA_IR.hn;
    NormDefinitions.LTS := ReferenceRanges.HOMA_IR.lt;
    NormDefinitions.HTS := ReferenceRanges.HOMA_IR.ht;
    NormDefinitions.LPS := ReferenceRanges.HOMA_IR.lp;
    NormDefinitions.HPS := ReferenceRanges.HOMA_IR.hp;
    NormDefinitions.LC := ReferenceRanges.HOMA_IR.ln;
    NormDefinitions.HC := ReferenceRanges.HOMA_IR.hn;
    NormDefinitions.LTC := ReferenceRanges.HOMA_IR.lt;
    NormDefinitions.HTC := ReferenceRanges.HOMA_IR.ht;
    NormDefinitions.LPC := ReferenceRanges.HOMA_IR.lp;
    NormDefinitions.HPC := ReferenceRanges.HOMA_IR.hp;
    NormDefinitions.startDateTime := ISO8601Date(now);
    AddSubNormNodes(Doc, BaseTestNode, NormDefinitions);

    {HOMA-IS:}

    BaseTestNode := Doc.CreateElement('BaseTest');
    TDOMElement(BaseTestNode).SetAttribute('DefiningEntity', 'C');
    theNode := Doc.CreateComment('Definitions for HOMA-IS:');
    BaseTestNode.AppendChild(theNode);
    BatteryNode.Appendchild(BaseTestNode);
    LabTestNode := Doc.CreateElement('LabTest');
    TDOMElement(LabTestNode).SetAttribute('ID', 'HOMA-IS');
    TDOMElement(LabTestNode).SetAttribute('Name',
      'Homeostasis Model Assessment for insulin sensitivity');
    BaseTestNode.Appendchild(LabTestNode);

    ExclusionDefinitions.Sex := 'F';
    ExclusionDefinitions.AgeL := 0;
    ExclusionDefinitions.AgeH := 999;
    ExclusionDefinitions.UOMS := '';
    ExclusionDefinitions.UOMC := '';
    ExclusionDefinitions.LXS := 0;
    ExclusionDefinitions.HXS := 10000;
    ExclusionDefinitions.LXC := 0;
    ExclusionDefinitions.HXC := 10000;
    ExclusionDefinitions.startDateTime := '2000-01-01T12:00:00+01:00';
    AddSubExNodes(Doc, BaseTestNode, ExclusionDefinitions);
    NormDefinitions.Sex := 'F';
    NormDefinitions.AgeL := 0;
    NormDefinitions.AgeH := 130;
    NormDefinitions.UOMS := '';
    NormDefinitions.UOMC := '';
    NormDefinitions.LS := ReferenceRanges.HOMA_IS.ln;
    NormDefinitions.HS := ReferenceRanges.HOMA_IS.hn;
    NormDefinitions.LTS := ReferenceRanges.HOMA_IS.lt;
    NormDefinitions.HTS := ReferenceRanges.HOMA_IS.ht;
    NormDefinitions.LPS := ReferenceRanges.HOMA_IS.lp;
    NormDefinitions.HPS := ReferenceRanges.HOMA_IS.hp;
    NormDefinitions.LC := ReferenceRanges.HOMA_IS.ln;
    NormDefinitions.HC := ReferenceRanges.HOMA_IS.hn;
    NormDefinitions.LTC := ReferenceRanges.HOMA_IS.lt;
    NormDefinitions.HTC := ReferenceRanges.HOMA_IS.ht;
    NormDefinitions.LPC := ReferenceRanges.HOMA_IS.lp;
    NormDefinitions.HPC := ReferenceRanges.HOMA_IS.hp;
    NormDefinitions.startDateTime := ISO8601Date(now);
    AddSubNormNodes(Doc, BaseTestNode, NormDefinitions);

    theNode := Doc.CreateComment('Add additional age classes here');
    BaseTestNode.AppendChild(theNode);

    ExclusionDefinitions.Sex := 'M';
    ExclusionDefinitions.AgeL := 0;
    ExclusionDefinitions.AgeH := 999;
    ExclusionDefinitions.UOMS := '';
    ExclusionDefinitions.UOMC := '';
    ExclusionDefinitions.LXS := 0;
    ExclusionDefinitions.HXS := 10000;
    ExclusionDefinitions.LXC := 0;
    ExclusionDefinitions.HXC := 10000;
    ExclusionDefinitions.startDateTime := '2000-01-01T12:00:00+01:00';
    AddSubExNodes(Doc, BaseTestNode, ExclusionDefinitions);
    NormDefinitions.Sex := 'M';
    NormDefinitions.AgeL := 0;
    NormDefinitions.AgeH := 130;
    NormDefinitions.UOMS := '';
    NormDefinitions.UOMC := '';
    NormDefinitions.LS := ReferenceRanges.HOMA_IS.ln;
    NormDefinitions.HS := ReferenceRanges.HOMA_IS.hn;
    NormDefinitions.LTS := ReferenceRanges.HOMA_IS.lt;
    NormDefinitions.HTS := ReferenceRanges.HOMA_IS.ht;
    NormDefinitions.LPS := ReferenceRanges.HOMA_IS.lp;
    NormDefinitions.HPS := ReferenceRanges.HOMA_IS.hp;
    NormDefinitions.LC := ReferenceRanges.HOMA_IS.ln;
    NormDefinitions.HC := ReferenceRanges.HOMA_IS.hn;
    NormDefinitions.LTC := ReferenceRanges.HOMA_IS.lt;
    NormDefinitions.HTC := ReferenceRanges.HOMA_IS.ht;
    NormDefinitions.LPC := ReferenceRanges.HOMA_IS.lp;
    NormDefinitions.HPC := ReferenceRanges.HOMA_IS.hp;
    NormDefinitions.startDateTime := ISO8601Date(now);
    AddSubNormNodes(Doc, BaseTestNode, NormDefinitions);

    {QUICKI:}

    BaseTestNode := Doc.CreateElement('BaseTest');
    TDOMElement(BaseTestNode).SetAttribute('DefiningEntity', 'C');
    theNode := Doc.CreateComment('Definitions for QUICKI:');
    BaseTestNode.AppendChild(theNode);
    BatteryNode.Appendchild(BaseTestNode);
    LabTestNode := Doc.CreateElement('LabTest');
    TDOMElement(LabTestNode).SetAttribute('ID', 'QUICKI');
    TDOMElement(LabTestNode).SetAttribute('Name',
      'Quantitative insulin sensitivity check index');
    BaseTestNode.Appendchild(LabTestNode);

    ExclusionDefinitions.Sex := 'F';
    ExclusionDefinitions.AgeL := 0;
    ExclusionDefinitions.AgeH := 999;
    ExclusionDefinitions.UOMS := '';
    ExclusionDefinitions.UOMC := '';
    ExclusionDefinitions.LXS := 0;
    ExclusionDefinitions.HXS := 10000;
    ExclusionDefinitions.LXC := 0;
    ExclusionDefinitions.HXC := 10000;
    ExclusionDefinitions.startDateTime := '2000-01-01T12:00:00+01:00';
    AddSubExNodes(Doc, BaseTestNode, ExclusionDefinitions);
    NormDefinitions.Sex := 'F';
    NormDefinitions.AgeL := 0;
    NormDefinitions.AgeH := 130;
    NormDefinitions.UOMS := '';
    NormDefinitions.UOMC := '';
    NormDefinitions.LS := ReferenceRanges.QUICKI.ln;
    NormDefinitions.HS := ReferenceRanges.QUICKI.hn;
    NormDefinitions.LTS := ReferenceRanges.QUICKI.lt;
    NormDefinitions.HTS := ReferenceRanges.QUICKI.ht;
    NormDefinitions.LPS := ReferenceRanges.QUICKI.lp;
    NormDefinitions.HPS := ReferenceRanges.QUICKI.hp;
    NormDefinitions.LC := ReferenceRanges.QUICKI.ln;
    NormDefinitions.HC := ReferenceRanges.QUICKI.hn;
    NormDefinitions.LTC := ReferenceRanges.QUICKI.lt;
    NormDefinitions.HTC := ReferenceRanges.QUICKI.ht;
    NormDefinitions.LPC := ReferenceRanges.QUICKI.lp;
    NormDefinitions.HPC := ReferenceRanges.QUICKI.hp;
    NormDefinitions.startDateTime := ISO8601Date(now);
    AddSubNormNodes(Doc, BaseTestNode, NormDefinitions);

    theNode := Doc.CreateComment('Add additional age classes here');
    BaseTestNode.AppendChild(theNode);

    ExclusionDefinitions.Sex := 'M';
    ExclusionDefinitions.AgeL := 0;
    ExclusionDefinitions.AgeH := 999;
    ExclusionDefinitions.UOMS := '';
    ExclusionDefinitions.UOMC := '';
    ExclusionDefinitions.LXS := 0;
    ExclusionDefinitions.HXS := 10000;
    ExclusionDefinitions.LXC := 0;
    ExclusionDefinitions.HXC := 10000;
    ExclusionDefinitions.startDateTime := '2000-01-01T12:00:00+01:00';
    AddSubExNodes(Doc, BaseTestNode, ExclusionDefinitions);
    NormDefinitions.Sex := 'M';
    NormDefinitions.AgeL := 0;
    NormDefinitions.AgeH := 130;
    NormDefinitions.UOMS := '';
    NormDefinitions.UOMC := '';
    NormDefinitions.LS := ReferenceRanges.QUICKI.ln;
    NormDefinitions.HS := ReferenceRanges.QUICKI.hn;
    NormDefinitions.LTS := ReferenceRanges.QUICKI.lt;
    NormDefinitions.HTS := ReferenceRanges.QUICKI.ht;
    NormDefinitions.LPS := ReferenceRanges.QUICKI.lp;
    NormDefinitions.HPS := ReferenceRanges.QUICKI.hp;
    NormDefinitions.LC := ReferenceRanges.QUICKI.ln;
    NormDefinitions.HC := ReferenceRanges.QUICKI.hn;
    NormDefinitions.LTC := ReferenceRanges.QUICKI.lt;
    NormDefinitions.HTC := ReferenceRanges.QUICKI.ht;
    NormDefinitions.LPC := ReferenceRanges.QUICKI.lp;
    NormDefinitions.HPC := ReferenceRanges.QUICKI.hp;
    NormDefinitions.startDateTime := ISO8601Date(now);
    AddSubNormNodes(Doc, BaseTestNode, NormDefinitions);

    {AIGR:}

    BaseTestNode := Doc.CreateElement('BaseTest');
    TDOMElement(BaseTestNode).SetAttribute('DefiningEntity', 'C');
    theNode := Doc.CreateComment('Definitions for AIGR:');
    BaseTestNode.AppendChild(theNode);
    BatteryNode.Appendchild(BaseTestNode);
    LabTestNode := Doc.CreateElement('LabTest');
    TDOMElement(LabTestNode).SetAttribute('ID', 'AIGR');
    TDOMElement(LabTestNode).SetAttribute('Name',
      'Amended Insulin Glucose Ratio');
    BaseTestNode.Appendchild(LabTestNode);

    ExclusionDefinitions.Sex := 'F';
    ExclusionDefinitions.AgeL := 0;
    ExclusionDefinitions.AgeH := 999;
    ExclusionDefinitions.UOMS := ReferenceRanges.AIGR.UoM;
    ExclusionDefinitions.UOMC := ReferenceRanges.AIGR.UoM;
    ExclusionDefinitions.LXS := 0;
    ExclusionDefinitions.HXS := 10000;
    ExclusionDefinitions.LXC := 0;
    ExclusionDefinitions.HXC := 10000;
    ExclusionDefinitions.startDateTime := '2000-01-01T12:00:00+01:00';
    AddSubExNodes(Doc, BaseTestNode, ExclusionDefinitions);
    NormDefinitions.Sex := 'F';
    NormDefinitions.AgeL := 0;
    NormDefinitions.AgeH := 130;
    NormDefinitions.UOMS := ReferenceRanges.AIGR.UoM;
    NormDefinitions.UOMC := ReferenceRanges.AIGR.UoM;
    NormDefinitions.LS := ReferenceRanges.AIGR.ln;
    NormDefinitions.HS := ReferenceRanges.AIGR.hn;
    NormDefinitions.LTS := ReferenceRanges.AIGR.lt;
    NormDefinitions.HTS := ReferenceRanges.AIGR.ht;
    NormDefinitions.LPS := ReferenceRanges.AIGR.lp;
    NormDefinitions.HPS := ReferenceRanges.AIGR.hp;
    NormDefinitions.LC := ReferenceRanges.AIGR.ln;
    NormDefinitions.HC := ReferenceRanges.AIGR.hn;
    NormDefinitions.LTC := ReferenceRanges.AIGR.lt;
    NormDefinitions.HTC := ReferenceRanges.AIGR.ht;
    NormDefinitions.LPC := ReferenceRanges.AIGR.lp;
    NormDefinitions.HPC := ReferenceRanges.AIGR.hp;
    NormDefinitions.startDateTime := ISO8601Date(now);
    AddSubNormNodes(Doc, BaseTestNode, NormDefinitions);

    theNode := Doc.CreateComment('Add additional age classes here');
    BaseTestNode.AppendChild(theNode);

    ExclusionDefinitions.Sex := 'M';
    ExclusionDefinitions.AgeL := 0;
    ExclusionDefinitions.AgeH := 999;
    ExclusionDefinitions.UOMS := ReferenceRanges.AIGR.UoM;
    ExclusionDefinitions.UOMC := ReferenceRanges.AIGR.UoM;
    ExclusionDefinitions.LXS := 0;
    ExclusionDefinitions.HXS := 10000;
    ExclusionDefinitions.LXC := 0;
    ExclusionDefinitions.HXC := 10000;
    ExclusionDefinitions.startDateTime := '2000-01-01T12:00:00+01:00';
    AddSubExNodes(Doc, BaseTestNode, ExclusionDefinitions);
    NormDefinitions.Sex := 'M';
    NormDefinitions.AgeL := 0;
    NormDefinitions.AgeH := 130;
    NormDefinitions.UOMS := ReferenceRanges.AIGR.UoM;
    NormDefinitions.UOMC := ReferenceRanges.AIGR.UoM;
    NormDefinitions.LS := ReferenceRanges.AIGR.ln;
    NormDefinitions.HS := ReferenceRanges.AIGR.hn;
    NormDefinitions.LTS := ReferenceRanges.AIGR.lt;
    NormDefinitions.HTS := ReferenceRanges.AIGR.ht;
    NormDefinitions.LPS := ReferenceRanges.AIGR.lp;
    NormDefinitions.HPS := ReferenceRanges.AIGR.hp;
    NormDefinitions.LC := ReferenceRanges.AIGR.ln;
    NormDefinitions.HC := ReferenceRanges.AIGR.hn;
    NormDefinitions.LTC := ReferenceRanges.AIGR.lt;
    NormDefinitions.HTC := ReferenceRanges.AIGR.ht;
    NormDefinitions.LPC := ReferenceRanges.AIGR.lp;
    NormDefinitions.HPC := ReferenceRanges.AIGR.hp;
    NormDefinitions.startDateTime := ISO8601Date(now);
    AddSubNormNodes(Doc, BaseTestNode, NormDefinitions);

    {CGR:}

    BaseTestNode := Doc.CreateElement('BaseTest');
    TDOMElement(BaseTestNode).SetAttribute('DefiningEntity', 'C');
    theNode := Doc.CreateComment('Definitions for CGR:');
    BaseTestNode.AppendChild(theNode);
    BatteryNode.Appendchild(BaseTestNode);
    LabTestNode := Doc.CreateElement('LabTest');
    TDOMElement(LabTestNode).SetAttribute('ID', 'CGR');
    TDOMElement(LabTestNode).SetAttribute('Name', 'C-peptide/glucose ratio');
    BaseTestNode.Appendchild(LabTestNode);

    ExclusionDefinitions.Sex := 'F';
    ExclusionDefinitions.AgeL := 0;
    ExclusionDefinitions.AgeH := 999;
    ExclusionDefinitions.UOMS := CGRUoM;
    ExclusionDefinitions.UOMC := CGRUoM;
    ExclusionDefinitions.LXS := 0;
    ExclusionDefinitions.HXS := 10000;
    ExclusionDefinitions.LXC := 0;
    ExclusionDefinitions.HXC := 10000;
    ExclusionDefinitions.startDateTime := '2000-01-01T12:00:00+01:00';
    AddSubExNodes(Doc, BaseTestNode, ExclusionDefinitions);
    NormDefinitions.Sex := 'F';
    NormDefinitions.AgeL := 0;
    NormDefinitions.AgeH := 130;
    NormDefinitions.UOMS := CGRUoM;
    NormDefinitions.UOMC := CGRUoM;
    NormDefinitions.LS := ReferenceRanges.CGR.ln;
    NormDefinitions.HS := ReferenceRanges.CGR.hn;
    NormDefinitions.LTS := ReferenceRanges.CGR.lt;
    NormDefinitions.HTS := ReferenceRanges.CGR.ht;
    NormDefinitions.LPS := ReferenceRanges.CGR.lp;
    NormDefinitions.HPS := ReferenceRanges.CGR.hp;
    NormDefinitions.LC := ReferenceRanges.CGR.ln;
    NormDefinitions.HC := ReferenceRanges.CGR.hn;
    NormDefinitions.LTC := ReferenceRanges.CGR.lt;
    NormDefinitions.HTC := ReferenceRanges.CGR.ht;
    NormDefinitions.LPC := ReferenceRanges.CGR.lp;
    NormDefinitions.HPC := ReferenceRanges.CGR.hp;
    NormDefinitions.startDateTime := ISO8601Date(now);
    AddSubNormNodes(Doc, BaseTestNode, NormDefinitions);

    theNode := Doc.CreateComment('Add additional age classes here');
    BaseTestNode.AppendChild(theNode);

    ExclusionDefinitions.Sex := 'M';
    ExclusionDefinitions.AgeL := 0;
    ExclusionDefinitions.AgeH := 999;
    ExclusionDefinitions.UOMS := CGRUoM;
    ExclusionDefinitions.UOMC := CGRUoM;
    ExclusionDefinitions.LXS := 0;
    ExclusionDefinitions.HXS := 10000;
    ExclusionDefinitions.LXC := 0;
    ExclusionDefinitions.HXC := 10000;
    ExclusionDefinitions.startDateTime := '2000-01-01T12:00:00+01:00';
    AddSubExNodes(Doc, BaseTestNode, ExclusionDefinitions);
    NormDefinitions.Sex := 'M';
    NormDefinitions.AgeL := 0;
    NormDefinitions.AgeH := 130;
    NormDefinitions.UOMS := CGRUoM;
    NormDefinitions.UOMC := CGRUoM;
    NormDefinitions.LS := ReferenceRanges.CGR.ln;
    NormDefinitions.HS := ReferenceRanges.CGR.hn;
    NormDefinitions.LTS := ReferenceRanges.CGR.lt;
    NormDefinitions.HTS := ReferenceRanges.CGR.ht;
    NormDefinitions.LPS := ReferenceRanges.CGR.lp;
    NormDefinitions.HPS := ReferenceRanges.CGR.hp;
    NormDefinitions.LC := ReferenceRanges.CGR.ln;
    NormDefinitions.HC := ReferenceRanges.CGR.hn;
    NormDefinitions.LTC := ReferenceRanges.CGR.lt;
    NormDefinitions.HTC := ReferenceRanges.CGR.ht;
    NormDefinitions.LPC := ReferenceRanges.CGR.lp;
    NormDefinitions.HPC := ReferenceRanges.CGR.hp;
    NormDefinitions.startDateTime := ISO8601Date(now);
    AddSubNormNodes(Doc, BaseTestNode, NormDefinitions);

    writeXMLFile(Doc, theFileName);  // was writeXMLFile(Doc, UTF8ToSys(theFileName));

    returnCode := 0;
  finally
    Doc.Free;
  end;
end;

end.
