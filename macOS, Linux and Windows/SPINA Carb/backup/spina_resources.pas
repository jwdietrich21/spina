unit SPINA_Resources;

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

{ Locale-specific strings }

{ Source code released under the BSD License }
{ See http://spina.medical-cybernetics.de for details }

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils;

type
  TGuiLabels = record
    caseTab, EntryTab, CommentTab: string;
    ctCaseID, ctPID, ctName, ctGivenName, ctDOB, ctPlacer, ctOBD, ctOBT,
    ctNext: string;
    enGlucose, enInsulin, enCPep, enHintbox, enHintText, enBParBox, enResults,
    enCalculate: string;
    coCommentBox: string;
  end;

  TPrefLabels = record
    FormTitle: string;
    RangesTab, HL7Tab, OtherTab: string;
    rtRanges, rtGlucose, rtInsulin, rtCPep: string;
    hlIDGroup, hlVariableGroup: string;
    hlSender, hlPlacer, hlExport: string;
    otMandatory: string;
    otGlucose, otInsulin, otCPep: string;
    otFont: string;
    Cancel: string;
  end;

  TResults = record
    FormTitle, LabelTitle: string
  end;

  TMenuCaptions = record
    FileMenu, EditMenu, HelpMenu: string;
    AboutItem, SettingsItem: string;
    NewItem, OpenItem, CloseItem, SaveItem, CaseDataItem: string;
    PageSetupItem, PrintItem, QuitItem: string;
    UndoItem, RedoItem, CutItem, CopyItem, PasteItem, CopyResultItem: string;
    CopyFormattedItem, SelectAllItem: string;
    HelpItem: string;
  end;

const
  { Language 1: German (deutsch); language 2: English }
  kUncertified1_de = ' (Version für Forschungszwecke)';
  kUncertified1_en = ' (Research version)';
  kUncertified2_de =
    'Einsatz für Forschungszwecke nach §3 und §6 MPG und Art. 1(2)h der Richtlinie 93/42/EWG des Rates.';
  kUncertified2_en =
    'Research use according to article 1(2)h of the council directive 93/42/EEC.';
  kUncertified3_de = 'Einsatz nur zu Forschungszwecken.';
  kUncertified3_en = 'Research use only.';
  kUnCertified4_de =
    'Die Verwendung ist gemäß GCP, nach §12 MPG und Anhang X der Richtlinie 93/42/EWG zu dokumentieren.';
  kUnCertified4_en =
    'Usage has to be documented in accordance with GCP and annex X of the council directive 93/42/EEC.';

  kPreferencesHint_de =
    'Die Voreinstellungsdatei wurde neu angelegt. Bitte überprüfen Sie Parameter und Maßeinheiten.';
  kPreferencesHint_en =
    'A new preferences file has been created. Please check parameters and measurement units.';

  kBetaHint =
    'This version of SPINA Carb is beta software that is provided for purposes of testing.'
    + LineEnding +
    'Please report any errors via https://sourceforge.net/p/spina/_list/tickets.' +
    LineEnding + LineEnding +
    'Diese Version von SPINA Carb ist Beta-Software, die für Testzwecke bereitgestellt wird.'
    + LineEnding +
    'Bitte melden Sie Fehler über https://sourceforge.net/p/spina/_list/tickets.';

  HL7_Error = 'HL7 Error';
  pathError = 'Empty path';

  kMacUnavailable = 'Unavailable on macOS';

  kBPars_en = 'Behavioural parameters:';
  kSPars_en = 'Structural parameters:';
  kRR_en = 'Reference ranges:';
  kGluc_en = 'Glucose';
  kIns_en = 'Insulin';
  kCpt_en = 'C-Peptide';

  kBPars_de = 'Verhaltensparameter:';
  kSPars_de = 'Strukturparameter:';
  kRR_de = 'Referenzbereiche:';
  kGluc_de = 'Glukose';
  kIns_de = 'Insulin';
  kCpt_de = 'C-Peptid';

  kPID2_en = 'PID: ';
  kName2_en = 'Patient name: ';
  kPlacer2_en = 'Placer: ';
  kExamDate2_en = 'Examination Date: ';
  kDOB2_en = 'Date of Birth: ';
  kCaseNum2_en = 'Case / Admission Number: ';
  kPrintingDate_en = 'Printing Date: ';
  kUserName_en = 'User name: ';

  kPID2_de = 'PID: ';
  kName2_de = 'Name: ';
  kPlacer2_de = 'Einsender: ';
  kExamDate2_de = 'Untersuchungsdatum: ';
  kDOB2_de = 'Geburtsdatum: ';
  kCaseNum2_de = 'Fall- / Aufnahme-Nr.: ';
  kPrintingDate_de = 'Druckdatum: ';
  kUserName_de = 'Benutzerkennung: ';

  kGUILabels_de: TGuiLabels =
    (
    caseTab: 'Falldaten';
    EntryTab: 'Eingabe';
    CommentTab: 'Kommentar';
    ctCaseID: 'Aufnahme- oder Fall-Nr.:';
    ctPID: 'PID:'; ctName: 'Familienname:';
    ctGivenName: 'Vorname(n):';
    ctDOB: 'Geburtsdatum:';
    ctPlacer: 'Einsender:';
    ctOBD: 'Untersuchungsdatum:';
    ctOBT: '';
    ctNext: 'Weiter';
    enGlucose: kGluc_de;
    enInsulin: kIns_de;
    enCPep: kCpt_de;
    enHintBox: 'Hinweise:';
    enHintText:
    'Bitte geben Sie Nüchtern-Konzentrationen für Insulin and Glukose (und, optional, C-Peptid) ein und klicken Sie auf "Berechnen".'
    + LineEnding + LineEnding + 'Achten Sie bitte auf die korrekten Maßeinheiten.';
    enBParBox: kBPars_de;
    enResults: 'Ergebnisse:';
    enCalculate: 'Berechnen';
    coCommentBox: 'Kommentar'
    );

  kPrefLabels_de: TPrefLabels =
    (
    FormTitle: 'Einstellungen';
    RangesTab: 'Einheiten und Konzentrationen';
    HL7Tab: 'HL7-Nachrichten';
    OtherTab: 'Sonstiges';
    rtRanges: 'Referenzbereiche:';
    rtGlucose: 'Glukose:';
    rtInsulin: 'Insulin:';
    rtCPep: 'C-Peptid:';
    hlIDGroup: 'Praxis- oder Klinik-ID:';
    hlVariableGroup: 'Variablen:';
    hlSender: 'Einrichtung:';
    hlPlacer: 'Einsender:';
    hlExport: 'Export mit LOINC-Code';
    otMandatory: 'Pflichtfelder farbig hervorheben';
    otGlucose: 'Glukose';
    otInsulin: 'Insulin';
    otCPep: 'C-Peptid';
    otFont: 'Schriftart für Druck:';
    Cancel: 'Abbrechen'
    );

  kResLabels_de: TResults =
    (
    FormTitle: 'Ergebnisse';
    LabelTitle: 'Ergebnisse:'
    );

  kMenuCaptions_de: TMenuCaptions =
    (
    {$IFDEF DARWIN}
    FileMenu: 'Ablage';
    {$ELSE}
    FileMenu: 'Datei';
    {$ENDIF}
    EditMenu: 'Bearbeiten';
    HelpMenu: 'Hilfe';
    AboutItem: 'Über SPINA Carb ...';
    SettingsItem: 'Einstellungen ...';
    NewItem: 'Neue Berechnung ...';
    OpenItem: 'Öffnen';
    CloseItem: 'Schließen';
    SaveItem: 'Sichern als ...';
    CaseDataItem: 'Falldaten ...';
    PageSetupItem: 'Seite einrichten ...';
    PrintItem: 'Drucken ...';
    QuitItem: 'Beenden';
    UndoItem: 'Widerrufen';
    RedoItem: 'Wiederholen';
    CutItem: 'Ausschneiden';
    CopyItem: 'Kopieren';
    PasteItem: 'Einfügen';
    CopyResultItem: 'Ergebnis kopieren';
    CopyFormattedItem: 'Formatiert kopieren';
    SelectAllItem: 'Alles auswählen';
    HelpItem: 'Hilfe';
    );

var
  gUncertified1, gUncertified2, gUncertified3, gUncertified4: string;

implementation

end.
