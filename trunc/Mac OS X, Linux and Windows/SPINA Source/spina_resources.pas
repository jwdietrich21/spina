unit SPINA_Resources;

{ SPINA-Thyr }

{ Application for calculating structure parameters }
{ of thyrotropic feedback control }

{ Programm zur Berechnung von Strukturparametern }
{ des thyreotropen Regelkreises }

{ Version 3.4.0 }

{ (c) J. W. Dietrich, 1994 - 2014 }
{ (c) Ludwig Maximilian University of Munich 1995 - 2002 }
{ (c) University of Ulm Hospitals 2002-2004 }
{ (c) Ruhr University of Bochum 2005 - 2014 }

{ Locale-specific strings }

{ Source code released under the BSD License }
{ See http://spina.medical-cybernetics.de for details }

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SPINA_Types;

const
  { Language 1: German (deutsch); language 2: English }
  kAnleitung01 = '';
  kAnleitung02 = '';
  kAnleitung11 =
    'Bitte geben Sie die gemessenen Werte für TSH, T4 (oder FT4) und T3 (oder FT3) ein und klicken Sie dann auf "Berechnen".'#13#10#13#10'Im Falle einer Substitutionstherapie klicken Sie bitte auf die Boxen rechts in diesem Fenster unter "Therapie".';
  kAnleitung12 =
    'Please enter simultaneously obtained values for TSH, T4 (or FT4) and T3 (or FT3), and click on "Calculate".'#13#10#13#10'In case of substitution therapy please click the corresponding boxes in the right of this window under the captionn "Therapy".';
  kAnleitung21 = 'Sie können das Ergebnis nun ausdrucken oder in die Zwischenablage kopieren.';
  kAnleitung22 = 'You may want to print or copy the result now.';
  kVerhaltensparameter1 = 'Verhaltensparameter:';
  kVerhaltensparameter2 = 'Behavioural parameters:';
  kStrukturparameter1 = 'Strukturparameter:';
  kStrukturparameter2 = 'Structural parameters:';
  kReferenzbereiche1 = 'Referenzbereiche:';
  kReferenzbereiche2 = 'Reference ranges:';
  kNotCalculatable1 = '<Nicht berechenbar>';
  kNotCalculatable2 = '<Not computable>';
  kPatientenname1 = 'Patientenname: ';
  kPatientenname2 = 'Patient name: ';
  kUntersuchungsdatum1 = 'Untersuchungsdatum: ';
  kUntersuchungsdatum2 = 'Examination Date: ';
  kGeburtsdatum1 = 'Geburtsdatum: ';
  kGeburtsdatum2 = 'Birth date: ';
  kEinsender1 = 'Einsender: ';
  kEinsender2 = 'Sender: ';
  kDruckdatum1 = 'Druckdatum: ';
  kDruckdatum2 = 'Printing Date: ';
  kBenutzername1 = 'Benutzerkennung: ';
  kBenutzername2 = 'User name: ';
  kResultHint1 = 'Ergebnis:';
  kResultHint2 = 'Result:';
  kTherapyHint1 = 'Therapie:';
  kTherapyHint2 = 'Therapy:';
  kHintCaption1 = 'Hinweis:';
  kHintCaption2 = 'Hint:';
  kPreferencesHint1 = 'Die Voreinstellungsdatei wurde neu angelegt. Bitte überprüfen Sie Parameter und Maßeinheiten.';
  kPreferencesHint2 = 'A new preferences file has been created. Please check parameters and measurement units.';
  kPreferences1 = 'Einstellungen';
  kPreferences2 = 'Preferences';
  kMethodLabel1 = 'Methode';
  kMethodLabel2 = 'Method';
  kUnitLabel1 = 'Einheit';
  kUnitLabel2 = 'Unit';
  kUnitsGroupCaption1 = 'Methoden und Maßeinheiten:';
  kUnitsGroupCaption2 = 'Methods and measurement units:';
  kRemember1 = 'Letzte Maßeinheiten merken';
  kRemember2 = 'Remember last used unit';
  kCDISCCaption1 = 'Referenzbereiche:';
  kCDISCCaption2 = 'Reference values:';
  kCancel1 = 'Abbrechen';
  kCancel2 = 'Cancel';
  kReadCDISC1 = 'Aus Datei lesen ...';
  kReadCDISC2 = 'Read from file ...';
  kPflichtfelder1 = 'Pflichtfelder farbig hevorheben';
  kPflichtfelder2 = 'Mark mandatory fields by colouring';
  kPrefsCaption11 = 'Hormonspiegel';
  kPrefsCaption12 = 'Hormone levels';
  kPrefsCaption21 = 'Absender-ID';
  kPrefsCaption22 = 'Sender ID';
  kPrefsCaption31 = 'Sonstiges';
  kPrefsCaption32 = 'Other';
  kGroupCaption31 = 'Praxis- oder Klinik-ID';
  kGroupCaption32 = 'Physician or Hospital ID';


implementation

end.

