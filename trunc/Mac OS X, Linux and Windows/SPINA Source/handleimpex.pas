unit HandleImpEx;

{ SPINA-Thyr }

{ Application for calculating structure parameters }
{ of thyrotropic feedback control }

{ Programm zur Berechnung von Strukturparametern }
{ des thyreotropen Regelkreises }

{ Version 3.4.0 }

{ (c) J. W. Dietrich, 1994 - 2013 }
{ (c) Ludwig Maximilian University of Munich 1995 - 2002 }
{ (c) University of Ulm Hospitals 2002-2004 }
{ (c) Ruhr University of Bochum 2005 - 2013 }

{ This unit handles import and export of laboratory results and calculations }

{ Source code released under the BSD License }
{ See http://spina.medical-cybernetics.de for details }

{$mode objfpc}

interface

uses
  Classes, SysUtils, SPINA_Engine, SPINA_Types;

procedure SaveResults(caseRecord: tCaseRecord);

implementation

uses spina_toolbar;

procedure SaveResults(caseRecord: tCaseRecord);
var
  filePath: String;
  textFile: TFileStream = nil;
begin
  if SPINAToolbar.SaveResultsDialog.Execute then
  begin
    try
      filePath := SPINAToolbar.SaveResultsDialog.FileName;
      textFile := TFileStream.Create(filePath, fmOpenWrite or fmCreate);
      textFile.WriteAnsiString(gResultString);
    finally
      if textFile <> nil then textFile.Free;
    end;
  end;
end;

end.

