unit PrintCase;

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

{ Handler for printing a case on a printer or exporting as PDF }

{ Source code released under the BSD License }
{ See http://spina.medical-cybernetics.de for details }

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, LCLType, Graphics, Interfaces, Printers, Math,
  LCLProc, LazUTF8, Barcode, SPINATypes, CaseBroker, EnvironmentInfo,
  SPINA_Resources;

const
  AnInch = 2.54;

var
  tabX1, tabX2, tabX3: integer;
  gTopMargin, gBottomMargin, gLeftMargin, gRightMargin: double;
  gLineSpacing: integer;

procedure PrintCaseRecord(CaseRecord: tCaseRecord);

implementation

function GetLinesPerPage(ACanvas: TCanvas; ACanvasHeight, ADPI: integer): integer;
var
  H, DPC: integer;
begin
  DPC := Round(ADPI / AnInch);
  H := ACanvas.TextHeight('X') + gLineSpacing;
  Result := Round((ACanvasHeight - DPC * (gTopMargin - gBottomMargin)) / H - 3);
end;

function GetPoints(AUnits: double; ADPI: integer): integer;
begin
  Result := Round(AUnits * (ADPI / AnInch));
end;

procedure PrinterWrite(H: integer; var currentX, currentY: integer;
  theString: string; bold: boolean);
begin
  if bold then
    Printer.Canvas.Font.Style := [fsBold]
  else
    Printer.Canvas.Font.Style := [];
  Printer.Canvas.TextOut(currentX, currentY, theString);
end;

procedure PrinterWriteln(H: integer; var currentX, currentY: integer;
  theString: string; bold: boolean);
begin
  if bold then
    Printer.Canvas.Font.Style := [fsBold]
  else
    Printer.Canvas.Font.Style := [];
  Printer.Canvas.TextOut(currentX, currentY, theString);
  Inc(currentY, H);
end;

procedure PrintCaption(CaseRecord: tCaseRecord; H: integer;
  var currentX, currentY, rightMargin: integer);
var
  theSize, IDWidth: integer;
  IDPos: TPoint;
  slash: string[3];
  theBarcode: TBarcode;
begin
  theBarcode := TBarcode.Create(nil);
  { Print main header: }
  theSize := Printer.Canvas.Font.Size;
  Printer.Canvas.Font.Size := trunc(theSize * 1.7);
  PrinterWrite(H, currentX, currentY, 'SPINA Carb Report', True);
  { Print hospital / physician / placer ID: }
  if gPreferences.MSH_ID <> '' then
  begin
    Printer.Canvas.Font.Size := theSize;
    IDWidth := Printer.Canvas.TextWidth(gPreferences.MSH_ID);
    IDPos.x := Printer.PageWidth - rightMargin - IDWidth;
    IDPos.y := currentY + H div 2;
    PrinterWrite(H, IDPos.x, IDPos.y, gPreferences.MSH_ID, True);
    Printer.Canvas.Font.Size := trunc(theSize * 1.7);
  end;
  PrinterWriteln(H, currentX, currentY, '', True);
  PrinterWriteln(H, currentX, currentY, '', True);
  PrinterWriteln(H, currentX, currentY, '', True);
  Printer.Canvas.MoveTo(currentX, currentY - H div 2);
  Printer.Canvas.LineTo(Printer.PageWidth - rightMargin, currentY - H div 2);
  PrinterWriteln(H, currentX, currentY, '', True);
  Printer.Canvas.Font.Style := [];
  Printer.Canvas.Font.Size := theSize;
  { Print bar code: }
  if caseRecord.CaseID <> '' then
  begin
    theBarcode.Top := currentY;
    theBarcode.Left := tabX2;
    theBarcode.Typ := bcCode128B;
    theBarcode.Modul := GetPoints(0.02, Printer.YDPI);
    theBarcode.Ratio := 2.0;
    theBarcode.Height := GetPoints(0.3, Printer.YDPI);
    theBarcode.Text := caseRecord.CaseID;
    theBarcode.DrawBarcode(Printer.Canvas);
  end;
  { Print case-specific entries: }
  if (caseRecord.PID <> '') and (caseRecord.CaseID <> '') then
    slash := ' / '
  else
    slash := '';
    PrinterWrite(H, currentX, currentY, kPID2, False);
    if (caseRecord.PID <> '') or (caseRecord.CaseID <> '') then
      PrinterWrite(H, tabX1, currentY, caseRecord.PID +
        slash + caseRecord.CaseID, False);
    PrinterWriteln(H, currentX, currentY, '', False);
    PrinterWrite(H, currentX, currentY, kName2, False);
    if (caseRecord.Name <> '') and
      (caseRecord.GivenNames <> '') then
      PrinterWrite(H, tabX1, currentY, caseRecord.Name +
        ', ' + caseRecord.GivenNames, False);
    PrinterWrite(H, tabX2, currentY, kPlacer2, False);
    PrinterWriteln(H, tabX3, currentY, caseRecord.Placer, False);
    PrinterWrite(H, currentX, currentY, kDOB2, False);
    if not isNaN(caseRecord.DoBDate) then
      PrinterWrite(H, tabX1, currentY, DateToStr(caseRecord.DoBDate)
        , False);
    PrinterWrite(H, tabX2, currentY, kExamDate2, False);
    if not isNaN(caseRecord.OBDate) then
      PrinterWrite(H, tabX3, currentY, DateToStr(caseRecord.OBDate)
        , False);
    PrinterWriteln(H, currentX, currentY, '', False);
    PrinterWriteln(H, currentX, currentY, '', False);
    PrinterWriteln(H, currentX, currentY, '', False);
  Printer.Canvas.MoveTo(currentX, currentY - H div 2);
  Printer.Canvas.LineTo(Printer.PageWidth - rightMargin, currentY - H div 2);
  PrinterWriteln(H, currentX, currentY, '', False);
  theBarcode.Destroy;
end;

procedure PrintFooter(H: integer; var currentX, currentY, rightMargin: integer);
var
  theDate, theTime: string;
begin
  DateTimeToString(theDate, 'dddd"," dd mmmm yyyy', date);
  DateTimeToString(theTime, '"," t', time);
  theDate := SysToUTF8(theDate);
  PrinterWriteln(H, currentX, currentY, '', False);
  Printer.Canvas.MoveTo(currentX, currentY - H div 2);
  Printer.Canvas.LineTo(Printer.PageWidth - rightMargin, currentY - H div 2);
  Printer.Canvas.Font.Color := clGray;
  PrinterWriteln(H, currentX, currentY, concat(kUserName, UserName,
    '  |  ', kPrintingDate, DateToStr(date), theTime), False);
  PrinterWriteln(H, currentX, currentY, 'SPINA Carb ' + FileVersion, False);
  PrinterWriteln(H, currentX, currentY, '', False);
  Printer.Canvas.Font.Color := clBlack;
end;

procedure PrintCaseRecord(CaseRecord: tCaseRecord);
var
  H, ADPI, marginX, marginXr, currentX, currentY, lastY, returnPos, lastPos: integer;
  resultLine, remainder: string;
begin
  assert(assigned(Printer));
  begin
    gTopMargin := 2;
    gLeftMargin := 2;
    gRightMargin := 2;
    gBottomMargin := 2;
    gLineSpacing := 2;
    ADPI := Printer.YDPI;
    currentY := GetPoints(gTopMargin, ADPI);
    marginX := GetPoints(gLeftMargin, ADPI);
    marginXr := GetPoints(gRightMargin, ADPI) div 2;
    Printer.Title := 'SPINA Carb Report';
    currentX := marginX;
    Printer.BeginDoc;
    try
      Printer.Canvas.Font.Name := gPreferences.PrintFont;
      Printer.Canvas.Font.Size := 9;
      Printer.Canvas.Font.Style := [];
      Printer.Canvas.Pen.Color := clBlack;
      Printer.Canvas.Pen.Width := 2;
      H := (Printer.Canvas.TextHeight('X') + gLineSpacing);
      tabX1 := marginX + Printer.Canvas.TextWidth(kDOB2) + GetPoints(1, ADPI);
      tabX2 := Printer.PageWidth - marginXr -
        trunc(2.5 * Printer.Canvas.TextWidth(kExamDate2));
      tabX3 := tabX2 + Printer.Canvas.TextWidth(kCaseNum2) + GetPoints(0.5, ADPI);
      PrintCaption(CaseRecord, H, currentX, currentY, marginXr);
      lastPos := 1;
      lastY := currentY;
      remainder := CaseRecord.CombMessage; // measured and calculated parameters
      repeat
        returnPos := pos(LineEnding, remainder);
        if returnPos > 0 then
          resultLine := copy(remainder, 1, returnPos - 1)
        else
          resultLine := remainder;
        remainder := copy(remainder, returnPos + 1, length(remainder));
        PrinterWriteln(H, currentX, currentY, resultLine, False);
      until returnPos = 0;
      currentY := lastY;
      remainder := CaseRecord.RCompMessage; // reference ranges
      repeat
        returnPos := pos(LineEnding, remainder);
        if returnPos > 0 then
          resultLine := copy(remainder, 1, returnPos - 1)
        else
          resultLine := remainder;
        remainder := copy(remainder, returnPos + 1, length(remainder));
        PrinterWriteln(H, tabX2, currentY, resultLine, False);
      until returnPos = 0;
      PrinterWriteln(H, tabX2, currentY, '', False);
      PrinterWriteln(H, currentX, currentY, caseRecord.Comment, False);
      Printer.Canvas.Font.Color := clGray;
      PrinterWriteln(H, currentX, currentY, '', False);
      PrinterWriteln(H, currentX, currentY, '', False);
      PrinterWriteln(H, currentX, currentY, '', False);
      { #todo : Implement language-specific handler }
      {if not gCEcertified then
      begin
        PrinterWriteln(H, currentX, currentY, gUncertified2, False);
        PrinterWriteln(H, currentX, currentY, gUncertified4, False);
      end; }
      Printer.Canvas.Font.Color := clBlack;
      currentX := marginX;
      currentY := Printer.PageHeight - 5 * H;
      PrintFooter(H, currentX, currentY, marginXr);
    finally
    end;
    Printer.EndDoc;
  end;
end;


end.

