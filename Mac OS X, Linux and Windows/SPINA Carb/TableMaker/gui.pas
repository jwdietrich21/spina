unit GUI;

{ SPINA Carb }

{ Application for calculating structure parameters }
{ of insulin-glucose feedback control }

{ Programm zur Berechnung von Strukturparametern }
{ des Insulin-Glukose-Regelkreises }

{ Version 5.1.0 (Cyclone) }

{ (c) J. W. Dietrich, 1994 - 2024 }
{ (c) Ludwig Maximilian University of Munich 1995 - 2002 }
{ (c) University of Ulm Hospitals 2002 - 2004 }
{ (c) Ruhr University of Bochum 2005 - 2024 }

{ This unit provides the GUI }

{ Source code released under the BSD License }
{ See http://spina.medical-cybernetics.de for details }

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Grids, StdCtrls, Types,
  Math, SPINA_Engine;

const
  colLime = $32CD32;
  colOrange = $507FFF;
  colTomato = $4763FF;

type

  { TMainForm }

  TMainForm = class(TForm)
    StrucParCombo: TComboBox;
    MainTable: TStringGrid;
    procedure StrucParComboChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure DrawHeaders(Sender: TObject);
    procedure DrawResults(Sender: TObject);
    procedure MainTablePrepareCanvas(Sender: TObject; aCol, aRow: Integer;
      aState: TGridDrawState);

  private

  public

  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

{ TMainForm }

procedure TMainForm.StrucParComboChange(Sender: TObject);
begin
  Invalidate;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  DrawHeaders(Sender);
end;

procedure TMainForm.FormPaint(Sender: TObject);
begin
  DrawHeaders(Sender);
  DrawResults(Sender);
end;

procedure TMainForm.FormShow(Sender: TObject);
begin
  DrawHeaders(Sender);
  DrawResults(Sender);
end;

procedure TMainForm.DrawHeaders(Sender: TObject);
var
  i, j: integer;
begin
  for i := 2 to MainTable.ColCount - 1 do
    begin
      MainTable.Cells[i, 0] := IntToStr(i + 4);
      MainTable.Cells[i, 1] := IntToStr((i + 4) * kInsulinConversionFactor);
    end;
  for j := 3 to MainTable.RowCount - 1 do
    begin
      MainTable.Cells[0, j] := IntToStr(50 + (j - 3) * 5);
      MainTable.Cells[1, j] := FloatToStrF((50 + (j - 3) * 5) / kGlucoseConversionFactor, ffFixed, 0, 1);
    end;
end;

procedure TMainForm.DrawResults(Sender: TObject);
var
  i, j: integer;
begin
  for i := 2 to MainTable.ColCount - 1 do
    for j := 3 to MainTable.RowCount - 1 do
      begin
        case StrucParCombo.text of
        'SPINA-GBeta':
        MainTable.Cells[i, j] :=
          FloatToStrF(SPINA_GBeta(StrToFloatDef(MainTable.Cells[i, 1], Math.NaN),
            StrToFloatDef(MainTable.Cells[1, j], Math.NaN)), ffFixed, 0, 2);
        'SPINA-GR':
        MainTable.Cells[i, j] :=
          FloatToStrF(SPINA_GR(StrToFloatDef(MainTable.Cells[i, 1], Math.NaN),
            StrToFloatDef(MainTable.Cells[1, j], Math.NaN)), ffFixed, 0, 2);
        'SPINA-DI':
        MainTable.Cells[i, j] :=
          FloatToStrF(SPINA_DI(StrToFloatDef(MainTable.Cells[i, 1], Math.NaN),
            StrToFloatDef(MainTable.Cells[1, j], Math.NaN)), ffFixed, 0, 2);
          end;
      end;
end;

procedure TMainForm.MainTablePrepareCanvas(Sender: TObject; aCol,
  aRow: Integer; aState: TGridDrawState);
var
  theCanvas: TCanvas;
begin
  if Sender is TStringGrid then
    begin
      theCanvas := TStringGrid(Sender).Canvas;
      if (aCol > 1) and (aRow < 3) then
        theCanvas.Brush.color := colLime;
      if (aCol < 2) and (aRow > 2) then
        theCanvas.Brush.color := colLime;
      if (aCol < 2) and (aRow > 13) then
        theCanvas.Brush.color := TColor(colOrange);;
      if (aCol < 2) and (aRow > 17) then
        theCanvas.Brush.color := TColor(clRed);;
      //theCanvas.FillRect(aRect);
    end;
end;

end.

