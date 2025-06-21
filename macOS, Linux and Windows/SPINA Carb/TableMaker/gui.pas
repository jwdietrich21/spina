unit GUI;

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

{ This unit provides the GUI }

{ Source code released under the BSD License }
{ See http://spina.medical-cybernetics.de for details }

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Grids, StdCtrls, Menus,
  Types, Math, LCLType, SPINA_Engine, ActnList, StdActns;

const
  colLime = $32CD32;
  colOrange = $507FFF;
  colTomato = $4763FF;

type

  { TMainForm }

  TMainForm = class(TForm)
    AppleMenu: TMenuItem;
    CloseMenuItem: TMenuItem;
    CopyMenuItem: TMenuItem;
    CutMenuItem: TMenuItem;
    DisplayLabel: TLabel;
    Divider11: TMenuItem;
    Divider12: TMenuItem;
    Divider21: TMenuItem;
    EditMenu: TMenuItem;
    FileMenu: TMenuItem;
    HelpMenu: TMenuItem;
    MacAboutItem: TMenuItem;
    MainMenu1: TMainMenu;
    Divider22: TMenuItem;
    SelectAllMenuItem: TMenuItem;
    NewMenuItem: TMenuItem;
    OpenMenuItem: TMenuItem;
    PasteMenuItem: TMenuItem;
    QuitMenuItem: TMenuItem;
    RedoMenuItem: TMenuItem;
    SaveMenuItem: TMenuItem;
    StrucParCombo: TComboBox;
    MainTable: TStringGrid;
    UndoMenuItem: TMenuItem;
    WinAboutItem: TMenuItem;
    ActionList1: TActionList;
    EditCopy1: TEditCopy;
    EditSelectAll1: TEditSelectAll;
    procedure EditCopy1Execute(Sender: TObject);
    procedure EditSelectAll1Execute(Sender: TObject);
    procedure MacAboutItemClick(Sender: TObject);
    procedure WinAboutItemClick(Sender: TObject);
    procedure QuitMenuItemClick(Sender: TObject);
    procedure StrucParComboChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure DrawHeaders(Sender: TObject);
    procedure DrawResults(Sender: TObject);
    procedure MainTablePrepareCanvas(Sender: TObject; aCol, aRow: integer;
      aState: TGridDrawState);
    procedure AdaptMenus;

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

procedure TMainForm.QuitMenuItemClick(Sender: TObject);
begin
  application.Terminate;
end;

procedure TMainForm.FormCreate(Sender: TObject);
var
  s: TTextStyle;
begin
  AdaptMenus;
  s := MainTable.DefaultTextStyle;
  s.Alignment := taCenter;
  MainTable.DefaultTextStyle := s;
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
const
  kInsulinMin = 1;
  kGlucoseMin = 20;
  kTitleRows = 3;
  kTitleCols = 2;
  kGlucoseSteps = 5; // in mg/dl
var
  i, j: integer;
begin
  for i := kTitleCols to MainTable.ColCount - 1 do
  begin
    MainTable.Cells[i, 0] := IntToStr(kInsulinMin + (i - kTitleCols));
    MainTable.Cells[i, 1] := IntToStr((kInsulinMin + (i - kTitleCols)) *
      kInsulinConversionFactor);
  end;
  for j := kTitleRows to MainTable.RowCount - 1 do
  begin
    MainTable.Cells[0, j] := IntToStr(kGlucoseMin + (j - kTitleRows) * kGlucoseSteps);
    MainTable.Cells[1, j] :=
      FloatToStrF((kGlucoseMin + (j - kTitleRows) * kGlucoseSteps) /
      kGlucoseConversionFactor, ffFixed, 0, 1);
  end;
end;

procedure TMainForm.DrawResults(Sender: TObject);
var
  i, j: integer;
begin
  for i := 2 to MainTable.ColCount - 1 do
    for j := 3 to MainTable.RowCount - 1 do
    begin
      case StrucParCombo.Text of
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

procedure TMainForm.MainTablePrepareCanvas(Sender: TObject;
  aCol, aRow: integer; aState: TGridDrawState);
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
      theCanvas.Brush.color := TColor(colOrange);
    ;
    if (aCol < 2) and (aRow > 17) then
      theCanvas.Brush.color := TColor(clRed);
    ;
    //theCanvas.FillRect(aRect);
  end;
end;

procedure TMainForm.AdaptMenus;
{ Adapts Menus and Shortcuts to the interface style guidelines
  of the respective operating system }
var
  modifierKey: TShiftState;
begin
  {$IFDEF LCLcarbon}
  modifierKey := [ssMeta];
  WinAboutItem.Visible := False;
  AppleMenu.Visible := True;
  {$ELSE}
  {$IFDEF LCLCocoa}
  modifierKey := [ssMeta];
  WinAboutItem.Visible := False;
  AppleMenu.Visible := True;
  {$ELSE}
  modifierKey := [ssCtrl];
  WinAboutItem.Visible := True;
  AppleMenu.Visible := False;
  {$ENDIF}
  {$ENDIF}
  NewMenuItem.ShortCut := ShortCut(VK_N, modifierKey);
  OpenMenuItem.ShortCut := ShortCut(VK_O, modifierKey);
  CloseMenuItem.ShortCut := ShortCut(VK_W, modifierKey);
  SaveMenuItem.ShortCut := ShortCut(VK_S, modifierKey);
  QuitMenuItem.ShortCut := ShortCut(VK_Q, modifierKey);
  UndoMenuItem.ShortCut := ShortCut(VK_Z, modifierKey);
  RedoMenuItem.ShortCut := ShortCut(VK_Z, modifierKey + [ssShift]);
  CutMenuItem.ShortCut := ShortCut(VK_X, modifierKey);
  CopyMenuItem.ShortCut := ShortCut(VK_C, modifierKey);
  PasteMenuItem.ShortCut := ShortCut(VK_V, modifierKey);
  SelectAllMenuItem.ShortCut := ShortCut(VK_A, modifierKey);
end;

procedure TMainForm.MacAboutItemClick(Sender: TObject);
begin
  ShowMessage('TableMaker' + LineEnding + 'A test application for SPINA Carb' +
    LineEnding + LineEnding + 'Version 5.1.0');
end;

procedure TMainForm.EditSelectAll1Execute(Sender: TObject);
var
  GridRect: TGridRect;
begin
  GridRect.Top := 3;
  GridRect.Left := 2;
  GridRect.Right := MainTable.ColCount - 1;
  GridRect.Bottom := MainTable.RowCount - 1;
  MainTable.Selection := GridRect;
end;

procedure TMainForm.EditCopy1Execute(Sender: TObject);
begin
  MainTable.copyToClipboard(True);
end;

procedure TMainForm.WinAboutItemClick(Sender: TObject);
begin
  MacAboutItemClick(Sender);
end;

end.
