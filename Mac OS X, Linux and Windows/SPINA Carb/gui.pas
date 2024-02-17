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

{ GUI }

{ Source code released under the BSD License }
{ See http://spina.medical-cybernetics.de for details }

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  Spin, Menus, ActnList, StdActns, Math, LCLType;

type

  { THauptschirm }

  THauptschirm = class(TForm)
    ActionList1: TActionList;
    AppleMenu: TMenuItem;
    CalculateButton: TButton;
    CloseMenuItem: TMenuItem;
    CopyMenuItem: TMenuItem;
    CutMenuItem: TMenuItem;
    Divider11: TMenuItem;
    Divider12: TMenuItem;
    Divider21: TMenuItem;
    Divider22: TMenuItem;
    EditCopy1: TEditCopy;
    EditMenu: TMenuItem;
    EditSelectAll1: TEditSelectAll;
    FileMenu: TMenuItem;
    GlucoseUnitsCombo: TComboBox;
    GlucoseEdit: TEdit;
    EntryBox: TGroupBox;
    FeedbackImage: TImage;
    HelpMenu: TMenuItem;
    InsulinUnitsCombo: TComboBox;
    InsulinEdit: TEdit;
    GlucoseLabel: TLabel;
    InsulinLabel: TLabel;
    MacAboutItem: TMenuItem;
    MainMenu1: TMainMenu;
    NewMenuItem: TMenuItem;
    OpenMenuItem: TMenuItem;
    PasteMenuItem: TMenuItem;
    QuitMenuItem: TMenuItem;
    RedoMenuItem: TMenuItem;
    ResultsBox: TGroupBox;
    HintBox: TGroupBox;
    Memo1: TMemo;
    Panel1: TPanel;
    SaveMenuItem: TMenuItem;
    SelectAllMenuItem: TMenuItem;
    UndoMenuItem: TMenuItem;
    WinAboutItem: TMenuItem;
    procedure CalculateButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure QuitMenuItemClick(Sender: TObject);
  private

  public
    InsulinRaw, GlucoseRaw: extended;
    InsulinUoM, GlucoseUoM: String;
    procedure RegisterEntry(Sender: TObject);
    procedure AdaptMenus;
  end;

var
  Hauptschirm: THauptschirm;

implementation

{$R *.lfm}

{ THauptschirm }

procedure THauptschirm.CalculateButtonClick(Sender: TObject);
begin
  RegisterEntry(Sender);
end;

procedure THauptschirm.FormCreate(Sender: TObject);
begin
  AdaptMenus;
end;

procedure THauptschirm.QuitMenuItemClick(Sender: TObject);
begin
  application.Terminate;
end;

procedure THauptschirm.RegisterEntry(Sender: TObject);
begin
  InsulinRaw := StrToFloatDef(InsulinEdit.Text, Math.Nan);
  GlucoseRaw := StrToFloatDef(GlucoseEdit.Text, Math.Nan);
  InsulinUoM := InsulinUnitsCombo.Text;
  GlucoseUoM := GlucoseUnitsCombo.Text;
end;

procedure THauptschirm.AdaptMenus;
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

end.

