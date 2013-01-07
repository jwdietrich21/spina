unit spina_toolbar;

{ SPINA-Thyr }

{ Application for calculating structure parameters }
{ of thyrotropic feedback control }

{ Programm zur Berechnung von Strukturparametern }
{ des thyreotropen Regelkreises }

{ Version 3.3 }

{ (c) J. W. Dietrich, 1994 - 2013 }
{ (c) Ludwig Maximilian University of Munich 1995 - 2002 }
{ (c) University of Ulm Hospitals 2002-2004 }
{ (c) Ruhr University of Bochum 2005 - 2013 }

{ This unit implements a common toolbar, a menu bar and localization routines }

{ Source code released under the BSD License }
{ See http://spina.medical-cybernetics.de for details }

{$mode objfpc}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, StdActns, StdCtrls, LCLType, Menus, ActnList, ComCtrls,
  SPINA_Types, SPINA_Engine, SPINA_AboutBox, SPINA_Userinterface, VersionSupport;

type

  { TSPINAToolbar }

  TSPINAToolbar = class(TForm)
    AboutMenuItem: TMenuItem;
    AppleAboutMenuItem: TMenuItem;
    AppleMenu: TMenuItem;
    CloseMenuItem: TMenuItem;
    CopyMenuItem: TMenuItem;
    CopyResultMenuItem: TMenuItem;
    CutMenuItem: TMenuItem;
    DeleteMenuItem: TMenuItem;
    EditMenu: TMenuItem;
    FileMenu: TMenuItem;
    HelpMenu: TMenuItem;
    ImageList1: TImageList;
    MainMenu: TMainMenu;
    MenuItem10: TMenuItem;
    MenuItem13: TMenuItem;
    MenuItem16: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem5: TMenuItem;
    NewMenuItem: TMenuItem;
    PageSetupMenuItem: TMenuItem;
    PasteMenuItem: TMenuItem;
    PrintMenuItem: TMenuItem;
    QuitMenuItem: TMenuItem;
    SPINAThyrLabel: TLabel;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ToolButton10: TToolButton;
    ToolButton11: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    ToolButton6: TToolButton;
    ToolButton7: TToolButton;
    ToolButton8: TToolButton;
    ToolButton9: TToolButton;
    UndoMenuItem: TMenuItem;
    procedure AboutMenuItemClick(Sender: TObject);
    procedure AppleAboutMenuItemClick(Sender: TObject);
    procedure CloseMenuItemClick(Sender: TObject);
    procedure CopyResultMenuItemClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure NewMenuItemClick(Sender: TObject);
    procedure PageSetupMenuItemClick(Sender: TObject);
    procedure PrintMenuItemClick(Sender: TObject);
    procedure QuitMenuItemClick(Sender: TObject);
    procedure SPINAThyrLabelClick(Sender: TObject);
    procedure ToolButton1Click(Sender: TObject);
    procedure ToolButton2Click(Sender: TObject);
    procedure ToolButton6Click(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  SPINAToolbar: TSPINAToolbar;

implementation

{ TSPINAToolbar }

procedure TSPINAToolbar.ToolButton1Click(Sender: TObject);
begin

end;

procedure AdaptLanguages;
begin
  if gInterfaceLanguage = English then
  begin
    gAnleitung1 := kAnleitung12;
    gAnleitung2 := kAnleitung22;
    gVerhaltensparameter := kVerhaltensparameter2;
    gStrukturparameter := kStrukturparameter2;
    gReferenzbereiche := kReferenzbereiche2;
    gNotCalculable := kNotCalculatable2;
    gResultHint := kResultHint2;
    gHintCaption := kHintCaption2;
    gTherapyHint := kTherapyHint2;
    gPatientenname := kPatientenname2;
    gGeburtsdatum := kGeburtsdatum2;
    gUntersuchungsdatum := kUntersuchungsdatum2;
    gEinsender := kEinsender2;
    gBenutzername := kBenutzername2;
    gDruckdatum := kDruckdatum2;
    gPreferencesHint := kPreferencesHint2;
    Hauptschirm.Calculate_Button.Caption := 'Calculate';
    Hauptschirm.HintGroupBox.Caption := 'Hint:';
    Hauptschirm.FileMenu.Caption := 'File';
    Hauptschirm.NewMenuItem.Caption := 'New Calculation...';
    Hauptschirm.CloseMenuItem.Caption := 'Close';
    Hauptschirm.PrintMenuItem.Caption := 'Print';
    Hauptschirm.PageSetupMenuItem.Caption := 'Page Setup...';
    Hauptschirm.QuitMenuItem.Caption := 'Quit';
    Hauptschirm.EditMenu.Caption := 'Edit';
    Hauptschirm.UndoMenuItem.Caption := 'Undo';
    Hauptschirm.CutMenuItem.Caption := 'Cut';
    Hauptschirm.CopyMenuItem.Caption := 'Copy';
    Hauptschirm.PasteMenuItem.Caption := 'Paste';
    Hauptschirm.DeleteMenuItem.Caption := 'Clear';
    Hauptschirm.CopyResultMenuItem.Caption := 'Copy Result';
    Hauptschirm.AboutMenuItem.Caption := 'SPINA-Thyr Info...';
    Hauptschirm.AppleAboutMenuItem.Caption := 'SPINA-Thyr Info...';
    SPINAToolbar.FileMenu.Caption := 'File';
    SPINAToolbar.NewMenuItem.Caption := 'New Calculation...';
    SPINAToolbar.CloseMenuItem.Caption := 'Close';
    SPINAToolbar.PrintMenuItem.Caption := 'Print';
    SPINAToolbar.PageSetupMenuItem.Caption := 'Page Setup...';
    SPINAToolbar.QuitMenuItem.Caption := 'Quit';
    SPINAToolbar.EditMenu.Caption := 'Edit';
    SPINAToolbar.UndoMenuItem.Caption := 'Undo';
    SPINAToolbar.CutMenuItem.Caption := 'Cut';
    SPINAToolbar.CopyMenuItem.Caption := 'Copy';
    SPINAToolbar.PasteMenuItem.Caption := 'Paste';
    SPINAToolbar.DeleteMenuItem.Caption := 'Clear';
    SPINAToolbar.CopyResultMenuItem.Caption := 'Copy Result';
    SPINAToolbar.AboutMenuItem.Caption := 'SPINA-Thyr Info...';
    SPINAToolbar.AppleAboutMenuItem.Caption := 'SPINA-Thyr Info...';
  end
  else
  begin
      {$IFDEF LCLcarbon}
    Hauptschirm.FileMenu.Caption := 'Ablage';
    Hauptschirm.UndoMenuItem.Caption := 'Widerrufen';
    SPINAToolbar.FileMenu.Caption := 'Ablage';
    SPINAToolbar.UndoMenuItem.Caption := 'Widerrufen';
      {$ELSE}
    Hauptschirm.FileMenu.Caption := 'Datei';
    Hauptschirm.UndoMenuItem.Caption := 'Rückgängig';
    SPINAToolbar.FileMenu.Caption := 'Datei';
    SPINAToolbar.UndoMenuItem.Caption := 'Rückgängig';
      {$ENDIF}
    gAnleitung1 := kAnleitung11;
    gAnleitung2 := kAnleitung21;
    gVerhaltensparameter := kVerhaltensparameter1;
    gStrukturparameter := kStrukturparameter1;
    gReferenzbereiche := kReferenzbereiche1;
    gNotCalculable := kNotCalculatable1;
    gResultHint := kResultHint1;
    gHintCaption := kHintCaption1;
    gTherapyHint := kTherapyHint1;
    gPatientenname := kPatientenname1;
    gGeburtsdatum := kGeburtsdatum1;
    gUntersuchungsdatum := kUntersuchungsdatum1;
    gEinsender := kEinsender1;
    gBenutzername := kBenutzername1;
    gDruckdatum := kDruckdatum1;
    gPreferencesHint := kPreferencesHint1;
  end;
  AdaptMenus;
  Hauptschirm.ValuesGroupBox.Caption := gVerhaltensparameter;
  Hauptschirm.HintGroupBox.Caption := gHintCaption;
  Hauptschirm.ResultGroupBox.Caption := gResultHint;
  Hauptschirm.TherapyCheckGroup.Caption := gTherapyHint;
  if gPreferences.new then ShowMessage(gPreferencesHint);
end;

procedure TSPINAToolbar.FormCreate(Sender: TObject);
var
  modifierKey: TShiftState;
begin
  SPINAToolbar.SPINAThyrLabel.Caption := 'SPINA Thyr ' + GetFileVersion;
  AdaptLanguages;
  {$IFDEF LCLcarbon}
  modifierKey := [ssMeta];
  SPINAToolbar.HelpMenu.Visible := False;
  SPINAToolbar.AppleMenu.Visible := True;
  {$ELSE}
  modifierKey := [ssCtrl];
  SPINAToolbar.HelpMenu.Visible := True;
  SPINAToolbar.AppleMenu.Visible := False;
  {$ENDIF}
  SPINAToolbar.NewMenuItem.ShortCut := ShortCut(VK_N, modifierKey);
  SPINAToolbar.CloseMenuItem.ShortCut := ShortCut(VK_W, modifierKey);
  SPINAToolbar.PrintMenuItem.ShortCut := ShortCut(VK_P, modifierKey);
  SPINAToolbar.QuitMenuItem.ShortCut := ShortCut(VK_Q, modifierKey);
  SPINAToolbar.UndoMenuItem.ShortCut := ShortCut(VK_Z, modifierKey);
  SPINAToolbar.CutMenuItem.ShortCut := ShortCut(VK_X, modifierKey);
  SPINAToolbar.CopyMenuItem.ShortCut := ShortCut(VK_C, modifierKey);
  SPINAToolbar.PasteMenuItem.ShortCut := ShortCut(VK_V, modifierKey);
  SPINAToolbar.CopyResultMenuItem.ShortCut := ShortCut(VK_R, modifierKey);
  Hauptschirm.HintField.Text := gAnleitung1;
end;

procedure TSPINAToolbar.NewMenuItemClick(Sender: TObject);
begin

end;

procedure TSPINAToolbar.PageSetupMenuItemClick(Sender: TObject);
begin
  Hauptschirm.PageSetupMenuItemClick(Sender);
end;

procedure TSPINAToolbar.PrintMenuItemClick(Sender: TObject);
begin
  Hauptschirm.PrintMenuItemClick(Sender);
end;

procedure TSPINAToolbar.QuitMenuItemClick(Sender: TObject);
begin
  application.Terminate;
end;

procedure TSPINAToolbar.SPINAThyrLabelClick(Sender: TObject);
begin

end;

procedure TSPINAToolbar.AppleAboutMenuItemClick(Sender: TObject);
begin
  ShowAboutBox;
end;

procedure TSPINAToolbar.AboutMenuItemClick(Sender: TObject);
begin
  ShowAboutBox;
end;

procedure TSPINAToolbar.CloseMenuItemClick(Sender: TObject);
begin
  Hauptschirm.Close;
end;

procedure TSPINAToolbar.CopyResultMenuItemClick(Sender: TObject);
begin
  Hauptschirm.Ergebniskopieren1Click(Sender);
end;

procedure TSPINAToolbar.ToolButton2Click(Sender: TObject);
begin

end;

procedure TSPINAToolbar.ToolButton6Click(Sender: TObject);
begin
  Hauptschirm.PrintMenuItemClick(Sender);
end;

initialization
  {$I spina_toolbar.lrs}

end.