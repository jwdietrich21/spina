unit spina_toolbar;

{ SPINA-Thyr }

{ Application for calculating structure parameters }
{ of thyrotropic feedback control }

{ Programm zur Berechnung von Strukturparametern }
{ des thyreotropen Regelkreises }

{ Version 4.0.2 (Mercator) }

{ (c) J. W. Dietrich, 1994 - 2015 }
{ (c) Ludwig Maximilian University of Munich 1995 - 2002 }
{ (c) University of Ulm Hospitals 2002-2004 }
{ (c) Ruhr University of Bochum 2005 - 2013 }

{ This unit implements a common toolbar, a menu bar and localization routines }

{ Source code released under the BSD License }
{ See http://spina.medical-cybernetics.de for details }

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, StdActns, StdCtrls, LCLType, Menus, ActnList, ComCtrls, LCLIntf,
  SPINA_Types, SPINA_Resources, SPINA_Engine, SPINA_AboutBox, SPINA_Userinterface,
  SetPreferences, VersionSupport, spina_help, CaseEditor, HandleImpEx;

type

  { TSPINAToolbar }

  TSPINAToolbar = class(TForm)
    AboutMenuItem: TMenuItem;
    AppleAboutMenuItem: TMenuItem;
    AppleMenu: TMenuItem;
    ApplicationProperties1: TApplicationProperties;
    CloseMenuItem: TMenuItem;
    CopyMenuItem: TMenuItem;
    CopyResultMenuItem: TMenuItem;
    CopyResultToolButton: TToolButton;
    CopyToolButton: TToolButton;
    CutMenuItem: TMenuItem;
    CutToolButton: TToolButton;
    DeleteMenuItem: TMenuItem;
    DeleteToolButton: TToolButton;
    DividerTool1: TToolButton;
    DividerTool2: TToolButton;
    Divider_0_1: TMenuItem;
    Divider_0_2: TMenuItem;
    Divider_1_1: TMenuItem;
    Divider_1_3: TMenuItem;
    Divider_2_1: TMenuItem;
    Divider_2_2: TMenuItem;
    Divider_2_3: TMenuItem;
    Divider_3_1: TMenuItem;
    EditMenu: TMenuItem;
    FileMenu: TMenuItem;
    HelpItem: TMenuItem;
    HelpMenu: TMenuItem;
    CaseItem: TMenuItem;
    Divider_1_2: TMenuItem;
    OpenCaseDialog: TOpenDialog;
    SaveMenuItem: TMenuItem;
    OpenMenuItem: TMenuItem;
    ToolbarImageList: TImageList;
    MacPreferencesItem: TMenuItem;
    MainMenu: TMainMenu;
    NewMenuItem: TMenuItem;
    NewToolButton: TToolButton;
    OnlineInfoItem: TMenuItem;
    OpenToolButton: TToolButton;
    PageSetupMenuItem: TMenuItem;
    PasteMenuItem: TMenuItem;
    PasteToolButton: TToolButton;
    PrintMenuItem: TMenuItem;
    PrintToolButton: TToolButton;
    QuitMenuItem: TMenuItem;
    SaveAsToolButton: TToolButton;
    SaveResultsDialog: TSaveDialog;
    SaveToolButton: TToolButton;
    SPINAThyrLabel: TLabel;
    ToolBar1: TToolBar;
    DividerTool3: TToolButton;
    CaseEditorToolButton: TToolButton;
    UndoMenuItem: TMenuItem;
    UndoToolButton: TToolButton;
    WinPreferencesItem: TMenuItem;
    procedure AboutMenuItemClick(Sender: TObject);
    procedure AppleAboutMenuItemClick(Sender: TObject);
    procedure ApplicationProperties1DropFiles(Sender: TObject;
      const FileNames: array of String);
    procedure CaseItemClick(Sender: TObject);
    procedure CloseMenuItemClick(Sender: TObject);
    procedure CopyResultMenuItemClick(Sender: TObject);
    procedure CopyResultToolButtonClick(Sender: TObject);
    procedure CopyToolButtonClick(Sender: TObject);
    procedure CutToolButtonClick(Sender: TObject);
    procedure DeleteToolButtonClick(Sender: TObject);
    procedure EditMenuClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure HelpItemClick(Sender: TObject);
    procedure MacPreferencesItemClick(Sender: TObject);
    procedure NewMenuItemClick(Sender: TObject);
    procedure NewToolButtonClick(Sender: TObject);
    procedure OnlineInfoItemClick(Sender: TObject);
    procedure OpenMenuItemClick(Sender: TObject);
    procedure OpenToolButtonClick(Sender: TObject);
    procedure PageSetupMenuItemClick(Sender: TObject);
    procedure PasteToolButtonClick(Sender: TObject);
    procedure PrintMenuItemClick(Sender: TObject);
    procedure PrintToolButtonClick(Sender: TObject);
    procedure QuitMenuItemClick(Sender: TObject);
    procedure SaveAsToolButtonClick(Sender: TObject);
    procedure SaveMenuItemClick(Sender: TObject);
    procedure SaveToolButtonClick(Sender: TObject);
    procedure SPINAThyrLabelClick(Sender: TObject);
    procedure UndoToolButtonClick(Sender: TObject);
    procedure WinPreferencesItemClick(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  SPINAToolbar: TSPINAToolbar;

implementation

{ TSPINAToolbar }

procedure TSPINAToolbar.NewToolButtonClick(Sender: TObject);
begin
  Hauptschirm.NewMenuItemClick(Sender);
end;

procedure AdaptLanguages;
{i18n and l10n: adapts user interface elements to the language of the operating system}
begin
  if gInterfaceLanguage <> German then
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
    Hauptschirm.OpenMenuItem.Caption := 'Open';
    Hauptschirm.SaveMenuItem.Caption := 'Save as...';
    Hauptschirm.CloseMenuItem.Caption := 'Close';
    Hauptschirm.CaseItem.Caption := 'Case Data...';
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
    Hauptschirm.WinPreferencesItem.Caption := kPreferences2 + ' ...';
    Hauptschirm.MacPreferencesItem.Caption := kPreferences2 + ' ...';
    Hauptschirm.AboutMenuItem.Caption := 'SPINA-Thyr Info...';
    Hauptschirm.AppleAboutMenuItem.Caption := 'SPINA-Thyr Info...';
    Hauptschirm.HelpItem.Caption := 'SPINA Help ...';
    Hauptschirm.OnlineInfoItem.Caption := 'Online Info ...';
    Hauptschirm.TSHUnitComboBox.Hint := 'Measurement unit';
    Hauptschirm.T4UnitComboBox.Hint := 'Measurement unit';
    Hauptschirm.T3UnitComboBox.Hint := 'Measurement unit';
    Hauptschirm.T4MethodComboBox.Hint := 'Method';
    Hauptschirm.T3MethodComboBox.Hint := 'Method';
    Hauptschirm.TherapyCheckGroup.Hint := 'Please mark on therapy';
    SPINAToolbar.FileMenu.Caption := 'File';
    SPINAToolbar.NewMenuItem.Caption := 'New Calculation...';
    SPINAToolbar.OpenMenuItem.Caption := 'Open';
    SPINAToolbar.SaveMenuItem.Caption := 'Save as...';
    SPINAToolbar.CaseItem.Caption := 'Case Data...';
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
    SPINAToolbar.WinPreferencesItem.Caption := kPreferences2 + ' ...';
    SPINAToolbar.MacPreferencesItem.Caption := kPreferences2 + ' ...';
    SPINAToolbar.AboutMenuItem.Caption := 'SPINA-Thyr Info...';
    SPINAToolbar.AppleAboutMenuItem.Caption := 'SPINA-Thyr Info...';
    SPINAToolbar.HelpItem.Caption := 'SPINA Help ...';
    SPINAToolbar.OnlineInfoItem.Caption := 'Online Info ...';
       {$IFDEF LCLcarbon}
    Hauptschirm.HelpMenu.Caption := 'Help';
    SPINAToolbar.HelpMenu.Caption := 'Help';
      {$ELSE}
    Hauptschirm.HelpMenu.Caption := '?';
    SPINAToolbar.HelpMenu.Caption := '?';
      {$ENDIF}
    SPINAToolbar.ToolBar1.Buttons[0].Hint := 'New Calculation ...';
    SPINAToolbar.ToolBar1.Buttons[1].Hint := 'Open ...';
    SPINAToolbar.ToolBar1.Buttons[2].Hint := 'Save';
    SPINAToolbar.ToolBar1.Buttons[3].Hint := 'Save as ...';
    SPINAToolbar.ToolBar1.Buttons[5].Hint := 'Print';
    SPINAToolbar.ToolBar1.Buttons[7].Hint := 'Undo';
    SPINAToolbar.ToolBar1.Buttons[8].Hint := 'Cut';
    SPINAToolbar.ToolBar1.Buttons[9].Hint := 'Copy';
    SPINAToolbar.ToolBar1.Buttons[10].Hint := 'Paste';
    SPINAToolbar.ToolBar1.Buttons[11].Hint := 'Delete';
    SPINAToolbar.ToolBar1.Buttons[12].Hint := 'Copy result';
    SPINAToolbar.ToolBar1.Buttons[14].Hint := 'Edit case or patient record';
    gNegativeError := kNegativeError2;
    gReferenceRangeError := kReferenceRangeError2;
  end
  else
  begin
      {$IFDEF LCLcarbon}
    Hauptschirm.FileMenu.Caption := 'Ablage';
    Hauptschirm.UndoMenuItem.Caption := 'Widerrufen';
    Hauptschirm.HelpMenu.Caption := 'Hilfe';
    SPINAToolbar.FileMenu.Caption := 'Ablage';
    SPINAToolbar.UndoMenuItem.Caption := 'Widerrufen';
    SPINAToolbar.HelpMenu.Caption := 'Hilfe';
      {$ELSE}
    Hauptschirm.FileMenu.Caption := 'Datei';
    Hauptschirm.UndoMenuItem.Caption := 'Rückgängig';
    Hauptschirm.HelpMenu.Caption := '?';
    SPINAToolbar.FileMenu.Caption := 'Datei';
    SPINAToolbar.UndoMenuItem.Caption := 'Rückgängig';
    SPINAToolbar.HelpMenu.Caption := '?';
      {$ENDIF}
    Hauptschirm.HelpItem.Caption := 'SPINA-Hilfe ...';
    SPINAToolbar.HelpItem.Caption := 'SPINA-Hilfe ...';
    Hauptschirm.OnlineInfoItem.Caption := 'Online-Info ...';
    SPINAToolbar.OnlineInfoItem.Caption := 'Online-Info ...';
    SPINAToolbar.MacPreferencesItem.Caption := kPreferences1 + ' ...';
    SPINAToolbar.WinPreferencesItem.Caption := kPreferences1 + ' ...';
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
    SPINAToolbar.ToolBar1.Buttons[0].Hint := 'Neue Berechnung ...';
    SPINAToolbar.ToolBar1.Buttons[1].Hint := 'Öffnen ...';
    SPINAToolbar.ToolBar1.Buttons[2].Hint := 'Sichern';
    SPINAToolbar.ToolBar1.Buttons[3].Hint := 'Sichern als ...';
    SPINAToolbar.ToolBar1.Buttons[5].Hint := 'Drucken';
    SPINAToolbar.ToolBar1.Buttons[7].Hint := 'Widerrufen';
    SPINAToolbar.ToolBar1.Buttons[8].Hint := 'Ausschneiden';
    SPINAToolbar.ToolBar1.Buttons[9].Hint := 'Kopieren';
    SPINAToolbar.ToolBar1.Buttons[10].Hint := 'Einfügen';
    SPINAToolbar.ToolBar1.Buttons[11].Hint := 'Löschen';
    SPINAToolbar.ToolBar1.Buttons[12].Hint := 'Ergebnis kopieren';
    SPINAToolbar.ToolBar1.Buttons[14].Hint := 'Fall- oder Patienteninformationen bearbeiten';
    gNegativeError := kNegativeError1;
    gReferenceRangeError := kReferenceRangeError1;
  end;
  AdaptMenus;
  Hauptschirm.ValuesGroupBox.Caption := gVerhaltensparameter;
  Hauptschirm.HintGroupBox.Caption := gHintCaption;
  Hauptschirm.ResultGroupBox.Caption := gResultHint;
  Hauptschirm.TherapyCheckGroup.Caption := gTherapyHint;
  Hauptschirm.ValuesGroupBox.Hint := gAnleitung1;
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
  SPINAToolbar.AboutMenuItem.Visible := False;
  SPINAToolbar.Divider_3_1.Visible := False;
  SPINAToolbar.AppleMenu.Visible := True;
  SPINAToolbar.Divider_2_3.Visible := False;
  SPINAToolbar.WinPreferencesItem.Visible := False;
  {$ELSE}
  modifierKey := [ssCtrl];
  SPINAToolbar.AboutMenuItem.Visible := True;
  SPINAToolbar.Divider_3_1.Visible := True;
  SPINAToolbar.AppleMenu.Visible := False;
  SPINAToolbar.Divider_2_3.Visible := True;
  SPINAToolbar.WinPreferencesItem.Visible := True;
  SPINAToolbar.HelpItem.ShortCut := ShortCut(VK_F1, []);
  {$ENDIF}
  SPINAToolbar.NewMenuItem.ShortCut := ShortCut(VK_N, modifierKey);
  SPINAToolbar.OpenMenuItem.ShortCut := ShortCut(VK_O, modifierKey);
  SPINAToolbar.SaveMenuItem.ShortCut := ShortCut(VK_S, modifierKey);
  SPINAToolbar.CloseMenuItem.ShortCut := ShortCut(VK_W, modifierKey);
  SPINAToolbar.CaseItem.ShortCut := ShortCut(VK_C, modifierKey + [ssShift]);
  SPINAToolbar.PrintMenuItem.ShortCut := ShortCut(VK_P, modifierKey);
  SPINAToolbar.QuitMenuItem.ShortCut := ShortCut(VK_Q, modifierKey);
  SPINAToolbar.UndoMenuItem.ShortCut := ShortCut(VK_Z, modifierKey);
  SPINAToolbar.CutMenuItem.ShortCut := ShortCut(VK_X, modifierKey);
  SPINAToolbar.CopyMenuItem.ShortCut := ShortCut(VK_C, modifierKey);
  SPINAToolbar.PasteMenuItem.ShortCut := ShortCut(VK_V, modifierKey);
  SPINAToolbar.CopyResultMenuItem.ShortCut := ShortCut(VK_R, modifierKey);
  Hauptschirm.HintField.Text := gAnleitung1;
  Hauptschirm.HintField.Hint := Hauptschirm.HintField.Text;
end;

procedure TSPINAToolbar.HelpItemClick(Sender: TObject);
begin
  HelpWindow.ShowOnTop;
end;

procedure TSPINAToolbar.MacPreferencesItemClick(Sender: TObject);
begin
  DisplayPreferencesDlg;
end;

procedure TSPINAToolbar.NewMenuItemClick(Sender: TObject);
begin
  Hauptschirm.NewMenuItemClick(Sender);
end;

procedure TSPINAToolbar.OnlineInfoItemClick(Sender: TObject);
begin
  OpenURL(BASE_URL);
end;

procedure TSPINAToolbar.OpenMenuItemClick(Sender: TObject);
begin
  Hauptschirm.OpenMenuItemClick(Sender);
end;

procedure TSPINAToolbar.PageSetupMenuItemClick(Sender: TObject);
begin
  Hauptschirm.PageSetupMenuItemClick(Sender);
end;

procedure TSPINAToolbar.PasteToolButtonClick(Sender: TObject);
var
  theForm: TForm;
begin
  theForm := Screen.ActiveForm;
  if (theForm = Hauptschirm) or ((theForm = SPINAToolbar) and
    (gLastActiveCustomForm = Hauptschirm)) then
    begin
      Hauptschirm.BringToFront;
      Hauptschirm.PasteMenuItemClick(Sender);
    end;
end;

procedure TSPINAToolbar.PrintMenuItemClick(Sender: TObject);
begin
  Hauptschirm.PrintMenuItemClick(Sender);
end;

procedure TSPINAToolbar.QuitMenuItemClick(Sender: TObject);
begin
  application.Terminate;
end;

procedure TSPINAToolbar.SaveAsToolButtonClick(Sender: TObject);
begin
  CaseEditorForm.SetCaseRecord(Hauptschirm.caseRecord);
  SaveResults(Hauptschirm.caseRecord);
end;

procedure TSPINAToolbar.SaveMenuItemClick(Sender: TObject);
begin
  SaveAsToolButtonClick(Sender);
end;

procedure TSPINAToolbar.SaveToolButtonClick(Sender: TObject);
begin
  SaveAsToolButtonClick(Sender);
end;

procedure TSPINAToolbar.SPINAThyrLabelClick(Sender: TObject);
begin
  ShowAboutBox;
end;

procedure TSPINAToolbar.AppleAboutMenuItemClick(Sender: TObject);
begin
  ShowAboutBox;
end;

procedure TSPINAToolbar.ApplicationProperties1DropFiles(Sender: TObject;
  const FileNames: array of String);
begin
  Hauptschirm.FormDropFiles(Sender, FileNames);
end;

procedure TSPINAToolbar.CaseItemClick(Sender: TObject);
begin
  Hauptschirm.CaseItemClick(Sender);
end;

procedure TSPINAToolbar.AboutMenuItemClick(Sender: TObject);
begin
  ShowAboutBox;
end;

procedure TSPINAToolbar.CloseMenuItemClick(Sender: TObject);
begin
  Screen.ActiveForm.Close;
end;

procedure TSPINAToolbar.CopyResultMenuItemClick(Sender: TObject);
begin
  Hauptschirm.Ergebniskopieren1Click(Sender);
end;

procedure TSPINAToolbar.CopyResultToolButtonClick(Sender: TObject);
begin
   CopyResultMenuItemClick(Sender);
end;

procedure TSPINAToolbar.CopyToolButtonClick(Sender: TObject);
var
  theForm: TForm;
begin
  theForm := Screen.ActiveForm;
  if (theForm = Hauptschirm) or ((theForm = SPINAToolbar) and
    (gLastActiveCustomForm = Hauptschirm)) then
    begin
      Hauptschirm.BringToFront;
      Hauptschirm.CopyMenuItemClick(Sender);
    end;
end;

procedure TSPINAToolbar.CutToolButtonClick(Sender: TObject);
var
  theForm: TForm;
begin
  theForm := Screen.ActiveForm;
  if (theForm = Hauptschirm) or ((theForm = SPINAToolbar) and
    (gLastActiveCustomForm = Hauptschirm)) then
    begin
      Hauptschirm.BringToFront;
      Hauptschirm.CutMenuItemClick(Sender);
    end;
end;

procedure TSPINAToolbar.DeleteToolButtonClick(Sender: TObject);
var
  theForm: TForm;
begin
  theForm := Screen.ActiveForm;
  if (theForm = Hauptschirm) or ((theForm = SPINAToolbar) and
    (gLastActiveCustomForm = Hauptschirm)) then
    begin
      Hauptschirm.BringToFront;
      Hauptschirm.DeleteMenuItemClick(Sender);
    end;
end;

procedure TSPINAToolbar.EditMenuClick(Sender: TObject);
begin

end;

procedure TSPINAToolbar.FormClose(Sender: TObject; var CloseAction: TCloseAction);
{quits application, if toolbar is closed}
begin
  application.terminate;
end;

procedure TSPINAToolbar.OpenToolButtonClick(Sender: TObject);
begin
  Hauptschirm.OpenMenuItemClick(Sender);
end;

procedure TSPINAToolbar.PrintToolButtonClick(Sender: TObject);
begin
  Hauptschirm.PrintMenuItemClick(Sender);
end;

procedure TSPINAToolbar.UndoToolButtonClick(Sender: TObject);
var
  theForm: TForm;
begin
  theForm := Screen.ActiveForm;
  if (theForm = Hauptschirm) or ((theForm = SPINAToolbar) and
    (gLastActiveCustomForm = Hauptschirm)) then
    begin
      Hauptschirm.BringToFront;
      Hauptschirm.UndoMenuItemClick(Sender);
    end;
end;

procedure TSPINAToolbar.WinPreferencesItemClick(Sender: TObject);
begin
  DisplayPreferencesDlg;
end;

initialization
  {$I spina_toolbar.lrs}

end.
