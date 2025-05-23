unit ResultWindow;

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

{ Result form }

{ Source code released under the BSD License }
{ See http://spina.medical-cybernetics.de for details }

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  SPINATypes, SPINA_GUIServices, SPINA_Resources, LocaleServices;

type

  { TResultForm }

  TResultForm = class(TForm)
    BParMemo: TMemo;
    LogoImage: TImage;
    OKButton: TButton;
    ResultLabel: TLabel;
    SParMemo: TMemo;
    procedure FormActivate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure OKButtonClick(Sender: TObject);
    procedure ShowResults(BParString, SParString, ParHint, RefHint: string);
  private

  public

  end;

var
  ResultForm: TResultForm;

implementation

{$R *.lfm}

{ TResultForm }

procedure TResultForm.OKButtonClick(Sender: TObject);
begin
  Close;
end;

procedure TResultForm.FormActivate(Sender: TObject);
begin
  ActiveControl := OKButton;
end;

procedure TResultForm.FormCreate(Sender: TObject);
begin
  if gPreferredLanguage = 'de' then
    begin
      Caption := kResLabels_de.FormTitle;
      ResultLabel.Caption := kResLabels_de.LabelTitle;
    end;
end;

procedure TResultForm.FormShow(Sender: TObject);
begin
  if DarkTheme then
    Color := clDefault
  else
    Color := clWhite;
  ActiveControl := OKButton;
end;

procedure TResultForm.ShowResults(BParString, SParString, ParHint, RefHint: string);
begin
  BParMemo.Text := BParString;
  SParMemo.Text := SParString;
  BParMemo.Hint := ParHint;
  SParMemo.Hint := RefHint;
  ActiveControl := OKButton;
end;

end.

