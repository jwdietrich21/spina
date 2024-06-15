unit ResultWindow;

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

{ Result form }

{ Source code released under the BSD License }
{ See http://spina.medical-cybernetics.de for details }

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  SPINATypes;

type

  { TResultForm }

  TResultForm = class(TForm)
    BParMemo: TMemo;
    LogoImage: TImage;
    OKButton: TButton;
    ResultLabel: TLabel;
    SParMemo: TMemo;
    procedure OKButtonClick(Sender: TObject);
    procedure ShowResults(BParString, SParString: string);
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

procedure TResultForm.ShowResults(BParString, SParString: string);
begin
  BParMemo.Text := BParString;
  SParMemo.Text := SParString;
end;

end.

