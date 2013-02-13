unit Unit4;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls;

type
  TResultForm = class(TForm)
    Button1: TButton;
    Image1: TImage;
    MessageField1: TMemo;
    Label1: TLabel;
    MessageField2: TMemo;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure UpdateResultDisplay(Sender: TObject);
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  end;

var
  ResultForm: TResultForm;

implementation

uses Unit1;

{$R *.DFM}

procedure TResultForm.Button1Click(Sender: TObject);
begin
ResultForm.close
end;

procedure TResultForm.FormCreate(Sender: TObject);
begin
  if gInterfaceLanguage = English then
     ResultForm.Label1.Caption := gResultHint;
  SetWindowLong(Handle, GWL_Style, GetWindowLong(Handle, GWL_Style) and not WS_CAPTION);
  ClientHeight := Height;
  Refresh;
end;

procedure TResultForm.UpdateResultDisplay(Sender: TObject);
begin
  ResultForm.MessageField1.text := gResultDialogString1;
  ResultForm.messageField2.text := gResultDialogString2;
end;

end.
