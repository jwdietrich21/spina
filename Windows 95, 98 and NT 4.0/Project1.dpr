program Project1;

uses
  Forms,
  Unit1 in 'Unit1.pas' {Hauptschirm},
  Unit3 in 'Unit3.pas' {AboutBox},
  Unit2 in 'Unit2.pas' {SplashScreen},
  Unit4 in 'Unit4.pas' {ResultForm};

{$R *.RES}

begin
  SplashScreen := TSplashScreen.Create(Application);
  SplashScreen.Show;
  SplashScreen.Update;
  Application.Initialize;
  Application.Title := 'SPINA-Thyr';
  Application.HelpFile := 'C:\Programme\Borland\Delphi5\Projects\SPINA-Thyr\SPINA-Thyr 2.2\Spina_th.hlp';
  Application.CreateForm(THauptschirm, Hauptschirm);
  Application.CreateForm(TAboutBox, AboutBox);
  Application.CreateForm(TResultForm, ResultForm);
  Application.Run;
end.
