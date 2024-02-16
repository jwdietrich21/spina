program ScenarioTester;

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

{ This unit implements a scenario tester }

{ Source code released under the BSD License }
{ See http://spina.medical-cybernetics.de for details }

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes,
  SysUtils,
  CustApp,
  SPINA_Engine;

type

  { TScenarioTestApp }

  TScenarioTestApp = class(TCustomApplication)
  protected
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHelp; virtual;
  end;

  { TScenarioTestApp }

  procedure TScenarioTestApp.DoRun;
  const
    precision = 2;
    digits = 4;
  var
    ErrorMsg: string;
    Insulin, Glucose: real;
  begin
    // quick check parameters
    ErrorMsg := CheckOptions('h', 'help');
    if ErrorMsg <> '' then
    begin
      ShowException(Exception.Create(ErrorMsg));
      Terminate;
      Exit;
    end;

    // parse parameters
    if HasOption('h', 'help') then
    begin
      WriteHelp;
      Terminate;
      Exit;
    end;

    writeln();
    writeln('SPINA ScenarioTester');
    writeln('Vellore Dataset:');
    writeln();

    writeln('Additional test cases derived from simulations:');

    writeln('Scenario 100: GBeta = 2.8 pmol/s. GR = 2.3 mol/s:');
    Insulin := 63.01;
    Glucose := 4.34;
    Write('Insulin: ');
    writeln(FloatToStrF(Insulin, ffFixed, precision, digits));
    Write('Glucose: ');
    writeln(FloatToStrF(Glucose, ffFixed, precision, digits));
    writeln('SPINA-GBeta: ' + FloatToStrF(SPINA_GBeta(Insulin, Glucose),
      ffFixed, precision, digits) + ' pmol/s');
    writeln('SPINA-GR: ' + FloatToStrF(SPINA_GR(Insulin, Glucose),
      ffFixed, precision, digits) + ' mol/s');
    writeln('SPINA-DI: ' + FloatToStrF(SPINA_DI(Insulin, Glucose),
      ffFixed, precision, digits));
    writeln();

    writeln('Scenario 101: GBeta = 2.8 pmol/s. GR = 0.7 mol/s:');
    Insulin := 88.77;
    Glucose := 8.18;
    Write('Insulin: ');
    writeln(FloatToStrF(Insulin, ffFixed, precision, digits));
    Write('Glucose: ');
    writeln(FloatToStrF(Glucose, ffFixed, precision, digits));
    writeln('SPINA-GBeta: ' + FloatToStrF(SPINA_GBeta(Insulin, Glucose),
      ffFixed, precision, digits) + ' pmol/s');
    writeln('SPINA-GR: ' + FloatToStrF(SPINA_GR(Insulin, Glucose),
      ffFixed, precision, digits) + ' mol/s');
    writeln('SPINA-DI: ' + FloatToStrF(SPINA_DI(Insulin, Glucose),
      ffFixed, precision, digits));
    writeln();

    writeln('Scenario 102: GBeta = 0.6 pmol/s. GR = 2.3 mol/s:');
    Insulin := 20.33;
    Glucose := 9.51;
    Write('Insulin: ');
    writeln(FloatToStrF(Insulin, ffFixed, precision, digits));
    Write('Glucose: ');
    writeln(FloatToStrF(Glucose, ffFixed, precision, digits));
    writeln('SPINA-GBeta: ' + FloatToStrF(SPINA_GBeta(Insulin, Glucose),
      ffFixed, precision, digits) + ' pmol/s');
    writeln('SPINA-GR: ' + FloatToStrF(SPINA_GR(Insulin, Glucose),
      ffFixed, precision, digits) + ' mol/s');
    writeln('SPINA-DI: ' + FloatToStrF(SPINA_DI(Insulin, Glucose),
      ffFixed, precision, digits));
    writeln();

    writeln('Scenario 103: GBeta = 0.6 pmol/s. GR = 2.3 mol/s:');
    Insulin := 20.33;
    Glucose := 9.51;
    Write('Insulin: ');
    writeln(FloatToStrF(Insulin, ffFixed, precision, digits));
    Write('Glucose: ');
    writeln(FloatToStrF(Glucose, ffFixed, precision, digits));
    writeln('SPINA-GBeta: ' + FloatToStrF(SPINA_GBeta(Insulin, Glucose),
      ffFixed, precision, digits) + ' pmol/s');
    writeln('SPINA-GR: ' + FloatToStrF(SPINA_GR(Insulin, Glucose),
      ffFixed, precision, digits) + ' mol/s');
    writeln('SPINA-DI: ' + FloatToStrF(SPINA_DI(Insulin, Glucose),
      ffFixed, precision, digits));
    writeln();

    writeln('Scenario 104: GBeta = 13.0 pmol/s. GR = 2.3 mol/s:');
    Insulin := 167.09;
    Glucose := 1.96;
    Write('Insulin: ');
    writeln(FloatToStrF(Insulin, ffFixed, precision, digits));
    Write('Glucose: ');
    writeln(FloatToStrF(Glucose, ffFixed, precision, digits));
    writeln('SPINA-GBeta: ' + FloatToStrF(SPINA_GBeta(Insulin, Glucose),
      ffFixed, precision, digits) + ' pmol/s');
    writeln('SPINA-GR: ' + FloatToStrF(SPINA_GR(Insulin, Glucose),
      ffFixed, precision, digits) + ' mol/s');
     writeln('SPINA-DI: ' + FloatToStrF(SPINA_DI(Insulin, Glucose),
      ffFixed, precision, digits));
    writeln();

    writeln('Scenario T1');
    Insulin := 10 * kInsulinConversionFactor;
    Glucose := 100 / kGlucoseConversionFactor;
    Write('Insulin: ');
    writeln(FloatToStrF(Insulin, ffFixed, precision, digits));
    Write('Glucose: ');
    writeln(FloatToStrF(Glucose, ffFixed, precision, digits));
    writeln('SPINA-GBeta: ' + FloatToStrF(SPINA_GBeta(Insulin, Glucose),
      ffFixed, precision, digits) + ' pmol/s');
    writeln('SPINA-GR: ' + FloatToStrF(SPINA_GR(Insulin, Glucose),
      ffFixed, precision, digits) + ' mol/s');
    writeln('SPINA-DI: ' + FloatToStrF(SPINA_DI(Insulin, Glucose),
      ffFixed, precision, digits));
    writeln('HOMA-Beta: ' + FloatToStrF(HOMA_Beta(Insulin, Glucose),
      ffFixed, precision, digits) + '%');
    writeln('HOMA-IR: ' + FloatToStrF(HOMA_IR(Insulin, Glucose),
      ffFixed, precision, digits));
    writeln('HOMA-IS: ' + FloatToStrF(HOMA_IS(Insulin, Glucose),
      ffFixed, precision, digits));
    writeln('QUICKI: ' + FloatToStrF(QUICKI(Insulin, Glucose),
      ffFixed, precision, digits));
    writeln();

    writeln('Scenario T2');
    Insulin := 6 * kInsulinConversionFactor;
    Glucose := 2.8;
    Write('Insulin: ');
    writeln(FloatToStrF(Insulin, ffFixed, precision, digits));
    Write('Glucose: ');
    writeln(FloatToStrF(Glucose, ffFixed, precision, digits));
    writeln('SPINA-GBeta: ' + FloatToStrF(SPINA_GBeta(Insulin, Glucose),
      ffFixed, precision, digits) + ' pmol/s');
    writeln('SPINA-GR: ' + FloatToStrF(SPINA_GR(Insulin, Glucose),
      ffFixed, precision, digits) + ' mol/s');
    writeln('SPINA-DI: ' + FloatToStrF(SPINA_DI(Insulin, Glucose),
      ffFixed, precision, digits));
    writeln('HOMA-Beta: ' + FloatToStrF(HOMA_Beta(Insulin, Glucose),
      ffFixed, precision, digits) + '%');
    writeln('HOMA-IR: ' + FloatToStrF(HOMA_IR(Insulin, Glucose),
      ffFixed, precision, digits));
    writeln('HOMA-IS: ' + FloatToStrF(HOMA_IS(Insulin, Glucose),
      ffFixed, precision, digits));
    writeln('QUICKI: ' + FloatToStrF(QUICKI(Insulin, Glucose),
      ffFixed, precision, digits));
    writeln();

    writeln('Scenario T3');
    Insulin := 16 * kInsulinConversionFactor;
    Glucose := 10.6;
    Write('Insulin: ');
    writeln(FloatToStrF(Insulin, ffFixed, precision, digits));
    Write('Glucose: ');
    writeln(FloatToStrF(Glucose, ffFixed, precision, digits));
    writeln('SPINA-GBeta: ' + FloatToStrF(SPINA_GBeta(Insulin, Glucose),
      ffFixed, precision, digits) + ' pmol/s');
    writeln('SPINA-GR: ' + FloatToStrF(SPINA_GR(Insulin, Glucose),
      ffFixed, precision, digits) + ' mol/s');
    writeln('SPINA-DI: ' + FloatToStrF(SPINA_DI(Insulin, Glucose),
      ffFixed, precision, digits));
    writeln('HOMA-Beta: ' + FloatToStrF(HOMA_Beta(Insulin, Glucose),
      ffFixed, precision, digits) + '%');
    writeln('HOMA-IR: ' + FloatToStrF(HOMA_IR(Insulin, Glucose),
      ffFixed, precision, digits));
    writeln('HOMA-IS: ' + FloatToStrF(HOMA_IS(Insulin, Glucose),
      ffFixed, precision, digits));
    writeln('QUICKI: ' + FloatToStrF(QUICKI(Insulin, Glucose),
      ffFixed, precision, digits));
    writeln();

    writeln('Scenario E1: Extreme values for insulin and glucose');
    Insulin := 200 * kInsulinConversionFactor;
    Glucose := 700 / kGlucoseConversionFactor;
    Write('Insulin: ');
    writeln(FloatToStrF(Insulin, ffFixed, precision, digits));
    Write('Glucose: ');
    writeln(FloatToStrF(Glucose, ffFixed, precision, digits));
    writeln('SPINA-GBeta: ' + FloatToStrF(SPINA_GBeta(Insulin, Glucose),
      ffFixed, precision, digits) + ' pmol/s');
    writeln('SPINA-GR: ' + FloatToStrF(SPINA_GR(Insulin, Glucose),
      ffFixed, precision, digits) + ' mol/s');
    writeln('SPINA-DI: ' + FloatToStrF(SPINA_DI(Insulin, Glucose),
      ffFixed, precision, digits));
    writeln('HOMA-Beta: ' + FloatToStrF(HOMA_Beta(Insulin, Glucose),
      ffFixed, precision, digits) + '%');
    writeln('HOMA-IR: ' + FloatToStrF(HOMA_IR(Insulin, Glucose),
      ffFixed, precision, digits));
    writeln('HOMA-IS: ' + FloatToStrF(HOMA_IS(Insulin, Glucose),
      ffFixed, precision, digits));
    writeln('QUICKI: ' + FloatToStrF(QUICKI(Insulin, Glucose),
      ffFixed, precision, digits));
    writeln();

    writeln('Scenario E2: Extreme values for insulin and glucose');
    Insulin := 0.1 * kInsulinConversionFactor;
    Glucose := 800 / kGlucoseConversionFactor;
    Write('Insulin: ');
    writeln(FloatToStrF(Insulin, ffFixed, precision, digits));
    Write('Glucose: ');
    writeln(FloatToStrF(Glucose, ffFixed, precision, digits));
    writeln('SPINA-GBeta: ' + FloatToStrF(SPINA_GBeta(Insulin, Glucose),
      ffFixed, precision, digits) + ' pmol/s');
    writeln('SPINA-GR: ' + FloatToStrF(SPINA_GR(Insulin, Glucose),
      ffFixed, precision, digits) + ' mol/s');
    writeln('SPINA-DI: ' + FloatToStrF(SPINA_DI(Insulin, Glucose),
      ffFixed, precision, digits));
    writeln('HOMA-Beta: ' + FloatToStrF(HOMA_Beta(Insulin, Glucose),
      ffFixed, precision, digits) + '%');
    writeln('HOMA-IR: ' + FloatToStrF(HOMA_IR(Insulin, Glucose),
      ffFixed, precision, digits));
    writeln('HOMA-IS: ' + FloatToStrF(HOMA_IS(Insulin, Glucose),
      ffFixed, precision, digits));
    writeln('QUICKI: ' + FloatToStrF(QUICKI(Insulin, Glucose),
      ffFixed, precision, digits));
    writeln();

    writeln('Scenario E3: Extreme values for insulin and glucose');
    Insulin := 0 * kInsulinConversionFactor;
    Glucose := 800 / kGlucoseConversionFactor;
    Write('Insulin: ');
    writeln(FloatToStrF(Insulin, ffFixed, precision, digits));
    Write('Glucose: ');
    writeln(FloatToStrF(Glucose, ffFixed, precision, digits));
    writeln('SPINA-GBeta: ' + FloatToStrF(SPINA_GBeta(Insulin, Glucose),
      ffFixed, precision, digits) + ' pmol/s');
    writeln('SPINA-GR: ' + FloatToStrF(SPINA_GR(Insulin, Glucose),
      ffFixed, precision, digits) + ' mol/s');
    writeln('SPINA-DI: ' + FloatToStrF(SPINA_DI(Insulin, Glucose),
      ffFixed, precision, digits));
    writeln('HOMA-Beta: ' + FloatToStrF(HOMA_Beta(Insulin, Glucose),
      ffFixed, precision, digits) + '%');
    writeln('HOMA-IR: ' + FloatToStrF(HOMA_IR(Insulin, Glucose),
      ffFixed, precision, digits));
    writeln('HOMA-IS: ' + FloatToStrF(HOMA_IS(Insulin, Glucose),
      ffFixed, precision, digits));
    writeln('QUICKI: ' + FloatToStrF(QUICKI(Insulin, Glucose),
      ffFixed, precision, digits));
    writeln();

    writeln('Scenario E4: Extreme values for insulin and glucose');
    Insulin := 1 * kInsulinConversionFactor;
    Glucose := 30 / kGlucoseConversionFactor;
    Write('Insulin: ');
    writeln(FloatToStrF(Insulin, ffFixed, precision, digits));
    Write('Glucose: ');
    writeln(FloatToStrF(Glucose, ffFixed, precision, digits));
    writeln('SPINA-GBeta: ' + FloatToStrF(SPINA_GBeta(Insulin, Glucose),
      ffFixed, precision, digits));
    writeln('SPINA-GBeta: ' + FloatToStrF(SPINA_GBeta(Insulin, Glucose),
      ffFixed, precision, digits) + ' pmol/s');
    writeln('SPINA-GR: ' + FloatToStrF(SPINA_GR(Insulin, Glucose),
      ffFixed, precision, digits) + ' mol/s');
    writeln('HOMA-Beta: ' + FloatToStrF(HOMA_Beta(Insulin, Glucose),
      ffFixed, precision, digits) + '%');
    writeln('HOMA-IR: ' + FloatToStrF(HOMA_IR(Insulin, Glucose),
      ffFixed, precision, digits));
    writeln('HOMA-IS: ' + FloatToStrF(HOMA_IS(Insulin, Glucose),
      ffFixed, precision, digits));
    writeln('QUICKI: ' + FloatToStrF(QUICKI(Insulin, Glucose),
      ffFixed, precision, digits));
    writeln();

    Terminate;
  end;

  constructor TScenarioTestApp.Create(TheOwner: TComponent);
  begin
    inherited Create(TheOwner);
    StopOnException := True;
  end;

  destructor TScenarioTestApp.Destroy;
  begin
    inherited Destroy;
  end;

  procedure TScenarioTestApp.WriteHelp;
  begin
    { add your help code here }
    writeln('Usage: ', ExeName, ' -h');
  end;

var
  Application: TScenarioTestApp;

  {$R *.res}

begin
  Application := TScenarioTestApp.Create(nil);
  Application.Title := 'ScenarioTester';
  Application.Run;
  Application.Free;
end.
