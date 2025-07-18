unit SPINATypes;

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

{ Global types and constants }

{ Source code released under the BSD License }
{ See http://spina.medical-cybernetics.de for details }

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Graphics, Math;

const
  kNUL = char(0);           {Special ASCII characters}
  kENTER = char(3);
  kTAB = char(9);
  kS4Tab = '    ' + kTAB;
  kS10Tab = '          ' + kTAB;
  kS12Tab = '            ' + kTAB;
  kLF = char(10);
  kRETURN = char(13);
  kESCAPE = char(27);
  kPERIOD = '.';
  kSPACE = ' ';
  kSLASH = '/';
  kCOLON = ':';
  kOMIT = '•';
  kParOpen = '(';
  kParClose = ')';
  kCRLF = #13#10;
  DEC_POINT = '.';
  DEC_COMMA = ',';

  kSPINA_GBeta = 'SPINA-GBeta';
  kSPINA_GR = 'SPINA-GR';
  kSPINA_DI = 'SPINA-DI';
  kHOMA_Beta = 'HOMA-Beta';
  kHOMA_IR = 'HOMA-IR';
  kHOMA_IS = 'HOMA-IS';
  kQUICKI = 'QUICKI';
  kAIGR = 'AIGR';
  kCGR = 'CGR';

  GlucSUom = 'mg/dl'; {Standard UoMs for GUI}
  InsSUoM = 'mIU/l';  {see below for standard UoMs for Engine}
  CPepCUom = 'ng/ml';
  GBetaUoM = 'pmol/s';
  GRUoM = 'mol/s';
  HOMABetaUoM = '%';
  AIGRUoM = 'pmol/mmol';
  CGRUoM = 'pmol/10 mg';

  NA_MARK = 'N/A';
  REF_RANGE_FLAG = '*'; {flag sign for marking results outside the reference range}

  BASE_URL = 'http://spina.medical-cybernetics.de';
  SPINA_GLOBAL_ID = 'net.sf.spina';
  SPINA_CARB_GLOBAL_ID = 'net.sf.spina.carb';
  HELP_URL = 'http://spina.sourceforge.net/manual.html';
  PORTAL_URL = 'http://www.ruhr-uni-bochum.de/spina-portal';

  IMPLEMENTATION_MESSAGE =
    'This function is not implemented in this version of SPINA Thyr.';
  FORMAT_MESSAGE = 'Please check your input.';
  FILE_FORMAT_MESSAGE = 'File format error.';
  SAVE_ERROR_MESSAGE = 'Error saving the file.';
  FILE_EMPTY_MESSAGE = 'No lab results available.';
  RR_FORMAT_ERROR_MESSAGE = 'The CDISC Lab model XML file is malformatted.';
  RR_SPINA_ERROR_MESSAGE =
    'Definitions for structure parameters in CDISC Lab model XML file are missing.';
  PREFERENCES_READ_ERROR_MESSAGE =
    'Preferences could not be read. Please check access rights of your user or home folder';
  PREFERENCES_SAVE_ERROR_MESSAGE =
    'The preferences could not be saved permanently, however, they are valid for this session.';
  FOLDER_NOT_SUPPORTED_MESSAGE =
    'Folders are not supported in this version of SPINA Carb.';
  URLS_NOT_SUPPORTED_MESSAGE = 'URLs are not supported in this version of SPINA Carb.';

  ISO_8601_DATE_FORMAT = 'YYYY-MM-DD"T"hh:nn:ss';
  {Date/time format in XML representation}
  STANDARD_NUM_FORMAT = '###,##0.0000';
  SHORT_NUM_FORMAT = '###,###.00';
  STANDARD_TIME_FORMAT = '"d"D hh:nn:ss';

  clLtYellow = TColor($66FFFF);
  clLtOrange = TColor($89E9FF);

  BParLabels = 'Glucose: ' + LineEnding + 'Insulin: ' + LineEnding +
    'C-peptide: ';
  SParLabels = 'SPINA-GBeta: ' + LineEnding + 'SPINA-GR: ' + LineEnding +
    'SPINA-DI: ' + LineEnding + 'HOMA-Beta: ' + LineEnding + 'HOMA-IR: ' +
    LineEnding + 'HOMA-IS: ' + LineEnding + 'QUICKI: ' + LineEnding +
    'AIGR: ' + LineEnding + 'CGR : ';

type
  tReferenceLimits = record
    ln, hn, lt, ht, lp, hp: extended;
    UoM: string;
  end;

  tReferenceExDefinitions = record
    Sex: char;
    AgeL, AgeH: integer;
    UOMS, UOMC: string;
    LXS, HXS, LXC, HXC: real;
    startDateTime: string
  end;

  tReferenceNormDefinitions = record
    Sex: char;
    AgeL, AgeH: integer;
    UOMS, UOMC: string;
    LS, HS, LTS, HTS, LPS, HPS, LC, HC, LTC, HTC, LPC, HPC: real;
    startDateTime: string
  end;

  tReferenceValues = record
    Insulin, Glucose, CPeptide: tReferenceLimits;
    SPINA_GBeta, SPINA_GR, SPINA_DI: tReferenceLimits;
    HOMA_Beta, HOMA_IR, HOMA_IS, QUICKI, AIGR, CGR: tReferenceLimits;
  end;

  tPreferredUoMs = record
    Insulin, Glucose, CPeptide: String;
    AIGR, CGR: String;
  end;

  tPreferences = record
    new, rememberUsedUnits, colouriseMandatoryFields, exportLOINC: boolean;
    MandatoryColor: TColor;
    MSH_ID, Placer_ID: string;
    PrintFont: string;
    ReferenceValues: tReferenceValues;
    PreferredUoMs: tPreferredUoMs;
  end;

const
  kEngineUoMs: tPreferredUoMs = { UoMs for Engine }
    (Insulin: 'pmol/l';
    Glucose: 'mmol/l';
    CPeptide: 'nmol/l';
    AIGR: AIGRUoM;
    CGR: CGRUoM);

  {Standard reference ranges according to to the references provided below}
  sReferenceValues: tReferenceValues =
                    (Insulin: (ln: 2; hn: 25; lt: NaN; ht: NaN; lp: NaN;
                              hp: NaN; UoM: InsSUoM);
                    Glucose: (ln: 70; hn: 100; lt: 70; ht: 180; lp: 54;
                             hp: 250; UoM: GlucSUom);
                    CPeptide: (ln: 0.8; hn: 3.1; lt: NaN; ht: NaN; lp: NaN;
                              hp: NaN; UoM: CPepCUom);
                    SPINA_GBeta: (ln: 0.64; hn: 3.73; lt: NaN; ht: NaN; lp: NaN;
                              hp: NaN; UoM: GBetaUoM);
                    SPINA_GR: (ln: 1.41; hn: 9.00; lt: NaN; ht: NaN; lp: NaN;
                              hp: NaN; UoM: GRUoM);
                    SPINA_DI: (ln: 4.01; hn: 7.65; lt: NaN; ht: NaN; lp: NaN;
                              hp: NaN; UoM: '');
                    HOMA_Beta: (ln: 45.4; hn: 179.4; lt: NaN; ht: NaN; lp: NaN;
                              hp: NaN; UoM: HOMABetaUoM);
                    HOMA_IR: (ln: 0.0; hn: 2.5; lt: NaN; ht: NaN; lp: NaN;
                              hp: NaN; UoM: '');
                    HOMA_IS: (ln: 0.4; hn: Infinity; lt: NaN; ht: NaN; lp: NaN;
                              hp: NaN; UoM: '');
                    QUICKI: (ln: 0.4; hn: Infinity; lt: NaN; ht: NaN; lp: NaN;
                              hp: NaN; UoM: '');
                    AIGR: (ln: 0; hn: 53.6; lt: NaN; ht: NaN; lp: NaN;
                              hp: NaN; UoM: AIGRUoM);
                    CGR: (ln: 5.0; hn: Infinity; lt: NaN; ht: NaN; lp: NaN;
                              hp: NaN; UoM: CGRUoM));

{
1. Khalili D, Khayamzadeh M, Kohansal K, Ahanchi NS, Hasheminia M, Hadaegh F,
   Tohidi M, Azizi F, Habibi-Moeini AS. Are HOMA-IR and HOMA-B good predictors
   for diabetes and pre-diabetes subtypes? BMC Endocr Disord. 2023 Feb 14;
   23(1):39. doi: 10.1186/s12902-023-01291-9. PMID: 36788521; PMCID: PMC9926772.

2. Dietrich JW, Abood A, Dasgupta R, Anoop S, Jebasingh FK, Spurgeon R,
   Thomas N, Boehm BO. A novel simple disposition index (SPINA-DI) from fasting
   insulin and glucose concentration as a robust measure of carbohydrate
   homeostasis. J Diabetes. 2024 Sep;16(9):e13525. doi: 10.1111/1753-0407.13525.
   Epub 2024 Jan 2. PMID: 38169110; PMCID: PMC11418405.

3. Nauck MA, Meier JJ. Diagnostic accuracy of an "amended" insulin-glucose ratio
   for the biochemical diagnosis of insulinomas. Ann Intern Med. 2012 Dec 4;157
   (11):767-75. doi: 10.7326/0003-4819-157-11-201212040-00004. PMID: 23208166.

4. Fritsche A, Heni M, Peter A, Gallwitz B, Kellerer M, Birkenfeld AL,
   Häring HU, Wagner R. Considering Insulin Secretory Capacity as Measured by a
   Fasting C-Peptide/Glucose Ratio in Selecting Glucose-Lowering Medications.
   Exp Clin Endocrinol Diabetes. 2022 Mar;130(3):200-204.
   doi: 10.1055/a-1242-9809. Epub 2020 Sep 18. PMID: 32947641;
   PMCID: PMC8926455.
}
var
  gPreferences: tPreferences;
  gNumberFormat, gDateTimeFormat: string;
  gStandardMandatoryColor: TColor;
  gCEcertified: boolean;

implementation

initialization
  gNumberFormat := STANDARD_NUM_FORMAT;
  gDateTimeFormat := STANDARD_TIME_FORMAT;

end.
