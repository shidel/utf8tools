{
   Copyright (c) 2025 Jerome Shidel
   BSD-3-Clause license
}

unit Diction;

{$IFDEF FPC}
  {$mode objfpc}{$H+}
{$ELSE}
  {$DEFINE TP70}
{$ENDIF}

interface

uses
  Classes, SysUtils, Common;

function DetectLanguage(const S : TUTF8String): String; override;
procedure DetectLanguage(Codepage : integer; const S : TUTF8String;
  out Lang : String; out Recognized : integer); override;
function LanguageCodepage(S : String) : integer;
function DetectableLanguages : TStringArray;

implementation

type
  TLangTest = record
    Map : TMapTree;
    Language : String;
    Codepage : Integer;
  end;
  TLangTests = array of TLangTest;

var
  LangTests : TLangTests;

function DetectableLanguages : TStringArray;
var
  I : integer;
begin
  DetectableLanguages:=[];
  SetLength(DetectableLanguages, Length(LangTests));
  for I := 0 to Length(LangTests) - 1 do
    DetectableLanguages[I]:=LangTests[I].language;
end;

function CheckLanguage(var Map : TMapTree; const Data : String) : integer;
const
  B : String=SPACE+'`1234567890-=[]\;,./~!@#$%^&*()_+{}:<>?"' + QUOTE+CR+LF+TAB;
var
  P, L, M : integer;
  S : String;
begin
  CheckLanguage:=0;
  P := 0;
  M := Length(Data);
  While P < M do begin
    Inc(P);
    While (P < M) and (Pos(Data[P], B) > 0) do Inc(P);
    L:=1;
    While (P+L < M) and (Pos(Data[P+L], B) < 1) do
      Inc(L);
    if (L < 3) then Continue;
    S := Copy(Data, P, L);
    Inc(P,L);
    if Assigned(Map.Find(S)) then
      Inc(CheckLanguage, L);
  end;
end;

procedure DetectLanguage(Codepage : integer; const S : TUTF8String; out Lang : String;
  out Recognized : integer);
var
  I : integer;
  C : integer;
begin
  Lang:='';
  Recognized:=0;
  for I := 0 to Length(LangTests) - 1 do begin
    if Codepage <> LangTests[I].Codepage then Continue;
    C := CheckLanguage(LangTests[I].Map, S);
    // WriteLn(LangTests[I].Language, ': ', C);
    if C > Recognized then begin
      Recognized:=C;
      Lang:=LangTests[I].Language;
    end;
  end;
end;

function DetectLanguage(const S : TUTF8String): String;
var
  I : integer;
  C, R : integer;
begin
  DetectLanguage:='';
  R:=0;
  for I := 0 to Length(LangTests) - 1 do begin
    C := CheckLanguage(LangTests[I].Map, S);
    if C > R then begin
      R:=C;
      DetectLanguage:=LangTests[I].Language;
    end;
  end;
end;

function LanguageCodepage(S : String) : integer;
var
  I : integer;
begin
  S := Trim(Uppercase(S));
  LanguageCodepage:=-1;
  for I := 0 to Length(LangTests) - 1 do
    if Uppercase(LangTests[I].Language) = S then begin
      LanguageCodepage:=LangTests[I].Codepage;
      Break;
    end;
end;

procedure AddLang(const A : TStringArray; Lang : String; CP : Integer);
var
  H : integer;
  I : integer;
begin
  H := Length(LangTests);
  SetLength(LangTests, H+1);
  LangTests[H].Language := Lang;
  LangTests[H].Codepage := CP;
  LangTests[H].Map := TMapTree.Create;
  for I := Low(A) to High(A) do
    LangTests[H].Map.Add(A[I]);
end;


{$I words/English.inc}
{$I words/German.inc}
{$I words/French.inc}
{$I words/Turkish.inc}
{$I words/Swedish.inc}
{$I words/Norwegian.inc}
{$I words/Russian.inc}
{$I words/Spanish.inc}
{$I words/Esperanto.inc}

{$I words/Arabic.inc}
{$I words/Chinese-Simplified.inc}
{$I words/Chinese-Traditional.inc}
{$I words/Japanese.inc}
{$I words/French-Canadian.inc}
{$I words/Greek.inc}
{$I words/Croatian.inc}
{$I words/Czech.inc}
{$I words/Polish.inc}
{$I words/Romanian.inc}
{$I words/Dutch.inc}
{$I words/Faroese.inc}
{$I words/Haitian-Creole.inc}
{$I words/Hawaiian.inc}
{$I words/Irish.inc}
{$I words/Italian.inc}
{$I words/Welsh.inc}
{$I words/Samoan.inc}
{$I words/Venetian.inc}
{$I words/Swahili.inc}
{$I words/Corsican.inc}
{$I words/Latin.inc}
{$I words/Slovak.inc}
{$I words/Hebrew.inc}
{$I words/Yiddish.inc}
{$I words/Danish.inc}
{$I words/Korean.inc}
{$I words/Portuguese-Brazil.inc}
{$I words/Portuguese-Portugal.inc}
{$I words/Serbian.inc}
{$I words/Ukrainian.inc}
{$I words/Mongolian.inc}
{$I words/Sudanese.inc}
{$I words/Thai.inc}
{$I words/Georgian.inc}
{$I words/Hindi.inc}
{$I words/Vietnamese.inc}
{$I words/Sanskrit.inc}
{$I words/Tibetan.inc}

procedure Initialize;
begin
  LangTests:=[];
  AddLang(EnglishLanguageWords, 'English', 437);
  AddLang(GermanLanguageWords, 'German', 850); // Alternate 858
  AddLang(FrenchLanguageWords, 'French', 850);
  AddLang(TurkishLanguageWords, 'Turkish', 857);
  AddLang(SwedishLanguageWords, 'Swedish', 865); // 865, 850 or 1106
  AddLang(SpanishLanguageWords, 'Spanish', 850); // 220 for Spanish and Catalan
  AddLang(EsperantoLanguageWords, 'Esperanto', 850);  // Not in DOS
  AddLang(NorwegianLanguageWords, 'Norwegian', 865); // 865 or 850
  AddLang(RussianLanguageWords, 'Russian', 866);
  AddLang(ArabicLanguageWords, 'Arabic', 864); // 720 or 864
  AddLang(ChineseSimplifiedLanguageWords, 'Chinese (Simplified)', 936);
  AddLang(ChineseTraditionalLanguageWords, 'Chinese (Traditional)', 950);
  AddLang(JapaneseLanguageWords, 'Japanese', 932);
  AddLang(FrenchCanadianLanguageWords, 'French Canadian', 863);
  AddLang(GreekLanguageWords, 'Greek', 737); // 737 Or 869
  AddLang(CroatianLanguageWords, 'Croatian', 852); // Serbo-Croatian
  AddLang(CzechLanguageWords, 'Czech', 852);
  AddLang(PolishLanguageWords, 'Polish', 852);
  AddLang(RomanianLanguageWords, 'Romanian', 852);

  AddLang(DutchLanguageWords, 'Dutch', 850);
  AddLang(FaroeseLanguageWords, 'Faroese', 850);
  AddLang(HaitianCreoleLanguageWords, 'Haitian-Creole', 850);
  AddLang(HawaiianLanguageWords, 'Hawaiian', 850); // Maybe 850 or 862
  AddLang(IrishLanguageWords, 'Irish', 850);
  AddLang(ItalianLanguageWords, 'Italian', 850); // Prefered Windows-1252
  AddLang(SamoanLanguageWords, 'Samoan', 850);
  AddLang(VenetianLanguageWords, 'Venetian', 850);
  AddLang(WelshLanguageWords, 'Welsh', 850);
  AddLang(SwahiliLanguageWords, 'Swahili', 850);

  AddLang(CorsicanLanguageWords, 'Corsican', 852); // Not specificly listed.
  AddLang(LatinLanguageWords, 'Latin', 852);
  AddLang(SlovakLanguageWords, 'Slovak', 852); // Also 895 Czech Kamenicky

  AddLang(HebrewLanguageWords, 'Hebrew', 862); // Partial 862, Windows-1255
  AddLang(YiddishLanguageWords, 'Yiddish', 862);
  AddLang(DanishLanguageWords, 'Danish', 865);
  AddLang(KoreanLanguageWords, 'Korean', 949);

  AddLang(PortugueseBrazilLanguageWords, 'Portuguese (Brazil)', 860);
  AddLang(PortuguesePortugalLanguageWords, 'Portuguese (Portugal)', 860);

  AddLang(SerbianLanguageWords, 'Serbian', 866);
  AddLang(UkrainianLanguageWords, 'Ukrainian', 866);
  AddLang(MongolianLanguageWords, 'Mongolian', 866);  // No actual codepage in DOS.

  AddLang(SudaneseLanguageWords, 'Sudanese',  864); // Partial DOS support
  AddLang(ThaiLanguageWords, 'Thai', 874);

  AddLang(GeorgianLanguageWords, 'Georgian', 60853);  // 60853 or 59829
  AddLang(HindiLanguageWords, 'Hindi', 1137); // 1137

  AddLang(VietnameseLanguageWords, 'Vietnamese', 1258);  // Windows-1258
  AddLang(SanskritLanguageWords, 'Sanskrit', -1);  // Not in DOS.
  AddLang(TibetanLanguageWords, 'Tibetan', -1);   // Not for DOS

  // AddLang(HungarianLanguageWords, '', 852);
  // AddLang(SloveneLanguageWords, '', 852);


end;

procedure Finalize;
var
   I : Integer;
begin
  for I := 0 to Length(LangTests) - 1 do
    FreeAndNil(LangTests[I].Map);
  SetLength(LangTests, 0);
end;

initialization

  Initialize;

finalization

  Finalize;

end.
