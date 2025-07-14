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

{$I words/English.inc}
{$I words/German.inc}
{$I words/French.inc}
{$I words/Turkish.inc}
{$I words/Swedish.inc}
{$I words/Norwegian.inc}
{$I words/Russian.inc}

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
      Inc(CheckLanguage);
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

procedure Initialize;
begin
  LangTests:=[];
  AddLang(EnglishLanguageWords, 'English', 437);
  AddLang(GermanLanguageWords, 'German', 850);
  AddLang(FrenchLanguageWords, 'French', 850);
  AddLang(TurkishLanguageWords, 'Turkish', 857);
  AddLang(SwedishLanguageWords, 'Swedish', 850);
  AddLang(NorwegianLanguageWords, 'Norwegian', 865);
  AddLang(RussianLanguageWords, 'Russian', 866);
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
