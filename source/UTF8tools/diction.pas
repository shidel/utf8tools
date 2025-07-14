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

procedure DetectLanguage(const S : TUTF8String; out Lang : String;
  out Recognized : integer); override;
function DetectLanguage(const S : TUTF8String): String; override;
function LanguageCodepage(S : String) : integer;

implementation

type
  TLangTest = record
    Map : TMapTree;
    Language : String;
    Codepage : Integer;
    Count : Integer;
  end;
  TLangTests = array of TLangTest;

var
  LangTests : TLangTests;

{$I words/English.inc}
{$I words/German.inc}
{$I words/French.inc}
{$I words/Turkish.inc}
{$I words/Swedish.inc}

procedure DetectLanguage(const S : TUTF8String; out Lang : String;
  out Recognized : integer);
begin
  Lang:='';
  Recognized:=0;
end;

function DetectLanguage(const S : TUTF8String): String; override;
var
  N : String;
  C : Integer;
begin
  DetectLanguage(S, N, C);
  DetectLanguage:=N;
end;

function LanguageCodepage(S : String) : integer;
begin
  LanguageCodepage:=-1;
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
