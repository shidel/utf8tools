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

procedure DetectLanguage(const S : TUTF8String; out Lang : String; out Recognized : integer);
function LanguageCodepage(S : String) : integer;

implementation

procedure DetectLanguage(const S : TUTF8String; out Lang : String; out Recognized : integer);
begin
  Lang:='';
  Recognized:=0;
end;

function LanguageCodepage(S : String) : integer;
begin
  LanguageCodepage:=-1;
end;


end.
