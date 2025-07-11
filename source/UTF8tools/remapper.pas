{
   Copyright (c) 2025 Jerome Shidel
   BSD-3-Clause license
}

unit Remapper;

{$IFDEF FPC}
  {$mode objfpc}{$H+}
{$ELSE}
  {$DEFINE TP70}
{$ENDIF}

interface

uses Classes, SysUtils, Common, Unicode;

implementation

{$I maps\map_html.inc}
{$I maps\map_utf8.inc}

var
  FHTML,
  FUTF8 : TMapTree;

function Encode(var H : TMapString; U : TMapString) : TUTF8CodePoint;
begin
  if not IntsToCodePoint(U, Encode) then begin
    Encode:='';
    Exit;
  end;
  H := '&' + H + ';';
end;

procedure MapHTML;
var
  I : integer;
  H : TMapString;
  U : TUTF8CodePoint;
begin
  for I := Low(HTMLtoUTF8RemapList) to High(HTMLtoUTF8RemapList) do begin
    H := HTMLtoUTF8RemapList[I].Value;
    U :=Encode(H, HTMLtoUTF8RemapList[I].Data);
    if U = '' then
      Raise Exception.Create('invalid UTF-8 codepoint entry for HTML map entry ' + H);
    FHTML.Add(H, U);
  end;
end;

procedure MapUTF8;
var
  I : integer;
  H : TMapString;
  U : TUTF8CodePoint;
begin
  for I := Low(UTF8toHTMLRemapList) to High(UTF8toHTMLRemapList) do begin
    H := UTF8toHTMLRemapList[I].Data;
    U :=Encode(H, UTF8toHTMLRemapList[I].Value);
    if U = '' then
      Raise Exception.Create('invalid Codepoint entry ' + UTF8toHTMLRemapList[I].Value + ' in UTF-8 map');
    FUTF8.Add(U, H);
  end;
end;

Procedure Initialize;
begin
  FHTML:=TMapTree.Create;
  FUTF8:=TMapTree.Create;
  MapHTML;
  MapUTF8;
end;

procedure Finalize;
begin
  FreeAndNil(FUTF8);
  FreeAndNil(FHTML);
end;

initialization
  Initialize;
finalization
  Finalize;
end.
