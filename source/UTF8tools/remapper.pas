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

function UTF8toHTML( const U : TUTF8String; Options : TConvertOpts = [] )
  : TUTF8String;

function HTMLtoUTF8( const H : TUTF8String; Options : TConvertOpts = [] )
  : TUTF8String;

implementation

{$I maps\map_html.inc}
{$I maps\map_utf8.inc}

var
  FHTML,
  FUTF8 : TMapTree;

function UTF8toHTML( const U : TUTF8String; Options : TConvertOpts = [] )
  : TUTF8String;
begin
  UTF8toHTML:=U;
end;

function HTMLtoUTF8( const H : TUTF8String; Options : TConvertOpts = [] ) : TUTF8String;
var
  Node : TMapNode;
  P, A, N : Integer;
  T, X : String;
begin
  HTMLtoUTF8:='';
  P:=1;
  While P <= Length(H) do begin
    T:=Copy(H, P, 1);
    if (T = '&') then begin
      A := Pos(';', H, P);
      N := Pos(SPACE, H, P) - 1;
      if A = 0 then A := Length(H);
      if (N > 0) and (N <A) then A := N;
      T:=Copy(H, P, A - P + 1);
      P:=A;
      // if T='&' then T:='&amp;'; { fix broken & }
    end;
    Inc(P);
    if Length(T) > 1 then begin
      X:='';
      if Copy(T,1,2) = '&#' then begin
        Val(Copy(T, 3, Length(T) - 3), N, A);
        if A = 0 then
          ValueToCodePoint(N, X);
      end else begin
        Node := FHTML.Find(T);
        if Assigned(Node) then X:=Node.Data;
      end;

      { check on don't covert stuff }
      { no double unicode chars are in any of the don't convert }
      if (Length(X) = 1) then begin
        N:=Ord(X[1]);
        if (N=38) or (N=60) or (N=62) then begin
          { < > & }
          if not (cvHTMLCtrl in Options) then X:='';
        end else if (N<128) then begin
          { General Punctuation and Symbols }
          if not (cvPunctuation in Options) then X:='';
        end;
      end;
      { So if we still got X, whatever it was, it should be replaced }
      if X <> '' then T:=X;
    end;
    HTMLtoUTF8:=HTMLtoUTF8+T;
  end;
end;

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
