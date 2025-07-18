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

function HTMLCode (T:String) : String;
var
  S : String;
  V, E : Integer;
begin
  HTMLCode:='';
  CodePointToInts(T, S);
  while S <> '' do begin
    T:=PopDelim(S, ',');
    Val(T, V, E);
    if E <> 0 then Continue;
    if V < 999 then
      T := IntToStr(V)
    else begin
      T := HexStr(V, 6);
      While (T[1] = '0') do delete(T, 1,1);
      T:='x'+T;
    end;
    HTMLCode:=HTMLCode + '&#' + T + ';';
  end;
end;

function GetHTMLCode(T : string; Options : TConvertOpts) : String;
var
  N : TMapNode;
begin
  GetHTMLCode:=T;
  if (T = SPACE) or (T='') then
    Exit;
  N:=FUTF8.Find(T);
  if Assigned(N) then begin
    if cvHTMLCodes in Options then
      GetHTMLCode:=HTMLCode(T)
    else
      GetHTMLCode:=N.Data;
  end
  else if (Length(T) > 1) or (T[1] > #127) then
    GetHTMLCode:=HTMLCode(T);
end;

function UTF8toHTML(const U : TUTF8String; Options : TConvertOpts = [] )
  : TUTF8String;
var
  P, L, A, N  : Integer;
  T, X : String;
begin
  UTF8toHTML:='';
  P := 1;
  While P <= Length(U) do begin
    X:='';
    L:=CodePointLength(Copy(U, P, 4)); // 1 to 4 bytes of string
    if L < 1 then L := 1;
    T:=Copy(U, P, L);
    Inc(P,L);
    if (T = '&') then begin
      Dec(P);
      A := Pos(';', U, P);
      N := Pos(SPACE, U, P) - 1;
      if A = 0 then A := Length(U);
      if (N > 0) and (N <A) then A := N;
      T:=Copy(U, P, A - P + 1);
      P:=A + 1;
      // if T='&' then T:='&amp;'; { fix broken & }
    end else
    if (L=1) and (T[1] < #128) then begin
      if (T = '&') then begin
        if (cvHTMLCtrl in Options) then
          X:=GetHTMLCode(T, Options);
      end else if (cvPunctuation in Options) then begin
         X:=GetHTMLCode(T, Options);
      end;
    end else
      X:=GetHTMLCode(T, Options);
    if X <> '' then
      UTF8toHTML:=UTF8toHTML+X
    else
      UTF8toHTML:=UTF8toHTML+T;
  end;
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
