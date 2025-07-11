{
   Copyright (c) 2025 Jerome Shidel
   BSD-3-Clause license
}

unit Unicode;

{$IFDEF FPC}
  {$mode objfpc}{$H+}
{$ELSE}
  {$DEFINE TP70}
{$ENDIF}

interface

uses Classes, SysUtils, Common;

const
  Invalid7F : boolean = true;

type
  { String containing one or more bytes for UTF-8 encoded characters.
    Characters such as "Ç, É, Æ, £ and many more" }
  TUTF8CodePoint = String;
  { Numeric value of a decoded UTF-8 character }
  TUTF8Value = Int32;

{ Return the number of bytes required for the first encoded
  UTF-8 character in a string of one or more encoded UTF-8 characters. }
function CodePointLength(C : TUTF8CodePoint) : integer;

{ Decode the first UTF-8 encoded character in a string and return
  the numeric value forthat character. This is the save value used
  in unnamed HTML characters like "&#x2302;"
  If not encoded correctly, will return Value=-1 and result=false. }
function CodePointToValue(C : TUTF8CodePoint; out Value : TUTF8Value) : boolean; overload;

{ Decode the first UTF-8 encoded character in a string and return
  the numeric value forthat character. This is the save value used
  in unnamed HTML characters like "&#x2302;"
  If not encoded correctly, will return Value=-1. }
function CodePointToValue(C : TUTF8CodePoint) : TUTF8Value; overload;

{ Will encode a character value into a series of bytes representing
  a UTF-8 character. If out of the valid range of characters, will
  return false. Acceptable charcter values range from 0x00 to 0x10ffff. }
function ValueToCodePoint(Value : TUTF8Value; out C : TUTF8CodePoint) : boolean; overload;

{ Will encode a character value into a series of bytes representing
  a UTF-8 character. If out of the valid range of characters, will
  return a null string.}
function ValueToCodePoint(Value : TUTF8Value): TUTF8CodePoint; overload;

function IntsToCodePoint(Ints : TMapString; out C : TUTF8CodePoint) : boolean;
function CodePointToInts(C : TUTF8CodePoint; out Ints : TMapString) : boolean;

procedure Initialize;
procedure Finalize;

implementation

const
  CodePointMasks : array[1..4] of record A, O : byte end = (
    (A:$7f; O:$80),
    (A:$1f; O:$c0),
    (A:$0f; O:$e0),
    (A:$0f; O:$f0)
  );

function CodePointLength(C : TUTF8CodePoint) : integer;
var
  V : byte;
begin
  CodePointLength:=-1;
  if C = '' then exit;
  V:=Byte(C[Low(C)]);

  if (V and $80) = $00 then begin
    if (V <> $7f) or (not Invalid7F) then
      CodePointLength := 1;
    {
      // special 127, basically ignored followed by 3 bytes fopr "Home" char
      else
      // CodePointLength := 4
    }
  end
  else if (V and $f0) = $f0 then
    CodePointLength := 4
  else if (V and $e0) = $e0 then
    CodePointLength := 3
  else if (V and $e0) = $c0 then
    CodePointLength := 2;
  {
  // I don't think is actually a "legal" character.
  else
    CodePointLength := -1;
  }
end;

function CodePointToValue(C : TUTF8CodePoint; out Value : TUTF8Value) : Boolean; overload;
var
  P, L, M, V : integer;
begin
  CodePointToValue:=False;
  Value:=-1;
  if Length(C) = 0 then Exit;
  L := 0;
  P := 0;
  V := Byte(C[Low(C) + P]);
  if (V and $80) = 0 then begin // High bit not set
    L := 1;
    if V = $7f then begin
      // Not really a valid UTF-8 character
      // probably followed by value x2302 (226 140 130)
      // Browsers either ignore it or display a Box. Then the x2302 "house"
      // symbol. So, I will treat as a UTF-8 Encoding error
      exit;
    end;
  end
  else if (V and $f0) = $f0 then  // 1111????
    L := 4
  else if (V and $e0) = $e0 then  // 1110????
    L := 3
  else if (V and $e0) = $c0 then  // 110?????
    L := 2
  else                            // 10??????
    // I dont think this is possible for the first character
    // M := $3f;
    // L := 1;
    Exit;
  if Length(C) < L then Exit; // error, string not long enough
  V := 0;
  M := CodePointMasks[L].A;
  while P < L do begin
    if (P > 0) and ((Byte(C[Low(C) + P]) and $c0) <> $80) then Exit; // After first, 10??????
    V:=(V shl 6) + (Byte(C[Low(C) + P]) and M);
    M := $3f;
    Inc(P);
  end;
  Value :=V;
  CodePointToValue:= True;
end;

function CodePointToValue(C : TUTF8CodePoint) : TUTF8Value; overload;
begin
  CodePointToValue(C,CodePointToValue);
end;

function ValueToCodePoint(Value : TUTF8Value; out C : TUTF8CodePoint) : boolean; overload;
var
  P, L, A, O : integer;
begin
  C := '';
  ValueToCodePoint:=False;
  if Value > $10ffff then Exit; // Too big
  if Value > $ffff then
    P := 4
  else
  if Value > $07ff then
    P := 3
  else
  if Value > $007f then
    P := 2
  else
  if Value = $007f then
    Exit // Treat $7f as invalid
  else
    P := 1;
  SetLength(C, P);
  L := P;
  A := $3f;
  O := $80;
  while P > 0 do begin
    if P = 1 then begin
      A := CodePointMasks[L].A;
      if L > 1 then
        O := CodePointMasks[L].O
      else
        O := 0;
    end;
    Byte(C[Low(C) + P - 1]) := (Value and A) or O;
    Value := Value shr 6;
    Dec(P);
  end;
  ValueToCodePoint:=True;
end;

function ValueToCodePoint(Value : TUTF8Value): TUTF8CodePoint; overload;
begin
  ValueToCodePoint(Value, ValueToCodePoint);
end;

function IntsToCodePoint(Ints: TMapString; out C: TUTF8CodePoint): boolean;
var
  T : TUTF8CodePoint;
  V : TUTF8Value;
  N : TMapString;
  E : Integer;
begin
  IntsToCodePoint:=False;
  C:='';
  while Ints <> '' do begin
    N := PopDelim(Ints, ',');
    Val(N, V, E);
    if E <> 0 then Exit;
    if not ValueToCodePoint(V, T) then Exit;
    C:=C+T;
  end;
  IntsToCodePoint:=True;
end;

function CodePointToInts(C: TUTF8CodePoint; out Ints: TMapString): boolean;
var
  V : TUTF8Value;
  L : Integer;
begin
  CodePointToInts := False;
  Ints:='';
  while C <> '' do begin
    L := CodePointLength(C);
    if (L<1) then Exit;
    if not CodePointToValue(C, V) then Exit;
    if Ints <> '' then Ints:=Ints+',';
    Ints:=Ints+IntToStr(V);
    Delete(C, 1, L);
  end;
  CodePointToInts := True;
end;

{ Unit initialization routines }
var
  OldExitProc : pointer;

procedure Initialize;
begin
  if Assigned(OldExitProc) then Exit;
  OldExitProc := ExitProc;
  ExitProc := @Finalize;
end;

procedure Finalize;
begin
  if not Assigned(OldExitProc) then Exit;
  ExitProc := OldExitProc;
  OldExitProc := nil;
end;

begin
  OldExitProc := nil;
  Initialize;
end.
