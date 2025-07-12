{
   Copyright (c) 2025 Jerome Shidel
   BSD-3-Clause license
}

unit Codepage;

{$IFDEF FPC}
  {$mode objfpc}{$H+}
{$ELSE}
  {$DEFINE TP70}
{$ENDIF}

interface

uses Classes, SysUtils, Common, Unicode;

type
  { Results from converting text from UTF-8 to ASCII.
    ASCII = The converted string.
    Count = Number of processed characters.
    Unicode = Number of valid unicode characters.
    Match = Number of Unicode characters converted to Codepage. Indicates
      correct codepage for conversion. Unicode = Match then proper codepage.
    Other = Number of control and miscellaneous characters like CR, LF, etc.
    Errors = Number of Broken, damaged or illegal UTF-8 characters. Indicates
      either a corrupt file or non-UTF-8 encoded file.
  }
  TResultsCP = record
    Ascii : TAsciiString;
    Count : integer;
    Unicode : integer;
    Match : integer;
    Other : integer;
    Errors : integer;
  end;

{ Return a comma separated sting of available codepages }
function Codepages : String; overload;
{ Return an array of available codepages }
procedure Codepages(out List : TStringArray); overload;

{ Converts a string of ASCII characters to UTF-8. Returns false if
the requested codepage is not available. }
function CodePageToUTF8(Codepage : integer; const S : TAsciiString;
  out U : TUTF8String; Options : TConvertOpts = []) : boolean; overload;

{ Converts a UTF-8 string to ASCII text. Returns false if the requested
codepage is not available. The converted text along with statistics are
returned in a TResultsCP record. }
function UTF8ToCodepage(Codepage : integer; const U : TUTF8String;
  out R : TResultsCP) : boolean;

implementation

{$I maps\map_437.inc}
{$I maps\map_850.inc}
{$I maps\map_857.inc}
{$I maps\map_858.inc}
{$I maps\map_863.inc}
{$I maps\map_866.inc}

{$I maps\map_uchk.inc}

type
  TCodepage = record
    CodePage : integer;
    Mapping : TCodepageRemapEntries;
    UTF8 : TMapTree;
  end;

var
  FCodePages : array of TCodepage;

function Codepages: String; overload;
var
  I : Integer;
begin
  Codepages:='';
  for I := Low(FCodepages) to High(FCodepages) do begin
    if I > Low(FCodepages) then
      Codepages:=Codepages+', ';
    Codepages:=Codepages+IntToStr(FCodepages[I].CodePage);
  end;
end;

procedure Codepages(out List: TStringArray); overload;
begin
  List:=Explode(Codepages,', ');
end;

function CodePageIndex(Codepage : Integer) :integer;
var
  I : integer;
begin
  CodePageIndex:=-1;
  for I := Low(FCodepages) to High(FCodepages) do
    if Codepage=FCodepages[I].CodePage then begin
      CodePageIndex := I;
      Break;
    end;
end;

function CodePageToUTF8(Codepage : integer; const S : TAsciiString; out U : TUTF8String; Options : TConvertOpts = []) : boolean;
var
  P, I, C, V: Integer;
  X : TUTF8CodePoint;
begin
  CodePageToUTF8:=False;
  U:='';
  P := CodePageIndex(CodePage);
  if P = -1 then Exit;
  for I := 1 to Length(S) do begin
    V := Byte(S[I]);
    C := FCodepages[P].Mapping[V];
    case Char(V) of
      CR   : if not (cvCR  in Options) then C:=-1;
      LF   : if not (cvLF  in Options) then C:=-1;
      TAB  : if not (cvTAB in Options) then C:=-1;
    else
      if (V < 32) and (not (cvCtrlChars in Options)) then C:=-1;
    end;
    if C = -1 then
      U:=U+Char(V)
    else begin
      if not ValueToCodePoint(C,X) then Exit;
      U:=U+X;
    end;
  end;
  CodePageToUTF8:=True;
end;

function UTF8ToCodepage(Codepage: integer; const U: TUTF8String; out
  R: TResultsCP): boolean;
var
  P : Integer;
  W : TMapString;
  I : integer;
  L : integer;
  N : TMapNode;
begin
  UTF8ToCodepage:=False;
  R.Ascii:='';
  R.Count:=0;
  R.Unicode:=0;
  R.Match:=0;
  R.Other:=0;
  R.Errors:=0;
  P:=CodePageIndex(CodePage);
  if P = -1 then Exit;
  I := 1;
  while I <= Length(U) do begin
    W:=Copy(U,I, 4);
    L := CodePointLength(W);
    Inc(R.Count);
    if L < 1 then begin
      L := 1;
      Inc(R.Errors);
      N := Nil;
    end else begin
      if L > 1 then Inc(R.Unicode);
      if L = 1 then { Only 1 char then is not a Unicode character }
        N := Nil
      else
        N:=FCodePages[P].UTF8.Search(W, True); { no combo UTF-8 chars }
      if Assigned(N) then
        Inc(R.Match)
      else
        Inc(R.Other);
    end;
    if Assigned(N) then begin
      R.Ascii:=R.Ascii + N.Data;
    end else begin
      R.Ascii := R.Ascii + Copy(U,I,L);
    end;
    Inc(I, L);
  end;
  UTF8ToCodepage:=True;
end;


{ Unit initialization routines }

procedure AddCodepage(Codepage : integer; const Mapping : TCodepageRemapEntries);
var
  I, V, E : integer;
  T, C, D : TMapString;
begin
  SetLength(FCodepages, Length(FCodepages) + 1);
  FCodepages[High(FCodepages)].Codepage := Codepage;
  FCodepages[High(FCodepages)].Mapping := Mapping;
  FCodepages[High(FCodepages)].UTF8 := TMapTree.Create;
  for I := Low(UTF8toASCIIRemapList) to High(UTF8toASCIIRemapList) do begin
    T := UTF8toASCIIRemapList[I].Data;
    while T <> '' do begin
      D := PopDelim(T, '/');
      C := PopDelim(D, ',');
      Val(C, V, E);
      if (E <> 0) or (V <> Codepage) then Continue;
      Val(D, V, E);
      if E <> 0 then Continue;
      D := Char(V);
      if IntsToCodePoint(UTF8toASCIIRemapList[I].Value, C) then
        FCodepages[High(FCodepages)].UTF8.Add(C, D);
    end;
  end;
end;

procedure Initialize;
begin
  FCodepages:=[];
  AddCodePage(437, CP437toUTF8RemapList);
  AddCodePage(850, CP850toUTF8RemapList);
  AddCodePage(858, CP858toUTF8RemapList);
  AddCodePage(857, CP857toUTF8RemapList);
  AddCodePage(863, CP863toUTF8RemapList);
  AddCodePage(866, CP866toUTF8RemapList);
end;

procedure Finalize;
var
  I : integer;
begin
  for I := High(FCodePages) downto Low(FCodepages) do
    FreeAndNil(FCodepages[I].UTF8);
  SetLength(FCodepages, 0);
end;

initialization
  Initialize;
finalization
  Finalize;
end.
