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

function CodepageList : String;

procedure Initialize;
procedure Finalize;

implementation

uses SysUtils, Unicode;

{$I maps\map_437.inc}
{$I maps\map_850.inc}
{$I maps\map_857.inc}
{$I maps\map_858.inc}
{$I maps\map_863.inc}
{$I maps\map_866.inc}

{$I maps\map_uchk.inc}

type
  TCodepage = record
    CodePage:integer;
    Mapping :TCodepageRemapEntries
  end;

var
  FCodePages : array of TCodepage;

function CodepageList: String;
var
  I : Integer;
begin
  CodepageList:='';
  for I := Low(FCodepages) to High(FCodepages) do begin
    if I > Low(FCodepages) then
      CodepageList:=CodepageList+', ';
    CodepageList:=CodepageList+IntToStr(FCodepages[I].CodePage);
  end;
end;

{ Unit initialization routines }
var
  OldExitProc : pointer;

procedure AddCodepage(Codepage : integer; const Mapping : TCodepageRemapEntries);
begin
  SetLength(FCodepages, Length(FCodepages) + 1);
  FCodepages[High(FCodepages)].Codepage := Codepage;
  FCodepages[High(FCodepages)].Mapping := Mapping;
end;

procedure Initialize;
begin
  if Assigned(OldExitProc) then exit;
  FCodepages:=[];
  OldExitProc := ExitProc;
  ExitProc := @Finalize;
  AddCodePage(437, CP437toUTF8RemapList);
  AddCodePage(850, CP850toUTF8RemapList);
  AddCodePage(858, CP858toUTF8RemapList);
  AddCodePage(857, CP857toUTF8RemapList);
  AddCodePage(863, CP863toUTF8RemapList);
  AddCodePage(866, CP866toUTF8RemapList);
end;

procedure Finalize;
begin
  if not Assigned(OldExitProc) then exit;
  ExitProc := OldExitProc;
  OldExitProc := nil;
end;

begin
   OldExitProc := nil;
   Initialize;
end.
