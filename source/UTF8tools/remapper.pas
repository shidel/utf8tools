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

procedure Initialize;
procedure Finalize;

implementation

uses Unicode;

{$I maps\map_html.inc}
{$I maps\map_utf8.inc}

{ Unit initialization routines }
var
  OldExitProc : pointer;

procedure Initialize;
begin
  if Assigned(OldExitProc) then exit;
  OldExitProc := ExitProc;
  ExitProc := @Finalize;
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
