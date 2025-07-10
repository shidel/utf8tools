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

var
  OldExitProc : pointer;

procedure Initialize;
begin
  OldExitProc := ExitProc;
  ExitProc := @Finalize;
end;

procedure Finalize;
begin
  ExitProc := OldExitProc;
end;

begin
   Initialize;
end.
