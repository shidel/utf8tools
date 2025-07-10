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


procedure Initialize;
procedure Finalize;

implementation

uses Unicode;

{$I maps\map_437.inc}
{$I maps\map_850.inc}
{$I maps\map_857.inc}
{$I maps\map_858.inc}
{$I maps\map_863.inc}
{$I maps\map_866.inc}

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
