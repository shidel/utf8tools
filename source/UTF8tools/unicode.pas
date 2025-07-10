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

procedure Initialize;
procedure Finalize;

implementation

uses Common;

{$I maps\map_uchk.inc}

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
