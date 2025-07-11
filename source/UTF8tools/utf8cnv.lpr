{
   Copyright (c) 2025 Jerome Shidel
   BSD-3-Clause license
}

program utf8cnv;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes, SysUtils, CustApp, Common, Unicode, Codepage, Remapper;

type

  { TUTF8Convert }

  TUTF8Convert = class(TCustomApplication)
  protected
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHelp; virtual;
  end;

{ TUTF8Convert }

procedure TUTF8Convert.DoRun;
var
  I : integer;
  S : TAsciiString;
  U : TUTF8String;
begin
  if ParamCOunt = 0 then WriteHelp;
  for I := 1 to ParamCount do begin
    if not LoadFile(ParamStr(I), S) then begin
      WriteLn(ParamStr(I), ' load failed.');
      Continue;
    end;
    if not CodepageToUTF8(437, S, U) then begin
      WriteLn(ParamStr(I), ' conversion failed.');
      Continue;
    end;
    if S = U then begin
      WriteLn(ParamStr(I), ' unchanged, conversion not required.');
      Continue;
    end;
    if not SaveFile(ParamStr(I) + '.utf8', U) then begin
      WriteLn(ParamStr(I), '.utf8 save failed.');
      Continue;
    end;
    WriteLn('save ', ParamStr(I), '.utf8');
  end;
  // stop program loop
  Terminate;
end;

constructor TUTF8Convert.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException:=True;
end;

destructor TUTF8Convert.Destroy;
begin
  inherited Destroy;
end;

procedure TUTF8Convert.WriteHelp;
begin
  WriteLn('Usage: ', 'utf8cnv', ' ', CommandSwitch, 'h');
  WriteLn;
  WriteLn('Available code pages: ', Codepages);
end;

var
  Application: TUTF8Convert;

{$R *.res}

begin
  Application:=TUTF8Convert.Create(nil);
  Application.Title:='UTF8 Converter';
  Application.Run;
  Application.Free;
end.

