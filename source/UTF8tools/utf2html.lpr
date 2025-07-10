{
   Copyright (c) 2025 Jerome Shidel
   BSD-3-Clause license
}

program utf2html;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes, SysUtils, CustApp,
  Common;

type

  { TUTF8Tool }

  TUTF8Tool = class(TCustomApplication)
  protected
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHelp; virtual;
  end;

{ TUTF8Tool }

procedure TUTF8Tool.DoRun;
var
  ErrorMsg: String;
begin
  // quick check parameters
  ErrorMsg:=CheckOptions('h', 'help');
  if ErrorMsg<>'' then begin
    ShowException(Exception.Create(ErrorMsg));
    Terminate;
    Exit;
  end;

  // parse parameters
  if HasOption('h', 'help') then begin
    WriteHelp;
    Terminate;
    Exit;
  end;



  // stop program loop
  Terminate;
end;

constructor TUTF8Tool.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException:=True;
end;

destructor TUTF8Tool.Destroy;
begin
  inherited Destroy;
end;

procedure TUTF8Tool.WriteHelp;
begin
  { add your help code here }
  writeln('Usage: ', ExeName, ' -h');
end;

var
  Application: TUTF8Tool;

{$R *.res}

begin
  Application:=TUTF8Tool.Create(nil);
  Application.Title:='UTF8 Tool';
  Application.Run;
  Application.Free;
end.

