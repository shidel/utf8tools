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
  Classes, SysUtils, CustApp, Common, Unicode, Codepage, Remapper;

type

  { TUTF8toHTML }

  TUTF8toHTML = class(TCustomApplication)
  protected
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHelp; virtual;
  end;

{ TUTF8toHTML }

procedure TUTF8toHTML.DoRun;
begin
  // stop program loop
  Terminate;
end;

constructor TUTF8toHTML.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException:=True;
end;

destructor TUTF8toHTML.Destroy;
begin
  inherited Destroy;
end;

procedure TUTF8toHTML.WriteHelp;
begin
  { add your help code here }
  writeln('Usage: ', ExeName, ' ', CommandSwitch, 'h');
end;

var
  Application: TUTF8toHTML;

{$R *.res}

begin
  Application:=TUTF8toHTML.Create(nil);
  Application.Title:='UTF8 to HTML Converter';
  Application.Run;
  Application.Free;
end.

