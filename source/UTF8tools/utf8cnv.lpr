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
  private
    FFiles: TStringList;
    FOutPath: String;
    FOverwrite: boolean;
    FReportOnly: boolean;
    FSaveAnyway: boolean;
  protected
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteBanner; virtual;
    procedure WriteHelp; virtual;
    procedure MakeFileList; virtual;
    procedure ProcessFiles; virtual;
  end;

{ TUTF8Convert }

procedure TUTF8Convert.DoRun;
var
  I : integer;
  O : String;
begin
  if ParamCount = 0 then
    WriteHelp
  else begin
    I := 0;
    While (I < ParamCount) and (not Terminated) do begin
      Inc(I);
      O := ParamStr(I);
      if Copy(O, 1, 1) = CommandSwitch then begin
        repeat
          Delete(O, 1, 1);
          case Copy(O, 1, 1) of
            '?', 'h' : WriteHelp;
            's' : FSaveAnyway:=True;
            'w' : FOverwrite:=True;
            'r' : FReportOnly:=True;
            'o' : begin
              if I >= ParamCount then begin
                WriteLn('output path not specified');
                Terminate(1);
              end;
              Inc(I);
              FOutPath := ParamStr(I);
            end
          else
            WriteLn('invalid command line option "', CommandSwitch, O, '"');
            Terminate(1);
          end;
        until (Length(O) = 1) or Terminated;
      end else begin {switch}
        FFiles.Add(O);
      end;
    end; {param}
  end;
  if not Terminated then MakeFileList;
  if not Terminated then ProcessFiles;
  // stop program loop
  Terminate;
end;

constructor TUTF8Convert.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException:=True;
  FFiles:=TStringList.Create;
end;

destructor TUTF8Convert.Destroy;
begin
  FFiles.Free;
  inherited Destroy;
end;

procedure TUTF8Convert.WriteBanner;
begin
  WriteLn(APP_FILEDESCRIPTION, ' (v', APP_VERSION, ')');
  WriteLn( APP_COMMENTS, ', Copyright ', APP_LEGALCOPYRIGHT);
  WriteLn;
end;

procedure TUTF8Convert.WriteHelp;
begin
  WriteLn('Usage: ', 'utf8cnv', ' [options] files...');
  WriteLn;
  WriteLn('  ', CommandSwitch, 'h', TAB2, 'display help text');
  WriteLn;
  WriteLn('  ', CommandSwitch, 'r', TAB2, 'report only');
  WriteLn('  ', CommandSwitch, 's', TAB2, 'save even if not modified');
  WriteLn('  ', CommandSwitch, 'w', TAB2, 'overwrite existing files');
  WriteLn('  ', CommandSwitch, 'o path', TAB, 'designate output path');

  WriteLn;
  WriteLn('Available code pages: ', Codepages);
  Terminate;
end;

procedure TUTF8Convert.MakeFileList;
var
  L : TStringList;
  S : TSearchRec;
  P : String;
  I, E : integer;
begin
  if (FFiles.Count = 0) then begin
     WriteLn('no files specified');
     Terminate(1);
     Exit;
   end;
  L :=TStringList.Create;
  L.Assign(FFiles);
  FFiles.Clear;
  FFiles.Sorted:=True;
  FFiles.Duplicates:=dupIgnore;
  for I := 0 to L.Count - 1 do begin
    P := ExtractFilePath(L[I]);
    E := FindFirst(L[I], faAnyFile, S);
    if E <> 0 then begin
      WriteLn('file not found: ', L[I]);
      Terminate(1);
      { Break; }
    end else while E = 0 do begin
      FFiles.Add(P + S.Name);
      E :=FindNext(S);
    end;
    FindClose(S);
  end;
  L.Free;
end;

procedure TUTF8Convert.ProcessFiles;
var
  I : integer;
begin
  for I := 0 to FFiles.Count - 1 do begin
    if Terminated then Break;
  end;
end;

var
  Application: TUTF8Convert;

{$R *.res}

begin
  Application:=TUTF8Convert.Create(nil);
  Application.Title:=APP_PRODUCTNAME;
  Application.Run;
  Application.Free;
end.

