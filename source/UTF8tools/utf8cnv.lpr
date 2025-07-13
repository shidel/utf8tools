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
    FHTMLCodes: boolean;
    FHTMLasText: boolean;
    FControlCodes: boolean;
    FCodepage: integer;
    FToUTF8: boolean;
    FNoSuffix: boolean;
    FForced: boolean;
  protected
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteBanner; virtual;
    procedure WriteHelp; virtual;
    procedure WontSave(M : String = ''); virtual;
    function DontOverwrite : boolean; virtual;
    function NeedSaved(const A, B : String) : boolean; virtual;
    procedure MakeFileList; virtual;
    procedure ProcessFiles; virtual;
    procedure FileText(Filename : String); virtual;
    procedure FileHTML(Filename : String); virtual;
  end;

{ TUTF8Convert }

procedure TUTF8Convert.DoRun;
var
  I : integer;
  E : integer;
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
            't' : FReportOnly:=True;
            'x' : FHTMLCodes:=True;
            'e' : FHTMLCodes:=False;
            'u' : FToUTF8:=True;
            'f' : FForced:=True;
            'n' : FNoSuffix:=True;
            'k' : FControlCodes:=True;
            'j' : FHTMLasText:=True;
            'c' : begin
              if I >= ParamCount then begin
                WriteLn('code page ID not specified');
                Terminate(1);
              end;
              Inc(I);
              if FCodePage <> -1 then begin
                WriteLn('code page ID already specified as ', FCodePage);
                Terminate(1);
              end else begin
                Val(ParamStr(I), FCodepage, E);
                if E <> 0 then begin
                  WriteLn('invalid code page ID: "', ParamStr(I), '"');
                  Terminate(1);
                end else if not CodePageKnown(FCodePage) then begin
                  WriteLn('code page ', FCodePage, ' is not supported');
                  WriteLn('available code pages: ', Codepages);
                  Terminate(1);
                end;
              end;
            end;
            'o' : begin
              if I >= ParamCount then begin
                WriteLn('output path not specified');
                Terminate(1);
              end;
              Inc(I);
              FOutPath := IncludeTrailingPathDelimiter(ParamStr(I));
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
  FCodePage:=-1;
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
  WriteLn('  ', CommandSwitch, 't', TAB2, 'test, report only');
  WriteLn('  ', CommandSwitch, 's', TAB2, 'save even if not modified');
  WriteLn('  ', CommandSwitch, 'w', TAB2, 'overwrite existing files');
  WriteLn('  ', CommandSwitch, 'n', TAB2, 'do not append conversion suffix');
  WriteLn('  ', CommandSwitch, 'o path', TAB, 'designate output path');
  WriteLn;
  WriteLn('  ', CommandSwitch, 'x', TAB2, 'HTML, prefer value codes');
  WriteLn('  ', CommandSwitch, 'e', TAB2, 'HTML, prefer entity names (default)');
  WriteLn;
  WriteLn('  ', CommandSwitch, 'u', TAB2, 'convert to UTF-8 instead of from');
  WriteLn;
  WriteLn('  ', CommandSwitch, 'c id', TAB2, 'specify a specific code page');
  WriteLn;
  WriteLn('The followng options are generally not recommended:');
  WriteLn;
  WriteLn('  ', CommandSwitch, 'f', TAB2, 'force conversion');
  WriteLn('  ', CommandSwitch, 'k', TAB2, 'convert control codes');
  WriteLn('  ', CommandSwitch, 'j', TAB2, 'HTML as plain text');
  WriteLn;
  WriteLn('Available code pages: ', Codepages);
  Terminate;
end;

procedure TUTF8Convert.WontSave(M : String = '');
begin
  if M <> '' then M := M + SPACE;
  WriteLn(TAB, 'warning: ', M, 'file will not be saved');
end;

function TUTF8Convert.DontOverwrite: boolean;
begin
  if FOverwrite then
    WriteLn(TAB,'warning: will overwrite existing file')
  else
    WontSave('already exists.');
  DontOverwrite:=not FOverwrite;
end;

function TUTF8Convert.NeedSaved(const A, B: String): boolean;
begin
  NeedSaved:=True;
  if A = B then begin
    WriteLn(TAB, 'notice: no conversion required');
    NeedSaved:=FSaveAnyway;
    if FSaveAnyway then
      WriteLn(TAB, 'notice: will save copy anyway');
  end;
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
    case LowerCase(ExtractFileExt(FFiles[I])) of
      '.html', '.htm' : FileHTML(FFiles[I]);
    else
      FileText(FFiles[I]);
    end;
    if Terminated then Break;
  end;
end;

procedure TUTF8Convert.FileText(Filename: String);
var
  U : TUTF8String;
  R : TArrayResultsCP;
  D, P, I : integer;
  Q : Integer;
  O : String;
begin
  WriteLn('Processing: ', Filename);
  if FToUTF8 then begin

  end else begin
    if not LoadFile(FileName, U) then begin
      WriteLn('error loading file: ', Filename);
      Terminate(1);
      Exit;
    end;
    D := UTF8toCodepage(U, R);
    P := D;
    if D = -1 then begin
      if R[0].Errors = 0 then begin
        WriteLn(TAB, 'conversion not required.');
        if Not FSaveAnyway then Exit;
      end else begin
        WriteLn(TAB, 'not UTF-8 encoded.');
        if Not FForced then Exit;
      end;
    end;
    if P = -1 then P:=0;
    if FReportOnly then begin
      WriteLn(TAB,'Codepage UTF-8 compatibility:');
      for I := 0 to Length(R) - 1 do begin
        Q:=Percent(R[I].Match, R[I].Unicode);
        Write(TAB2, R[I].Codepage, ', ', Q, '% match');
        if D = I then Write(' (detected)') else
        if P = I then Write(' (default)');
        if R[I].Codepage = FCodepage then Write(' (override)');
        WriteLn;
      end;
    end;
    for I := 0 to Length(R) - 1 do
      if R[I].Codepage = FCodepage then
        P:=I;
    Q:=Percent(R[P].Match, R[P].Unicode);
    if Q = 0 then
      WriteLn(TAB, 'warning: not compatible with codepage ', R[P].Codepage)
    else if Q <> 100 then
      WriteLn(TAB, 'warning: only ', Q, '% match with codepage ', R[P].Codepage);
    if (Q <> 100) and (not FForced) then begin
      WontSave;
      Exit;
    end;
    O := FileName;
    if not FNoSuffix then
      O := O + SUFFIXDELIM + IntToStr(R[P].Codepage);
    if FOutPath <> '' then
      O := FOutPath + ExtractFilename(O);
    WriteLn(TAB, 'output file: ', O);
    if FileExists(O) then
      if DontOverwrite then Exit;
    if not NeedSaved(U, R[P].Ascii) then Exit;
    if FReportOnly then Exit;

  end;
end;

procedure TUTF8Convert.FileHTML(Filename: String);
begin

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

