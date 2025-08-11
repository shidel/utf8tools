{
   Copyright (c) 2025 Jerome Shidel
   BSD-3-Clause license
}

program utf8cnv;

{$I defines.pp}

uses
  {$IFDEF USES_CWSTRING} cwstring, {$ENDIF}
  {$IFDEF UNIX} cthreads, {$ENDIF}
  Classes, SysUtils, CustApp,
  Common, Diction;
  // Unicode, Codepage, Remapper;

type

  // TOnSubString = function (S : String) : String of object;

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
    FOptions:TConvertOpts;

  protected
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteBanner; virtual;
    procedure WriteHelp; virtual;
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
                end;
(*                else
                if not CodePageKnown(FCodePage) then begin
                  WriteLn('code page ', FCodePage, ' is not supported');
                  WriteLn('available code pages: ', Codepages);
                  Terminate(1);
                end; *)
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
  FOptions:=FOptions + [cvPunctuation];
  if FControlCodes then
    FOptions:=FOptions + [cvCtrlChars];
  if FHTMLCodes then
    FOptions:=FOptions + [cvHTMLCodes];
  // if not Terminated then MakeFileList;
  // if not Terminated then ProcessFiles;
  Terminate;
end;

constructor TUTF8Convert.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException:=True;
  FFiles:=TStringList.Create;
  FOptions:=[];
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
  WriteLn('  ', CommandSwitch, 'u', TAB2, 'convert to UTF-8 instead of from');
  WriteLn;
  WriteLn('Options which are usually not needed or recommended:');
  WriteLn;
  WriteLn('  ', CommandSwitch, 'c id', TAB2, 'use a specific code page');
  WriteLn;
  WriteLn('  ', CommandSwitch, 'f', TAB2, 'force conversion regardless');
  WriteLn('  ', CommandSwitch, 'k', TAB2, 'convert control codes');
  WriteLn('  ', CommandSwitch, 'x', TAB2, 'HTML, use values instead of entities');
  WriteLn('  ', CommandSwitch, 'j', TAB2, 'HTML, as plain text');
  WriteLn;
  WriteLn('Available code pages: ' {,  Codepages });
//  WriteLn(Length(DetectableLanguages), ' known languages.');
  Terminate;
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

