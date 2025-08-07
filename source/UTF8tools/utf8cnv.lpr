{
   Copyright (c) 2025 Jerome Shidel
   BSD-3-Clause license
}

program utf8cnv;

{$I defines.pp}

uses
  {$IFDEF USES_CWSTRING} cwstring, {$ENDIF}
  {$IFDEF UNIX} cthreads, {$ENDIF}
  Classes, SysUtils, CustApp, Common;
  // Unicode, Codepage, Remapper, Diction;

type

  TOnSubString = function (S : String) : String of object;

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
    FOnSubStr : TOnSubString;
    FDetectLangOnly : boolean;

  protected
    procedure DoRun; override;
    function OnToHTML (S : String) : string;
    function OnToUTF8 (S : String) : string;
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
    function Load(Filename : String; out Data : AnsiString) : boolean; virtual;
    function Save(Filename : String; const Data : AnsiString) : boolean; virtual;
    function OutName(FileName, Suffix : String) : String; virtual;
    procedure NotSupported(Filename : String);
    procedure FileText(Filename : String); virtual;
    procedure FileHTML(Filename : String); virtual;
    procedure UTF8toTEXT(Filename : String); virtual;
    procedure TEXTtoUTF8(Filename : String); virtual;
    function HTMLtextOnly(const S : String) : String; virtual;
    function PromoteUTF8(var Data : String) : boolean; virtual;
    procedure DetectText(Filename:String); virtual;
    procedure DetectHTML(Filename:String); virtual;
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
            'd' : FDetectLangOnly:=True;
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
  FOptions:=FOptions + [cvPunctuation];
  if FControlCodes then
    FOptions:=FOptions + [cvCtrlChars];
  if FHTMLCodes then
    FOptions:=FOptions + [cvHTMLCodes];
  if not Terminated then MakeFileList;
  if not Terminated then ProcessFiles;
  // stop program loop
  Terminate;
end;

function TUTF8Convert.OnToHTML(S: String): string;
begin
  OnToHTML:=UTF8toHTML(S, FOptions);
end;

function TUTF8Convert.OnToUTF8(S: String): string;
begin
  OnToUTF8:=HTMLtoUTF8(S, FOptions);
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
  WriteLn('  ', CommandSwitch, 'd', TAB2, 'detect language only');
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
  WriteLn('Available code pages: ', Codepages);
  WriteLn(Length(DetectableLanguages), ' known languages.');
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
    WriteLn(TAB, 'notice: conversion not required');
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
      '.bmp', '.jpg', '.jpeg', '.jp2', '.gif', '.png', '.tiff',
      '.wav', '.mp3', '.aac', '.voc', '.cda', '.avi', '.mp2', '.mp4',
      '.mov', '.qt', '.ac3', '.aa', '.flac', '.img', '.iso', '.xz',
      '.zip', '.bzip', '.bz2', '.gzip', '.gz', '.tar', '.tgz', '.tbz'
      : begin
        WriteLn('Ignore: ', FFiles[I]);
        Continue;
      end
    else
      WriteLn('Processing: ', FFiles[I]);
    end;
    case LowerCase(ExtractFileExt(FFiles[I])) of
      '.xml', '.xhtm', '.xhtml' : begin
        NotSupported(FFiles[I]);
        if FForced then begin
          if FHTMLasText then
            FileText(FFiles[I])
          else
            FileHTML(FFiles[I]);
        end;
      end;
      '.css' : begin
        NotSupported(FFiles[I]);
        if FForced then FileText(FFiles[I]);
      end;
      '.html', '.htm' : begin
        if FHTMLasText then
          FileText(FFiles[I])
        else
          FileHTML(FFiles[I]);
      end;
    else
      FileText(FFiles[I]);
    end;
    if Terminated then Break;
  end;
end;

function TUTF8Convert.Load(Filename: String; out Data: AnsiString): boolean;
begin
  if not LoadFile(Filename, Data) then begin
    WriteLn('error loading file: ', Filename);
    Load:=False;
    Terminate(1);
  end else
    Load:=True
end;

function TUTF8Convert.Save(Filename: String; const Data: AnsiString): boolean;
begin
  if not SaveFile(Filename, Data) then begin
    WriteLn('error saving file: ', Filename);
    Save:=False;
    Terminate(1);
  end else begin
    WriteLn(TAB, 'file saved');
    Save:=True
  end;
end;

function TUTF8Convert.OutName(FileName, Suffix: String): String;
begin
  OutName := Filename;
   if not FNoSuffix then begin
     OutName:=Copy(OutName, 1, Length(OutName) - Length(ExtractFileExt(OutName))) +
     SUFFIXDELIM + Suffix + ExtractFileExt(OutName);
   end;
   if FOutPath <> '' then
     OutName := FOutPath + ExtractFilename(OutName);
   WriteLn(TAB, 'output file: ', OutName);
end;

procedure TUTF8Convert.NotSupported(Filename: String);
begin
  WriteLn(TAB, 'File type "', UpperCase(Copy(ExtractFileExt(Filename),1)),
  '" are not supported.');
end;

procedure TUTF8Convert.FileText(Filename: String);
begin
  if FDetectLangOnly then begin
    DetectText(Filename);
    Exit;
  end;
  if FToUTF8 then
    TEXTtoUTF8(Filename)
  else
    UTF8toTEXT(Filename)
end;

procedure TUTF8Convert.FileHTML(Filename: String);
var
  S, D: TUTF8String;
  L, P : integer;
  T : String;
begin
  if FDetectLangOnly then begin
    DetectHTML(Filename);
    Exit;
  end;
  if not Load(FileName,S) then Exit;
  if not PromoteUTF8(S) then Exit;
  if FToUTF8 then
    FOnSubStr:=@OnToUTF8
  else
    FOnSubStr:=@OnToHTML;

  { update the HTML text }
  D:='';
  T:= '';
  L := 1;
  repeat
    case Copy(S, L, 1) of
      CR, LF : begin
        P := L;
        D:=D+Copy(S,P,1);
      end
    else
      P := Pos('<', S, L);
      if P = 0 then P := Length(S);
      if Lowercase(T) <> '<style>' then
        D:=D+FOnSubStr(Copy(S, L, P - L))
      else
        D:=D+Copy(S, L, P - L);
      L:=P + 1;
      P := Pos('>', S, L);
      if P = 0 then P := Length(S);
      T:=Copy(S, L-1, P - L +2);
      D:=D+T;
    end;
    L:=P + 1;
  until P >= Length(S);

  // Exit;
  { compare and save }
  if not NeedSaved(S, D) then Exit;
  Filename:=OutName(FileName, 'new');
  if FileExists(Filename) then
    if DontOverwrite then Exit;
  if FReportOnly then Exit;
  Save(Filename, D);
end;

procedure TUTF8Convert.UTF8toTEXT(Filename: String);
var
  U : TUTF8String;
  R : TArrayResultsCP;
  D, P, I, C : integer;
  Q : Integer;
  L : String;
begin
  if not Load(Filename, U) then Exit;
  D := UTF8toCodepage(U, R);
  if D = -1 then begin
    if R[0].Errors = 0 then begin
      WriteLn(TAB, 'conversion not required.');
      if Not FSaveAnyway then Exit;
    end else begin
      WriteLn(TAB, 'not UTF-8 encoded.');
      if Not FForced then Exit;
    end;
  end;
  L:=DetectLanguage(U);
  C:=LanguageCodepage(L);
  P := CodepageIndex(C);
  if (L = '') or (C=-1) then begin
    WriteLn(TAB, 'unable to determine language');
  end else begin
    WriteLn(TAB, 'appears that the text is ', L);
    WriteLn(TAB, 'should convert to codepage ', C);
  end;
  if P = -1 then P:=D;
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
  Filename:=OutName(FileName, IntToStr(R[P].Codepage));
  if FileExists(Filename) then
    if DontOverwrite then Exit;
  if not NeedSaved(U, R[P].Ascii) then Exit;
  if FReportOnly then Exit;
  Save(Filename, R[P].Ascii);
end;

procedure TUTF8Convert.TEXTtoUTF8(Filename: String);
var
  A : TAsciiString;
  C : TIntArray;
  U : array of TUTF8String;
  I : Integer;
  P, TP : Integer;
  L, TL : String;
  W, TW : integer;
  R : TResultsCP;
begin
  U:=[];
  if not Load(Filename, A) then Exit;
  Codepages(C);
  UTF8toCodepage(437,A,R);
  { test if already UTF-8 encoded }
  if (R.Unicode > 0) and (R.Errors = 0) then begin
    WriteLn(TAB, 'warning: already encoded as UTF-8');
    if (not FForced) and FSaveAnyway then begin
      Filename:=OutName(FileName, 'utf8');
      if FileExists(Filename) then
        if DontOverwrite then Exit;
      Save(Filename, A);
      Exit;
    end;
    if (not FForced) then begin
      WontSave;
      Exit;
    end;
  end;
  { test if requires encoding }
  if (R.Unicode = 0) and (R.Errors = 0) then begin
     WriteLn(TAB, 'warning: UTF-8 encoding is not needed');
     if (not FForced) and FSaveAnyway then begin
       Filename:=OutName(FileName, 'utf8');
       if FileExists(Filename) then
         if DontOverwrite then Exit;
       Save(Filename, A);
       Exit;
     end;
     if (not FForced) then begin
       WontSave;
       Exit;
     end;
   end;
  { Encode with each codepage to detect language }
  W := 0;
  L := '';
  P := -1;
  TP := -1;
  SetLength(U, Length(C));
  for I := 0 to Length(C) - 1 do begin
    CodepageToUTF8(C[I], A, U[I], FOptions);
    if C[I] = FCodePage then TP := I;
    DetectLanguage(C[I], U[I], TL, TW);
    if TW > W then begin
      L:=TL;
      W:=TW;
      P:=I;
    end;
  end;
  if P = -1 then begin
    WriteLn(TAB, 'warning: unable to detect codepage');
    P := 0;
  end
  else begin
    WriteLn(TAB, 'appears that the text is ', L);
    WriteLn(TAB, 'should convert from codepage ', C[P]);
  end;

  if (TP <> -1) and (TP <> P) then begin
    P := TP;
    WriteLn(TAB, 'warning: convert from codepage ', C[P]);
  end;
  if not NeedSaved(A, U[P]) then Exit;
  Filename:=OutName(FileName, 'utf8');
  if FileExists(Filename) then
    if DontOverwrite then Exit;
  if FReportOnly then Exit;
  Save(Filename, U[P]);
end;

function TUTF8Convert.HTMLtextOnly(const S : String): String;
var
  L, P : integer;
  T : String;
begin
  // WriteLn(Remapper.Levels);
  HTMLtextOnly:='';
  T:= '';
  L := 1;
  repeat
    case Copy(S, L, 1) of
      CR, LF : begin
        P := L;
        HTMLtextOnly:=HTMLtextOnly+Copy(S,P,1);
      end
    else
      P := Pos('<', S, L);
      if P = 0 then P := Length(S);
      if Lowercase(T) <> '<style>' then
        HTMLtextOnly:=HTMLtextOnly+Copy(S, L, P - L);
      L:=P + 1;
      P := Pos('>', S, L);
      if P = 0 then P := Length(S);
      T:=Copy(S, L-1, P - L +2);
    end;
    L:=P + 1;
  until P >= Length(S);
end;

function TUTF8Convert.PromoteUTF8(var Data: String) : boolean;
var
  R : TResultsCP;
  C : TIntArray;
  U : array of TUTF8String;
  I : Integer;
  P, TP : Integer;
  L, TL : String;
  W, TW : integer;
  H, X : TUTF8String;
begin
  U:=[];
  PromoteUTF8:=True;
  Codepages(C);
  H:=HtmltextOnly(Data);
  UTF8toCodepage(437,H,R);
  { Same as either Unicode or ASCII }
  if (R.Unicode = 0) and (R.Errors = 0) then Exit;
  { Already Unicode }
  if (R.Unicode > 0) and (R.Errors = 0) then Exit;
  { Convert to UTF-8 }
  PromoteUTF8:=False;
  WriteLn(TAB, 'warning: not UTF-8 encoded');
  W := 0;
  L := '';
  P := -1;
  TP := -1;
  SetLength(U, Length(C));
  for I := 0 to Length(C) - 1 do begin
    CodepageToUTF8(C[I], Data, U[I], FOptions);
    CodepageToUTF8(C[I], H, X, FOptions);
    if C[I] = FCodePage then TP := I;
    DetectLanguage(C[I], X, TL, TW);
    if TW > W then begin
      L:=TL;
      W:=TW;
      P:=I;
    end;
  end;
  if P = -1 then begin
    WriteLn(TAB, 'warning: unable to detect codepage');
    P := 0;
  end
  else begin
    WriteLn(TAB, 'appears that the text is ', L);
    WriteLn(TAB, 'should convert from codepage ', C[P]);
  end;

  if (TP <> -1) and (TP <> P) then begin
    P := TP;
    WriteLn(TAB, 'warning: convert from codepage ', C[P]);
  end;

  { Return Data as UTF-8 that best matched a codepage }
  Data:=U[P];
  PromoteUTF8:=True;
end;

procedure TUTF8Convert.DetectText(Filename: String);
begin

end;

procedure TUTF8Convert.DetectHTML(Filename: String);
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

