
// TO-DO: Remove words that exist in multiple languages.

program utf8lwdb;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes, SysUtils, CustApp, Common, Unicode, Codepage;

const
  WordPath = 'words/';

type

  { TMyApplication }

  TMyApplication = class(TCustomApplication)
  protected
    FLang : String;
    FName : String;
    FExclude : TStringList;
    FWords : TStringList;
    FCount : TStringList;
    FCodepage : integer;
    FNotEnglish : boolean;
    FP : integer;
    FAll : boolean;
    FNoValidate : boolean;
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHelp; virtual;
    procedure Main;
    function LoadWords : boolean;
    function SaveWords : boolean;
    function ScanWords(const Data : String) : boolean;
    procedure PlusWord(S : String);
    procedure PruneWords;
    procedure CompareWords;
  end;

{ TMyApplication }

procedure TMyApplication.DoRun;
begin
  FP := 1;
  if ParamStr(FP) = '-a' then begin
     FAll:=True;
     Inc(FP);
  end;
  if ParamStr(FP) = '-n' then begin
     FNoValidate:=True;
     Inc(FP);
  end;
  if ParamStr(FP) = '-p' then
     PruneWords
  else
  if ParamCount < FP + 2 then
    WriteHelp
  else
    Main;

  // stop program loop
  Terminate;
end;

constructor TMyApplication.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException:=True;
  FExclude:=TStringList.Create;
  FExclude.Duplicates:=dupIgnore;
  FExclude.Sorted:=True;
  FWords := TStringList.Create;
  FCount:= TStringList.Create;
  FExclude.LoadFromFile(WordPath + 'Exclude.lst', true);
end;

destructor TMyApplication.Destroy;
begin
  FExclude.SaveToFile(WordPath + 'Exclude.lst', true);
  FCount.Free;
  FWords.Free;
  FExclude.Free;
  inherited Destroy;
end;

procedure TMyApplication.WriteHelp;
begin
  { add your help code here }
  writeln('Usage: ', 'utf8lwdb',  ' [options] codepage LangName files...');
  writeln;
  writeln(' -a    Include words that do not require unicode characters');
  writeln(' -n    Do not validate codepage or unicode characters.');
  writeLn(' -p    Prune cross language words');
  Terminate;
end;

procedure TMyApplication.Main;
var
  I : integer;
  E : Integer;
  D : AnsiString;
begin
  Val(ParamStr(FP), FCodePage, E);
  if E <> 0 then begin
     WriteLn('Invalid codepage');
     Exit;
  end;
  if CodePageKnown(FCodepage) = false then begin
    WriteLn('unsupported codepage');
    if not FNoValidate then Exit;
  end;
  Inc(FP);
  FLang:=ParamStr(FP);
  FNotEnglish := LowerCase(FLang) <> 'english';
  FName := WordPath + FLang + '.lng';
  if not FileExists(FName) then begin
    WriteLn('file not found: ', FName);
    Terminate(1);
    Exit;
  end;
  Inc(FP);
  if not LoadWords then Exit;
  for I := FP to ParamCount do begin
      WriteLn(ParamStr(I));
      if not LoadFile(ParamStr(I), D) then begin
        WriteLn('could not load file.');
        Exit;
      end;
      if not ScanWords(D) then Exit;
  end;
  if not SaveWords then Exit;
end;

function TMyApplication.LoadWords: boolean;
var
  T : TStringList;
  I: integer;
  W, C : String;
begin
  FWords.Clear;
  FCount.Clear;
  LoadWords:=False;
  T := TStringList.Create;
  T.LoadFromFile(FName);
  for I := 0 to T.Count - 1 do begin
    W:= T[I];
    C := Trim(PopDelim(W, ','));
    W := Trim(W);
    if (C='') or (W='') then continue;
    FWords.Add(W);
    FCount.Add(C);
  end;
  T.Free;
  LoadWords:=True;
end;

function TMyApplication.SaveWords: boolean;
var
  T : TStringList;
  I : integer;
  S, W, L : String;
begin
  SaveWords:=False;
  T := TStringList.Create;
  for I := 0 to FWords.Count - 1 do
    T.Add(ZeroPad(FCount[I], 12) + ',' + FWords[I]);
  T.Sort;
  S := '';
  for I := T.Count - 1 downto 0 do
    S := S + T[I] + LF;
  SaveFile(FName, S);
  S := 'const' + LF + '  ' + StringReplace(FLang, '-', '', [rfReplaceAll]) +
    'LanguageWords : TStringArray = (' + LF;
  L := '    ';
  for I := T.Count - 1 downto 0 do begin
    W:=T[I];
    PopDelim(W, ',');
    W:=QUOTE + W + QUOTE;
    if Length(W) + Length(L) > 78 then begin
      S := S + TrimRight(L) + LF;
      L := '    ';
    end;
    L := L + W;
    if I > 0 then L := L + ', '
  end;
  S := S + TrimRight(L) + LF + '  );' + LF;
  SaveFile(WordPath + FLang + '.inc', S);
  T.Free;
  SaveWords:=True;
end;

function TMyApplication.ScanWords(const Data : String): boolean;
const
  B : String=SPACE+'`1234567890-=[]\;,./~!@#$%^&*()_+{}:<>?"' + QUOTE+CR+LF+TAB;
var
  P, L, M : integer;
  S, U : String;
  R : TResultsCP;
begin
  ScanWords:=True;
  P := 0;
  M := Length(Data);
  While P < M do begin
    Inc(P);
    While (P < M) and (Pos(Data[P], B) > 0) do Inc(P);
    L:=1;
    While (P+L < M) and (Pos(Data[P+L], B) < 1) do
      Inc(L);
    if (L < 3) then Continue;
    S := Copy(Data, P, L);
    Inc(P,L);
    if (FNoValidate) then begin
      if L < 4 then Continue;
      U:=S;
    end else begin
      UTF8toCodepage(FCodePage, S, R);
      if (R.Errors = 0) and (R.Unicode > 0) then begin
        U:=S;
        S:=R.ASCII;
      end else
        CodePageToUTF8(FCodePage, S, U);
      if FCodePage <> 437 then begin
         if (S=U) and (not FAll) then Continue;
      end else
      if Lowercase(S) <> S then Continue
      else
      if L < 4 then Continue;
    end;
    WriteLn(U);
    PlusWord(U);
  end;
end;

procedure TMyApplication.PlusWord(S: String);
var
  I : Integer;
  V : UInt64;
  E : integer;
begin
  I := FWords.IndexOf(S);
  if I = -1 then begin
     FWords.Add(S);
     FCount.Add(ZeroPad(1, 12));
  end else begin
    Val(FCount[I], V, E);
    if E <> 0 then begin
       Raise Exception.Create('number conversion error');
       exit;
    end;
    if V < $ffffffff then begin
       Inc(V);
       FCount[I]:=ZeroPad(V,12);
    end;
  end;
end;

procedure TMyApplication.PruneWords;
var
  S : TSearchRec;
  R : integer;
  I, X : integer;
begin
  R := FindFirst(WordPath + '*.lng', faAnyFile, S);
  While R = 0 do begin
    WriteLn(S.Name);
    FLang:=ChangeFileExt(S.Name,'');
    FName:=WordPath + S.Name;
    if not LoadWords then Exit;
    for I := 0 to FExclude.Count - 1 do begin
      X:=FWords.IndexOf(FExclude[I]);
      if X < 0 then Continue;
      FWords.Delete(X);
      FCount.Delete(X);
      WriteLn(TAB, TAB, FExclude[I]);
    end;
    CompareWords;
    if not SaveWords then Exit;
    R := FindNext(S);
  end;
  FindClose(S);
end;

procedure TMyApplication.CompareWords;
var
  S : TSearchRec;
  R : integer;
  N : String;
  O : TStringList;
  I, X : integer;
  W : String;
begin
  R := FindFirst(WordPath + '*.lng', faAnyFile, S);
  While R = 0 do begin
    N:=WordPath + S.Name;
    R := FindNext(S);
    if N = FName then Continue;
    WriteLn(TAB, ExtractFileName(N));
    O := TStringList.Create;
    O.LoadFromFile(N,True);
    for I := 0 to O.Count - 1 do begin
      W:=O[I];
      PopDelim(W, ',');
      X := FWords.IndexOf(W);
      if X < 0 then Continue;
      FExclude.Add(W);
      FWords.Delete(X);
      FCount.Delete(X);
      WriteLn(TAB,TAB,W);
    end;
    O.Free;
  end;
  FindClose(S);
end;

var
  Application: TMyApplication;
begin
  Application:=TMyApplication.Create(nil);
  Application.Title:='My Application';
  Application.Run;
  Application.Free;
end.

