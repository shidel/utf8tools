program utf8lwdb;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes, SysUtils, CustApp, Common, Unicode, Codepage;

type

  { TMyApplication }

  TMyApplication = class(TCustomApplication)
  protected
    FLang : String;
    FName : String;
    FWords : TStringList;
    FCount : TStringList;
    FCodepage : integer;
    FNotEnglish : boolean;
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
  end;

{ TMyApplication }

procedure TMyApplication.DoRun;
begin
  if ParamCount < 3 then
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
  FWords := TStringList.Create;
  FCount:= TStringList.Create;
end;

destructor TMyApplication.Destroy;
begin
  FCount.Free;
  FWords.Free;
  inherited Destroy;
end;

procedure TMyApplication.WriteHelp;
begin
  { add your help code here }
  writeln('Usage: ', ExeName, ' Codepage LangName files...');
end;



procedure TMyApplication.Main;
var
  I : integer;
  E : Integer;
  D : AnsiString;
begin
  Val(ParamStr(1), FCodePage, E);
  if E <> 0 then begin
     WriteLn('Invalid codepage');
     Exit;
  end;
  if CodePageKnown(FCodepage) = false then begin
    WriteLn('unsupported codepage');
    Exit;
  end;
  FLang:=ParamStr(2);
  FNotEnglish := LowerCase(FLang) <> 'english';
  FName := 'words/' + FLang + '.lng';
  if not FileExists(FName) then begin
    WriteLn('file not found: ', FName);
    Terminate(1);
    Exit;
  end;
  if not LoadWords then Exit;
  for I := 3 to ParamCount do begin
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
  S := 'const' + LF + '  ' + FLang + 'LanguageWords : TStringArray = (' + LF;
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
  SaveFile('words/' + FLang + '.inc', S);
  T.Free;
  SaveWords:=True;
end;

function TMyApplication.ScanWords(const Data : String): boolean;
const
  B : String=SPACE+'`1234567890-=[]\;,./~!@#$%^&*()_+{}:<>?"' + QUOTE+CR+LF+TAB;
var
  P, L, M : integer;
  S, U : String;
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
    CodePageToUTF8(FCodePage, S, U);
    if FCodePage <> 437 then begin
       if (S=U) then Continue;
    end else
    if Lowercase(S) <> S then Continue
    else
    if L < 4 then Continue;
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

var
  Application: TMyApplication;
begin
  Application:=TMyApplication.Create(nil);
  Application.Title:='My Application';
  Application.Run;
  Application.Free;
end.

