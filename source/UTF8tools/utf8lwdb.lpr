
// TO-DO: Remove words that exist in multiple languages.

program utf8lwdb;

{$I defines.pp}

uses
  {$IFDEF USES_CWSTRING} cwstring, {$ENDIF}
  {$IFDEF UNIX} cthreads, {$ENDIF}
  Classes, SysUtils, CustApp, Common;

const
  LangPath  = 'langs/';
  WordPath  = 'words/';
  Threshold = 5;

type

  { TMyApplication }

  TMyApplication = class(TCustomApplication)
  protected
    FUbar    : Boolean;
    FExclude : TMapTree;
    FLang    : String;
    FLangID  : Integer;
    FNames   : TStringArray;
    FWords   : TMapTree;
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHelp; virtual;
    procedure Main;
    procedure LoadData;
    procedure LoadExclude;
    procedure LoadLangs;
    procedure LoadWords(ID : integer);
    procedure SaveData;
    procedure SaveExclude;
    procedure SaveLangs;
    procedure SaveWords(ID : integer);
    procedure SaveInclude(ID : integer; var Unique, Shared : TStringList);
    procedure ProcessFile(FileName:String);
    procedure AddWord(const W : UnicodeString; ID : integer; Count : integer = 1);
  end;

{ TMyApplication }

procedure TMyApplication.DoRun;
begin
  if not DirectoryExists(LangPath) then CreateDir(LangPath);
  if not DirectoryExists(WordPath) then CreateDir(WordPath);
  if (ParamCount =1) and (ParamStr(1) = '-p') then begin
    FLang:='';
    FUbar:=True;
    LoadData;
    SaveData;
  end else
  if ParamCount < 2 then
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
  FUbar:=False;
  FNames:=[];
  FExclude:=TMapTree.Create;
  FWords:=TMapTree.Create;
end;

destructor TMyApplication.Destroy;
begin
  FExclude.Free;
  FWords.Free;
  SetLength(FNames, 0);
  inherited Destroy;
end;

procedure TMyApplication.WriteHelp;
begin
  { add your help code here }
  writeln('Usage: ', 'utf8lwdb',  '[-p] [LangName files...]');
  writeln;
  WriteLn('  -p  Prune duplicate words and export Pascal include files.');
  Terminate;
end;

procedure TMyApplication.Main;
var
  F : TStringList;
  I : Integer;
  J : Integer;
begin
  FLang:=ParamStr(1);
  LoadData;
  WriteLn('Language: ', FNames[FLangID]);
  for I := 2 to ParamCount do begin
    if not ScanDir(ParamStr(I), F) then begin
      WriteLn('File or directory not found: ', ParamStr(I));
      Continue;
    end;
    for J := 0 to F.Count - 1 do
      ProcessFile(ExtractFilePath(ParamStr(I)) + F[J]);
    F.Free;
  end;
  SaveData;
end;

procedure TMyApplication.LoadData;
begin
  LoadExclude;
  LoadLangs;
end;

procedure TMyApplication.LoadExclude;
var
  S : TStringList;
  I : integer;
begin
  if not FileExists(WordPath + 'exclude.lst') then Exit;
  S := TStringList.Create;
  S.LoadFromFile(WordPath + 'exclude.lst', true);
  For I := 0 to S.Count - 1 do
    if Trim(S[I]) <> '' then
      FExclude.Add(UnicodeString(S[I]));
  S.Free;
  WriteLn('loaded: ', WordPath + 'exclude.lst');
end;

procedure TMyApplication.LoadLangs;
var
  L : TStringList;
  I : Integer;
begin
  FLangID:=-1;
  if ScanDir(WordPath + '*.dat', L) then begin
    SetLength(FNames, L.Count);
    for I := 0 to L.Count - 1 do begin
      FNames[I]:=ChangeFileExt(L[I], '');
      if Lowercase(FNames[I]) = Lowercase(FLang) then
        FLangID:=I;
      if FUbar or (FLangID=I) then LoadWords(I);
    end;
    L.Free;
  end;
  if (FLangID=-1) and (not FUbar) then begin
    FLangID:=Length(FNames);
    SetLength(FNames, FLangID + 1);
    FNames[FLangID]:=FLang;
  end;
end;

procedure TMyApplication.LoadWords(ID: integer);
var
  S : TStringList;
  I : integer;
  K : UnicodeString;
  C : LongInt;
begin
  if not FileExists(WordPath + FNames[ID] + '.dat') then Exit;
  S := TStringList.Create;
  S.LoadFromFile(WordPath + FNames[ID] + '.dat', true);
  for I := 0 to S.Count - 1 do
    if Trim(S[I]) <> '' then begin
      K:=UnicodeString(S[I]);
      C:=StrToInt(RawByteString(PopDelim(K)));
      AddWord(K, ID, C);
    end;
  S.Free;
  WriteLn('loaded: ', WordPath + FNames[ID] + '.dat');
end;

procedure TMyApplication.SaveData;
begin
  SaveExclude;
  SaveLangs;
end;

procedure TMyApplication.SaveExclude;
var
  S : TStringList;

  procedure AddNodeText(Node:TMapNode);
  begin
    if not Assigned(Node) then Exit;
    S.Add(Node.Key);
    AddNodeText(Node.Lesser);
    AddNodeText(Node.Greater);
  end;

begin
  S := TStringList.Create;
  AddNodeText(FExclude.RootNode);
  S.SaveToFile(WordPath + 'exclude.lst', true);
  S.Free;
  WriteLn('saved: ', WordPath + 'exclude.lst');
end;

procedure TMyApplication.SaveLangs;
var
  I : integer;
  S : TStringList;
begin
  for I := 0 to Length(FNames) - 1 do begin
    if FUbar or (FLangID=I) then SaveWords(I);
  end;
  S := TStringList.Create;
  for I := 0 to Length(FNames) - 1 do
    S.Add('{$I ' + LangPath + LowerCase(FNames[I]) + '.inc}');
  S.Sort;
  if FUBar then S.SaveToFile(LangPath + 'languages.pp');
  S.Free;
end;

procedure TMyApplication.SaveWords(ID: integer);
var
  Both, Unique, Shared : TStringList;
  D : RawByteString;

  procedure AddNodeText(Node:TMapNode);
  var
    V : TRawByteStringArray;
    I : integer;
    L, C : RawByteString;
  begin
    if not Assigned(Node) then Exit;
    V:=Explode(Node.Value);
    for I := 0 to Length(V) - 1 do begin
      C:=V[I];
      L:=PopDelim(C);
      if L = D then begin
        Both.Add(ZeroPad(StrToInt(C),10) + SPACE + Node.Key);
        if Length(V) = 1 then
          Unique.Add(ZeroPad(StrToInt(C),10) + SPACE + Node.Key)
        else
          Shared.Add(ZeroPad(StrToInt(C),10) + SPACE + Node.Key);
        Break;
      end;
    end;
    AddNodeText(Node.Lesser);
    AddNodeText(Node.Greater);
  end;

begin
  D := IntToStr(ID);
  Both := TStringList.Create;
  Unique := TStringList.Create;
  Shared := TStringList.Create;
  AddNodeText(FWords.RootNode);
  Both.SaveToFile(WordPath + FNames[ID] + '.dat', true);
  if FUbar then SaveInclude(ID, Unique, Shared);
  Shared.Free;
  Unique.Free;
  Both.Free;
  WriteLn('saved: ', FNames[ID]);
end;

procedure TMyApplication.SaveInclude(ID: integer;var Unique, Shared : TStringList);
var
  S : UnicodeString;

  procedure MakeArray(var List : TStringList; Name : UnicodeString);
  var
    W, L  : UnicodeString;
    I : Integer;
  begin
    List.Sort;
    S := S + '  ' +
      UnicodeString(StringReplace(FNames[ID], '-', '', [rfReplaceAll])) +
      Name + 'Words' + ' : TStringArray = ( // ' +
      UnicodeString(IntToStr(List.Count)) + ' words' + LF;
    L := '    ';
    for I := List.Count - 1 downto 0 do begin
      W:=UnicodeString(List[I]);
      PopDelim(W);
      W:=QUOTE + W + QUOTE;
      if Length(W) + Length(L) > 78 then begin
        S := S + TrimRight(L) + LF;
        L := '    ';
      end;
      L := L + W;
      if I > 0 then L := L + ', '
    end;
    S := S + TrimRight(L) + LF + '  );' + LF;
  end;

begin
  S:='const' + LF;
  MakeArray(Unique, 'Unique');
  S:= S + LF;
  MakeArray(Shared, 'Shared');
  SaveFile(LangPath + Lowercase(FNames[ID]) + '.inc', S);
end;

procedure TMyApplication.ProcessFile(FileName: String);
var
  P, L : integer;
  S : UnicodeString;
  W : UnicodeString;
begin
  if not LoadFile(FileName, S) then begin
    WriteLn(TAB, 'error loading file: ', FileName);
    Exit;
  end;
  WriteLn(TAB, 'processing: ', FileName);
  L := Length(S);
  P := 0;
  while P < L do begin
    W:=LowerCase(NextWord(S, P));
    if FExclude.Find(W) = nil then
      AddWord(W, FLangID);
  end;
end;

procedure TMyApplication.AddWord(const W: UnicodeString; ID: integer;
  Count: integer);
var
  N : TMapNode;
  V : TRawByteStringArray;
  I : Integer;
  D, L, C : RawByteString;
  WC: LongInt;
begin
  N:=FWords.Find(W);
  if Not Assigned(N) then begin
    FWords.Add(RawByteString(W), IntToStr(ID) + SPACE + IntToStr(Count));
  end else begin
    V:=Explode(N.Value);
    D:=IntToStr(ID);
    for I := 0 to Length(V) - 1 do begin
      C:=V[I];
      L:=PopDelim(C);
      if L = D then begin
        WC:=StrToInt(C);
        if WC < MaxLongInt then begin
          if WC + Count < (MaxLongInt div 2) then
            WC := WC + Count
          else
            WC := MaxLongInt;
          V[I]:=L + SPACE + IntToStr(WC);
        end;
        ID:=-1;
        Break;
      end;
    end;
    if ID <> -1 then begin
      SetLength(V, Length(V)+1);
      V[Length(V)-1]:=IntToStr(ID) + SPACE + IntToStr(Count);
    end;
    if Length(V) > Threshold then begin
      FWords.Delete(N);
      FExclude.Add(W);
    end else
      N.Value:=Implode(V);
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

