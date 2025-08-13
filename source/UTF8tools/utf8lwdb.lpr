
// TO-DO: Remove words that exist in multiple languages.

program utf8lwdb;

{$I defines.pp}

uses
  {$IFDEF USES_CWSTRING} cwstring, {$ENDIF}
  {$IFDEF UNIX} cthreads, {$ENDIF}
  Classes, SysUtils, CustApp, Common;

{ DEFINE SAVE_SORTED} (* for saving inc files sorted in alpha or binary mode *)
{$DEFINE BIG_LANG}    (* generate a single large language detection include *)

const
  {$IFNDEF BIG_LANG}
  LangPath    = 'langs/';
  {$ENDIF}
  WordPath    = 'words/';
  Threshold   : integer = 5;
  DiscardOver : integer = 40;
  MaximumInc  : integer = 500000; // 200000;

  INDENT1 = SPACE2;
  INDENT2 = SPACE4;

type

  { TMyApplication }

  TMyApplication = class(TCustomApplication)
  protected
    FUbar     : Boolean;
    FExclude  : TMapTree;
    FLang     : String;
    FLangID   : Integer;
    FNames    : TStringArray;
    FModified : TBooleanArray;
    FWords    : TMapTree;
    FEnglish  : boolean;
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
    {$IFNDEF BIG_LANG}
    procedure SaveInclude(ID : integer; var Unique, Shared : TStringList);
    {$ELSE}
    function LanguageName(Value:RawByteString) : UnicodeString;
    function LangConst(Value : integer) : UnicodeString; overload;
    function LangConst(Value : RawByteString) : RawByteString; overload;
    function DOSCodePages(Value:RawByteString) : UnicodeString;
    procedure SaveLanguageInclude;
    procedure SaveDictionaryInclude;
    {$ENDIF}
    procedure ProcessFile(FileName:String);
    function TestWord(W : UnicodeString) : boolean;
    procedure AddWord(const W : UnicodeString; ID : integer; Count : integer = 1);
    procedure BinSort(var List : TStringList); overload;
    procedure BinSort(var List : TStringList; First, Last : Integer); overload;
    procedure OptiSort(var List : TStringList);
  end;

{ TMyApplication }

procedure TMyApplication.DoRun;
var
  I : Integer;
begin
  {$IFNDEF BIG_LANG}
  if not DirectoryExists(LangPath) then CreateDir(LangPath);
  {$ENDIF}
  if not DirectoryExists(WordPath) then CreateDir(WordPath);
  if (ParamCount =1) and (ParamStr(1) = '-p') then begin
    FLang:='';
    FUbar:=True;
    LoadData;
    SaveData;
  end else
  if (ParamCount =1) and (ParamStr(1) = '-r') then begin
    FLang:='';
    FUbar:=True;
    MaximumInc:=200000;
    LoadData;
    for I := 0 to Length(FModified) - 1 do
      FModified[I]:=True;
    SaveData;
  end else
  if ParamCount < 2 then
    WriteHelp
  else
    Main;

  Terminate;
end;

constructor TMyApplication.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException:=True;
  FUbar:=False;
  FNames:=[];
  FModified:=[];
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
  FEnglish:=LowerCase(ParamStr(1)) = 'english';
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
    SetLength(FModified, L.Count);
    for I := 0 to L.Count - 1 do begin
      FModified[I]:=False;
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
  BinSort(S);
  S.SaveToFile(WordPath + 'exclude.lst', true);
  WriteLn('saved: ', WordPath + 'exclude.lst');
  S.Free;
end;

procedure TMyApplication.SaveLangs;
var
  I : integer;
  {$IFNDEF BIG_LANG}
  S : TStringList;
  {$ENDIF}
begin
  for I := 0 to Length(FNames) - 1 do
    if FModified[I] or (I=FLangID) then SaveWords(I);
  {$IFNDEF BIG_LANG}
  S := TStringList.Create;
  for I := 0 to Length(FNames) - 1 do
    S.Add('{$I ' + LangPath + LowerCase(FNames[I]) + '.inc}');
  S.Sort;
  if FUbar then S.SaveToFile(LangPath + 'languages.pp');
  S.Free;
  {$ELSE}
  if FUbar then begin
    SaveLanguageInclude;
    SaveDictionaryInclude;
  end;
  {$ENDIF BIG_LANG}
end;

procedure TMyApplication.SaveWords(ID: integer);
var
  {$IFNDEF BIG_LANG} Unique, Shared, {$ENDIF}
  Both : TStringList;
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
        {$IFNDEF BIG_LANG}
        if Length(V) = 1 then
          Unique.Add(ZeroPad(StrToInt(C),10) + SPACE + Node.Key)
        else
          Shared.Add(ZeroPad(StrToInt(C),10) + SPACE + Node.Key);
        {$ENDIF}
        Break;
      end;
    end;
    AddNodeText(Node.Lesser);
    AddNodeText(Node.Greater);
  end;

begin
  D := IntToStr(ID);
  Both := TStringList.Create;
  {$IFNDEF BIG_LANG}
  Unique := TStringList.Create;
  Shared := TStringList.Create;
  {$ENDIF}
  AddNodeText(FWords.RootNode);
  OptiSort(Both);
  Both.SaveToFile(WordPath + FNames[ID] + '.dat', true);
  {$IFNDEF BIG_LANG}
  if FUbar then SaveInclude(ID, Unique, Shared);
  Shared.Free;
  Unique.Free;
  {$ENDIF}
  Both.Free;
  WriteLn('saved: ', FNames[ID]);
end;

function TMyApplication.LanguageName(Value: RawByteString): UnicodeString;
begin
  Value:=StringReplace(Value, HYPHEN, SPACE, [rfReplaceAll]);
  case Value of
    'Chinese Simplified'      : Value:='Chinese (Simplified)';
    'Chinese Traditional'     : Value:='Chinese (Traditional)';
    'Crimean Tartar Cyrillic' : Value:='Crimean Tartar (Cyrillic)';
    'Crimean Tartar Latin'    : Value:='Crimean Tartar (Latin)';
    'French Canadian'         : Value:='French (Canadian)';
    'Inuktut Latin'           : Value:='Inuktut (Latin)';
    'Inuktut Syllabics'       : Value:='Inuktut (Syllabics)';
    'Kurdish Kurmanji'        : Value:='Kurdish (Kurmanji)';
    'Kurdish Sorani'          : Value:='Kurdish (Sorani)';
    'Malay Jawi'              : Value:='Malay (Jawi)';
    'Meiteilon Manipuri'      : Value:='Meiteilon (Manipuri)';
    'Myanmar Burmese'         : Value:='Myanmar (Burmese)';
    'Nahuati Eastern Huasteca': Value:='Nahuati (Eastern Huasteca)';
    'Ndebele South'           : Value:='Ndebele (South)';
    'Nepalbhasa Newari'       : Value:='Nepalbhasa (Newari)';
    'Odia Oriya'              : Value:='Odia (Oriya)';
    'Portuguese Brazil'       : Value:='Portuguese (Brazil)';
    'Portuguese Portugal'     : Value:='Portuguese (Portugal)';
    'Punjabi Gurmukhi'        : Value:='Punjabi (Gurmukhi)';
    'Punjabi Shahmukhi'       : Value:='Punjabi (Shahmukhi)';
    'Qeqchi'                  : Value:='Q’eqchi’';
    'Sami North'              : Value:='Sami (North)';
    'Santali Latin'           : Value:='Santali (Latin)';
    'Santali Ol Chiki'        : Value:='Santali (Ol Chiki)';
    'Tamazight Tifinagh'      : Value:='Tamazight (Tifinagh)';
  end;
  LanguageName:=UnicodeString(Value);

end;

function TMyApplication.LangConst(Value: integer): UnicodeString;
begin
   LangConst:=UnicodeString('lng' +
        StringReplace(RawByteString(FNames[Value]), HYPHEN, '', [rfReplaceAll]));
end;

function TMyApplication.LangConst(Value: RawByteString): RawByteString;
var
  N, E : Integer;
begin
  Val(Value,N,E);
  if E <> 0 then raise Exception.Create('integer conversion error');
  LangConst:=RawByteString(LangConst(N));
end;

function TMyApplication.DOSCodePages(Value: RawByteString): UnicodeString;
begin
  Value:=StringReplace(Value, HYPHEN, SPACE, [rfReplaceAll]);
  case Value of
    'Arabic'                  : Value:='864,720';
    'Chinese Simplified'      : Value:='936';
    'Chinese Traditional'     : Value:='950';
    'Corsican'                : Value:='-1,852'; // Not specified for DOS
    'Croatian'                : Value:='852'; // Serbo-Croation
    'Czech'                   : Value:='852';
    'Danish'                  : Value:='865';
    'Dutch'                   : Value:='850';
    'English'                 : Value:='437';
    'Esperanto'               : Value:='-1,850,858'; // Maybe
    'Faroese'                 : Value:='850';
    'French Canadian'         : Value:='863';
    'French'                  : Value:='850,858';
    'Georgian'                : Value:='60853,59829';
    'German'                  : Value:='850,858';
    'Greek'                   : Value:='737,869';
    'Haitian Creole'          : Value:='850';
    'Hawaiian'                : Value:='850';
    'Hebrew'                  : Value:='1255,862'; // Partial 862
    'Hindi'                   : Value:='1137';
    'Irish'                   : Value:='850';
    'Italian'                 : Value:='1252,850';
    'Japanese'                : Value:='932';
    'Korean'                  : Value:='949';
    'Latin'                   : Value:='852';
    'Mongolian'               : Value:='-1,866,852';  // Maybe 866, 852
    'Norwegian'               : Value:='865,850';
    'Polish'                  : Value:='852';
    'Portuguese Brazil'       : Value:='860';
    'Portuguese Portugal'     : Value:='860';
    'Romanian'                : Value:='852';
    'Russian'                 : Value:='866';
    'Samoan'                  : Value:='850';
    'Sanskrit'                : Value:='-1'; // No DOS Support
    'Serbian'                 : Value:='852'; // Maybe 866
    'Slovak'                  : Value:='852,895';
    'Spanish'                 : Value:='220,850,858'; // 220 for Spanish and Catalan
    'Sudanese'                : Value:='864'; // Partial 864
    'Swahili'                 : Value:='850';
    'Swedish'                 : Value:='865,850,1106';
    'Thai'                    : Value:='874';
    'Tibetan'                 : Value:='-1'; // No DOS Support
    'Turkish'                 : Value:='857';
    'Ukrainian'               : Value:='866';
    'Venetian'                : Value:='850';
    'Vietnamese'              : Value:='1258';
    'Welsh'                   : Value:='850';
    'Yiddish'                 : Value:='-1,862';   // Probably same as Hebrew
  else
    Value:='';
  end;
  DOSCodePages:=UnicodeString(Value);
end;

procedure TMyApplication.SaveLanguageInclude;
var
  S : UnicodeString;
  I : Integer;

begin
  S:=
  'type' + LF +
  INDENT1 + 'TLanguageInfo = record' + LF +
  INDENT2 + 'Caption : UnicodeString;' + LF +
  INDENT2 + 'CodePages : TIntegerArray;' + LF +
  INDENT1 + 'end;' + LF +
  INDENT1 + 'TLanguageInfoArray = array of TLanguageInfo;' + LF +
  LF +
  'const' + LF;
  for I := 0 to Length(FNames) - 1 do
    S:=S+INDENT1 + RightPad(LangConst(I),30) +
      '=' + SPACE + UnicodeString(IntToStr(I)) +';'+ LF;
  S:=S+LF;
  S:=S+ INDENT1 + 'LanguageNames : TLanguageInfoArray = ( ' + LF;
  for I := 0 to Length(FNames) - 1 do begin
    S:=S + INDENT2 + '(Caption:' +
      RightPad(QUOTE + LanguageName(FNames[I]) + QUOTE + ';', 30) +
      'CodePages:(' + DOSCodePages(FNames[I]) + '))';
    if I < Length(FNames) - 1 then S:=S+COMMA;
    S:=S+LF;
  end;
  S:=S+INDENT1+');'+LF;
  SaveFile('language.pp', S);
  WriteLn('saved: ', 'language.pp');
end;

procedure TMyApplication.SaveDictionaryInclude;
var
  L : TStringList;
  N, P : Integer;

  procedure AddNodeText(Node:TMapNode);
  var
    U : UnicodeString;
    S, K : String;
    V : TStringArray;
    I : integer;
  begin
    if not Assigned(Node) then Exit;
    AddNodeText(Node.Lesser);
    K:=Trim(Node.Key);
    if (Length(K)>0) and (Length(K) <= DiscardOver) then begin
      V:=Explode(Node.Value);
      for I := 0 to Length(V) - 1 do begin
        S:=V[I];
        V[I]:=LangConst(PopDelim(S));
      end;
      S:=Implode(V);
      U:=UnicodeString(K);
      N:=FWords.Count;
      N:=Percent(L.Count, N) div 10;
      if N <> P then begin
        L.Add('{$HINT Dictionary ' + IntToStr(N * 10) + '% compiled}');
        P:=N;
      end;
      L.Add(INDENT2 + '(Caption:' + QUOTE + K + QUOTE + ';' + SPACE +
        'Codepages:(' + S + ')),'
        // + TAB + '// ' + IntToStr(Integer(U[1]))
        );
    end;
    AddNodeText(Node.Greater);
  end;

begin
  P:=0;
  L := TStringList.Create;
  L.Add('const' + LF + INDENT1 + 'DictionaryWords : TLanguageInfoArray = ( ');
  AddNodeText(FWords.RootNode);
  L[L.Count-1]:=Copy(L[L.Count-1], 1, Length(L[L.Count-1]) - 1);
  L.Add(INDENT1 + ');' + LF);
  L.Add('{$WARNING');
  L.Add('This may take a very long time to assemble. Possible 30 to 60 minutes. ');
  L.Add('Or even longer. ');
  L.Add('You may think FPC has frozen. But, it just takes an extremely long time. ');
  L.Add('Be patient. Go eat lunch or something. ');
  L.Add('}');
  L.SaveToFile('dictionary.pp', true);
  L.Free;
  WriteLn('saved: ', 'dictionary.pp');
end;

{$IFNDEF BIG_LANG}
procedure TMyApplication.SaveInclude(ID: integer;var Unique, Shared : TStringList);
var
  S : UnicodeString;

  procedure MakeArray(var List : TStringList; Name : UnicodeString);
  var
    W, L  : UnicodeString;
    I, C : Integer;
  begin
    OptiSort(List);
    for I := 0 to List.Count - 1 do begin
      W:=UnicodeString(List[I]);
      PopDelim(W);
      List[I]:=RawByteString(W);
    end;
    {$IFDEF SAVE_SORTED}
    List.Sort;
    {$ENDIF}
    S := S + INDNET1 +
      UnicodeString(StringReplace(FNames[ID], '-', '', [rfReplaceAll])) +
      Name + 'Words' + ' : TStringArray = ( ' + LF;
    L := INDENT2;
    C:=0;
    for I := 0 to List.Count - 1 do begin
      W:=UnicodeString(List[I]);
      if Length(W) > DiscardOver then Continue;
      Inc(C);
      W:=QUOTE + W + QUOTE;
      if Length(W) + Length(L) > 78 then begin
        S := S + TrimRight(L) + LF;
        L := INDENT2;
      end;
      L := L + W;
      if I < List.Count - 1 then L := L + ', '
    end;
    S := S + TrimRight(L) + LF + INDENT1 + '); // ' +
      UnicodeString(IntToStr(C)) + ' words' + LF;
  end;

begin
  S:='const' + LF;
  MakeArray(Unique, 'Unique');
  S:= S + LF;
  MakeArray(Shared, 'Shared');
  SaveFile(LangPath + Lowercase(FNames[ID]) + '.inc', S);
end;
{$ENDIF}

procedure TMyApplication.ProcessFile(FileName: String);
var
  P, L : integer;
  S : UnicodeString;
  W : UnicodeString;
begin
  if not LoadFile(FileName, S) then begin
    WriteLn(INDENT1, 'error loading file: ', FileName);
    Exit;
  end;
  WriteLn(INDENT1, 'processing: ', FileName);
  L := Length(S);
  P := 0;
  while P < L do begin
    W:=LowerCase(NextWord(S, P));
    if Length(W) < 2 then Continue;
    if not TestWord(W) then Continue;
    if FExclude.Find(W) = nil then
      AddWord(W, FLangID);
  end;
end;

function TMyApplication.TestWord(W: UnicodeString): boolean;
var
  R : RawByteString;
  I : integer;
begin
  TestWord:=False;
  if FEnglish then begin
    if isUnicode(W) then Exit;
    R:=RawByteString(W);
    for I := 1 to Length(R) do
      if (R[I] < 'a') or (R[I] > 'z') then Exit;
  end;
  TestWord:=True;
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
      for I := 0 to Length(V) - 1 do
        FModified[StrToInt(PopDelim(V[I]))]:=True;
      FWords.Delete(N);
      FExclude.Add(W);
    end else
      N.Value:=Implode(V);
  end;
end;

procedure TMyApplication.BinSort(var List: TStringList);
begin
  if List.Count = 0 then Exit;
  List.Sort;
  BinSort(List, 0, List.Count - 1);
end;

procedure TMyApplication.BinSort(var List: TStringList; First, Last: Integer);
var
  X : TStringList;
  I : Integer;

  procedure SortRange(F, L : integer);
  var
    M : integer;
  begin
    M:=F + (L-F) div 2;
    X.Add(List[M]);
    if M < L then SortRange(M + 1, L);
    if M > F then SortRange(F, M - 1);
  end;

begin
  if First=Last then Exit;
  // WriteLn(First, ':', Last, ' ', List[First], ' - ', List[Last]);
  X := TStringList.Create;
  SortRange(First, Last);
  for I := 0 to X.Count - 1 do
    List[First + I] := X[I];
  X.Free;
end;

procedure TMyApplication.OptiSort(var List: TStringList);
var
  F, I : integer;
  X, V : RawByteString;
begin
  if List.Count = 0 then Exit;
  List.Sort;
  for I := 0 to (List.Count - 1) div 2 do
    Exchange(List, I, List.Count - I - 1);
  F:=0;
  I:=0;
  while (I < List.Count) and (F < MaximumInc) do begin
    Inc(F, Length(List[I]));
    Inc(I);
  end;
  While I < List.Count do
    List.Delete(I);
  X:=List[0];
  X:=PopDelim(X);
  F:=0;
  for I := 1 to List.Count - 1 do begin
    V:=List[I];
    V:=PopDelim(V);
    if V <> X then begin
      BinSort(List, F, I-1);
      F:=I;
      X:=V;
    end;
  end;
  BinSort(List, F, List.Count - 1);
end;

var
  Application: TMyApplication;
begin
  Application:=TMyApplication.Create(nil);
  Application.Title:='My Application';
  Application.Run;
  Application.Free;
end.

