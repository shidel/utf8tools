{
   Copyright (c) 2025 Jerome Shidel
   BSD-3-Clause license
}

unit Common;

{$I defines.pp}

interface

uses
  {$IFDEF USES_CWSTRING} cwstring, {$ENDIF}
  Classes, SysUtils;

{$I version.inc}

const
  {$if defined(TP70)}
  CommandSwitch : String = '/';
  DirWildCard : string = '*.*';
  {$elseif defined(windows)}
  CommandSwitch : String = '/';
  DirWildCard : string = '*';
  {$elseif defined(darwin)}
  CommandSwitch : String = '-';
  DirWildCard : string = '*';
  {$elseif defined(linux)}
  CommandSwitch : String = '-';
  DirWildCard : string = '*';
  {$else}
    {$ERROR unknown platform}
  {$endif}

  CR          = #$0d;
  LF          = #$0a;
  SPACE       = #$20;
  TAB         = #$09;
  VTAB        = #$0b;
  FORMFEED    = #$0c;
  AMPERSAND   = #$26;
  LESSTHAN    = #$3c;
  GREATERTHAN = #$3e;
  COLON       = #$3a;
  SEMICOLON   = #$3b;
  UNDERSCORE  = #$5f;
  PERIOD      = #$2e;
  QUOTE       = #$27;
  QUOTEDOUBLE = #$22;
  HYPHEN      = #$2d;
  COMMA       = #$2c;

  NOBREAKSPACE = #$0A;
  SOFTHYPHEN   = #$AD;

  TAB2        = TAB + TAB;

  SPACE2      = SPACE  + SPACE;
  SPACE4      = SPACE2 + SPACE2;
  SPACE6      = SPACE4 + SPACE2;
  SPACE8      = SPACE4 + SPACE4;

  SUFFIXDELIM = PERIOD; { UNDERSCORE; HYPHEN; }

  MaxInteger  = MaxLongInt;
  MaxInt64    = $7fffffffffffffff;

type

  TRawByteStringArray = array of RawByteString;
  TUnicodeArray = array of UnicodeString;
  TIntegerArray = array of Integer;
  TInt64Array = array of Int64;
  TBooleanArray = array of Boolean;

  TConvertOpts = set of (
    { All conversions from UTF-8 to/from ASCII }
    cvCR, cvLF, cvTAB,     { really never should be enabled }
    { Basic other cotrol codes for conversions }
    cvCtrlChars,           { mostly safe in ASCII, but are meant to represent
                           direct to display characters. Not standard output
                           behaviour/characters. }

    { for conversion to & from HTML}
    cvHTMLCodes, cvPunctuation,
    cvHTMLCtrl             { don't ever turn this on, coverts < > & chars }
    );



  { TMapNode }


  TMapData = class
  private
  protected
  public
  published
  end;

  TMapNode = class(TMapData)
  private
    FParent: TMapNode;
    FGreater: TMapNode;
    FLesser: TMapNode;
    FKey: RawByteString;
    FValue: RawByteString;
    FData: TMapData;
    procedure SetData(AValue: TMapData);
  protected
  public
    property Key : RawByteString read FKey;
    property Value : RawByteString read FValue write FValue;
    property Data : TMapData read FData write SetData;
    property Parent : TMapNode read FParent;
    property Lesser : TMapNode read FLesser;
    property Greater : TMapNode read FGreater;
    constructor Create;
    destructor Destroy; override;
  published
  end;

  { TMapTree }

  TMapTree = class(TMapData)
  private
    FCount: int64;
    FRootNode: TMapNode;
  protected
    procedure Attach(var Node : TMapNode);
    procedure Dettach(var Node : TMapNode);
  public
    property Count : int64 read FCount;
    property RootNode : TMapNode read FRootNode;
    function Add(Key :RawByteString; Value : RawByteString='') : TMapNode; overload;
    function Find(Key : RawByteString) : TMapNode; overload;
    function Search(Key : RawByteString; FirstMatch : boolean = false) : TMapNode; overload;
    function Add(Key :UnicodeString; Value : UnicodeString='') : TMapNode; overload;
    function Find(Key : UnicodeString) : TMapNode; overload;
    function Search(Key : UnicodeString; FirstMatch : boolean = false) : TMapNode; overload;
    procedure Delete(var Node : TMapNode); overload;
    procedure Clear;
    constructor Create;
    destructor Destroy; override;
  published
  end;

function Percent(Value, Total : integer) : Integer; overload;

procedure Exchange(var A, B : RawByteString); overload;
procedure Exchange(var A, B : UnicodeString); overload;
procedure Exchange(var A, B : AnsiString); overload;
procedure Exchange(var  List : TStringList; IndexA, IndexB : Integer); overload;
procedure Exchange(var A, B : Int64); overload;
procedure Exchange(var A, B : LongInt); overload;
procedure Exchange(var A, B : QWORD); overload;

function isUnicode(const AStr : UnicodeString) : boolean; overload;
function isUnicode(const AStr : RawByteString) : boolean; overload;

function PopDelim(var AStr : AnsiString; ADelim: AnsiString = SPACE): AnsiString; overload;
function PopDelim(var AStr : RawByteString; ADelim: RawByteString = SPACE): RawByteString; overload;
function PopDelim(var AStr : UnicodeString; ADelim: UnicodeString = SPACE): UnicodeString; overload;

function NextWord(const AStr : UnicodeString; var P : integer) : UnicodeString;
function NextWord(const AStr : RawByteString; var P : integer) : RawByteString;

procedure Explode(AStr : RawByteString; var AStrs : TStringList; ADelim : RawByteString = ','); overload;
procedure Explode(AStr : UnicodeString; var AStrs : TStringList; ADelim : UnicodeString = ','); overload;
function Explode(AStr : RawByteString; ADelim : RawByteString = ','):TRawByteStringArray; overload;
function Explode(AStr : UnicodeString; ADelim : UnicodeString = ','):TUnicodeArray; overload;

procedure Implode(AStrs : TStringList; out AStr : RawByteString; ADelim : RawByteString = ','); overload;
procedure Implode(AStrs : TStringList; out AStr : UnicodeString; ADelim : UnicodeString = ','); overload;
function Implode(AStrs : TRawByteStringArray; ADelim : RawByteString = ','):RawByteString; overload;
function Implode(AStrs : TUnicodeArray; ADelim : UnicodeString = ','):UnicodeString; overload;

function RightPad(Str : RawByteString; W : integer; Padding : RawByteString = SPACE; Clip : boolean = false) : RawByteString;
function RightPad(Str : UnicodeString; W : integer; Padding : UnicodeString = SPACE; Clip : boolean = false) : UnicodeString;
function LeftPad(Str : RawByteString; W : integer; Padding : RawByteString = SPACE; Clip : boolean = false) : RawByteString;
function LeftPad(Str : UnicodeString; W : integer; Padding : UnicodeString = SPACE; Clip : boolean = false) : UnicodeString;

function ZeroPad(I : LongInt; Width : integer) : RawByteString; overload;
function ZeroPad(I : LongInt; Width : integer) : UnicodeString; overload;
function ZeroPad(I : Int64; Width : integer) : RawByteString; overload;
function ZeroPad(I : Int64; Width : integer) : UnicodeString; overload;
function ZeroPad(I : QWORD; Width : integer) : RawByteString; overload;
function ZeroPad(I : QWORD; Width : integer) : UnicodeString; overload;

function SaveFile(AFileName: String; const AValue : RawByteString) : boolean; overload;
function SaveFile(AFileName: String; const AValue : UnicodeString) : boolean; overload;
function LoadFile(AFileName: String; out AValue : RawByteString) : boolean; overload;
function LoadFile(AFileName: String; out AValue : UnicodeString) : boolean; overload;

type
  TScanDirOpts = set of (sdSorted, sdRecurse, sdExcludeFiles, sdDirectories);

function ScanDir(const Path : RawByteString;
    out Files : TStringList; Opts : TScanDirOpts = []): Boolean; overload;
function ScanDir(const Path : RawByteString; Attr : LongInt;
    out Files : TStringList; Opts : TScanDirOpts = []): Boolean; overload;

function ScanDir(const Path : RawByteString;
    out Files : TStringArray; Opts : TScanDirOpts = []): Boolean; overload;
function ScanDir(const Path : RawByteString; Attr : LongInt;
    out Files : TStringArray; Opts : TScanDirOpts = []): Boolean; overload;


implementation

const
  WordBreakChars : UnicodeString =
    SPACE + CR + LF + TAB + QUOTE + VTAB + FORMFEED +
    '01234567890' +  '`!@#$%^&*()_+{}:"<>?`-=[]\;,./|~';
    // '！¡¿’«»，،、（）。“։ ”：「」—፣一™；؛·„… ﴾﴿';

{ TMapNode }

procedure TMapNode.SetData(AValue: TMapData);
begin
  if FData=AValue then Exit;
  FData:=AValue;
end;

constructor TMapNode.Create;
begin
  inherited Create;
  FGreater:=nil;
  FLesser:=nil;
  FParent:=nil;
  FData:=nil;
  FKey:='';
  FValue:='';
end;

destructor TMapNode.Destroy;
begin
  if Assigned(FData) then FreeAndNil(FData);
  if Assigned(FLesser) then FreeAndNil(FLesser);
  if Assigned(FGreater) then FreeAndNil(FGreater);
  inherited Destroy;
end;

{ TMapTree }

procedure TMapTree.Attach(var Node: TMapNode);
var
  This, Last : TMapNode;
begin
  Last:=Nil;
  This:=FRootNode;
  While Assigned(This) do begin
    if This.FKey = Node.FKey then begin
      // Key Exists in tree. It must be unique.
      FreeAndNil(Node);
      Exit;
    end;
    Last:=This;
    if Node.FKey < This.FKey then
      This:=This.FLesser
    else
      This:=This.FGreater;
  end;
  // Add to tree under Last
  if Not Assigned(Last) then
    FRootNode:=Node
  else if Node.Key < Last.FKey then
    Last.FLesser:=Node
  else
    Last.FGreater:=Node;
  Node.FParent:=Last;
end;

procedure TMapTree.Dettach(var Node: TMapNode);
begin
  if Assigned(Node.FParent) then begin
    if Node.FParent.FLesser=Node then
      Node.FParent.FLesser:=nil
    else
      Node.FParent.FGreater:=nil;
    Node.FParent:=nil;
  end;
  if Assigned(Node.FLesser) then begin
    Node.FLesser.FParent:= nil;
    Attach(Node.FLesser);
    Node.FLesser:=nil;
  end;
  if Assigned(Node.FGreater) then begin
    Node.FGreater.FParent:= nil;
    Attach(Node.FGreater);
    Node.FGreater:=nil;
  end;
end;

function TMapTree.Add(Key :RawByteString; Value : RawByteString=''): TMapNode;
var
  Node : TMapNode;
begin
  Add:=Nil;
  if Key='' then Exit;
  Node := TMapNode.Create;
  Node.FKey := Key;
  Node.FValue := Value;
  Attach(Node);
  Add:=Node;
  Inc(FCount);
end;

function TMapTree.Find(Key: RawByteString): TMapNode;
begin
  Find := FRootNode;
  While Assigned(Find) do begin
    if Key = Find.FKey then Break;
    if Key < Find.FKey then
      Find := Find.FLesser
    else
      Find := Find.FGreater;
  end;
end;

function TMapTree.Search(Key: RawByteString; FirstMatch : boolean = false): TMapNode;
var
  C : RawByteString;
  F : TMapNode;
begin
  Search := nil;
  F := FRootNode;
  While Assigned(F) do begin
    C := Copy(Key, 1, Length(F.FKey));
    if C = F.FKey then begin
      Search:=F;
      if (FirstMatch) or (Not Assigned(F.FGreater)) then
        Break;
    end;
    if C < F.FKey then
      F := F.FLesser
    else
      F := F.FGreater;
  end;
end;

function TMapTree.Add(Key: UnicodeString; Value: UnicodeString): TMapNode;
begin
  Add:=Add(RawByteString(Key), RawByteString(Value));
end;

function TMapTree.Find(Key: UnicodeString): TMapNode;
begin
  Find:=Find(RawByteString(Key));
end;

function TMapTree.Search(Key: UnicodeString; FirstMatch: boolean): TMapNode;
begin
  Search:=Search(RawByteString(Key), FirstMatch);
end;

procedure TMapTree.Delete(var Node: TMapNode);
begin
  if Not Assigned(Node) then Exit;
  Dec(FCount);
  Dettach(Node);
  FreeAndNil(Node);
end;

procedure TMapTree.Clear;
begin
  FCount:=0;
  if Assigned(FRootNode) then FreeAndNil(FRootNode);
end;

constructor TMapTree.Create;
begin
  inherited Create;
  FRootNode := nil;
  FCount:=0;
end;

destructor TMapTree.Destroy;
begin
  Clear;
  inherited Destroy;
end;

{ Unit functions and procedures }

function Percent(Value, Total: integer): Integer;
begin
  Percent:=0;
  if Total = 0 then Exit;
  Percent:=Value * 100 div Total;
  if (Percent = 100) and (Value<>Total) then Percent:=99;
  if (Percent = 0) and (Value<>0) then Percent:=1;
end;

procedure Exchange(var A, B: RawByteString);
var
  T:RawByteString;
begin
  T:=A;
  A:=B;
  B:=T;
end;

procedure Exchange(var A, B: UnicodeString);
var
  T:UnicodeString;
begin
  T:=A;
  A:=B;
  B:=T;
end;

procedure Exchange(var A, B: AnsiString);
var
  T:AnsiString;
begin
  T:=A;
  A:=B;
  B:=T;
end;

procedure Exchange(var List: TStringList; IndexA, IndexB: Integer);
var
  T : AnsiString;
begin
  T:=List[IndexA];
  List[IndexA]:=List[IndexB];
  List.Strings[IndexB]:=T;
end;

procedure Exchange(var A, B: Int64);
var
  T:Int64;
begin
  T:=A;
  A:=B;
  B:=T;
end;

procedure Exchange(var A, B: LongInt);
var
  T:LongInt;
begin
  T:=A;
  A:=B;
  B:=T;
end;

procedure Exchange(var A, B: QWORD);
var
  T:QWORD;
begin
  T:=A;
  A:=B;
  B:=T;
end;

function isUnicode(const AStr: UnicodeString): boolean;
begin
  isUnicode:=Length(AStr) <> Length(RawByteString(AStr));
end;

function isUnicode(const AStr: RawByteString): boolean;
begin
  isUnicode:=Length(AStr) <> Length(UnicodeString(AStr));
end;

function PopDelim(var AStr: AnsiString; ADelim: AnsiString): AnsiString;
var
  P : integer;
begin
  P := Pos(ADelim, AStr);
  if P <= 0 then P := Length(AStr) + 1;
  Result := Copy(AStr, 1, P - 1);
  Delete(AStr, 1, P - 1 + Length(ADelim));
end;

function PopDelim(var AStr: RawByteString; ADelim: RawByteString): RawByteString;
var
  P : integer;
begin
  P := Pos(ADelim, AStr);
  if P <= 0 then P := Length(AStr) + 1;
  Result := Copy(AStr, 1, P - 1);
  Delete(AStr, 1, P - 1 + Length(ADelim));
end;

function PopDelim(var AStr: UnicodeString; ADelim: UnicodeString
  ): UnicodeString;
var
  P : integer;
begin
  P := Pos(ADelim, AStr);
  if P <= 0 then P := Length(AStr) + 1;
  Result := Copy(AStr, 1, P - 1);
  Delete(AStr, 1, P - 1 + Length(ADelim));
end;

function NextWord(const AStr: UnicodeString; var P: integer): UnicodeString;
var
   L : integer;
begin
  L := 0;
  While (P+L<=Length(AStr)) and (Pos(AStr[P+L], WordBreakChars) < 1) do
    Inc(L);
  NextWord:=Copy(AStr,P, L);
  P:=P+L+1;
end;

function NextWord(const AStr: RawByteString; var P: integer): RawByteString;
var
   S : UnicodeString;
   L : integer;
begin
  S := UnicodeString(Copy(AStr, P));
  L := 0;
  While (L<=Length(S)) and (Pos(S[L], WordBreakChars) < 1) do
    Inc(L);
  NextWord:=RawByteString(Copy(S, 1, L));
  P:=P+Length(NextWord)+1;
end;

procedure Explode(AStr : RawByteString; var AStrs : TStringList; ADelim : RawByteString); overload;
var
  S : RawByteString;
begin
  While Length(AStr) > 0 do begin
    S := PopDelim(AStr, ADelim);
    AStrs.Add(S);
  end;
end;

procedure Explode(AStr: UnicodeString; var AStrs: TStringList;
  ADelim: UnicodeString);
var
  S : UnicodeString;
begin
  While Length(AStr) > 0 do begin
    S := PopDelim(AStr, ADelim);
    AStrs.Add(RawByteString(S));
  end;
end;

function Explode(AStr: RawByteString; ADelim: RawByteString
  ): TRawByteStringArray;
var
  I : Integer;
begin
  I := 0;
  Explode:=[];
  While Length(AStr) > 0 do begin
    if I = Length(Explode) then
      SetLength(Explode, Length(Explode) + 50);
    Explode[I] := PopDelim(AStr, ADelim);
    Inc(I);
  end;
  SetLength(Explode, I);
end;

function Explode(AStr: UnicodeString; ADelim: UnicodeString): TUnicodeArray;
var
  I : Integer;
begin
  I := 0;
  Explode:=[];
  While Length(AStr) > 0 do begin
    if I = Length(Explode) then
      SetLength(Explode, Length(Explode) + 50);
    Explode[I] := PopDelim(AStr, ADelim);
    Inc(I);
  end;
  SetLength(Explode, I);
end;

procedure Implode(AStrs: TStringList; out AStr: RawByteString;
  ADelim: RawByteString);
var
  I : Integer;
begin
  AStr:='';
  for I := 0 to AStrs.Count - 1 do begin
    if I > 0 then AStr:=AStr+ADelim;
    AStr:=AStr+AStrs[I];
  end;
end;

procedure Implode(AStrs: TStringList; out AStr: UnicodeString;
  ADelim: UnicodeString);
var
  I : Integer;
begin
  AStr:='';
  for I := 0 to AStrs.Count - 1 do begin
    if I > 0 then AStr:=AStr+ADelim;
    AStr:=AStr+UnicodeString(AStrs[I]);
  end;
end;

function Implode(AStrs: TRawByteStringArray; ADelim: RawByteString
  ): RawByteString;
var
  I : Integer;
begin
  Implode:='';
  for I := 0 to Length(AStrs) - 1 do begin
    if I > 0 then IMplode:=Implode+ADelim;
    Implode:=Implode+AStrs[I];
  end;
end;

function Implode(AStrs: TUnicodeArray; ADelim: UnicodeString
  ): UnicodeString;
var
  I : Integer;
begin
  Implode:='';
  for I := 0 to Length(AStrs) - 1 do begin
    if I > 0 then IMplode:=Implode+ADelim;
    Implode:=Implode+AStrs[I];
  end;
end;

function RightPad(Str: RawByteString; W: integer; Padding: RawByteString;
  Clip: boolean): RawByteString;
begin
  While Length(Str) < W do Str:=Str+Padding;
  if Clip then
    RightPad:=Copy(Str, 1, W)
  else
    RightPad:=Str;
end;

function RightPad(Str: UnicodeString; W: integer; Padding: UnicodeString;
  Clip: boolean): UnicodeString;
begin
  While Length(Str) < W do Str:=Str+Padding;
  if Clip then
    RightPad:=Copy(Str, 1, W)
  else
    RightPad:=Str;
end;

function LeftPad(Str: RawByteString; W: integer; Padding: RawByteString;
  Clip: boolean): RawByteString;
begin
  While Length(Str) < W do Str:=Padding + Str;
  if Clip then
    LeftPad:=Copy(Str, 1, W)
  else
    LeftPad:=Str;
end;

function LeftPad(Str: UnicodeString; W: integer; Padding: UnicodeString;
  Clip: boolean): UnicodeString;
begin
  While Length(Str) < W do Str:=Padding + Str;
  if Clip then
    LeftPad:=Copy(Str, 1, W)
  else
    LeftPad:=Str;
end;

function ZeroPad(I: LongInt; Width: integer): RawByteString;
begin
  ZeroPad := LeftPad(IntToStr(I), Width, '0');
end;

function ZeroPad(I: LongInt; Width: integer): UnicodeString;
begin
  ZeroPad := UnicodeString(ZeroPad(I, Width));
end;

function ZeroPad(I: Int64; Width: integer): RawByteString;
begin
  ZeroPad := LeftPad(IntToStr(I), Width, '0');
end;

function ZeroPad(I: Int64; Width: integer): UnicodeString;
begin
  ZeroPad := UnicodeString(ZeroPad(I, Width));
end;

function ZeroPad(I: QWORD; Width: integer): RawByteString;
begin
  ZeroPad := LeftPad(IntToStr(I), Width, '0');
end;

function ZeroPad(I: QWORD; Width: integer): UnicodeString;
begin
  ZeroPad := UnicodeString(ZeroPad(I, Width));
end;

{$PUSH}
{$I-}

function SaveFile(AFileName: String; const AValue : RawByteString) : boolean; overload;
var
   F : File;
   R, E : integer;
begin
  System.Assign(F, AFileName);
  Rewrite(F,1);
  R := IOResult;
  if (R = 0) then begin
    if (Length(AValue) > 0) then
      BlockWrite(F, AValue[1], Length(AValue));
    R := IOResult;
    Close(F);
    E := IOResult;
    if R = 0 then R := E;
  end;
  Result := R = 0;
end;

function SaveFile(AFileName: String; const AValue: UnicodeString): boolean;
begin
  Result := SaveFile(AFileName, RawByteString(AValue));
end;

function LoadFile(AFileName: String; out AValue : RawByteString) : boolean; overload;
var
   F : File;
   R, E : integer;
begin
  AValue:='';
  System.Assign(F, AFileName);
  Reset(F, 1);
  R := IOResult;
  if R = 0 then begin
    if FileSize(F) >= MaxInteger then
      R := 8
    else begin
        SetLength(AValue, FileSize(F));
        BlockRead(F, AValue[1], Length(AValue));
    end;
    Close(F);
    E := IOResult;
    if R = 0 then R := E;
  end;
  Result := R = 0;
end;

function LoadFile(AFileName: String; out AValue: UnicodeString): boolean;
var
   Raw : RawByteString;
begin
  LoadFile:=LoadFile(AFileName, Raw);
  AValue:=UnicodeString(Raw);
end;

{$POP}

(* Improve later to handle links, and other options *)
procedure ScanDirPath(const Base, Sub, Spec : RawByteString; Attr : LongInt;
  var Files : TStringList; Opts : TScanDirOpts = []);
var
   S : TSearchRec;
begin
  if FindFirst(Base + Sub + Spec, Attr, S) = 0 then begin
    repeat
      if (S.Name = '.') or (S.Name = '..') then continue;
      if S.Attr and faDirectory = faDirectory then begin
        if sdDirectories in Opts then
          Files.Add(Sub + S.Name);
      end else begin
        if not (sdExcludeFiles in Opts) then
          Files.Add(Sub + S.Name);
      end;
    until FindNext(S) <> 0;
    FindClose(S);
  end;
  if sdRecurse in Opts then begin
    if FindFirst(Base + Sub + DirWildCard, faAnyFile, S) = 0 then begin
       repeat
         if (S.Name = '.') or (S.Name = '..') then continue;
         if S.Attr and faDirectory = faDirectory then
           ScanDirPath(Base, IncludeTrailingPathDelimiter(Sub + S.Name), Spec,
             Attr, Files, Opts);
       until FindNext(S) <> 0;
       FindClose(S);
     end;
  end;
end;

function ScanDir(const Path : RawByteString; Attr : LongInt;
  out Files : TStringList; Opts : TScanDirOpts = []): Boolean; overload;
var
   P, S : RawByteString;
begin
  Files:=TStringList.Create;
  P:=ExtractFilePath(Path);
  S:=ExtractFileName(Path);
  if (P='') and (S='') then begin
    S:=DirWildCard;
  end else if P = '' then begin
    if DirectoryExists(S) then begin
      P:=S;
      S:='';
    end;
  end;
  if (P <> '') then
    P:=IncludeTrailingPathDelimiter(P);
  if S = '' then
     S:=DirWildCard;
  ScanDirPath(P, '', S, Attr, Files, Opts);
  ScanDir := Files.Count > 0;
  if ScanDir then begin
    if sdSorted in Opts then begin
      Files.Sorted:=True;
    end;
  end else
    FreeAndNil(Files);
end;

function ScanDir(const Path : RawByteString;
  out Files : TStringList; Opts : TScanDirOpts = []): Boolean; overload;
begin
  ScanDir:=ScanDir(Path, faAnyFile, Files, Opts);
end;

function ScanDir(const Path : RawByteString; Attr : LongInt;
  out Files : TStringArray; Opts : TScanDirOpts = []): Boolean; overload;
var
   I : integer;
   SL : TStringList;
begin
  Files:=[];
  ScanDir:=ScanDir(Path, Attr, SL, Opts);
  if Assigned(Files) then begin
    SetLength(Files, SL.Count);
      for I := 0 to SL.Count - 1 do
          Files[I]:=SL[I];
  end;
end;

function ScanDir(const Path : RawByteString;
  out Files : TStringArray; Opts : TScanDirOpts = []): Boolean; overload;
begin
  ScanDir:=ScanDir(Path, faAnyFile, Files, Opts);
end;


procedure AppendBreakChars(C : TIntegerArray);
var
   I : Integer;
   S : UnicodeString;
begin
  for I:=0 to Length(C) - 1 do begin
    S:=SPACE4;
    Word(S[1]):=C[I];
    WordBreakChars:=WordBreakChars+Trim(S);
  end;
end;

procedure ShowBreakChars;
var
   I : integer;
begin
//  WordBreakChars:=WordBreakChars+'•';
  for I := 1 to Length(WordBreakChars) do
    WriteLn(WordBreakChars[I], ', ', Integer(WordBreakChars[I]));
  Halt(1);
end;

initialization
  AppendBreakChars([65281, 161, 191, 8217, 171, 187, 65292, 1548, 12289]);
  AppendBreakChars([65288, 65289, 12290, 8220, 1417, 32, 8221, 65306, 12300]);
  AppendBreakChars([12301, 8212, 4963, 19968, 8482, 65307, 1563, 183, 8222]);
  AppendBreakChars([8230, 32, 64830, 64831, 8203, 65279, 8213, 8266]);
  AppendBreakChars([160, 173, 8199, 8200, 8201, 8202, 8204, 65531, 61533]);
  AppendBreakChars([8206,8207,8234,8235,8236,8237,8238,8239,8291,12288,65529]);
end.
