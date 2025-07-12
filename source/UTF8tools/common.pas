{
   Copyright (c) 2025 Jerome Shidel
   BSD-3-Clause license
}

unit Common;

{$IFDEF FPC}
  {$mode objfpc}{$H+}
{$ELSE}
  {$DEFINE TP70}
{$ENDIF}

interface

uses
  Classes, SysUtils;

{$I version.inc}

const
  {$if defined(TP70)}
  CommandSwitch : String = '/';
  {$elseif defined(windows)}
  CommandSwitch : String = '/';
  {$elseif defined(darwin)}
  CommandSwitch : String = '-';
  {$elseif defined(linux)}
  CommandSwitch : String = '-';
  {$else}
    {$ERROR unknown platform}
  {$endif}

  CR          = #$0d;
  LF          = #$0a;
  SPACE       = #$20;
  TAB         = #$09;
  AMPERSAND   = #$26;
  LESSTHAN    = #$3c;
  GREATERTHAN = #$3e;
  COLON       = #$3a;
  SEMICOLON   = #$3b;

  TAB2        = TAB + TAB;

  MaxInteger  = MaxLongInt;

type

  { maybe use RawByteString instead of AnsiString }
  TMapString = AnsiString;
  TAsciiString = AnsiString;
  TUTF8String = AnsiString;

  TConvertOpts = set of ( cvCR, cvLF, cvTAB, cvCtrlChars );

  { TMapNode }

  TMapNode = class
  private
    FValue: TMapString;
    FData: TMapString;
    FGreater: TMapNode;
    FLesser: TMapNode;
  public
    property Value : TMapString read FValue;
    property Data : TMapString read FData;
    property Lesser : TMapNode read FLesser;
    property Greater : TMapNode read FGreater;
    constructor Create;
    destructor Destroy; override;
  published
  end;

  { TMapTree }

  TMapTree = class
  private
    FRootNode: TMapNode;
    FLevels: integer;
  private
  public
    property RootNode : TMapNode read FRootNode;
    function Add(AValue, AData : TMapString) : TMapNode;
    function Find(AValue : TMapString) : TMapNode;
    function Search(AValue : TMapString; FirstMatch : boolean = false) : TMapNode;
    procedure Clear;
    constructor Create;
    destructor Destroy; override;
  published
  end;

function PopDelim(var AStr : TMapString; ADelim: TMapString = #32): TMapString;
procedure Explode(AStr : String; var AStrs : TStringList; ADelim : String = ','); overload;
function Explode(AStr : String; ADelim : String = ',' ):TStringArray; overload;

function SaveFile(AFileName: String; const AValue : AnsiString) : boolean; overload;
function LoadFile(AFileName: String; out AValue : AnsiString) : boolean; overload;

implementation

{ TMapNode }

constructor TMapNode.Create;
begin
  inherited Create;
  FGreater:=nil;
  FLesser:=nil;
  FValue:='';
  FData:='';
end;

destructor TMapNode.Destroy;
begin
  if Assigned(FLesser) then FreeAndNil(FLesser);
  if Assigned(FGreater) then FreeAndNil(FGreater);
  inherited Destroy;
end;

{ TMapTree }

function TMapTree.Add(AValue, AData: TMapString): TMapNode;
var
  This, Last : TMapNode;
  L : integer;
begin
  Add:=Nil;
  Last:=Nil;
  This:=FRootNode;
  L:=0;
  While Assigned(This) do begin
    if This.FValue = AValue then Exit; // Already in List
    Last:=This;
    if AValue < This.FValue then
      This:=This.FLesser
    else
      This:=This.FGreater;
    Inc(L);
  end;
  if L > FLevels then FLevels:=L;
  Add := TMapNode.Create;
  Add.FValue := AValue;
  Add.FData := AData;
  if Not Assigned(Last) then
    FRootNode:=Add
  else if AValue < Last.FValue then
    Last.FLesser:=Add
  else
    Last.FGreater:= Add;
end;

function TMapTree.Find(AValue: TMapString): TMapNode;
begin
  Find := FRootNode;
  While Assigned(Find) do begin
    if AValue = Find.FValue then Break;
    if AValue < Find.FValue then
      Find := Find.FLesser
    else
      Find := Find.FGreater;
  end;
end;

function TMapTree.Search(AValue: TMapString; FirstMatch : boolean = false): TMapNode;
var
  C : TMapString;
  F : TMapNode;
begin
  Search := nil;
  F := FRootNode;
  While Assigned(F) do begin
    C := Copy(AValue, 1, Length(F.FValue));
    if C = F.FValue then begin
      Search:=F;
      if FirstMatch or (Not Assigned(F.FGreater)) then
        Break;
    end;
    if C < F.FValue then
      F := F.FLesser
    else
      F := F.FGreater;
  end;
end;

procedure TMapTree.Clear;
begin
  if Assigned(FRootNode) then FreeAndNil(FRootNode);
  FLevels:=0;
end;

constructor TMapTree.Create;
begin
  inherited Create;
  FRootNode := nil;
  FLevels:=0;
end;

destructor TMapTree.Destroy;
begin
  Clear;
  inherited Destroy;
end;

{ Unit functions and procedures }

function PopDelim(var AStr: TMapString; ADelim: TMapString): TMapString;
var
  P : integer;
begin
  P := Pos(ADelim, AStr);
  if P <= 0 then P := Length(AStr) + 1;
  Result := Copy(AStr, 1, P - 1);
  Delete(AStr, 1, P - 1 + Length(ADelim));
end;

procedure Explode(AStr : String; var AStrs : TStringList; ADelim : String); overload;
var
  S : String;
begin
  While Length(AStr) > 0 do begin
    S := PopDelim(AStr, ADelim);
    AStrs.Add(S);
  end;
end;

function Explode(AStr : String; ADelim : String):TStringArray; overload;
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

{$PUSH}
{$I-}

function SaveFile(AFileName: String; const AValue : AnsiString) : boolean; overload;
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

function LoadFile(AFileName: String; out AValue : AnsiString) : boolean; overload;
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

{$POP}

end.
