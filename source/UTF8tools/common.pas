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
  CommandSwitch = '-';

type

  TMapString = AnsiString;

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
    procedure Clear;
    constructor Create;
    destructor Destroy; override;
  published
  end;

procedure Initialize;
procedure Finalize;

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
  Search, Last : TMapNode;
  L : integer;
begin
  Add:=Nil;
  Last:=Nil;
  Search:=FRootNode;
  L:=0;
  While Assigned(Search) do begin
    if Search.FValue = AValue then Exit; // Already in List
    Last:=Search;
    if AValue < Search.FValue then
      Search:=Search.FLesser
    else
      Search:=Search.FGreater;
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

{ Unit initialization routines }

var
  OldExitProc : pointer;

procedure Initialize;
begin
  if Assigned(OldExitProc) then exit;
  OldExitProc := ExitProc;
  ExitProc := @Finalize;
end;

procedure Finalize;
begin
  if not Assigned(OldExitProc) then exit;
  ExitProc := OldExitProc;
  OldExitProc := nil;
end;

begin
  OldExitProc := nil;
  Initialize;
end.
