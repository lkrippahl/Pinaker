{*******************************************************************************
                        This file is part of Pinaker.
                       This source code is public domain.
********************************************************************************
Author: Ludwig Krippahl
Date: 13.1.2011
Purpose:
  Base types and utility functions for handling arrays and strings
Requirements:
Revisions:
To do:
*******************************************************************************}

unit basetypes;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

const
  //EAN record states
  stateOK=0;
  stateInvalid=1;
  stateChecksumError=2;
  stateSuspect=3;


type
  TPointPair=record
    x1,y1,x2,y2:Integer;
  end;

  TSimpleStrings=array of string;
  TCardinals=array of Cardinal;
  TIntegers=array of Integer;
  TBooleans=array of Boolean;
  TPointPairs=array of TPointPair;


  TDecodedRec=record
    Decoded:string;       //the decoded barcode
    State:Integer;
    FileName:string;      //image file name (with full path)
    Lines:TPointPairs;    //the coordinates of each scan line
  end;

  TDecodedRecs=array of TDecodedRec;


function FixLineBreaks(s:string):string;    //converts to current OS
function StrCompare(s1,s2:string):Boolean;  // string comparison indifferent to case

//Returns first word deleting from text
function GrabWord(var Text:string;const Sep:string=' '):string;

function GrabBetween(var Text:string;const Sep1,Sep2:string):string;

function SplitString(Text:string;const Sep:string=' '):TSimpleStrings; overload;
procedure SplitString(Text:string;Words:TStringList;const Sep:string=' ');overload;

//Add regardless of existing element
procedure AddToArray(Elm:string;var Arr:TSimpleStrings);overload;
procedure AddToArray(Elm:Cardinal;var Arr:TCardinals);overload;
procedure AddToArray(Elm:Integer;var Arr:TIntegers);overload;
procedure AddToArray(Elm:TDecodedRec;var Arr:TDecodedRecs);overload;
procedure AddToArray(Elm:Boolean;var Arr:TBooleans);overload;
procedure AddToArray(Elm:TPointPair;var Arr:TPointPairs);overload;


//Add only of not exist
procedure AddUniqueToArray(Elm:string;var Arr:TSimpleStrings);overload;
procedure AddUniqueToArray(Elm:Cardinal;var Arr:TCardinals);overload;
procedure AddUniqueToArray(Elm:Integer;var Arr:TIntegers);overload;

//appends to the end of the array
procedure AppendToArray(const Suffix:TSimpleStrings; var Arr:TSimpleStrings);overload;
procedure AppendToArray(const Suffix:TCardinals; var Arr:TCardinals);overload;
procedure AppendToArray(const Suffix:TIntegers; var Arr:TIntegers);overload;

//appends only those that do not exist already
procedure AppendUniquesToArray(const Suffix:TSimpleStrings; var Arr:TSimpleStrings);overload;
//TO DO: other types

//Return index of first element in array matching Elm
function IndexInArray(Elm:string; const Arr:TSimpleStrings):Integer;overload;
function IndexInArray(Elm:Cardinal; const Arr:TCardinals):Integer;overload;
function IndexInArray(Elm:Integer; const Arr:TIntegers):Integer;overload;

//delete elements
procedure DeleteFromArray(var Arr:TSimpleStrings; First,Count:Integer);overload;
//TO DO: other types


implementation

function FixLineBreaks(s:string):string;

var f:Integer;

begin
  Result:='';
  for f:=1 to Length(s) do
    if s[f]=#10 then Result:=Result+LineEnding
end;

function StrCompare(s1,s2:string):Boolean;
//ignores case

begin
  Result:=UpperCase(s1)=UpperCase(s2);
end;

function GrabWord(var Text:string;const Sep:string=' '):string;
//consumes the original string

var p:Integer;

begin
  while Pos(Sep,Text)=1 do
    Text:=Copy(Text,Length(Sep)+1,Length(Text));
  p:=Pos(Sep,Text);
  if p<0 then
    begin
    Result:=Text;
    Text:='';
    end
  else
    begin
    Result:=Copy(Text,1,p-1);
    Text:=Copy(Text,p+Length(Sep),Length(Text));
    end;
end;


function GrabBetween(var Text:string;const Sep1,Sep2:string):string;
//consumes the original string, Text

var p1,p2:Integer;

begin
  p1:=Pos(Sep1,Text);
  p2:=Pos(Sep2,Text);
  if p1<p2 then
    begin
    Result:=Copy(Text,p1+Length(Sep1),p2-p1-Length(Sep1));
    Text:=Copy(Text,1,p1-1)+Copy(Text,p2+Length(Sep2),Length(Text));
    end;
end;

function SplitString(Text: string; const Sep: string): TSimpleStrings;overload;
begin
  Result:=nil;
  while Text<>'' do
    begin
    SetLength(Result,Length(Result)+1);
    Result[High(Result)]:=GrabWord(Text,Sep);
    end;
end;

procedure SplitString(Text:string;Words:TStringList;const Sep:string=' ');overload;
begin
  while Text<>'' do Words.Add(GrabWord(Text,Sep));
end;



procedure AddToArray(Elm:string;var Arr:TSimpleStrings);overload;
begin
  SetLength(Arr,Length(Arr)+1);
  Arr[High(Arr)]:=Elm;
end;

procedure AddToArray(Elm:Cardinal;var Arr:TCardinals);overload;
begin
  SetLength(Arr,Length(Arr)+1);
  Arr[High(Arr)]:=Elm;
end;

procedure AddToArray(Elm:Integer;var Arr:TIntegers);overload;
begin
  SetLength(Arr,Length(Arr)+1);
  Arr[High(Arr)]:=Elm;
end;

procedure AddToArray(Elm:TDecodedRec;var Arr:TDecodedRecs);overload;

begin
  SetLength(Arr,Length(Arr)+1);
  Arr[High(Arr)]:=Elm;
end;

procedure AddToArray(Elm:Boolean;var Arr:TBooleans);overload;

begin
  SetLength(Arr,Length(Arr)+1);
  Arr[High(Arr)]:=Elm;
end;

procedure AddToArray(Elm:TPointPair;var Arr:TPointPairs);overload;

begin
  SetLength(Arr,Length(Arr)+1);
  Arr[High(Arr)]:=Elm;
end;

function IndexInArray(Elm:string; const Arr:TSimpleStrings):Integer;overload;

var f,h:Integer;

begin
  h:=High(Arr);
  for f:=0 to h do
    if Elm=Arr[f] then
        Exit(f);
  Exit(-1); //if not found
end;

function IndexInArray(Elm:Cardinal; const Arr:TCardinals):Integer;overload;

var f,h:Integer;

begin
  h:=High(Arr);
  for f:=0 to h do
    if Elm=Arr[f] then
      Exit(f);
  Exit(-1); //if not found
end;
function IndexInArray(Elm:Integer; const Arr:TIntegers):Integer;overload;

var f,h:Integer;

begin
  h:=High(Arr);
  for f:=0 to h do
    if Elm=Arr[f] then
        Exit(f);
  Exit(-1); //if not found
end;

procedure AddUniqueToArray(Elm:string;var Arr:TSimpleStrings);overload;

begin
  if IndexInArray(Elm,Arr)<0 then
      AddToArray(Elm,Arr);
end;

procedure AddUniqueToArray(Elm:Cardinal;var Arr:TCardinals);overload;

begin
  if IndexInArray(Elm,Arr)<0 then
      AddToArray(Elm,Arr);
end;

procedure AddUniqueToArray(Elm:Integer;var Arr:TIntegers);overload;

begin
  if IndexInArray(Elm,Arr)<0 then
      AddToArray(Elm,Arr);
end;

procedure AppendToArray(const Suffix:TSimpleStrings; var Arr:TSimpleStrings);overload;

var
  f,len:Integer;

begin
  if Suffix<>nil then
    begin
      len:=Length(Arr);
      SetLength(Arr,len+Length(Suffix));
      for f:=0 to High(Suffix) do
        Arr[f+len]:=Suffix[f];
    end;
end;

procedure AppendToArray(const Suffix:TCardinals; var Arr:TCardinals);overload;

var
  f,len:Integer;

begin
  if Suffix<>nil then
    begin
      len:=Length(Arr);
      SetLength(Arr,len+Length(Suffix));
      for f:=0 to High(Suffix) do
        Arr[f+len]:=Suffix[f];
    end;
end;

procedure AppendToArray(const Suffix:TIntegers; var Arr:TIntegers);overload;

var
  f,len:Integer;

begin
  if Suffix<>nil then
    begin
      len:=Length(Arr);
      SetLength(Arr,len+Length(Suffix));
      for f:=0 to High(Suffix) do
        Arr[f+len]:=Suffix[f];
    end;
end;

procedure AppendUniquesToArray(const Suffix:TSimpleStrings; var Arr:TSimpleStrings);overload;

var
  f,len:Integer;

begin
  if Suffix<>nil then
    for f:=0 to High(Suffix) do
      AddUniqueToArray(Suffix[f],Arr);
end;

procedure DeleteFromArray(var Arr:TSimpleStrings; First,Count:Integer);overload;

var
  f,c,last:Integer;
  tmp:TSimpleStrings;

begin
  last:=First+Count;
  if last>High(Arr) then last:=High(Arr);
  if First<=last then
    begin
    SetLength(tmp,Length(Arr) - (last-First+1));
    c:=0;
    if First>0 then
      for f:=0 to First-1 do
        begin
        tmp[c]:=Arr[f];
        Inc(c);
        end;
    if last<High(Arr) then
      for f:=last+1 to High(Arr) do
        begin
        tmp[c]:=Arr[f];
        Inc(c);
        end;
    Arr:=tmp;
    end;
end;

end.

