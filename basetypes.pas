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


type
  TStrings=array of string;
  TCardinals=array of Cardinal;
  TIntegers=array of Integer;

  TDecodedRec=record
    Decoded:string;       //the decoded barcode
    State:Integer;
    FileName:string;      //image file name (with full path)
    x1,y1,x2,y2:Integer;  //the coordinates of the scan line
  end;

  TDecodedRecs=array of TDecodedRec;


function FixLineBreaks(s:string):string;    //converts to current OS
function StrCompare(s1,s2:string):Boolean;  // string comparison indifferent to case

//Returns first word deleting from text
function GrabWord(var Text:string;const Sep:string=' '):string;

function GrabBetween(var Text:string;const Sep1,Sep2:string):string;

function SplitString(Text:string;const Sep:string=' '):TStrings; overload;
procedure SplitString(Text:string;Words:TStringList;const Sep:string=' ');overload;

//Add regardless of existing element
procedure AddToArray(Elm:string;var Arr:TStrings);overload;
procedure AddToArray(Elm:Cardinal;var Arr:TCardinals);overload;
procedure AddToArray(Elm:Integer;var Arr:TIntegers);overload;
procedure AddToArray(Elm:TDecodedRec;var Arr:TDecodedRecs);overload;


//Add only of not exist
procedure AddUniqueToArray(Elm:string;var Arr:TStrings);overload;
procedure AddUniqueToArray(Elm:Cardinal;var Arr:TCardinals);overload;
procedure AddUniqueToArray(Elm:Integer;var Arr:TIntegers);overload;

procedure AppendToArray(const Suffix:TStrings; var Arr:TStrings);overload;
procedure AppendToArray(const Suffix:TCardinals; var Arr:TCardinals);overload;
procedure AppendToArray(const Suffix:TIntegers; var Arr:TIntegers);overload;

//Return index of first element in array matching Elm
function IndexInArray(Elm:string; var Arr:TStrings):Integer;overload;
function IndexInArray(Elm:Cardinal; var Arr:TCardinals):Integer;overload;
function IndexInArray(Elm:Integer; var Arr:TIntegers):Integer;overload;



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

function SplitString(Text: string; const Sep: string): TStrings;overload;
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



procedure AddToArray(Elm:string;var Arr:TStrings);overload;
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

function IndexInArray(Elm:string; var Arr:TStrings):Integer;overload;

var f,h:Integer;

begin
  h:=High(Arr);
  for f:=0 to h do
    if Elm=Arr[f] then
        Exit(f);
  Exit(-1); //if not found
end;

function IndexInArray(Elm:Cardinal; var Arr:TCardinals):Integer;overload;

var f,h:Integer;

begin
  h:=High(Arr);
  for f:=0 to h do
    if Elm=Arr[f] then
      Exit(f);
  Exit(-1); //if not found
end;
function IndexInArray(Elm:Integer; var Arr:TIntegers):Integer;overload;

var f,h:Integer;

begin
  h:=High(Arr);
  for f:=0 to h do
    if Elm=Arr[f] then
        Exit(f);
  Exit(-1); //if not found
end;

procedure AddUniqueToArray(Elm:string;var Arr:TStrings);overload;

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

procedure AppendToArray(const Suffix:TStrings; var Arr:TStrings);overload;

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

end.

