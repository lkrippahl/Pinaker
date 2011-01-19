{*******************************************************************************
                        This file is part of Pinaker.
                       This source code is public domain.
********************************************************************************
Author: Ludwig Krippahl
        Thanks to Johann C. Rocholl and his thesis "Robust 1D Barcode
        Recognition on Mobile Devices".
Date: 13.1.2011
Purpose:
  Decodes EAN-13 numbers from an array of intensities (high:white, low:black)

Requirements:
Revisions:
To do:
*******************************************************************************}

unit eanthirteen;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,basetypes,LCLProc;

const
  //3 guard bits+42 number bits +5 guard bits +42 number bits+3 guard bits
  BitCount=95;

  //for each digit, odd pariti left, even parity left, and right-hand
  EncodingTable:array [0..9] of array [1..3] of byte=
    ((%0001101,%0100111,%1110010),
     (%0011001,%0110011,%1100110),
     (%0010011,%0011011,%1101100),
     (%0111101,%0100001,%1000010),
     (%0100011,%0011101,%1011100),
     (%0110001,%0111001,%1001110),
     (%0101111,%0000101,%1010000),
     (%0111011,%0010001,%1000100),
     (%0110111,%0001001,%1001000),
     (%0001011,%0010111,%1110100));
  ParityTable:array [0..9] of string=
        ('OOOOO',
        'OEOEE',
        'OEEOE',
        'OEEEO',
        'EOOEE',
        'EEOOE',
        'EEEOO',
        'EOEOE',
        'EOEEO',
        'EEOEO');


//returns an array with the contrast (max-min) inside a window of size
//2*winrad+1 (winrad pixels to the left and right)
function ContrastPlot(Intensities:TIntegers;WinRad:Integer):TIntegers;

//returns the clipped intensity array (a copy) and the index X, in the old
//array, of the first element of the clipped array
function ClipLine(const Intensities:TIntegers;var X:Integer):TIntegers;

function CompressLine(Intensities:TIntegers;Bins:Integer):TIntegers;
function CheckDigit(Ean:string):Char;



//returns the bits from a clipped line
function GetBits(Intensities:TIntegers):TIntegers;

function ArrayToByte(Bits:TIntegers):Byte;

function Decode(Value:Byte;Column:Integer):Char;

function GetFirstDigit(Parity:string):Char;

//returns barcode characters from a bitline, a 0 and 1
//array with the barcode
function DecodeLine(BitLine:TIntegers):string;

//evaluates string
function EAN13State(Ean:string):Integer;


implementation

function ContrastPlot(Intensities:TIntegers;WinRad:Integer):TIntegers;
//TO DO: Optimize code, very innefficient


var
  f,g:Integer;
  i1,i2,top,bot:integer;


begin
  SetLength(Result,Length(Intensities));
  for f:=0 to High(Intensities) do
    begin
    top:=Intensities[f];
    bot:=top;
    i1:=f-WinRad;
    if i1<0 then i1:=0;
    i2:=f+WinRad;
    if i2>High(Intensities) then i2:=High(Intensities);
    for g:=i1 to i2 do
      begin
      if Intensities[g]>top then top:=Intensities[g];
      if Intensities[g]<bot then bot:=Intensities[g];
      end;
    Result[f]:=top-bot;
    end;
end;

function Decode(Value:Byte;Column:Integer):Char;
//returns ? if not found

var f:Integer;

begin
  for f:=0 to 9 do
    if Value=EncodingTable[f,Column] then
        Exit(Chr(Ord('0')+f));
  Exit('?');
end;

function ClipLine(const Intensities:TIntegers;var X:Integer):TIntegers;
//assumes barcode contains the center of the array

var
  contrast:TIntegers;
  i1,i2,cc:Integer;

begin
  Result:=nil;
  contrast:=ContrastPlot(Intensities,20);

  //search for contrast drop from the center point
  i1:=Length(Intensities) div 2;
  i2:=i1;
  cc:=contrast[i1] div 2;
  while (i1>0) and (contrast[i1]>cc) do Dec(i1);
  while (i2<High(contrast)) and (contrast[i2]>cc) do Inc(i2);

  //fine tune, because contrast drops outside the barcode zone, which is
  //supposed to be white. The threshold is midpoint between white and black,
  //estimated from contrast at the midpoint (cc is half that contrast)
  cc:=Intensities[i1] - cc;
  while (i1<High(Intensities)) and (intensities[i1]>cc) do Inc(i1);
  while (i2>0) and (intensities[i2]>cc) do Dec(i2);

  if i2>i1+3*95 then //at least 3 pixels per bin
    Result:=Copy(Intensities,i1,i2-i1+1);
  X:=i1;
end;

function CompressLine(Intensities:TIntegers;Bins:Integer):TIntegers;

var
  f,ix:Integer;
  hitcount:TIntegers;

begin
  SetLength(Result,BitCount);
  SetLength(hitcount,BitCount);
  for f:=0 to High(Result) do
    begin
    Result[f]:=0;
    hitcount[f]:=0;
    end;

  //add all intensities into each corresponding bit
  for f:=0 to High(Intensities) do
    begin
    ix:=Trunc(f/Length(Intensities)*BitCount);
    Result[ix]:=Result[ix]+Intensities[f];
    Inc(hitcount[ix]);
    end;

  //average for number of bins
  for f:=0 to High(Result) do
    if hitcount[f]>0 then
        Result[f]:=result[f] div hitcount[f];
end;

function CheckDigit(Ean:string):Char;

var
  f,total,weight:Integer;

begin
  if Length(Ean)<12 then Exit('*'); //must have at least 12 digits
  try
  total:=0;
  weight:=1;
  for f:=1 to 12 do
    begin
    total:=total+StrToInt(Ean[f])*weight;
    if weight=3 then weight:=1 else weight:=3;
    end;
  total:=10-total mod 10;
  if total=10 then total:=0;
  Result:=IntToStr(total)[1];
  except
    Result:='*'; //failed checksum due to unexpected error
  end;
end;


function GetBits(Intensities:TIntegers):TIntegers;

var
  f,top,bot:Integer;
  hitcount:TIntegers;

begin
  //Resize the intensities vector
  if Length(Intensities)<>BitCount then
      Result:=CompressLine(Intensities,BitCount)
  else Result:=Intensities;

  //get extremes
  top:=Result[0];
  bot:=Result[0];
  for f:=1 to High(Result) do
    begin
    if Result[f]>top then top:=Result[f];
    if Result[f]<bot then bot:=Result[f];
    end;
  //reduce to 0 and 1
  top:=(top+bot) div 2;
  for f:=0 to High(Result) do
    if Result[f]>top then Result[f]:=0  //0 is white, 1 is black
    else Result[f]:=1;
end;

function ArrayToByte(Bits:TIntegers):Byte;

var
  f,mult,sum:Integer;

begin
  f:=High(Bits);
  sum:=Bits[f];
  mult:=1;
  while f>0 do
    begin
    Dec(f);
    mult:=mult*2;
    sum:=sum+mult*Bits[f];
    end;
  Result:=sum;

end;

function GetFirstDigit(Parity:string):Char;

var f:Integer;

begin
  for f:=0 to 9 do
    if Parity=ParityTable[f] then Exit(Chr(f+Ord('0')));
  Exit('?');
end;

function DecodeLine(BitLine:TIntegers):string;

var
  parity:string;
  f:Integer;

function GuardsFail:Boolean;

begin
  Result:=not (
    //left guards
    (BitLine[0]=1) and
    (BitLine[1]=0) and
    (BitLine[2]=1) and
    //right guards
    (BitLine[94]=1) and
    (BitLine[93]=0) and
    (BitLine[92]=1) and
    //center guards
    (BitLine[45]=0) and
    (BitLine[46]=1) and
    (BitLine[47]=0) and
    (BitLine[48]=1) and
    (BitLine[49]=0))
end;

procedure DecodeAt(Ix:Integer);

var
  b:Byte;
  c:Char;

begin
  c:='?';
  if Ix<=6 then
    begin
    b:=ArrayToByte(Copy(BitLine,Ix*7-4,7));
    c:=Decode(b,1);
    if c='?' then
      begin
      c:=Decode(b,2); //try with even parity if not found with odd
      parity:=parity+'E';
      end
    else
      parity:=parity+'O'
    end
  else
    begin
    b:=ArrayToByte(Copy(BitLine,Ix*7+1,7));
    c:=Decode(b,3);
    end;
  Result:=Result+c;
end;

begin
  Result:='';
  parity:='';
  if GuardsFail then
    Exit('')
  else
    for f:=1 to 12 do
      DecodeAt(f);
  Delete(parity,1,1); //ignore parity of second manufacturer digit;
  Result:=GetFirstDigit(parity)+Result;
end;

function EAN13State(Ean: string): Integer;

begin
  if (Ean='') or (Pos('?',Ean)>0) or (Length(Ean)<>13) then
    Result:=stateInvalid
  else if CheckDigit(Ean)<>Ean[13] then
    Result:=stateChecksumError
  //currently ISBN-13 numbers start with 978 or 979
  else if (Pos('97',Ean)<>1) then Result:=stateSuspect
  else Result:=stateOK;
end;

end.

