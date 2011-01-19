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
unit imageprocessor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,Graphics,basetypes,eanthirteen,
  GraphType, IntfGraphics, LCLType, LCLProc,  LCLIntf,FPImage;

function EAN13FromJpg(AJpgFile:string; MaxLines:Integer=400):TDecodedRec;

implementation


function EAN13FromJpg(AJpgFile:string; MaxLines:Integer=400):TDecodedRec;

var
  jpg:TJpegImage;
  bmp:TBitMap;
  x,cy,y,f:Integer;
  dy:Single;
  tmp:TLazIntfImage; // needed because Lazarus has no scanlines on bmp
  scanline,bitline:TIntegers;
  numlines:Integer;
  totalsline:TIntegers;
  pp:TPointPair;


begin
  Result.FileName:=AJpgFile;
  Result.Lines:=nil;
  //setup images
  Jpg:=TJpegImage.Create;
  Jpg.LoadFromFile(AJpgFile);
  Bmp:=TBitMap.Create;
  Bmp.Assign(Jpg);
  Bmp.PixelFormat:=pf24bit;
  Bmp.HandleType:=bmDIB;
  Tmp:=TLazIntfImage.Create(0,0);
  Tmp.LoadFromBitmap(Bmp.Handle,Bmp.MaskHandle);

  SetLength(scanline,Bmp.Width);
  SetLength(totalsline,BitCount);

  y:=Bmp.Height div 2;      //start from center

  for cy:=1 to MaxLines do
    begin
    //read scanline
    for x:=0 to High(scanline) do
      with Tmp.Colors[x,y] do
        scanline[x]:=red div 255
                    +green div 255
                    +blue div 255;
    //clip and store coordinates
    bitline:=ClipLine(scanline,x);
    if bitline<>nil then
      begin
      pp.y1:=y;
      pp.y2:=y;
      pp.x1:=x;
      pp.x2:=x+Length(bitline);       //bitline is the clipped scanline here
      AddToArray(pp,Result.Lines);

      bitline:=CompressLine(bitline,BitCount); //resize to 95 bins

      //and add values to totals, for first lines only
      //TO DO: avoid hardcoded parameter
      if cy<100 then
        for f:=0 to High(bitline) do totalsline[f]:=totalsline[f]+bitline[f];

      //convert to 0s 1s and decode
      bitline:=GetBits(bitline);
      Result.Decoded:=DecodeLine(bitline);
      Result.State:=EAN13State(Result.Decoded);

      //also break on UPC, since that is not a reading problem
      if (Result.State=stateOK) or (Result.State=stateSuspect) then Break;
      end;

    //calculate next scan line, jumping across the middle
    if y<Bmp.Height div 2 then
      y:=Bmp.Height-y
        // this way because of roundoff errors in div
    else y:=Bmp.Height div 2-(y-Bmp.Height div 2)-1;

    end;
  //if not found try to use the totals
  if (Result.State<>stateOK) then
    begin
    bitline:=GetBits(totalsline);
    Result.Decoded:=DecodeLine(bitline);
    Result.State:=EAN13State(Result.Decoded);

    //diagonal to mark that the total was used
    //TO DO: store x limits or something...
    pp.y1:=Bmp.Height div 2-MaxLines div 2;
    pp.y2:=Bmp.Height div 2+MaxLines div 2;
    pp.x1:=Bmp.Width div 3;
    pp.x2:=2*Bmp.Width div 3;
    AddToArray(pp,Result.Lines);
    end;

  Tmp.Free;
  Jpg.Free;
  Bmp.Free;
end;

end.

