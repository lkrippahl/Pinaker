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

function EAN13FromJpg(AJpgFile:string):TDecodedRec;

implementation


function EAN13FromJpg(AJpgFile:string):TDecodedRec;

var
  jpg:TJpegImage;
  bmp:TBitMap;
  x,y:Integer;
  dy:Single;
  tmp:TLazIntfImage; // needed because Lazarus has no scanlines on bmp
  scanline,bitline:TIntegers;
  numlines:Integer;

begin
  Result.FileName:=AJpgFile;
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

  //scan lines in mid third
  //TO DO: argument to control this?

  for y:=(Bmp.Height div 3) to (2*Bmp.Height div 3) do
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
      Result.y1:=y;
      Result.y2:=y;
      Result.x1:=x;
      Result.x2:=x+Length(bitline);

      //decode and check
      bitline:=GetBits(bitline);
      Result.Decoded:=DecodeLine(bitline);
      Result.State:=EAN13State(Result.Decoded);
      if Result.State=stateOK then Break;
      end;
    end;


  Tmp.Free;
  Jpg.Free;
  Bmp.Free;
end;

end.

