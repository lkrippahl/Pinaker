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

function EAN13FromJpg(AJpgFile:string):string;

implementation

function EAN13FromJpg(AJpgFile:string):string;

var
  jpg:TJpegImage;
  bmp:TBitMap;
  x,y,y1,y2:Integer;
  tmp:TLazIntfImage; // needed because Lazarus has no scanlines on bmp
  scanline:TIntegers;

begin
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

  //set scanline region
  y:=3;//Bmp.Height div 20; //width of scanline
  y1:=Bmp.Height div 2 - y;
  y2:=Bmp.Height div 2 + y;



  for x:=0 to High(scanline) do
    begin
    scanline[x]:=0;
    for y:=y1 to y2 do
      begin
      with Tmp.Colors[x,y] do
        scanline[x]:=scanline[x]
                    +red div 255
                    +green div 255
                    +blue div 255;
      end;
    //DebugLn(IntToStr(x)+'-'+IntToStr(scanline[x]));
    end;
  Tmp.Free;
  Jpg.Free;
  Bmp.Free;

  Result:=DecodeLine(scanline);
end;

end.

