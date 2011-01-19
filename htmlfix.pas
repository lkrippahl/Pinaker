{*******************************************************************************
                        This file is part of Pinaker.
                       This source code is public domain.
********************************************************************************
Author: Ludwig Krippahl
Date: 19.1.2011
Purpose:
  Loads a lookup table to convert &..; html codes into utf-8 characters
Requirements:
  Needs the htmlcodes file
  MUST BE INITIALIZED before using by running LoadHtmlTable(htmlcodes file)
Revisions:
To do:
*******************************************************************************}

unit htmlfix;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, basetypes,LCLProc;

function FixHtmlString(Text:string):string;
procedure LoadHtmlTable(FileName:string);
procedure FixHtmlStrings(var Ss:TSimpleStrings);
procedure ExportTable(Sl:TStrings);

implementation

var
  //the two tables with the html codes and the characters
  Keys,Vals:TSimpleStrings;

function FixHtmlString(Text:string):string;

var
  ix,cix:Integer;
  s:string;

begin
  Result:='';
  repeat
    cix:=Pos('&',Text);
    if cix>0 then
      //if & found, look for ; with no spaces between them
      begin
      Result:=Result+Copy(Text,1,cix-1);
      Delete(Text,1,cix-1);
      cix:=Pos(';',Text);
      if cix<=0 then cix:=Length(Text);        //if not found, grab all Text
      s:=Copy(Text,1,cix);
      Delete(Text,1,cix);
      if Pos(' ',s)<=0 then                    //no spaces, possible identifier
        begin
        ix:=IndexInArray(s,Keys);
        if ix>=0 then s:=Vals[ix];             //replace s if special character
        end;
      Result:=Result+s;
      end
    else
      begin
      Result:=Result+Text;
      Text:='';
      end;
  until Text='';
end;

procedure LoadHtmlTable(FileName:string);

var
  sl:TStringList;
  f:Integer;
  s:string;

begin
  Keys:=nil;
  Vals:=nil;
  sl:=TStringList.Create;
  sl.LoadFromFile(FileName);
  for f:=0 to sl.Count-1 do
    begin
    s:=Copy(Sl.Strings[f],1,Pos(#9,sl.Strings[f])-1);
    AddToArray(s,Vals);
    s:=Copy(Sl.Strings[f],Pos(#9,sl.Strings[f])+1,Length(Sl.Strings[f]));
    AddToArray(s,Keys);
    end;
  sl.Free;
end;

procedure FixHtmlStrings(var Ss:TSimpleStrings);

var f:Integer;

begin
  for f:=0 to High(Ss) do
    Ss[f]:=FixHtmlString(Ss[f]);
end;

procedure ExportTable(Sl:TStrings);

var f:Integer;

begin
  for f:=0 to High(Keys) do
    Sl.Add(Keys[f]+':'+ANSItoUTF8(Vals[f]));
end;

end.

