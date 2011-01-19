{*******************************************************************************
                        This file is part of Pinaker.
                       This source code is public domain.
********************************************************************************
Author: Ludwig Krippahl
Date: 14.1.2011
Purpose:
  Managing configuration files, multiplatform support
  Only selects the file and folder.
Requirements:
Revisions:
To do:
*******************************************************************************}
unit pinakerconfig;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,Forms;

const
  ScriptExt='pws'; //extension for the web source scripts

var

  ConfigFolder:string;
  ConfigFile:string;
  ScriptFolder:string; // folder containing the web source scripts

implementation

procedure InitPaths;
//NOTE: configuration paths are global in windows, local in linux
//(storing config in the exe path is great for portable applications)

begin
  {$ifdef win32}
  ConfigFile := ExtractFilePath(Application.EXEName) + 'pinaker.ini';
  ConfigFolder:=ExtractFilePath(Application.EXEName);
  ScriptFolder:=ConfigFolder+'/websource';
  {$endif}
  {$ifdef Unix}
  ConfigFile := GetAppConfigFile(False);
  ConfigFolder:=GetAppConfigDir(False);
  //no need for a subfolder in linux
  ScriptFolder:=ConfigFolder;
  {$endif}

end;

initialization
  InitPaths;
end.

