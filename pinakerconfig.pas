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

var

  ConfigFolder:string;
  ConfigFile:string;

implementation

procedure InitPaths;

begin
  {$ifdef win32}
  ConfigFile := ExtractFilePath(Application.EXEName) + 'pinaker.ini';
  {$endif}
  {$ifdef Unix}
  ConfigFile := GetAppConfigFile(False) + '.conf';
  {$endif}
  ConfigFolder:=ExtractFilePath(ConfigFile);
end;

initialization
  InitPaths;
end.

