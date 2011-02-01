{********************************************************************************
Author: Ludwig Krippahl
Date: 17.1.2011
Purpose:
  Stores, manages and processes the websource scripts
Requirements:
Revisions:
To do:
  RunScripts, priority system to determine how repeated fields are filled
  error handling (skip, break, etc)
*******************************************************************************}

unit scripts;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, pinakerconfig, basetypes, LCLProc, htmlfix, httpsend;

const
  //Separator for listing script shortnames and longnames
  ScriptNameSep=': ';

  //keywords used in script
  //note that ean13 is a reserved variable but used by the main form
  //the parser handles it just like other variables
  KeyWords:array[0..4] of string=('NAME','POST','DELTO','GRABF','GRABV');
  kwName=0;
  kwPost=1;
  kwDelTo=2;
  kwGrabF=3;
  kwGrabV=4;

  EANReserved='ean13'; //reserved for inputing ean-13 code into script


type
  TScriptString=record
    //fragments of the string, parts within or without quote marks
    //the first one is always the keyword
    Parts:TSimpleStrings;
    //same length as parts, true if the part is an identifier
    //identifiers are field names and vars
    IsID:TBooleans;
  end;

  TScriptLine=record
    Keyword:Integer;
    Args:TScriptString;
  end;

  TScriptLines=array of TScriptLine;

  TParsedScript=record
    ShortName,LongName:string;
    Source:TStringList;
    Variables:TSimpleStrings;          //all variable names that have a grabv
    Params:TSimpleStrings;             //all variable names without a grabv
    Fields:TSimpleStrings;             //all fields referred by grabf
    ScriptLines:TScriptLines;
  end;

  //Results of running a set of scripts
  TScriptResults=record
    FieldNames,FieldVals:TSimpleStrings;
  end;

  { TScriptManager }

  TScriptManager=class
    private
      FParsedScripts:array of TParsedScript;

      //running environment
      FVerbose:Boolean;                 //Set to true to report all steps;
      FParsingError:Boolean;            //Set to false by SetupScript, true on error
      FVarVals:TSimpleStrings;          //created by SetupScript, edited on grabv
      FFieldVals:TSimpleStrings;        //created by SetupScript, edited on grabf
      FParamVals:TSimpleStrings;        //created by SetupScript, not edited
      FText:string;                     //the text is set on each POST



      function TrimSpaces(S:string):string;
      function ParseString(S:string):TScriptString;
      function ParseCode(Sl:TStringList):TScriptLines;
      function IDKeyWord(s:string):Integer;
      procedure ClearScripts;

      //Builds a string from parts and variables, starting at part Start
      function BuildString(ScriptIx,Start:Integer;const Line:TScriptString):string;

      function CommandPost(Ix:Integer;const Line:TScriptString;
        const Errors:TStrings):Boolean;

      //generic grab, for both grab commands, returns grabbed string and uptades
      //FText. Reports error if end string not found
      function GrabX(Ix:Integer;const Line:TScriptString;Errors:TStrings):string;
      function CommandGrabF(Ix:Integer;const Line:TScriptString;
        const Errors:TStrings):Boolean;
      function CommandGrabV(Ix:Integer;const Line:TScriptString;
        const Errors:TStrings):Boolean;
      function CommandDelTo(Ix:Integer;const Line:TScriptString;
        const Errors:TStrings):Boolean;

      //Set running environment for script Ix, and chek params
      function SetupScript(Ix:Integer;
        const ParamNames,ParamVals:TSimpleStrings;
        const Errors:TStrings):Boolean;


      //processes the script to find all identifiers
      procedure ListIdentifiers(var Script:TParsedScript);

      //returns the identifiers in the script line
      function GetIdentifiers(Line:TScriptString):TSimpleStrings;

      //Runs one script. Returns false if failed, true otherwise
      function RunAScript(Ix: Integer; const Errors:TStrings):Boolean;


    public
      property Verbose:Boolean read FVerbose write FVerbose;
      constructor Create;
      procedure ReadFolder(Folder:string);

      //returns false if there is a parsing error
      function Reload(Ix:Integer):Boolean;

      procedure ListSource(Ix:Integer; Sl:TStrings);

      //short names are expected to be unique
      function IxByShortName(ShortName:string):Integer;

      //lists in the format Shortname: longname
      procedure ListNames(Sl:TStrings);


      function AllFields(ScriptIxs:TIntegers):TSimpleStrings;
      function AllParameters(ScriptIxs:TIntegers):TSimpleStrings;overload;
      function AllParameters:TSimpleStrings;overload;
      procedure UpdateAndSave(Ix:Integer;APath:string;ASource:TStrings);


      //Names: the shortnames of the scripts to run
      //Params: the names of variables to set
      //Values: the corresponding values (same length as params)
      //Errors: a stringlist for the error reports
      function RunScripts(Ixs:TIntegers; Params,Values:TSimpleStrings;Errors:TStrings):TScriptResults;
    end;


implementation

{ TScriptManager }

function TScriptManager.TrimSpaces(S: string): string;
begin
  while (S<>'') and (S[1]<=' ') do Delete(S,1,1);
  while (S<>'') and (S[Length(s)]<=' ') do Delete(S,Length(S),1);
  Exit(S);
end;

function TScriptManager.ParseString(S: string): TScriptString;

var
  t:string;
  ix:Integer;

begin
  Result.Parts:=nil;
  Result.IsId:=nil;
  //first element is always identifier, followed by space
  if Pos(' ',S)>0 then
    begin
    AddToArray(True,Result.IsID);
    AddToArray(Copy(S,1,Pos(' ',S)-1),Result.Parts);
    Delete(S,1,Pos(' ',S));
    end;

  repeat
    S:=TrimSpaces(S);
    if S<>'' then
      begin
      if S[1]='''' then  //one quote mark
        begin
        Delete(S,1,1);
        AddToArray(Copy(S,1,Pos('''',s)-1),Result.Parts);
        AddToArray(False,Result.IsID);
        Delete(S,1,Pos('''',s));
        end
      else
        begin
        //find next quote mark or end of string
        ix:=Pos('''',S)-1;
        if ix<0 then ix:=Length(S);

        t:=Copy(S,1,ix);
        AddToArray(TrimSpaces(t),Result.Parts);
        AddToArray(True,Result.IsID);
        Delete(S,1,ix);             //delete up to quote, but not the quote mark
        end;
      end;
  until S='';
end;

function TScriptManager.ParseCode(Sl: TStringList): TScriptLines;

var
  f:Integer;
  s:string;
  line:TScriptLine;

begin
  Result:=nil;
  for f:=0 to Sl.Count-1 do
    begin
    s:=Sl.Strings[f];
    if s<>'' then
      begin
      line.Args:=ParseString(s);
      if line.Args.Parts<>nil then
        begin
        line.Keyword:=IDKeyWord(line.Args.Parts[0]);
        if line.KeyWord>=0 then
          begin
          SetLength(Result,Length(Result)+1);
          Result[High(Result)]:=line;
          end;
        end;
      end;
    end;
end;

function TScriptManager.IDKeyWord(S: string): Integer;
begin
  Result:=High(KeyWords);
  S:=UpperCase(S);
  while (Result>=0) and (KeyWords[Result]<>S) do
    Dec(Result);
end;

procedure TScriptManager.ClearScripts;

var f:Integer;

begin
  for f:=0 to High(FParsedScripts) do
    FParsedScripts[f].Source.Free;
  FParsedScripts:=nil;
end;

function TScriptManager.BuildString(ScriptIx, Start: Integer;
  const Line: TScriptString):string;
//looks up variable values to build string

var
  f:Integer;
  ix:Integer;

begin
  Result:='';
  for f:=Start to High(Line.Parts) do
    if Line.IsID[f] then
      begin
      //check if variable or param
      ix:=IndexInArray(Line.Parts[f],FParsedScripts[ScriptIx].Variables);
      if ix>=0 then Result:=Result+FVarVals[ix]
      else
        begin
        ix:=IndexInArray(Line.Parts[f],FParsedScripts[ScriptIx].Params);
        Result:=Result+FParamVals[ix]
        end;
      end
    else Result:=Result+Line.Parts[f];
end;

function TScriptManager.CommandPost(Ix: Integer; const Line: TScriptString;
  const Errors: TStrings): Boolean;

var
  HTTP: THTTPSend;
  s:string;
  l:TStringList;
begin
  Result:=True;
  HTTP := THTTPSend.Create;
  HTTP.Headers.Clear;
  l:=TStringList.Create;
  //each post clears all previous text
  FText:='';
  //get the url string, replacing variables
  s:=BuildString(Ix,1,Line);
  try
  if HTTP.HTTPMethod('GET',s) then
    begin
    l.loadfromstream(HTTP.Document);
    FText:=l.Text;
    //report post
    if FVerbose then
      Errors.Add('Posted to '+s);
    end;
  finally
    HTTP.Free;
    l.Free;
  end;

end;

function TScriptManager.GrabX(Ix: Integer; const Line: TScriptString;
  Errors: TStrings): string;

var
  endplace:string;
  p:Integer;

begin
  Result:='';
  //parts 0 and 1 are the command and the field or variable to get
  //the rest defines the end of the string, if any
  endplace:=BuildString(Ix,2,Line);
  if endplace='' then
    begin
    Result:=FText;
    FText:='';
    end
  else
    begin
    p:=Pos(endplace,FText);
    if p>0 then
        begin
        Result:=Copy(FText,1,p-1);
        Delete(FText,1,p-1);
        end
    else
      begin
      Errors.Add(FParsedScripts[Ix].ShortName+', Warning: '+endplace+' not found');
      FParsingError:=True;
      end;
    end;
end;

function TScriptManager.CommandGrabF(Ix: Integer; const Line: TScriptString;
  const Errors: TStrings): Boolean;

var
  s:string;
  fix:Integer;

begin
  Result:=True;                                  //TO DO: some error checking
  s:=GrabX(Ix,Line,Errors);
  //find the field
  fix:=IndexInArray(Line.Parts[1],FParsedScripts[Ix].Fields);
  FFieldVals[fix]:=s;
  if FVerbose then
    Errors.Add('Grabbed '+s+' to field '+FParsedScripts[Ix].Fields[fix]);
end;

function TScriptManager.CommandGrabV(Ix: Integer; const Line: TScriptString;
  const Errors: TStrings): Boolean;
var
  s:string;
  fix:Integer;

begin
  Result:=True;                                  //TO DO: some error checking
  s:=GrabX(Ix,Line,Errors);
  //find the field
  fix:=IndexInArray(Line.Parts[1],FParsedScripts[Ix].Variables);
  FVarVals[fix]:=s;
  if FVerbose then
    Errors.Add('Grabbed '+s+' to variable '+FParsedScripts[Ix].Variables[fix]);

end;

function TScriptManager.CommandDelTo(Ix: Integer; const Line: TScriptString;
  const Errors: TStrings): Boolean;

var
  s:string;
  dix:Integer;

begin
  Result:=True;                                  //TO DO: some error checking
  //part 0 is the command
  s:=BuildString(Ix,1,Line);
  dix:=Pos(s,FText);
  if dix>0 then
    begin
    Delete(FText,1,dix+Length(s)-1);
    if FVerbose then
      Errors.Add('Deleted to '+s);
    end
  else
    begin
    Errors.Add('Warning: '+s+' not found on DelTo');
    FParsingError:=True;
    Result:=False;                                //is this redundant?
    end;
end;

function TScriptManager.SetupScript(Ix:Integer;
        const ParamNames,ParamVals:TSimpleStrings;
        const Errors:TStrings):Boolean;

//sets variable and field value arrays
//checks if all parameters are defined

var
  f:Integer;

begin
  Result:=True;
  FParsingError:=False;
  with FParsedScripts[Ix] do
    begin
    //set and check parameter values
    SetLength(FParamVals,Length(Params));
    for f:=0 to High(Params) do
      begin
      ix:=IndexInArray(Params[f],ParamNames);
      if ix<0 then
        begin
        //missing parameter, abort
        Errors.Add(ShortName+', Fatal error:'+Params[f]+' undefined');
        Exit(False);
        end
      else FParamVals[f]:=ParamVals[ix];
      end;
    //initialize variables and fields
    SetLength(FVarVals,Length(Variables));
    for f:=0 to High(FVarVals) do FVarVals[f]:='';
    SetLength(FFieldVals,Length(Fields));
    for f:=0 to High(FFieldVals) do FFieldVals[f]:='';
    end;
end;

procedure TScriptManager.ListIdentifiers(var Script: TParsedScript);
//runs through all parsed script lines to get variables, parameters (ungrabbed
//variables), fields and the long name for the script

var
  f:Integer;
  grabbed,called,tmp:TSimpleStrings;

begin
  Script.Variables:=nil;
  Script.Params:=nil;
  Script.Fields:=nil;
  Script.LongName:='';
  grabbed:=nil;     // grabbed variables
  called:=nil;      // called variables

  //list all fields, called variables and grabbed variables
  for f:=0 to High(Script.ScriptLines) do
    begin
    //No array checking. Script is to be discarded on exception
    //(see ReadFolder procedure)
    //Args[0] on scriptlines is always the keyword
    with Script.ScriptLines[f] do
    case KeyWord of
      kwName:Script.LongName:=Args.Parts[1];
      kwPost,kwDelTo:AppendUniquesToArray(GetIdentifiers(Args),called);
      kwGrabF:
        begin
        //Part[1] must be the field
        AddUniqueToArray(Args.Parts[1],Script.Fields);
        //the rest may contain calls to variables
        tmp:=GetIdentifiers(Args);
        DeleteFromArray(tmp,0,2); //delete keyword and field;
        AppendUniquesToArray(tmp,called);
        end;
      kwGrabV:
        begin
        //Part[1] must be the variable
        AddUniqueToArray(Args.Parts[1],grabbed);
        //the rest may contain calls to variables
        tmp:=GetIdentifiers(Args);
        DeleteFromArray(tmp,0,2); //delete keyword and field;
        AppendUniquesToArray(tmp,called);
        end;
      end;
    end;

  //merges grabbed and called into params and variables of the script
  //note: variables that are grabbed and not called are ignored
  //and are not stored on execution.
  for f:=0 to High(called) do
    begin
      if IndexInArray(called[f],grabbed)>=0 then
        AddUniqueToArray(called[f],Script.Variables)
      else AddUniqueToArray(called[f],Script.Params);
    end;

end;

function TScriptManager.GetIdentifiers(Line: TScriptString): TSimpleStrings;

var
  f:Integer;

begin
  Result:=nil;
  for f:=1 to High(Line.Parts) do
    if Line.IsID[f] then
      AddUniqueToArray(Line.Parts[f],Result);
end;

function TScriptManager.RunAScript(Ix: Integer; const Errors:TStrings):Boolean;

var
  f:Integer;

begin
  Result:=True;
  with FParsedScripts[Ix] do
    for f:=0 to High(ScriptLines) do
      with ScriptLines[f] do
        begin
        case KeyWord of
          kwPost:Result:=CommandPost(ix,Args,Errors);
          kwDelTo:Result:=CommandDelTo(ix,Args,Errors);
          kwGrabF:Result:=CommandGrabF(ix,Args,Errors);
          kwGrabV:Result:=CommandGrabV(ix,Args,Errors);
        end;
        if not Result then Break; //TO DO: this should not always break
        end;
end;

constructor TScriptManager.Create;
begin
  inherited;
  FParsedScripts:=nil;
  //For debugging
  FVerbose:=True;
end;

procedure TScriptManager.ReadFolder(Folder: string);

var
  sr:TSearchRec;
  path:string;
  ps:TParsedScript;

begin
  ClearScripts;
  path:=AppendPathDelim(Folder);
  if FindFirst(path+'*.'+ScriptExt,faAnyFile,sr)=0 then
      repeat
      ps.Source:=TStringList.Create;
      SetLength(FParsedScripts,Length(FParsedScripts)+1);
      try
        ps.ShortName:=ChangeFileExt(sr.name,''); //no extension
        ps.Source.LoadFromFile(path+sr.Name);
        ps.ScriptLines:=ParseCode(ps.Source);
        ListIdentifiers(ps);
        FParsedScripts[High(FParsedScripts)]:=ps;
      except
        //undo allocations if something goes wrong with the parsing
        ps.Source.Free;
        SetLength(FParsedScripts,Length(FParsedScripts)-1);
      end;
      until FindNext(sr)<>0;
  FindClose(sr);
end;

function TScriptManager.Reload(Ix: Integer):Boolean;

var
  path:string;
begin
  Result:=True;
  path:=AppendPathDelim(ScriptFolder);
  with FParsedScripts[Ix] do
    try
    Source.LoadFromFile(path+ShortName+'.'+ScriptExt);
    ScriptLines:=ParseCode(Source);
    ListIdentifiers(FParsedScripts[Ix]);
    except
      //parsing error...
      Result:=False;
    end;
end;

procedure TScriptManager.ListSource(Ix:Integer; Sl:TStrings);

begin
  Sl.Clear;
  if Ix>=0 then
    Sl.Assign(FParsedScripts[ix].Source);
end;

function TScriptManager.IxByShortName(ShortName: string): Integer;

var f:Integer;

begin
  for f:=0 to High(FParsedScripts) do
    if FParsedScripts[f].ShortName=ShortName then Exit(f);
  Exit(-1);
end;

procedure TScriptManager.ListNames(Sl: TStrings);

var f:Integer;

begin
  Sl.Clear;
  for f:=0 to High(FParsedScripts) do
    with FParsedScripts[f] do
      Sl.Add(ShortName+ScriptNameSep+LongName);
end;

function TScriptManager.AllFields(ScriptIxs: TIntegers): TSimpleStrings;

var f:Integer;

begin
  Result:=nil;
  for f:=0 to High(ScriptIxs) do
    AppendUniquesToArray(FParsedScripts[ScriptIxs[f]].Fields,Result);
end;

function TScriptManager.AllParameters(ScriptIxs: TIntegers): TSimpleStrings;
var f:Integer;

begin
  Result:=nil;
  for f:=0 to High(ScriptIxs) do
    AppendUniquesToArray(FParsedScripts[ScriptIxs[f]].Params,Result);
end;

function TScriptManager.AllParameters: TSimpleStrings;

var f:Integer;

begin
  Result:=nil;
  for f:=0 to High(FParsedScripts) do
    AppendUniquesToArray(FParsedScripts[f].Params,Result);
end;

procedure TScriptManager.UpdateAndSave(Ix:Integer;APath:string;ASource:TStrings);

begin
  if Ix>=0 then
    with FParsedScripts[Ix] do
      begin
      Source.Assign(ASource);
      ScriptLines:=ParseCode(Source);
      ListIdentifiers(FParsedScripts[Ix]);
      Source.SaveToFile(AppendPathDelim(APath)+ShortName+'.'+ScriptExt);
      end;
end;

function TScriptManager.RunScripts(Ixs:TIntegers; Params, Values: TSimpleStrings;
  Errors: TStrings): TScriptResults;

var
  f,g,ix,fix:Integer;

begin
  Result.FieldNames:=nil;
  Result.FieldVals:=nil;
  for f:=0 to High(Ixs) do
    begin
    ix:=Ixs[f];
    if SetupScript(ix,Params,Values,Errors) then
      RunAScript(ix,Errors);

    //add fields to result
    if not FParsingError then
    for g:=0 to High(FFieldVals) do
      begin
      //get field index, or add field
      fix:=IndexInArray(FParsedScripts[ix].Fields[g],Result.FieldNames);
      if fix<0 then
        //New field
        begin
        AddToArray(FParsedScripts[ix].Fields[g],Result.FieldNames);
        AddToArray(FFieldVals[g],Result.FieldVals);
        end
      else
        //replace field
        //TO DO: Find some priority system to determine how repeated fields are
        //filled
        Result.FieldVals[fix]:=FFieldVals[g];
      //fix html special characters
      FixHtmlStrings(Result.FieldVals);
      end
    else Errors.Add(FParsedScripts[ix].ShortName+': all fields ignored due to parsing errors.');
    end;
end;

end.

