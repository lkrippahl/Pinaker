{*******************************************************************************
                        This file is part of Pinaker.
                       This source code is public domain.
********************************************************************************
Author: Ludwig Krippahl
Date: 13.1.2011
Purpose:
  Pinaker main form
Requirements:
Revisions:
  18-1-2011: added the scripts page, save and load.
To do:
  Find a better place for the htmlcodes.txt, instead of the config folder
  Exception when image not found. Put "image not found" message
  Help.
*******************************************************************************}
unit MainForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, ComCtrls, Menus, imageprocessor, httpsend, basetypes, pinakerconfig,
  LCLProc, LazHelpHTML, UTF8Process, PopupNotifier, CheckLst, SynHighlighterAny,
  SynEdit, SynHighlighterPython, SynMemo, eanthirteen, INIFiles, scripts,htmlfix;


type

  { TPinakerForm }

  TPinakerForm = class(TForm)
    Button1: TButton;
    Button2: TButton;
    GroupBox1: TGroupBox;
    Label1: TLabel;
    LoadBt: TButton;
    FolderEd: TEdit;
    Label2: TLabel;
    ErrorsMm: TMemo;
    ScriptPrB: TProgressBar;
    RetrieveBt: TButton;
    SaveScriptBt: TButton;
    ScriptsClb: TCheckListBox;
    NewScriptBt: TButton;
    NewScriptEd: TEdit;
    GroupBox3: TGroupBox;
    DiscardScriptBt: TButton;
    ScriptVarEd: TEdit;
    GroupBox2: TGroupBox;
    Label3: TLabel;
    ParamsLb: TListBox;
    CorrectLb: TListBox;
    FixISBNEd: TEdit;
    ImagePb: TPaintBox;
    IncorrectLb: TListBox;
    InvalidsLbl: TLabel;
    Label5: TLabel;
    OpenDialog: TOpenDialog;
    PageControl1: TPageControl;
    ProcessBt: TButton;
    ProgressBar: TProgressBar;
    ProgressLbl: TLabel;
    SaveDialog: TSaveDialog;
    SelFolderBt: TButton;
    SourceSMm: TSynMemo;
    TabSheet1: TTabSheet;
    ProcessTs: TTabSheet;
    UpperCaseCb: TCheckBox;
    AppendResultsCb: TCheckBox;
    SkipScriptsCb: TCheckBox;
    ValidsLbl: TLabel;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FixISBNEdChange(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure LoadBtClick(Sender: TObject);
    procedure NewScriptBtClick(Sender: TObject);
    procedure NewScriptEdChange(Sender: TObject);
    procedure ParamsLbClick(Sender: TObject);
    procedure RetrieveBtClick(Sender: TObject);
    procedure FixISBNEdKeyPress(Sender: TObject; var Key: char);
    procedure FormCreate(Sender: TObject);
    procedure IncorrectLbClick(Sender: TObject);
    procedure ProcessBtClick(Sender: TObject);
    procedure SaveScriptBtClick(Sender: TObject);
    procedure ScriptsClbClick(Sender: TObject);
    procedure ScriptVarEdKeyPress(Sender: TObject; var Key: char);
    procedure SelFolderBtClick(Sender: TObject);
  private
    { private declarations }

    FSyn:TSynAnySyn; //highlighter for script code

    //Image scaling factors
    FScaleX,FScaleY:Single;

    //the current ISBN data
    FIsbns:TDecodedRecs;

    //for each listbox, keep the index for the FIsbns array
    FCorrectIxs,FIncorrectIxs:TIntegers;

    //Parameters for the scripts
    FParamNames,FParamValues:TSimpleStrings;//stored parameters
    FUsedScripts:TIntegers;                 //list of indexes scripts to run

    //Results
    FFieldNames:TSimpleStrings;             //names of the fields, from all scripts
                                            //First field is always "EAN-13"
    FEANList:TSimpleStrings;                //List of EAN-13 values to retrieve
    FFieldVals:array of TSimpleStrings;     //values for each EAN number

    FScriptMan:TScriptManager;

    procedure SaveResults(FileName:string);
    procedure LoadResults(FileName:string);

    function CountFiles(Path:string):Integer;
    procedure LoadConfig;
    procedure SaveConfig;

    //session save and load functions
    //TO DO: improve the format
    function ReadIsbnRec(Ix,LineIx:Integer;Sl:TStringList):Integer; //points to next line
    procedure WriteIsbnRec(Ix:Integer;Sl:TStringList);
    procedure SaveSession(FileName:string);
    procedure LoadSession(FileName:string);

    //Sets fields, script indexes,
    procedure SetUpScriptRun;
    //checks if has to run, runs and then updates data
    procedure RunScriptsForISBN(Ix:Integer);


  public
    {public declarations }
    procedure RefreshLists;
    procedure RefreshParams;
    procedure RefreshScriptList;
    procedure LoadScripts;
  end; 

var
  PinakerForm: TPinakerForm;

implementation

{$R *.lfm}

{ TPinakerForm }

procedure TPinakerForm.ProcessBtClick(Sender: TObject);

var
  sr:TSearchRec;
  path,s,ext:string;
  filecount:Integer;

begin
  FIsbns:=nil;

  //linux files are case sensitive
  if UppercaseCb.Checked then
    ext:='*.JPG'
  else ext:='*.jpg';

  //count the files and setup progress bar
  path:=AppendPathDelim(FolderEd.Text);
  filecount:=CountFiles(path+ext);

  //Exit if no files otherwise progressbar crashes...
  if filecount=0 then
    begin
    ProgressLbl.Caption:='No files found...';
    Exit;
    end;

  ProgressBar.Position:=0;
  ProgressBar.Max:=filecount;

  //process the images
  if FindFirst(path+ext,faAnyFile,sr)=0 then
    repeat
    AddToArray(EAN13FromJpg(path+sr.Name),FIsbns);
    ProgressBar.Position:=ProgressBar.Position+1;
    ProgressLbl.Caption:=sr.Name;
    Application.ProcessMessages;
    until FindNext(sr)<>0;
  FindClose(sr);
  ProgressBar.Position:=0;
  RefreshLists;
end;

procedure TPinakerForm.SaveScriptBtClick(Sender: TObject);

var
  scriptfile:string;
  ix:Integer;

begin
  ix:=ScriptsCLb.ItemIndex;
  if ix>=0 then
    begin
    //get filename from list and save
    //TO DO: this is messy; add update and sabe to FScriptManager, do this there
    scriptfile:=ScriptsClb.Items.Strings[ix];
    Delete(scriptfile,Pos(':',scriptfile),Length(scriptfile));
    scriptfile:=AppendPathDelim(ScriptFolder)+scriptfile+'.'+ScriptExt;
    SourceSMm.Lines.SaveToFile(scriptfile);
    LoadScripts;
    end;
end;

procedure TPinakerForm.ScriptsClbClick(Sender: TObject);
begin
  if ScriptsClb.ItemIndex>=0 then
    FScriptMan.ListSource(ScriptsClb.ItemIndex,SourceSMm.Lines);
end;

procedure TPinakerForm.ScriptVarEdKeyPress(Sender: TObject; var Key: char);
begin
  if (Key=#13) then
    begin
    if ParamsLb.ItemIndex>=0 then
      FParamValues[ParamsLb.ItemIndex]:=ScriptVarEd.Text;
    end;
end;

procedure TPinakerForm.SelFolderBtClick(Sender: TObject);

var dir:string;

begin
  if SelectDirectory('Image folder','',dir) then
    FolderEd.Text:=dir;
end;

function TPinakerForm.CountFiles(Path: string): Integer;

var sr:TSearchRec;

begin
  Result:=0;
  if FindFirst(Path,faAnyFile,sr)=0 then
    repeat
    Inc(Result);
    until FindNext(sr)<>0;
  FindClose(sr);
end;

procedure TPinakerForm.LoadConfig;

var
  ini:TINIFile;
  sl:TStringList;
  s:string;
  f:Integer;

begin
  if FileExists(ConfigFile) then
    begin
    ini:=TINIFile.Create(ConfigFile);
    UpperCaseCb.Checked:=ini.ReadBool('General','Uppercase JPG',True);
    AppendResultsCb.Checked:=ini.ReadBool('General','Append Results',True);
    SkipScriptsCb.Checked:=ini.ReadBool('General','Skip Unnecessary Scripts',True);
    FolderEd.Text:=ini.ReadString('General','Working Folder','');

    //read parameters for script
    sl:=TStringList.Create;
    ini.ReadSection('Parameters',sl);

    //this should not happen but, just in case, ignore params set with the
    //reserved EAN parameter
    if sl.IndexOf(EANReserved)>0 then
      sl.Delete(sl.IndexOf(EANReserved));

    SetLength(FParamNames,sl.Count);
    SetLength(FParamValues,sl.Count);
    for f:=0 to sl.Count-1 do
      begin
      FParamNames[f]:=sl.Strings[f];
      FParamValues[f]:=ini.ReadString('Parameters',sl.Strings[f],'');
      end;
    ini.Free;
    sl.Free;
    end;
  RefreshParams;
end;

procedure TPinakerForm.SaveConfig;

var
  ini:TINIFile;
  f:Integer;
begin
  ini:=TINIFile.Create(ConfigFile);
  ini.WriteBool('General','Uppercase JPG',UpperCaseCb.Checked);
  ini.WriteBool('General','Append Results',AppendResultsCb.Checked);
  ini.WriteBool('General','Skip Unnecessary Scripts',SkipScriptsCb.Checked);
  ini.WriteString('General','Working Folder',FolderEd.Text);

  //write script parameters
  for f:=0 to High(FParamNames) do
      ini.WriteString('Parameters',FParamNames[f],FParamValues[f]);

  ini.Free;
end;

function TPinakerForm.ReadIsbnRec(Ix,LineIx:Integer; Sl: TStringList):Integer;

var f:Integer;
    p:TPointPair;

begin
  with FIsbns[Ix] do
    begin
    Decoded:=Sl.Strings[LineIx];
    State:=StrToInt(Sl.Strings[LineIx+1]);
    FileName:=Sl.Strings[LineIx+2];
    f:=StrToInt(Sl.Strings[LineIx+3]);
    SetLength(Lines,f);
    LineIx:=LineIx+4;
    for f:=0 to High(Lines) do
      begin
      p.x1:=StrToInt(Sl.Strings[LineIx]);
      p.x2:=StrToInt(Sl.Strings[LineIx+1]);
      p.y1:=StrToInt(Sl.Strings[LineIx+2]);
      p.y2:=StrToInt(Sl.Strings[LineIx+3]);
      Lines[f]:=p;
      LineIx:=LineIx+4;
      end;
    end;
  Result:=LineIx;
end;

procedure TPinakerForm.WriteIsbnRec(Ix: Integer; Sl: TStringList);

var f:Integer;

begin
  with FIsbns[Ix] do
    begin
    Sl.Add(Decoded);
    Sl.Add(IntToStr(State));
    Sl.Add(FileName);
    Sl.Add(IntToStr(Length(Lines)));
    for f:=0 to High(Lines) do
      begin
      Sl.Add(IntToStr(Lines[f].x1));
      Sl.Add(IntToStr(Lines[f].x2));
      Sl.Add(IntToStr(Lines[f].y1));
      Sl.Add(IntToStr(Lines[f].y2));
      end;
    end;
end;

procedure TPinakerForm.SaveSession(FileName: string);
//saves current session (FISBNs, working folder)
//TO DO: this is a quick-and-dirty implementation. Think about a better format
//than just a list of strings (user editable, perhaps...)

var
  f:Integer;
  l:TStringList;

begin
  l:=TStringList.Create;
  l.Add(FolderEd.Text);
  l.Add(IntToStr(Length(Fisbns)));
  for f:=0 to High(FIsbns) do
    WriteIsbnRec(f,l);
  l.SaveToFile(FileName);
  l.Free;
end;

procedure TPinakerForm.LoadSession(FileName: string);

var
  f,lix:Integer;
  l:TStringList;

begin
  l:=TStringList.Create;
  l.LoadFromFile(FileName);
  FolderEd.Text:=l.Strings[0];
  f:=StrToInt(l.Strings[1]);
  SetLength(FIsbns,f);

  //Line index for first record. Remember to change if adding stuff before
  lix:=2;
  for f:=0 to High(FIsbns) do
    lix:=ReadIsbnRec(f,lix,l);        //keep lix pointing to next record
  l.Free;
  RefreshLists;
end;

procedure TPinakerForm.SetUpScriptRun;

var
  g,oldlen,lf,f:Integer;

begin
  //add missing fields
  AppendUniquesToArray(FScriptMan.AllFields(FUsedScripts),FFieldNames);

  //resize and initialize missing field values (data may be loaded already)
  lf:=Length(FFieldNames);
  for f:=0 to High(FFieldVals) do
    begin
    oldlen:=Length(FFieldVals[f]);
    if oldlen<lf then
      begin
      setLength(FFieldVals[f],lf);
      for g:=oldlen to lf-1 do
        FFieldVals[f,g]:='';
      end;
    end;
end;

procedure TPinakerForm.RunScriptsForISBN(Ix: Integer);

var
  pnames,pvalues:TSimpleStrings;
  scriptresult:TScriptResults;

procedure AddFields(Isbn:string;Fields:TScriptResults);

var f,ix,fix:Integer;

begin
  ix:=-1;
  //find the isbn number, assuming field 0 is Isbn-13 entry
  for f:=0 to High(FFieldVals) do
    if FFieldVals[f,0]=Isbn then
      begin
      ix:=f;
      Break;
      end;
  //If not found, it's a new one
  if ix<0 then
    begin
    SetLength(FFieldVals,Length(FFieldVals)+1);
    ix:=High(FFieldVals);
    SetLength(FFieldVals[ix],Length(FFieldNames));
    FFieldVals[ix,0]:=Isbn;
    for f:=1 to High(FFieldVals[ix]) do
      FFieldVals[ix,f]:='';
    end;

  //merge the fields
  for f:=0 to High(Fields.FieldNames) do
    begin

    //this should always find the field name. If not, it's mess enough to
    //deserve an exception...
    fix:=IndexInArray(Fields.FieldNames[f],FFieldNames);
    if FFieldVals[ix,fix]='' then
      FFieldVals[ix,fix]:=Fields.FieldVals[f];
    end;
end;

begin

  //Set parameters
  pnames:=nil;
  pvalues:=nil;
  AddToArray('ean13',pnames);
  AddToArray(FIsbns[Ix].Decoded,pvalues);
  AppendToArray(FParamNames,pnames);
  AppendToArray(FParamValues,pvalues);

  //Run
  ErrorsMm.Lines.Add('Running '+FIsbns[Ix].Decoded);
  scriptresult:=FScriptMan.RunScripts(FUsedScripts,pnames,pvalues,ErrorsMm.Lines);

  //MergeResults
  AddFields(FIsbns[Ix].Decoded,scriptresult);

end;

procedure TPinakerForm.SaveResults(FileName: string);
var
  f:Integer;
  csv:TStringList;

function FlattenStrings(Ss:TSimpleStrings):string;
//Flattens a TSimpleStrings array into a csv line

var f:Integer;

begin
  if Length(Ss)>0 then
    begin
    Result:='"'+Ss[0]+'"';
    for f:=1 to High(Ss) do
      Result:=Result+',"'+Ss[f]+'"';
    end;
end;

begin
  csv:=TStringList.Create;
  csv.Add(FlattenStrings(FFieldNames));
  for f:=0 to High(FFieldVals) do
    csv.Add(FlattenStrings(FFieldVals[f]));
  csv.SaveToFile(FileName);
  ProgressLbl.Caption:='CSV file saved';
  csv.Free;
end;

procedure TPinakerForm.LoadResults(FileName: string);

var
  ix:Integer;
  csv:TStringList;
  lines:array of TSimpleStrings;
  s:string;

function Expand(Line:string):TSimpleStrings;


function GrabField(var s:string):string;

var f:Integer;

begin
  //remove spaces
  while (s<>'') and (s[1]<=' ') do Delete(s,1,1);
  //double quote field
  if s[1]='"' then
    begin
    f:=2;
    while f<=Length(s) do
      begin
      if s[f]='"' then
        begin
        //two double quotes found
        if (f<Length(s)) and (s[f+1]='"') then
          begin
          Result:=Result+'"';
          Inc(f);             //jump to last double quote
          end
        else Break;           //found termination double quote
        end
      else Result:=Result+s[f];
      Inc(f);
      end;
    //remove this field
    Delete(s,1,f);

    //and next comma if more fields, or empty the string
    if Pos(',',s)>0 then Delete(s,1,Pos(',',s))
    else s:='';
    end
  else      //no quotes field
    begin
    if Pos(',',s)>0 then
      begin
      Result:=Copy(s,1,Pos(',',s)-1);
      Delete(s,1,Pos(',',s));
      end
    else
      begin
      Result:=s;
      s:='';
      end;
    end;
end;

begin
  Result:=nil;
  while Line<>'' do
    AddToArray(GrabField(Line),Result);
end;

function OpenQuotes(S:string):Boolean;
//checks if there is an odd number of " in string

var f:Integer;

begin
  Result:=False;
  for f:=1 to Length(S) do if S[f]='"' then Result:=not Result;
end;

begin
  csv:=TStringList.Create;
  csv.LoadFromFile(FileName);
  lines:=nil;
  try
  if csv.Count>0 then
    begin
    ix:=0;
    s:=csv.Strings[0];
    while ix<csv.Count do
      begin
      s:=csv.Strings[ix];
      while (ix<csv.Count) and (OpenQuotes(s)) do
        begin
        Inc(ix);
        s:=s+csv.Strings[ix];
        end;
      SetLength(lines,Length(lines)+1);
      lines[High(lines)]:=Expand(s);
      Inc(Ix);
      end;
    FFieldNames:=lines[0];            //first line in CSV has the field names
    SetLength(FFieldVals,Length(lines)-1);
    for ix:=1 to High(lines) do
      FFieldVals[ix-1]:=lines[ix];
    end;
  finally
  csv.Free;
  end;

end;

procedure TPinakerForm.RefreshLists;

var f:Integer;

begin
  CorrectLb.Items.Clear;
  IncorrectLb.Items.Clear;

  //clear indeces too
  FCorrectIxs:=nil;
  FIncorrectIxs:=nil;

  for f:=0 to High(FIsbns) do
      case FIsbns[f].State of
      stateOK:
        begin
        CorrectLb.Items.Add(FIsbns[f].Decoded);
        AddToArray(f,FCorrectIxs);
        end;
      stateInvalid:
        begin
        IncorrectLb.Items.Add(FIsbns[f].Decoded+' (invalid string)');
        AddToArray(f,FIncorrectIxs);
        end;
      stateChecksumError:
        begin
        IncorrectLb.Items.Add(FIsbns[f].Decoded+' (checksum error)');
        AddToArray(f,FIncorrectIxs);
        end;
      stateSuspect:
        begin
        IncorrectLb.Items.Add(FIsbns[f].Decoded+' (suspected UPC)');
        AddToArray(f,FIncorrectIxs);
        end;
      end;
  ValidsLbl.Caption:=IntToStr(Length(FCorrectIxs))+' valid ISBN';
  InvalidsLbl.Caption:=IntToStr(Length(FIncorrectIxs))+' invalid ISBN';
end;

procedure TPinakerForm.RefreshParams;

var f:Integer;

begin
  ParamsLb.Items.Clear;
  for f:=0 to High(FParamNames) do ParamsLb.Items.Add(FParamNames[f]);
end;

procedure TPinakerForm.RefreshScriptList;
begin
  FScriptMan.ListNames(ScriptsClb.Items);
end;

procedure TPinakerForm.LoadScripts;

var
  f,oldl,lp:Integer;
  allf:TSimpleStrings;

begin
  FScriptMan.ReadFolder(ScriptFolder);
  allf:=FScriptMan.AllParameters;
  oldl:=Length(FParamNames);                  //needed to initialize extras
  for f:=0 to High(allf) do
    if allf[f]<>EANReserved then              //skip the reserved parameter for ean13
      AddUniqueToArray(allf[f],FParamNames);
  lp:=Length(FParamNames);

  if oldl<lp then                             //new parameters
    begin
    SetLength(FParamValues,lp);
    for f:=oldl to lp-1 do
      FParamValues[f]:='';
    end;
  RefreshScriptList;
  RefreshParams;
end;

procedure TPinakerForm.RetrieveBtClick(Sender: TObject);

var
  f:Integer;
  s,path,filename:string;


begin
  ErrorsMm.Lines.Clear;
  if FCorrectIxs=nil then           //nothing to do
    begin
    ErrorsMm.Lines.Add('No valid ISBN numbers to process...');
    Exit;
    end;

  //Set scripts to use
  FUsedScripts:=nil;
  for f:=0 to ScriptsCLb.Items.Count-1 do
    if ScriptsClb.Checked[f] then AddToArray(f,FUsedScripts);

  if FUsedScripts=nil then           //nothing to do
    begin
    ErrorsMm.Lines.Add('No scripts selected...');
    Exit;
    end;

  //Empty Results
  FFieldNames:=nil;
  FFieldVals:=nil;

  //Select a file
  SaveDialog.InitialDir:=FolderEd.Text;
  SaveDialog.DefaultExt:='csv';
  SaveDialog.Filter:='Csv files|*.csv';
  if SaveDialog.Execute then
    begin
    //load previous results, if selected
    if FileExists(SaveDialog.FileName) and AppendResultsCb.Checked then
      begin
      LoadResults(SaveDialog.FileName);
      //rename file, just in case
      try
        RenameFile(SaveDialog.FileName,SaveDialog.FileName+'.old');
        ErrorsMm.Lines.Add('Old file renamed to '+
          ExtractFileName(SaveDialog.FileName)+'.old');
      except
        ErrorsMm.Lines.Add('Old file not renamed (.old already exists?)');
      end;
      end
    else
      //if not, create the ISBN-13 field as the first field
      AddToArray('ISBN-13',FFieldNames);

    //setup progress
    ScriptPrB.Position:=0;
    ScriptPrB.Max:=Length(FCorrectIxs);

    //merge script fields, initialize values
    try
      SetUpScriptRun;
      for f:=0 to High(FCorrectIxs) do
        begin
        Application.ProcessMessages;
        RunScriptsForISBN(FCorrectIxs[f]);
        ScriptPrB.Position:=SCriptPrb.Position+1;
        end;
    except
      ErrorsMm.Lines.Add('Aborted due to an exception.');
    end;
    SaveResults(SaveDialog.FileName);
    ErrorsMm.Lines.Add('Results saved to '+SaveDialog.FileName);
    end;
end;

procedure TPinakerForm.Button1Click(Sender: TObject);
begin
  SaveConfig;
end;

procedure TPinakerForm.Button2Click(Sender: TObject);

begin
  SaveDialog.Filter:='Pinaker session|*.pns';
  SaveDialog.DefaultExt:='pns';
  if SaveDialog.Execute then
    SaveSession(SaveDialog.FileName);
end;

procedure TPinakerForm.FixISBNEdChange(Sender: TObject);

var
  s:string;
  th,ix:Integer;


begin
  if IncorrectLb.ItemIndex>=0 then
    begin
    s:=FixISBNEd.Text;
    while Length(s)<13 do s:=s+' '; //paint over deleted characters
    ix:=FIncorrectIxs[IncorrectLb.ItemIndex];
    th:=ImagePb.Font.GetTextHeight(s);
    with FIsbns[ix] do
      begin
      if Lines<>nil then
        ImagePb.Canvas.TextOut(Round(Lines[0].x1*FScaleX),
          ImagePb.Height-3*th,s);
      end;
    end;
end;

procedure TPinakerForm.FormClose(Sender: TObject; var CloseAction: TCloseAction
  );
begin
  SaveConfig;
  FSyn.Free;
end;

procedure TPinakerForm.LoadBtClick(Sender: TObject);
begin
  OpenDialog.Filter:='Pinaker session|*.pns';
  if OpenDialog.Execute then
    LoadSession(OpenDialog.FileName);
end;

procedure TPinakerForm.NewScriptBtClick(Sender: TObject);

var sl:TStringList;

begin
  //redundant, checked in NewScriptEdChange, but better safe...
  if (NewScriptEd.Text<>'') and (FScriptMan.IxByShortName(NewScriptEd.Text)<0) then
    begin

    //create script file
    sl:=TStringList.Create;
    //TO DO: add some initial code here... ?
    sl.SaveToFile(AppendPathDelim(ScriptFolder)+NewScriptEd.Text+'.'+ScriptExt);
    sl.Free;

    //reload, and point to new script for editing
    LoadScripts;

    //the long name is empy, so only shortname+separator
    ScriptsClb.ItemIndex:=
      ScriptsClb.Items.IndexOf(NewScriptEd.Text+ScriptNameSep);

    NewScriptEd.Text:='';
    ScriptsClbClick(Sender);
    end;
end;

procedure TPinakerForm.NewScriptEdChange(Sender: TObject);
//Controls the new script button depending on the text

begin
  NewScriptBt.Enabled:=
    (NewScriptEd.Text<>'') and (FScriptMan.IxByShortName(NewScriptEd.Text)<0);
end;

procedure TPinakerForm.ParamsLbClick(Sender: TObject);
begin
  if ParamsLb.ItemIndex>=0 then
    ScriptVarEd.Text:=FParamValues[ParamsLb.ItemIndex];
end;

procedure TPinakerForm.FixISBNEdKeyPress(Sender: TObject; var Key: char);

var
  s:string;
  x,oldix:Integer;

begin
  oldix:=IncorrectLb.ItemIndex;
  if Key=#13 then
    begin
    Key:=#0;                                  //ignore this keystroke
    s:=FixISBNEd.Text;
    if (IncorrectLb.ItemIndex>=0) then        //only if one selected
      begin
      if Length(s)<=10 then                   //is ISBN-10
        begin
        s:=Copy(s,1,9);                       //remove check digit, if any
        s:='978'+s;                           //assume old isbn-10 always 978
        end;
      if Length(s)=12 then
        s:=s+CheckDigit(s);
      if EAN13State(s)=stateOK then
        begin
        //update record
        FIsbns[FIncorrectIxs[oldix]].Decoded:=s;
        FIsbns[FIncorrectIxs[oldix]].State:=Ean13State(s);

        RefreshLists;

        //go to next
        if Incorrectlb.Items.Count>oldix then
            IncorrectLb.ItemIndex:=oldix
        //or to first of no next
        else if IncorrectLb.Items.Count>0 then
            IncorrectLb.ItemIndex:=0;

        IncorrectLbClick(IncorrectLb);      //to refresh the image
        FixISBNEd.Text:='';
        end;
      end;
    end;
end;

procedure TPinakerForm.FormCreate(Sender: TObject);

procedure CreateSyn;

var f:Integer;

begin
  FSyn:=TSynAnySyn.Create(Self);
  for f:=0 to High(KeyWords) do FSyn.KeyWords.Add(KeyWords[f]);
  FSyn.KeyAttri.ForeGround:=clBlue;
  FSyn.StringAttri.ForeGround:=clTeal;
  SourceSMm.Highlighter:=FSyn;
end;

begin
  //DEBUG:
  //ErrorsMm.Lines.SaveToFile('htmlcodes.txt');
  //ErrorsMm.Lines.Clear;
  CreateSyn;          // This must be done programmatically because of
                      // version issues (?) with the lazarus package.
                      // Newer versions of SynAnySyn have a FrameEdges property
                      // on CommentAttri
  LoadConfig;
  FScriptMan:=TScriptManager.Create;
  LoadScripts;
  //initialize the Html decoding table
  LoadHtmlTable(AppendPathDelim(ConfigFolder)+'htmlcodes.txt');

  //DEBUG:
  //ExportTable(ErrorsMm.Lines);
end;

procedure TPinakerForm.IncorrectLbClick(Sender: TObject);

var
  jpg:TJpegImage;
  ix,f:Integer;

begin
  if (Sender=IncorrectLb) and (IncorrectLb.ItemIndex>=0) then
    begin
    ix:=FIncorrectIxs[IncorrectLb.ItemIndex];
    FixISBNEd.Text:='';
    FixISBNEd.SetFocus;
    end
  else if (Sender=CorrectLb) and (CorrectLb.ItemIndex>=0) then
    ix:=FCorrectIxs[CorrectLb.ItemIndex]
  else Exit;  //if no item selected in listboxes

  jpg:=TJpegImage.Create;
  jpg.LoadFromFile(FIsbns[ix].FileName);

  //scale
  FScaleX:=ImagePb.Width/jpg.Width;
  FScaleY:=ImagePb.Height/jpg.Height;
  ImagePb.Canvas.Font.Color:=clWhite;
  ImagePb.Canvas.Font.Size:=20;
  ImagePb.Canvas.Brush.Color:=clBlack;
  ImagePb.Canvas.StretchDraw(Rect(0,0,ImagePb.Width-1,ImagePb.Height-1),jpg);
  ImagePb.Canvas.TextOut(5,5,ExtractFileName(FIsbns[ix].FileName));
  with FIsbns[ix] do
    begin
    //points outlining the scan region
    for f:=0 to High(Lines)-1 do
      begin
      ImagePb.Canvas.Pixels[Round(Lines[f].x1*FScaleX),Round(Lines[f].y1*FScaleY)]:=clRed;
      ImagePb.Canvas.Pixels[Round(Lines[f].x2*FScaleX),Round(Lines[f].y2*FScaleY)]:=clRed;
      end;
    // a line in the final scan line
    ImagePb.Canvas.Pen.Color:=clRed;
    with Lines[High(Lines)] do
      begin
      ImagePb.Canvas.MoveTo(Round(x1*FScaleX),Round(y1*FScaleY));
      ImagePb.Canvas.LineTo(Round(x2*FScaleX),Round(y2*FScaleY));
      end;
    end;
  jpg.Free;
end;

end.

