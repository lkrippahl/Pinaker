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
To do:
  Checksum validation in eanthirteen
  Help.
*******************************************************************************}
unit MainForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, ComCtrls, Menus, imageprocessor, httpsend, basetypes, pinakerconfig,
  LCLProc, LazHelpHTML, UTF8Process, PopupNotifier,eanthirteen;

const
  //ISBN record states
  stateOK=0;
  stateInvalid=1;
  stateChecksumError=2;

type

  { TPinakerForm }

  TPinakerForm = class(TForm)
    OpenDialog1: TOpenDialog;
    SaveDialog1: TSaveDialog;
    UpperCaseCb: TCheckBox;
    RetrieveBt: TButton;
    SkipFilesCb: TCheckBox;
    CompileBt: TButton;
    FixISBNEd: TEdit;
    KeyEd: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    ImagePb: TPaintBox;
    ValidsLbl: TLabel;
    InvalidsLbl: TLabel;
    Label5: TLabel;
    IncorrectLb: TListBox;
    CorrectLb: TListBox;
    ProgressLbl: TLabel;
    ProcessBt: TButton;
    FolderEd: TEdit;
    ProgressBar: TProgressBar;
    procedure RetrieveBtClick(Sender: TObject);
    procedure CompileBtClick(Sender: TObject);
    procedure FixISBNEdKeyPress(Sender: TObject; var Key: char);
    procedure FormCreate(Sender: TObject);
    procedure IncorrectLbClick(Sender: TObject);
    procedure ProcessBtClick(Sender: TObject);
  private
    { private declarations }
    FIsbns:TDecodedRecs;

    //for each listbox, keep the index for the FIsbns array
    FCorrectIxs,FIncorrectIxs:TIntegers;

    function CountFiles(Path:string):Integer;

  public
    {public declarations }
    procedure RefreshLists;
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
      end;
  ValidsLbl.Caption:=IntToStr(Length(FCorrectIxs))+' valid ISBN';
  InvalidsLbl.Caption:=IntToStr(Length(FCorrectIxs))+' invalid ISBN';
end;

procedure TPinakerForm.RetrieveBtClick(Sender: TObject);

var
  HTTP: THTTPSend;
  l: tstringlist;
  f:Integer;
  s,path,filename:string;
  ok,bad,skip:Integer;

begin
  if FCorrectIxs=nil then Exit;  //To avoid crashing the progressbar
  HTTP := THTTPSend.Create;
  l := TStringList.create;
  ProgressBar.Position:=0;
  ProgressBar.Max:=Length(FCorrectIxs);
  path:=AppendPathDelim(FolderEd.Text);

  //counters for successful or unsuccessful retrieved, and skipped
  ok:=0;
  bad:=0;
  skip:=0;

  try
    for f:=0 to High(FCorrectIxs) do
      begin
      filename:=path+FIsbns[FCorrectIxs[f]].Decoded+'.xml';
      if SkipFilesCb.Checked and FileExists(filename) then
        Inc(skip)
      else
        begin
        s:='http://isbndb.com/api/books.xml?access_key='+KeyEd.Text+
          '&index1=isbn&value1='+FIsbns[FCorrectIxs[f]].Decoded;
        DebugLn(s);
        HTTP.Headers.Clear;
        if HTTP.HTTPMethod('GET',s) then
          begin
          l.loadfromstream(Http.Document);
          l.SaveToFile(filename);
          Inc(ok);
          end
        else
          begin
          inc(bad);
          end;
        end;
      ProgressLbl.Caption:=IntToStr(ok)+' retrieved, '
                        +IntToStr(skip)+' skipped, of '
                        +IntTostr(Length(FCorrectIxs))+'. ('
                        +IntToStr(bad)+' failed)';
      ProgressBar.Position:=ProgressBar.Position+1;
      Application.ProcessMessages;
      end;
  finally
    HTTP.Free;
    l.Free;
    ProgressBar.Position:=0;
  end;
end;

procedure TPinakerForm.CompileBtClick(Sender: TObject);

var
  sr:TSearchRec;
  path,s:string;
  filecount:Integer;
  l,csv:TStringList;

function Flatten(s:string):string;
//Flattens the xml fields into a csv string


var f:Integer;

begin
  Result:='';
  //title
  Delete(s,1,Pos('<Title',s));
  Delete(s,1,Pos('>',s));
  Result:=Result+Copy(s,1,Pos('<',s)-1);
  //titlelong
  Delete(s,1,Pos('<Title',s));
  Delete(s,1,Pos('>',s));
  Result:=Result+#9+Copy(s,1,Pos('<',s)-1);
  //Autors
  Delete(s,1,Pos('<Authors',s));
  Delete(s,1,Pos('>',s));
  Result:=Result+#9+Copy(s,1,Pos('<',s)-1);
  //Publisher
  Delete(s,1,Pos('<Publishe',s));
  Delete(s,1,Pos('>',s));
  Result:=Result+#9+Copy(s,1,Pos('<',s)-1);

  //quick fix for line breaks
  s:=Result;
  Result:='';
  for f:=1 to Length(s) do
    if s[f]=#10 then Result:=Result+'; '
    else if s[f]<>#13 then Result:=Result+s[f];
end;

begin
  SaveDialog1.InitialDir:=FolderEd.Text;
  SaveDialog1.Filter:='Csv files|*.csv';
  if SaveDialog1.Execute then
    begin
    l:=TStringList.Create;
    csv:=TStringList.Create;
    csv.Add('Title'+#9+'Long Title'+#9+'Authors'+#9+'Publisher');
    try
    //count the files and setup progress bar
    path:=AppendPathDelim(FolderEd.Text);
    filecount:=CountFiles(path+'*.xml');

    if filecount=0 then
      begin
      ProgressLbl.Caption:='No files found...';
      Exit;
      end;

    ProgressBar.Max:=filecount;
    ProgressBar.Position:=0;

    //process the xml files
    if FindFirst(path+'*.xml',faAnyFile,sr)=0 then
      repeat
      l.LoadFromFile(path+sr.Name);
      csv.Add(Flatten(l.Text));
      ProgressBar.Position:=ProgressBar.Position+1;
      ProgressLbl.Caption:=sr.Name;
      Application.ProcessMessages;
      until FindNext(sr)<>0;
    FindClose(sr);
    ProgressBar.Position:=0;
    csv.SaveToFile(SaveDialog1.FileName);
    ProgressLbl.Caption:='CSV file saved';
  finally

  l.Free;
  csv.Free;
  end;
  end;
end;

procedure TPinakerForm.FixISBNEdKeyPress(Sender: TObject; var Key: char);

var
  s:string;
  oldix:Integer;

begin
  if Key=#13 then
    begin
    Key:=#0;                                  //ignore this keystroke
    s:=FixISBNEd.Text;
    if (IncorrectLb.ItemIndex>=0) then        //only if one selected
      begin
      if EAN13State(s)=stateOK then
        begin
        oldix:=IncorrectLb.ItemIndex;

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

var
  l:TStringList;

begin
  if FileExists(ConfigFolder+PathDelim+'pinakerdata.txt') then
    begin
    l:=TStringList.Create;
    l.LoadFromFile(ConfigFolder+PathDelim+'pinakerdata.txt');
    try
      FolderEd.Text:=l.Strings[0];
      KeyEd.Text:=l.Strings[1];
    except
      ShowMessage('There was a problem reading the mydata.txt file');
    end;
    end;
end;

procedure TPinakerForm.IncorrectLbClick(Sender: TObject);

var
  jpg:TJpegImage;
  ix:Integer;
  xs,ys:Single;

begin
  if (Sender=IncorrectLb) and (IncorrectLb.ItemIndex>=0) then
    ix:=FIncorrectIxs[IncorrectLb.ItemIndex]
  else if (Sender=CorrectLb) and (CorrectLb.ItemIndex>=0) then
    ix:=FCorrectIxs[CorrectLb.ItemIndex]
  else Exit;  //if no item selected in listboxes

  jpg:=TJpegImage.Create;
  jpg.LoadFromFile(FIsbns[ix].FileName);

  //scale
  xs:=ImagePb.Width/jpg.Width;
  ys:=ImagePb.Height/jpg.Height;

  ImagePb.Canvas.StretchDraw(Rect(0,0,ImagePb.Width-1,ImagePb.Height-1),jpg);
  ImagePb.Canvas.Pen.Color:=clRed;
  ImagePb.Canvas.MoveTo(Round(FIsbns[ix].x1*xs),Round(FIsbns[ix].y1*ys));
  ImagePb.Canvas.LineTo(Round(FIsbns[ix].x2*xs),Round(FIsbns[ix].y2*ys));
  DebugLn(IntToStr(Round(FIsbns[ix].x1*xs)));
  DebugLn(IntToStr(Round(FIsbns[ix].x2*xs)));
  DebugLn(IntToStr(Round(FIsbns[ix].y1*ys)));
  DebugLn(IntToStr(Round(FIsbns[ix].y2*ys)));

  jpg.Free;

end;

end.

