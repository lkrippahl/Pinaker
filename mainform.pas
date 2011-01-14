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
To do: Checksum validation in eanthirteen
*******************************************************************************}
unit MainForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, ComCtrls, imageprocessor, httpsend, basetypes;

const
  //ISBN record states
  stateOK=0;
  stateInvalid=1;
  stateChecksumError=2;

type
  TIsbnRec=record
    Isbn:string;
    State:Integer;
    FileName:string;
  end;

  TIsbnArray=array of TIsbnRec;

  { TPinakerForm }

  TPinakerForm = class(TForm)
    Button1: TButton;
    FixISBNEd: TEdit;
    KeyEd: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    ImagePb: TPaintBox;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    IncorrectLb: TListBox;
    CorrectLb: TListBox;
    ProgressLbl: TLabel;
    ProcessBt: TButton;
    FolderEd: TEdit;
    ProgressBar: TProgressBar;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ProcessBtClick(Sender: TObject);
  private
    { private declarations }
    FIsbns:TIsbnArray;

    //for each listbox, keep the index for the FIsbns array
    FCorrectIxs,FIncorrectIxs:TIntegers;

    procedure AddIsbn(Isbn,FileName:string);
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
  s:string;
  filecount:Integer;

begin
  FIsbns:=nil;
  //count the files and setup progress bar
  filecount:=0;
  if FindFirst(FolderEd.Text+'\*.jpg',faAnyFile,sr)=0 then
    repeat
    Inc(filecount);
    until FindNext(sr)<>0;
  FindClose(sr);
  ProgressBar.Position:=0;
  ProgressBar.Max:=filecount;

  //process the images
  if FindFirst(FolderEd.Text+'\*.jpg',faAnyFile,sr)=0 then
    repeat
    s:=EAN13FromJpg(FolderEd.Text+'\'+sr.Name);
    AddIsbn(s,FolderEd.Text+'\'+sr.Name);
    ProgressBar.Position:=ProgressBar.Position+1;
    ProgressLbl.Caption:=sr.Name;
    Application.ProcessMessages;
    until FindNext(sr)<>0;
  FindClose(sr);
  RefreshLists;
end;

procedure TPinakerForm.AddIsbn(Isbn, FileName: string);
begin
  SetLength(FIsbns, Length(FIsbns)+1);
  FIsbns[High(FIsbns)].Isbn:=Isbn;
  FIsbns[High(FIsbns)].FileName:=FileName;
  if (Isbn='') or (Pos('?',Isbn)>0) then
    FIsbns[High(FIsbns)].State:=stateInvalid
  else FIsbns[High(FIsbns)].State:=stateOK;
  //TO DO:Checksum

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
        CorrectLb.Items.Add(FIsbns[f].Isbn);
        AddToArray(f,FCorrectIxs);
        end;
      stateInvalid:
        begin
        IncorrectLb.Items.Add(FIsbns[f].Isbn+' (invalid string)');
        AddToArray(f,FIncorrectIxs);
        end;
      stateChecksumError:
        begin
        IncorrectLb.Items.Add(FIsbns[f].Isbn+' (checksum error)');
        AddToArray(f,FIncorrectIxs);
        end;
      end;
end;

procedure TPinakerForm.Button1Click(Sender: TObject);

var
  HTTP: THTTPSend;
  l: tstringlist;
  f:Integer;
  s:string;

begin
  HTTP := THTTPSend.Create;
  l := TStringList.create;
  try
    for f:=1 to High(FIsbns) do
      begin
      s:='http://isbndb.com/api/books.xml?access_key='+KeyEd.Text+
        '&index1=isbn&value1='+FIsbns[f].Isbn;
      HTTP.Headers.Clear;
      if HTTP.HTTPMethod('GET',s) then
        begin
        l.loadfromstream(Http.Document);
        l.SaveToFile(FolderEd.Text+'\'+FIsbns[f].Isbn+'.xml');
        end;
      end;
  finally
    HTTP.Free;
    l.Free
  end;
end;

procedure TPinakerForm.FormCreate(Sender: TObject);

var
  l:TStringList;

begin
  if FileExists('mydata.txt') then
    begin
    l:=TStringList.Create;
    l.LoadFromFile('mydata.txt');
    try
      FolderEd.Text:=l.Strings[0];
      KeyEd.Text:=l.Strings[1];
    except
      ShowMessage('There was a problem reading the mydata.txt file');
    end;
    end;
end;

end.

