program pinaker;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, MainForm, eanthirteen, basetypes, imageprocessor, pinakerconfig, 
scripts, htmlfix;

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TPinakerForm, PinakerForm);
  Application.Run;
end.

