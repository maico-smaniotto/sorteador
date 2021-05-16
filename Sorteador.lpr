{ **********************************************************************
    This file is part of the "Sorteador" program

    Program entry point

    Author   : Maico Smaniotto
    Created  : May, 2021
    Contact  : maicosmaniotto@yahoo.com.br
    Language : Object Pascal
    Compiler : Free Pascal v3.2.0 up
    Requires : Lazarus Component Library (LCL)

    Icon made by smalllikeart from www.flaticon.com

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY
  ********************************************************************** }
program Sorteador;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, FMain, FAbout
  { you can add units after this };

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Scaled := True;
  Application.MainFormOnTaskBar := True;
  Application.Initialize;
  Application.CreateForm(TFormMain, FormMain);
  Application.Run;
end.

