{ **********************************************************************
    This file is part of the "Sorteador" program

    About form unit

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
unit FAbout;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls, LCLIntf;

type
  TFormAbout = class(TForm)
    ImageIcon: TImage;
    LbProgramTitle: TLabel;
    BtClose: TButton;
    LbAutor: TLabel;
    LbMail: TLabel;
    LbVersion: TLabel;
    LbCredits: TLabel;
    procedure LbMailClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure LbVersionClick(Sender: TObject);
  private

  public

  end;

var
  FormAbout: TFormAbout;

implementation

{$R *.lfm}

{ TFormAbout }

procedure TFormAbout.LbMailClick(Sender: TObject);
begin
  OpenURL('mailto:' + LbMail.Caption + '?subject=' + LbProgramTitle.Caption + '&body=');
end;

procedure TFormAbout.FormCreate(Sender: TObject);
begin
  ImageIcon.Picture.Icon.Assign(Application.Icon);
end;

procedure TFormAbout.LbVersionClick(Sender: TObject);
begin
  OpenURL('https://github.com/maico-smaniotto/sorteador/releases');
end;

end.

