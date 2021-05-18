{ **********************************************************************
    This file is part of the "Sorteador" program

    Main form unit

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
unit FMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls, Buttons, Windows;

type
  TFormMain = class(TForm)
    BtDraw: TButton;
    EditFrom: TEdit;
    EditTo: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Shape1: TShape;
    LbDrawnNumber: TLabel;
    LstNumbers: TListBox;
    Label4: TLabel;
    BtDeleteSelected: TButton;
    BtReset: TButton;
    BtAbout: TSpeedButton;
    procedure BtDeleteSelectedClick(Sender: TObject);
    procedure BtResetClick(Sender: TObject);
    procedure BtDrawClick(Sender: TObject);
    procedure LstNumbersSelectionChange(Sender: TObject; User: boolean);
    procedure LstNumbersClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure LstNumbersKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure BtAboutClick(Sender: TObject);
  private
    function ItemExists(AItemText: String): Boolean;
    procedure CheckDeleteEnabled;
  public

  end;

var
  FormMain: TFormMain;

implementation

uses FAbout;

{$R *.lfm}

{ TFormMain }

procedure TFormMain.BtDeleteSelectedClick(Sender: TObject);
begin
  LstNumbers.DeleteSelected;

  CheckDeleteEnabled;
end;

procedure TFormMain.BtResetClick(Sender: TObject);
begin
  LstNumbers.Clear;
  LbDrawnNumber.Caption := '';

  EditFrom.ReadOnly := False;
  EditTo.ReadOnly   := False;

  EditFrom.ShowHint := False;
  EditTo.ShowHint   := False;

  EditFrom.SetFocus;
end;

procedure TFormMain.BtDrawClick(Sender: TObject);
var
  I, Total, RangeStart, RangeEnd, RandomNumber: Integer;
begin
  try
    RangeStart := Min(StrToInt(EditFrom.Text), StrToInt(EditTo.Text));
    RangeEnd   := Max(StrToInt(EditFrom.Text), StrToInt(EditTo.Text));
  except
    Application.MessageBox('Informe um intervalo numérico válido.', 'Erro', MB_ICONERROR + MB_OK);
    if EditFrom.CanFocus then
      EditFrom.SetFocus;
    Exit;
  end;

  Total := RangeEnd - RangeStart + 1;

  if LstNumbers.Count = Total then
  begin
    LbDrawnNumber.Caption := '';
    Application.MessageBox('Todos os números foram sorteados.', 'Informação', MB_ICONINFORMATION + MB_OK);
    Exit;
  end;

  // Após o sorteio iniciado, não permite alterar o intervalo
  EditFrom.ShowHint := True;
  EditFrom.ReadOnly := True;
  EditTo.ShowHint   := True;
  EditTo.ReadOnly   := True;

  BtDraw.Enabled := False;
  BtDraw.Caption := 'Sorteando...';
  try
    for I := 1 to 5 do
    begin
      LbDrawnNumber.Font.Color := clMedGray;

      if LstNumbers.Count = Total then Exit;
      repeat
        RandomNumber := Random(Total) + RangeStart;
      until (not ItemExists(IntToStr(RandomNumber))) or (LstNumbers.Count = Total);
      LbDrawnNumber.Caption := IntToStr(RandomNumber);
      Application.ProcessMessages;
      Sleep(250);
    end;

    LbDrawnNumber.Font.Color := clBlue;
    LstNumbers.AddItem(LbDrawnNumber.Caption, nil);
    LstNumbers.TopIndex := LstNumbers.Count - 1;
  finally
    BtDraw.Enabled := True;
    BtDraw.Caption := 'Sortear [F5]';
    BtDraw.SetFocus;
    CheckDeleteEnabled;
  end;
end;

procedure TFormMain.LstNumbersSelectionChange(Sender: TObject; User: boolean);
begin
  BtDeleteSelected.Enabled := LstNumbers.SelCount <> 0;
end;

procedure TFormMain.LstNumbersClick(Sender: TObject);
begin
  CheckDeleteEnabled;
end;

procedure TFormMain.FormShow(Sender: TObject);
begin
  Randomize;
  BtReset.Click;
  CheckDeleteEnabled;
end;

procedure TFormMain.LstNumbersKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if (Key = VK_DELETE) and BtDeleteSelected.Enabled then
    BtDeleteSelected.Click;
end;

procedure TFormMain.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if (Key = VK_F5) and BtDraw.Enabled then
    BtDraw.Click;
end;

procedure TFormMain.BtAboutClick(Sender: TObject);
begin
  FormAbout := TFormAbout.Create(Application);
  FormAbout.ShowModal;
  FormAbout.Free;
end;

function TFormMain.ItemExists(AItemText: String): Boolean;
begin
  Result := LstNumbers.Items.IndexOf(AItemText) >= 0;
end;

procedure TFormMain.CheckDeleteEnabled;
begin
  BtDeleteSelected.Enabled := LstNumbers.SelCount <> 0;
end;

end.

