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
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls, Buttons, Windows,
  fpjson;

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
    BtConfig: TSpeedButton;
    procedure BtDeleteSelectedClick(Sender: TObject);
    procedure BtResetClick(Sender: TObject);
    procedure BtDrawClick(Sender: TObject);
    procedure LstNumbersSelectionChange(Sender: TObject; User: boolean);
    procedure LstNumbersClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure LstNumbersKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure BtAboutClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure BtConfigClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
  private
    FConfigFilePath: String;
    FTime: Integer;
    function ItemExists(AItemText: String): Boolean;
    procedure CheckDeleteEnabled;
    procedure LoadConfig;
    procedure SaveConfig;
  public

  end;

var
  FormMain: TFormMain;

implementation

uses FAbout, FConfig;

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
  I, Steps, Total, RangeStart, RangeEnd, RandomNumber: Integer;
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
    Steps := FTime * 4;
    I := 0;
    repeat
      LbDrawnNumber.Font.Color := clMedGray;

      if LstNumbers.Count = Total then Exit;
      repeat
        RandomNumber := Random(Total) + RangeStart;
      until (not ItemExists(IntToStr(RandomNumber))) or (LstNumbers.Count = Total);

      LbDrawnNumber.Caption := IntToStr(RandomNumber);
      Application.ProcessMessages;

      if I < Steps then
      begin
        Sleep(250);
        Inc(I);
      end;
    until I >= Steps;

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
  // Set config default values
  FTime         := 1;
  EditFrom.Text := '1';
  EditTo.Text   := '10';

  // Try to load settings from file
  LoadConfig;

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

procedure TFormMain.FormCreate(Sender: TObject);
begin
  FConfigFilePath := IncludeTrailingPathDelimiter(ExtractFilePath(Application.ExeName)) + 'config.json';
end;

procedure TFormMain.BtConfigClick(Sender: TObject);
begin
  FormConfig := TFormConfig.Create(Self);
  FormConfig.EdTime.Value := FTime;
  if FormConfig.ShowModal = mrOk then
  begin
    FTime := FormConfig.EdTime.Value;
    SaveConfig;
  end;
  FormConfig.Free;
end;

procedure TFormMain.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  SaveConfig;
end;

function TFormMain.ItemExists(AItemText: String): Boolean;
begin
  Result := LstNumbers.Items.IndexOf(AItemText) >= 0;
end;

procedure TFormMain.CheckDeleteEnabled;
begin
  BtDeleteSelected.Enabled := LstNumbers.SelCount <> 0;
end;

procedure TFormMain.LoadConfig;
var
  ConfigStrs : TStrings;
  ConfigJSON : TJSONObject;
begin
  if not FileExists(FConfigFilePath) then
  begin
    // File does not exist, save default settings and exit (at this point default values are already set)
    SaveConfig;
    Exit;
  end;

  ConfigStrs := TStringList.Create;
  ConfigStrs.LoadFromFile(FConfigFilePath);

  ConfigJSON := GetJSON(ConfigStrs.Text) as TJSONObject;

  EditFrom.Text := ConfigJSON.FindPath('range.start').AsString;
  EditTo.Text   := ConfigJSON.FindPath('range.end').AsString;
  FTime         := ConfigJSON.FindPath('time').AsInteger;

  ConfigStrs.Free;
  ConfigJSON.Free;
end;

procedure TFormMain.SaveConfig;
var
  ConfigStrs : TStrings;
  ConfigJSON : TJSONObject;
  Range      : TJSONObject;
begin
  ConfigStrs := TStringList.Create;

  ConfigJSON := TJSONObject.Create();
  try
    Range := TJSONObject.Create(['start', StrToIntDef(EditFrom.Text, 1), 'end', StrToIntDef(EditTo.Text, 10)]);
    ConfigJSON.Add('range', Range);
    ConfigJSON.Add('time', FTime);

    ConfigStrs.Text := ConfigJSON.FormatJSON();
  finally
    ConfigJSON.Free;
  end;

  ConfigStrs.SaveToFile(FConfigFilePath);
  ConfigStrs.Free;
end;

end.

